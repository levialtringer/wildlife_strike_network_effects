
# ==========================================
# 1. LOAD LIBRARIES & DATA
# ==========================================
library(arrow)
library(dplyr)
library(lubridate)
library(fixest)
library(future)
library(furrr)

master_df <- read_parquet('data/flight_data.parquet')



# ==========================================
# 2. GLOBAL DATA ENGINEERING
# ==========================================

# ---- Define Outcome Variables ---- #
# Vectorized creation of binary indicators
for (t in c(15, 30, 60, 120)) {
  master_df[[paste0("delay_gt_", t)]] <- as.numeric((master_df$departure_delay_minutes >= t))
}
OUTCOMES <- c('delay_gt_15', 'delay_gt_30', 'delay_gt_60', 'delay_gt_120', 'departure_delay_minutes')
rm(t)
# ---- Define Treatment Variables (Event Study Engineering) ---- #
master_df <- master_df %>%
  mutate(
    scheduled_departure_datetime = as_datetime(scheduled_departure_datetime),
    strike_date_time = as_datetime(strike_date_time),
    # Calculate hours from strike
    hours_from_strike = as.numeric(difftime(scheduled_departure_datetime, strike_date_time, units = "hours")),
    is_strike_airline = as.numeric(carrier_code == strike_airline)
  )

# Function to generate lead/lag columns
# Note: In the placebo loop, we will regenerate these locally for speed
generate_event_study_cols <- function(data) {
  h <- data$hours_from_strike
  is_same <- data$is_strike_airline == 1
  
  for (i in 1:12) {
    # Leads (Pre-strike)
    data[[paste0("lead", i)]] <- as.numeric(h < -(i-1)*4 & h >= -i*4 & is_same)
    # Lags (Post-strike)
    data[[paste0("lag", i)]] <- as.numeric(h > (i-1)*4 & h <= i*4 & is_same)
  }
  return(data)
}

master_df <- generate_event_study_cols(master_df)

# ---- Define Covariate Variables ---- #
master_df <- master_df %>%
  mutate(
    hour_of_day = hour(scheduled_departure_datetime),
    date_only = as.Date(scheduled_departure_datetime)
  ) %>%
  group_by(airport, date_only, hour_of_day) %>%
  mutate(congestion = n()) %>%
  ungroup() %>%
  mutate(
    congestion_sq = congestion^2,
    temp_adj = temp + 25,
    temp_sq = temp_adj^2,
    windspeed_sq = windspeed^2,
    windgust_dum = if_else(is.na(windgust), 0, 1),
    precip_sq = precip^2,
    snow_dum = if_else(coalesce(snow, 0) > 0, 1, 0)
  )

COVARIATES <- c('congestion', 'congestion_sq', 'temp_adj', 'temp_sq', 
                'windspeed', 'windspeed_sq', 'windgust_dum', 
                'precip', 'precip_sq', 'snow_dum')

# ---- Define Fixed Effects ---- #
master_df <- master_df %>%
  mutate(
    airport_carrier = paste0(airport, "_", carrier_code),
    year = year(scheduled_departure_datetime),
    month = month(scheduled_departure_datetime),
    day_of_week = wday(scheduled_departure_datetime)
  )

FIXED_EFFECTS <- c('airport_carrier', 'hour_of_day', 'year', 'month', 'day_of_week')



# ==========================================
# 3. FUNCTION DEFINITIONS
# ==========================================

run_placebo_bootstrap_analysis <- function(n_iterations, damage_val, arr_dep_val, dv = "departure_delay_minutes") {
  
  # Filter early to reduce memory overhead
  df_base <- master_df %>% 
    filter(strike_damage == damage_val, strike_arrival_departure == arr_dep_val)
  
  lags <- paste0("lag", 1:12)
  formula_str <- as.formula(paste(dv, "~", paste(c(lags, COVARIATES), collapse = " + "), "|", paste(FIXED_EFFECTS, collapse = " + ")))
  
  results_list <- vector("list", n_iterations) # Pre-allocate list size
  
  for (i in 1:n_iterations) {
    try({
      # 1. Sample indices only (lightweight)
      selected_indices <- sample(unique(df_base$strike_index_nr), replace = TRUE)
      
      # 2. Reconstruct the iteration data
      # Use 'inner_join' if possible to keep it lean
      df_iter <- data.frame(strike_index_nr = selected_indices) %>%
        inner_join(df_base, by = "strike_index_nr", relationship = "many-to-many") %>%
        filter(is_strike_airline == 0)
      
      # 3. Placebo Assignment
      df_iter <- df_iter %>%
        group_by(strike_index_nr) %>%
        mutate(is_strike_airline = as.numeric(carrier_code == sample(unique(carrier_code), 1))) %>%
        ungroup() %>%
        filter(is_strike_airline == 1) # Filter to treated-only IMMEDIATELY
      
      # 4. Define Lags on the tiny subset
      h <- df_iter$hours_from_strike
      for (lag_i in 1:12) {
        df_iter[[paste0("lag", lag_i)]] <- as.numeric(h > (lag_i-1)*4 & h <= lag_i*4)
      }
      
      # 5. Estimate & Extract
      model_res <- feols(formula_str, data = df_iter, vcov = "iid")
      
      results_list[[i]] <- data.frame(
        iteration = i,
        term = names(coef(model_res)),
        estimate = as.numeric(coef(model_res))
      ) %>% filter(term %in% lags)
      
      # --- THE CLEANUP ---
      rm(df_iter, model_res, h) # Explicitly delete the heavy objects
      if (i %% 25 == 0) gc(full = TRUE) # Force garbage collection every 25 models
      
    }, silent = FALSE)
    
    if (i %% 10 == 0) cat(paste("Iteration", i, "done\n"))
  }
  
  final_df <- bind_rows(results_list)
  
  # Formatting for save
  dmg_label <- if_else(damage_val == 1, "damaging", "non_damaging")
  dir.create("bootstrap_results/placebo", recursive = TRUE, showWarnings = FALSE)
  write.csv(final_df, sprintf("bootstrap_results/placebo/%s_%s_%s.csv", dmg_label, tolower(arr_dep_val), dv), row.names = FALSE)
  
  return(final_df)
}

run_actual_bootstrap_analysis <- function(n_iterations, damage_val, arr_dep_val, dv = "departure_delay_minutes") {
  
  df_base <- master_df %>% 
    filter(strike_damage == damage_val, strike_arrival_departure == arr_dep_val, is_strike_airline == 1) # Filter here!
  
  lags <- paste0("lag", 1:12)
  formula_str <- as.formula(paste(dv, "~", paste(c(lags, COVARIATES), collapse = " + "), "|", paste(FIXED_EFFECTS, collapse = " + ")))
  
  results_list <- vector("list", n_iterations)
  
  for (i in 1:n_iterations) {
    try({
      selected_indices <- sample(unique(df_base$strike_index_nr), replace = TRUE)
      
      df_iter <- data.frame(strike_index_nr = selected_indices) %>%
        inner_join(df_base, by = "strike_index_nr", relationship = "many-to-many")
      
      model_res <- feols(formula_str, data = df_iter, vcov = "iid")
      
      results_list[[i]] <- data.frame(
        iteration = i,
        term = names(coef(model_res)),
        estimate = as.numeric(coef(model_res)),
        std_error = as.numeric(se(model_res))
      ) %>% filter(term %in% lags)
      
      # --- THE CLEANUP ---
      rm(df_iter, model_res)
      if (i %% 50 == 0) gc(full = TRUE)
      
    }, silent = FALSE)
  }
  
  # Combine and Save
  final_df <- bind_rows(results_list)
  
  # Formatting for save
  dmg_label <- if_else(damage_val == 1, "damaging", "non_damaging")
  dir.create("bootstrap_results/actual", recursive = TRUE, showWarnings = FALSE)
  write.csv(final_df, sprintf("bootstrap_results/actual/%s_%s_%s.csv", dmg_label, tolower(arr_dep_val), dv), row.names = FALSE)
  
  return(final_df)
}



# ==========================================
# 4. PRE-EXECUTION SLIMMING & TASK SETUP
# ==========================================

# Slim the data to the essentials
master_df <- master_df %>% 
  select(strike_index_nr, strike_damage, strike_arrival_departure, 
         carrier_code, is_strike_airline, hours_from_strike,
         all_of(OUTCOMES), all_of(COVARIATES), all_of(FIXED_EFFECTS), 
         starts_with("lag"))

# Define the 8 scenarios (2 damage levels x 2 types x 2 analysis types)
tasks <- expand.grid(
  damage_val = c(0, 1),
  arr_dep_val = c("Arrival", "Departure"),
  type = c("placebo", "actual"),
  stringsAsFactors = FALSE
)


# ==========================================
# 5. PARALLEL EXECUTION ENGINE
# ==========================================

# Increase the limit to 2GB (2000 * 1024^2 bytes)
options(future.globals.maxSize = 2000 * 1024^2)

# Plan for 4 workers (perfect for your 32GB RAM / 0.5GB data)
plan(multisession, workers = 4) 

cat("Engine started. Running 8 batches of 500 models...\n")

# future_pwalk automatically matches 'damage_val' from the tasks df 
# to 'damage_val' in your function arguments.
future_pwalk(tasks, function(damage_val, arr_dep_val, type) {
  
  # Note: message() will usually show up in the RStudio background jobs log
  message(sprintf("Starting: %s | Damage: %s | %s", type, damage_val, arr_dep_val))
  
  if (type == "placebo") {
    run_placebo_bootstrap_analysis(
      n_iterations = 500, 
      damage_val = damage_val, 
      arr_dep_val = arr_dep_val
    )
  } else {
    run_actual_bootstrap_analysis(
      n_iterations = 500, 
      damage_val = damage_val, 
      arr_dep_val = arr_dep_val
    )
  }
  
  # Force worker to dump memory after finishing its batch
  gc(full = TRUE)
  
}, .options = furrr_options(globals = TRUE, seed = TRUE))

# Shut down the workers and return to normal mode
plan(sequential)

cat("\n--- ALL JOBS COMPLETE. CHECK 'bootstrap_results' FOLDER. ---\n")

# 2. Run the specific missing combo manually
cat("Starting manual run for: Actual | Damage: 1 | Departure\n")
    