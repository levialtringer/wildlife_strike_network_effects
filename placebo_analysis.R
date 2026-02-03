
library(arrow)
library(dplyr)
library(lubridate)
library(fixest)
library(purrr)
library(tidyr)

# ---- Load Data ---- #
master_df <- read_parquet('data/flight_data.parquet')

# ---- Define Outcome Variables ---- #
# Vectorized creation of binary indicators
for (t in c(15, 30, 60, 120)) {
  master_df[[paste0("delay_gt_", t)]] <- as.numeric((master_df$departure_delay_minutes >= t) | (master_df$cancelled == 1))
}
master_df$departure_delay_minutes <- ifelse(master_df$cancelled == 1, 18, master_df$departure_delay_minutes)
OUTCOMES <- c('delay_gt_15', 'delay_gt_30', 'delay_gt_60', 'delay_gt_120', 'departure_delay_minutes')

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


run_placebo_bootstrap_analysis <- function(n_iterations, damage_val, arr_dep_val, dv = "departure_delay_minutes") {
  
  # Initial Filter
  df_base <- master_df %>%
    filter(strike_damage == damage_val, strike_arrival_departure == arr_dep_val)
  
  lags <- paste0("lag", 1:12)
  formula_str <- as.formula(
    paste(dv, "~", paste(c(lags, COVARIATES), collapse = " + "), "|", paste(FIXED_EFFECTS, collapse = " + "))
  )
  
  results_list <- list()
  cat(paste("Starting", n_iterations, "Placebo Bootstrap iterations...\n"))
  
  for (i in 1:n_iterations) {
    try({
      # 1. BOOTSTRAP STEP: Sample strikes with replacement
      # This handles the duplicates that %in% would ignore
      selected_strikes_df <- data.frame(
        strike_index_nr = sample(unique(df_base$strike_index_nr), replace = TRUE)
      )
      
      # 2. JOIN STEP: Keep all copies of sampled strikes and remove true treated airline
      df_iter <- selected_strikes_df %>%
        left_join(df_base, by = "strike_index_nr", relationship = "many-to-many") %>%
        filter(is_strike_airline == 0)
      
      # 3. PLACEBO STEP: Randomly select 1 placebo airline per (sampled) strike
      df_iter <- df_iter %>%
        group_by(strike_index_nr) %>%
        mutate(is_strike_airline = as.numeric(carrier_code == sample(unique(carrier_code), 1))) %>%
        ungroup()
      
      # 4. REDEFINE LAGS based on fake assignment
      h <- df_iter$hours_from_strike
      is_p <- df_iter$is_strike_airline == 1
      for (lag_i in 1:12) {
        df_iter[[paste0("lag", lag_i)]] <- as.numeric(h > (lag_i-1)*4 & h <= lag_i*4 & is_p)
      }
      
      # 5. SUBSET TO TREATED ONLY (Match Within-Estimator logic)
      df_treated_only <- df_iter %>% filter(is_strike_airline == 1)
      
      # 6. ESTIMATE
      model_res <- feols(formula_str, data = df_treated_only, vcov = "iid")
      
      # 7. EXTRACT
      tidy_res <- data.frame(
        iteration = i,
        term = names(coef(model_res)),
        estimate = as.numeric(coef(model_res))
      ) %>%
        filter(term %in% lags)
      
      results_list[[i]] <- tidy_res
      
      if (i %% 10 == 0) cat(paste("Iteration", i, "done\n"))
      
    }, silent = FALSE)
  }
  
  final_df <- bind_rows(results_list)
  
  # Formatting for save
  dmg_label <- if_else(damage_val == 1, "damaging", "non_damaging")
  dir.create("bootstrap_results/placebo", recursive = TRUE, showWarnings = FALSE)
  write.csv(final_df, sprintf("bootstrap_results/placebo/%s_%s_%s.csv", dmg_label, tolower(arr_dep_val), dv), row.names = FALSE)
  
  return(final_df)
}

run_actual_bootstrap_analysis <- function(n_iterations, damage_val, arr_dep_val, dv = "departure_delay_minutes") {
  
  # Initial Filter
  df_base <- master_df %>%
    filter(strike_damage == damage_val, strike_arrival_departure == arr_dep_val)
  
  # Construct the regression formula
  # i(lag1, lag2...) is handled via paste for simplicity
  lags <- paste0("lag", 1:12)
  formula_str <- as.formula(
    paste(dv, "~", paste(c(lags, COVARIATES), collapse = " + "), "|", paste(FIXED_EFFECTS, collapse = " + "))
  )
  
  results_list <- list()
  
  cat(paste("Starting", n_iterations, "placebo iterations...\n"))
  
  for (i in 1:n_iterations) {
    try({
      # 1. Block Bootstrap: Sample strikes with replacement
      selected_df <- data.frame(strike_index_nr = sample(unique(df_base$strike_index_nr), replace = TRUE))
      
      # 2. Join to keep all copies of sampled strikes
      df_iter <- selected_df %>%
        left_join(df_base, by = "strike_index_nr", relationship = "many-to-many")
      
      # 3. Filter for the actual treated airline (as per your within-estimator)
      df_treated_only <- df_iter %>% filter(is_strike_airline == 1)
      
      # Using feols for speed
      model_res <- feols(formula_str, data = df_treated_only, vcov = "iid")
      
      # 6. Extract Coefficients and stats
      tidy_res <- data.frame(
        iteration = i,
        term = names(coef(model_res)),
        estimate = as.numeric(coef(model_res)),
        std_error = as.numeric(se(model_res))
      ) %>%
        filter(term %in% lags) # Keep only the lags
      
      results_list[[i]] <- tidy_res
      
      if (i %% 10 == 0) cat(paste("Iteration", i, "done\n"))
      
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

run_placebo_bootstrap_analysis(n_iterations = 500, damage_val=0, arr_dep_val="Arrival", dv = "departure_delay_minutes")
run_placebo_bootstrap_analysis(n_iterations = 500, damage_val=1, arr_dep_val="Arrival", dv = "departure_delay_minutes")
run_placebo_bootstrap_analysis(n_iterations = 500, damage_val=0, arr_dep_val="Departure", dv = "departure_delay_minutes")
run_placebo_bootstrap_analysis(n_iterations = 500, damage_val=1, arr_dep_val="Departure", dv = "departure_delay_minutes")

run_actual_bootstrap_analysis(n_iterations = 500, damage_val=0, arr_dep_val="Arrival", dv = "departure_delay_minutes")
run_actual_bootstrap_analysis(n_iterations = 500, damage_val=1, arr_dep_val="Arrival", dv = "departure_delay_minutes")
run_actual_bootstrap_analysis(n_iterations = 500, damage_val=0, arr_dep_val="Departure", dv = "departure_delay_minutes")
run_actual_bootstrap_analysis(n_iterations = 500, damage_val=1, arr_dep_val="Departure", dv = "departure_delay_minutes")
