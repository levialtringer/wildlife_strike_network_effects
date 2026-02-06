# ==========================================
# 1. LOAD LIBRARIES & DATA
# ==========================================
library(arrow)
library(dplyr)
library(lubridate)
library(fixest)
library(future)
library(furrr)
library(purrr)
library(ggplot2)
library(patchwork)

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
lag_cols  <- paste0("lag", 1:12)
lead_cols <- paste0("lead", 1:12)
WITHIN_TREATMENT <- lag_cols
CROSS_TREATMENT  <- c(lead_cols[-1], lag_cols)

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


master_df <- master_df %>%
  group_by(strike_index_nr) %>%
  mutate(
    # Total flights at the airports involved during the strike window
    total_strike_context_flights = n(),
    
    # Flights operated by the airline actually on strike
    struck_airline_flights = sum(is_strike_airline == 1),
    
    # The 'Strike Concentration' - how much of the local capacity was "held" by the struck carrier
    strike_conc = struck_airline_flights / total_strike_context_flights
  ) %>%
  ungroup()



# ==========================================
# 3. FUNCTION DEFINITIONS
# ==========================================

# Helper to build the formula string dynamically
build_formula <- function(dv, indeps, fes) {
  # Format: DV ~ Var1 + Var2 | FE1 + FE2
  as.formula(paste(dv, "~", paste(indeps, collapse = " + "), "|", paste(fes, collapse = " + ")))
}

run_within_airline_event_study <- function(df, dv) {
  # 1. Subset to treated airline (Within-Airline Strategy)
  df_treated <- df %>% filter(is_strike_airline == 1)
  
  # 2. Filter variables available in current slice
  current_indeps <- intersect(c(WITHIN_TREATMENT, COVARIATES), colnames(df_treated))
  current_fes <- intersect(FIXED_EFFECTS, colnames(df_treated))
  
  message(paste("Running 'Within-Airline Estimator' on", format(nrow(df_treated), big.mark=","), "observations..."))
  
  # 3. Estimate using feols (Fast Fixed Effects OLS)
  # 'fml' uses the pipe | to separate exogenous vars from fixed effects
  # feols handles collinearity/absorption automatically
  model <- feols(
    fml = build_formula(dv, current_indeps, current_fes),
    data = df_treated,
    vcov = "hetero" # Equivalent to 'heteroskedastic'
  )
  
  return(model)
}

process_res <- function(model) {
  if (is.null(model)) return(NULL)
  
  # Use coeftable to get names and stats reliably
  ct <- as.data.frame(coeftable(model))
  
  df_res <- data.frame(
    term = rownames(ct),
    coef = ct[, 1],
    se   = ct[, 2]
  ) %>%
    # Filter only for 'lag' variables to avoid trying to parse hours from 'snow_dum'
    filter(grepl("lag", term)) %>% 
    mutate(
      lower = coef - (1.96 * se),
      upper = coef + (1.96 * se),
      # Improved regex: find the number specifically after the word 'lag'
      hour = as.numeric(gsub(".*?lag(\\d+).*", "\\1", term)) * 4 
    )
  
  return(df_res)
}

run_cross_airline_event_study <- function(df, dv) {
  # 1. Filter variables available in current slice
  current_indeps <- intersect(c(CROSS_TREATMENT, COVARIATES), colnames(df))
  current_fes <- intersect(FIXED_EFFECTS, colnames(df))
  
  message(paste("Running 'Cross-Airline Estimator' on", format(nrow(df), big.mark=","), "observations..."))
  
  # 2. Estimate
  model <- feols(
    fml = build_formula(dv, current_indeps, current_fes),
    data = df,
    vcov = "hetero"
  )
  
  return(model)
}


get_dodged_era_comparison_fixed_axis <- function(testing_mode, df, damage, arrival_departure, dv = "departure_delay_minutes", min_conc = 0, max_conc = 1) {
  
  # 1. Initial Filter
  df_filtered <- df %>%
    dplyr::filter(strike_damage == damage, 
                  strike_arrival_departure == arrival_departure,
                  strike_conc >= min_conc,
                  strike_conc <= max_conc)
  
  if (testing_mode) df_filtered <- df_filtered %>% sample_frac(0.1)
  
  # 2. GLOBAL WEIGHTING (Calculated once for the entire 2017-2024 period)
  # This ensures we use the same "typical" flight volume for both eras
  global_counts <- purrr::map_df(1:12, function(i) {
    avg_count <- df_filtered %>%
      dplyr::filter(hours_from_strike > (i-1)*4, hours_from_strike <= i*4, is_strike_airline == 1) %>%
      dplyr::group_by(strike_index_nr) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
      dplyr::summarise(mean_n = mean(n, na.rm = TRUE))
    
    tibble::tibble(hour = i * 4, flight_count = ifelse(is.na(avg_count$mean_n), 0, avg_count$mean_n))
  })
  
  # 3. Data Preparation Block
  prepare_era_data <- function(sub_df, global_w) {
    if (nrow(sub_df) == 0) return(NULL)
    
    # Era-Specific Baseline (Still needs to be era-specific for the secondary axis)
    baseline_val <- sub_df %>%
      dplyr::filter(is_strike_airline == 1, hours_from_strike < 0, hours_from_strike >= -48) %>%
      dplyr::group_by(strike_index_nr) %>%
      dplyr::summarise(total_pre_delay = sum(get(dv), na.rm = TRUE), .groups = "drop") %>%
      dplyr::summarise(avg_baseline = mean(total_pre_delay, na.rm = TRUE)) %>%
      dplyr::pull(avg_baseline)
    
    calc_cumul <- function(mod) {
      if (is.null(mod)) return(list(coef = NA, ci = NA))
      proc <- process_res(mod) %>% 
        dplyr::filter(hour >= 0) %>% 
        dplyr::inner_join(global_w, by = "hour") %>% # Using the GLOBAL weights here
        dplyr::mutate(v_coef = coef * flight_count,
                      v_se = ((upper - lower) / 3.92) * flight_count)
      return(list(coef = sum(proc$v_coef), ci = 1.96 * sqrt(sum(proc$v_se^2))))
    }
    
    w_res <- calc_cumul(tryCatch(run_within_airline_event_study(sub_df, dv), error = function(e) NULL))
    c_res <- calc_cumul(tryCatch(run_cross_airline_event_study(sub_df, dv), error = function(e) NULL))
    
    tibble::tibble(
      estimator = c("Within-Airline", "Cross-Airline"),
      coef = c(w_res$coef, c_res$coef),
      ci = c(w_res$ci, c_res$ci),
      baseline = baseline_val
    ) %>% dplyr::mutate(rel_impact = coef / baseline)
  }
  
  data_pre  <- prepare_era_data(df_filtered[df_filtered$year >= 2015 & df_filtered$year <= 2019, ], global_counts)
  data_post <- prepare_era_data(df_filtered[df_filtered$year >= 2020 & df_filtered$year <= 2024, ], global_counts)
  
  # Calculate Global Y Max for fixed axes
  global_y_max <- max(c(data_pre$coef + data_pre$ci, data_post$coef + data_post$ci), na.rm = TRUE) * 1.1
  
  # 4. Plotting Helper
  render_plot <- function(plot_df, era_label, y_limit) {
    ggplot(plot_df, aes(x = estimator, y = coef, color = estimator, shape = estimator)) +
      geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.4) +
      geom_errorbar(aes(ymin = coef - ci, ymax = coef + ci), width = 0.2, size = 1, position = position_dodge(0.5)) +
      geom_point(size = 5, fill = "white", stroke = 1.5, position = position_dodge(0.5)) +
      scale_color_manual(values = c("Within-Airline" = "blue", "Cross-Airline" = "black")) +
      scale_shape_manual(values = c("Within-Airline" = 22, "Cross-Airline" = 21)) +
      scale_y_continuous(
        limits = c(min(0, min(plot_df$coef - plot_df$ci, na.rm = TRUE)), y_limit),
        name = "Standardized Delay (Minutes)",
        sec.axis = sec_axis(~ . / unique(plot_df$baseline), 
                            name = "Rel. Impact (% Era Baseline)", 
                            labels = scales::percent_format())
      ) +
      theme_minimal() +
      labs(title = era_label, subtitle = paste("Baseline:", round(unique(plot_df$baseline), 1), "mins"), x = NULL) +
      theme(legend.position = "none", plot.title = element_text(face = "bold", hjust = 0.5))
  }
  
  p1 <- render_plot(data_pre, "Pre-COVID (2017-2019)", global_y_max)
  p2 <- render_plot(data_post, "Post-COVID (2020-2024)", global_y_max)
  
  return((p1 | p2) + patchwork::plot_annotation(title = "Standardized Volume Comparison: Within vs. Cross Estimators"))
}



all_df <- get_dodged_era_comparison_fixed_axis(testing_mode=FALSE, df=master_df, 
                                       damage=1, arrival_departure="Arrival", 
                                       dv = "departure_delay_minutes",
                                       min_conc = 0, max_conc = 1)

high_df <- get_dodged_era_comparison_fixed_axis(testing_mode=FALSE, df=master_df, 
                                               damage=1, arrival_departure="Arrival", 
                                               dv = "departure_delay_minutes",
                                               min_conc = 0.6, max_conc = 1)

all_df <- get_dodged_era_comparison_fixed_axis(testing_mode=FALSE, df=master_df, 
                                       damage=1, arrival_departure="Departure", 
                                       dv = "departure_delay_minutes",
                                       min_conc = 0, max_conc = 1)

high_df <- get_dodged_era_comparison_fixed_axis(testing_mode=FALSE, df=master_df, 
                                       damage=1, arrival_departure="Departure", 
                                       dv = "departure_delay_minutes",
                                       min_conc = 0.6, max_conc = 1)



plot_strike_era_comparison(df = master_df, 
                                       dv = "departure_delay_minutes", 
                                       damage = 1, 
                                       arrival_departure = "Departure",
                                       era1 = c(2015, 2019),
                                       era2 = c(2020, 2024), 
                                       min_conc = 0, max_conc = 1)
