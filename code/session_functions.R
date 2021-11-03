library(scales)

# Get helper functions
source("code/helper_functions.R")

# Functions that take in a single session and output relevant metrics to use as predictors
# First arguments passed to each function is assumed to be something like:
df <- hexo %>%
  filter(Session==2) %>%
  mutate(HeartRateValid=valid_measurement(HeartRate),
         BreathingRateValid=valid_measurement(BreathingRate)) %>%
  filter(HeartRateValid, BreathingRateValid) %>%
  arrange(Time)

# For each of night (2300-0900), day (0900-2300), and rolling window (7200 seconds before the measurement times):
# -compute 
#   HR mean, HR sd, HR rms, 
#   BR mean, BR sd, BR rms
#   Cadence total (raw) / time, Cadence spikes (raw) / time, Cadence number motion periods /time, Cadence total motion period / time

# Night ----
get_night_data <- function(df, measurement, window_length=7200) {
  period <- "N"
  if (measurement %in% c(1)) {
    N_C_total <- NA
    N_C_spikes <- NA
    N_C_motion_number <- NA
    N_C_motion_time <- NA
    N_HR_mean <- NA
    N_HR_sd <- NA
    N_HR_rms <- NA
    N_BR_mean <- NA
    N_BR_sd <- NA
    N_BR_rms <- NA
  } else {
    dfs <- subset_session_hexodata(df, measurement = measurement, period=period, window_length = window_length)
    cadence <- detect_spikes(dfs, l=60, d=600, debug=F)
    tdiff <-  as.numeric(max(dfs$Time) - min(dfs$Time), units="secs")
    if (length(cadence)==1) {
      N_C_total <- sum(dfs$Cadence) / tdiff
      N_C_spikes <- 0
      N_C_motion_number <- 0
      N_C_motion_time <- 0
    } else {
      N_C_total <- sum(dfs$Cadence) / tdiff
      N_C_spikes <- 1000*max(cadence$spike_summary$Spike) / tdiff
      N_C_motion_number <- 1000*max(cadence$motion_summary$Motion_Event) / tdiff
      N_C_motion_time <- sum(cadence$motion_summary$Motion_Length) / tdiff
    }
    N_HR_mean <- get_mean(dfs, col = "HeartRate")
    N_HR_sd <- get_variability_sd(dfs, col="HeartRate")
    N_HR_rms <- get_variability_rms(dfs, col="HeartRate")
    N_BR_mean <- get_mean(dfs, col = "BreathingRate")
    N_BR_sd <- get_variability_sd(dfs, col="BreathingRate")
    N_BR_rms <- get_variability_rms(dfs, col="BreathingRate")
  }
  
  return(
    list(
      N_C_total = N_C_total,
      N_C_spikes = N_C_spikes,
      N_C_motion_number = N_C_motion_number,
      N_C_motion_time = N_C_motion_time,
      N_HR_mean = N_HR_mean,
      N_HR_sd = N_HR_sd,
      N_HR_rms = N_HR_rms,
      N_BR_mean = N_BR_mean,
      N_BR_sd = N_BR_sd,
      N_BR_rms = N_BR_rms
    )
  )
}

# Day ----
get_day_data <- function(df, measurement, window_length=7200) {
  period <- "D"
  if (measurement %in% c(1,2)) {
    D_C_total <- NA
    D_C_spikes <- NA
    D_C_motion_number <- NA
    D_C_motion_time <- NA
    D_HR_mean <- NA
    D_HR_sd <- NA
    D_HR_rms <- NA
    D_BR_mean <- NA
    D_BR_sd <- NA
    D_BR_rms <- NA
  } else {
    dfs <- subset_session_hexodata(df, measurement = measurement, period=period, window_length = window_length)
    cadence <- detect_spikes(dfs, l=60, d=600, debug=F)
    tdiff <-  as.numeric(max(dfs$Time) - min(dfs$Time), units="secs")
    if (length(cadence)==1) {
      D_C_total <- sum(dfs$Cadence) / tdiff
      D_C_spikes <- 0
      D_C_motion_number <- 0
      D_C_motion_time <- 0
    } else {
      D_C_total <- sum(dfs$Cadence) / tdiff
      D_C_spikes <- 1000*max(cadence$spike_summary$Spike) / tdiff
      D_C_motion_number <- 1000*max(cadence$motion_summary$Motion_Event) / tdiff
      D_C_motion_time <- sum(cadence$motion_summary$Motion_Length) / tdiff
    }
    D_HR_mean <- get_mean(dfs, col = "HeartRate")
    D_HR_sd <- get_variability_sd(dfs, col="HeartRate")
    D_HR_rms <- get_variability_rms(dfs, col="HeartRate")
    D_BR_mean <- get_mean(dfs, col = "BreathingRate")
    D_BR_sd <- get_variability_sd(dfs, col="BreathingRate")
    D_BR_rms <- get_variability_rms(dfs, col="BreathingRate")
  }
  
  return(
    list(
      D_C_total = D_C_total,
      D_C_spikes = D_C_spikes,
      D_C_motion_number = D_C_motion_number,
      D_C_motion_time = D_C_motion_time,
      D_HR_mean = D_HR_mean,
      D_HR_sd = D_HR_sd,
      D_HR_rms = D_HR_rms,
      D_BR_mean = D_BR_mean,
      D_BR_sd = D_BR_sd,
      D_BR_rms = D_BR_rms
    )
  )
}

# Window ----
get_window_data <- function(df, measurement, window_length=7200) {
  period <- "W"
  if (measurement %in% c(1)) {
    W_C_total <- NA
    W_C_spikes <- NA
    W_C_motion_number <- NA
    W_C_motion_time <- NA
    W_HR_mean <- NA
    W_HR_sd <- NA
    W_HR_rms <- NA
    W_BR_mean <- NA
    W_BR_sd <- NA
    W_BR_rms <- NA
  } else {
    dfs <- subset_session_hexodata(df, measurement = measurement, period=period, window_length = window_length)
    cadence <- detect_spikes(dfs, l=60, d=600, debug=F)
    tdiff <-  as.numeric(max(dfs$Time) - min(dfs$Time), units="secs")
    if (length(cadence)==1) {
      W_C_total <- sum(dfs$Cadence) / tdiff
      W_C_spikes <- 0
      W_C_motion_number <- 0
      W_C_motion_time <- 0
    } else {
      W_C_total <- sum(dfs$Cadence) / tdiff
      W_C_spikes <- 1000*max(cadence$spike_summary$Spike) / tdiff
      W_C_motion_number <- 1000*max(cadence$motion_summary$Motion_Event) / tdiff
      W_C_motion_time <- sum(cadence$motion_summary$Motion_Length) / tdiff
    }
    W_HR_mean <- get_mean(dfs, col = "HeartRate")
    W_HR_sd <- get_variability_sd(dfs, col="HeartRate")
    W_HR_rms <- get_variability_rms(dfs, col="HeartRate")
    W_BR_mean <- get_mean(dfs, col = "BreathingRate")
    W_BR_sd <- get_variability_sd(dfs, col="BreathingRate")
    W_BR_rms <- get_variability_rms(dfs, col="BreathingRate")
  }
  
  return(
    list(
      W_C_total = W_C_total,
      W_C_spikes = W_C_spikes,
      W_C_motion_number = W_C_motion_number,
      W_C_motion_time = W_C_motion_time,
      W_HR_mean = W_HR_mean,
      W_HR_sd = W_HR_sd,
      W_HR_rms = W_HR_rms,
      W_BR_mean = W_BR_mean,
      W_BR_sd = W_BR_sd,
      W_BR_rms = W_BR_rms
    )
  )
}

# Combine ---
get_all_data <- function(df, window_length=7200) {

  dfm_list <- list()
  for (measurement in 1:5) {
    session <- df$Session[1]
    permutation <- cog %>%
      filter(Session==session) %>%
      pull(Permutation)
    activity <- perminfo %>%
      filter(Permutation==permutation, Measurement==measurement) %>%
      pull(Activity)
    
    dfm <- cbind(tibble(Session=session, Permutation=permutation, Activity=activity, measurement=measurement, window_length = window_length),
                 as_tibble(get_night_data(df, measurement = measurement, window_length = window_length)),
                 as_tibble(get_day_data(df, measurement = measurement, window_length = window_length)),
                 as_tibble(get_window_data(df, measurement = measurement, window_length = window_length)))
    dfm_list[[measurement]] <- as_tibble(dfm)
  }
  dfm <- bind_rows(dfm_list)
  return(dfm)
}


# For night period + hour around ("Possible sleep period", 1100-1000), estimate:
#   Hours of sleep (non motion), Number of sleep events, average length of sleep event
#   Mean over sleep events each night:
#     HR mean, HR sd, HR rms
#     BR mean, BR sd, BR rms
#   HR FT:
#     Peak frequency, power
#   BR FT:
#     Peak frequency, power

# Sleep ----
# sleep_total_cadence <- function(df, sleep_session, debug=F) {
#   sleep_time_range <- get_sleep_time_range(df, sleep_session)
#   df_sleep <- df %>%
#     filter(Time >= sleep_time_range[1] & Time <= sleep_time_range[2])
#   
#   if (debug) {
#     df_sleep %>%
#       ggplot(aes(x=Time, y=Cadence)) +
#       geom_path() + 
#       scale_x_datetime(breaks = date_breaks("1 hour"), date_labels = "%H:%M") +
#       theme_bw() + 
#       theme(axis.text.x = element_text(angle=45, hjust=1))
#   }
#   
#   # Define based
# }
  

