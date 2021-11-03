library(tidyverse)
library(glue)
library(tibbletime)
library(scales)

# Function to detect invalid measurements
# Invalid measurement if not changing in a 2 minute period
valid_measurement <- rollify(function(x) {!(length(unique(x))==1)}, window=120)

# Cadence filtering
# Assume cadence vector is sampled every second (no missing data in between)
# Remove spikes that are less than a certain width and not near any other ones
# Then group spikes within larger movement events
detect_spikes <- function(df, l=60, d=600, debug=F) {
  # Spike detection - label each set of consecutive nonzero cadence measurements as a single spike
  nonzero_measurements <- which(df$Cadence != 0)
  new_nonzero_measurements <- c(1, which((nonzero_measurements - lag(nonzero_measurements, 1)) != 1), length(nonzero_measurements) + 1)
  kvec <- integer(length(nonzero_measurements))
  for (i in 2:length(new_nonzero_measurements)) {
    kvec[new_nonzero_measurements[i-1]:(new_nonzero_measurements[i]-1)] <- i-1
  }
  
  # Put spike data into tibble
  spike_df <- tibble(measurement_indices=nonzero_measurements, Spike=kvec)
  
  # Get time difference between spike events
  # This can probably be optimized a bit
  n_spikes <- max(kvec)
  Quiet_Time_Previous <- Quiet_Time_Next <- integer(length = n_spikes)
  prog <- txtProgressBar(max=n_spikes, style=3)
  for (spike in 1:n_spikes) {
    
    # Before
    if (spike==1) {
      Quiet_Time_Previous[spike] <- NA
    } else {
      t_start <- spike_df %>% filter(Spike==(spike-1)) %>% pull(measurement_indices) %>% max()
      t_end <- spike_df %>% filter(Spike==spike) %>% pull(measurement_indices) %>% min()
      Quiet_Time_Previous[spike] <- t_end - t_start
    }
    
    # After
    if (spike==n_spikes) {
      Quiet_Time_Next[spike] <- NA
    } else {
      t_start <- spike_df %>% filter(Spike==spike) %>% pull(measurement_indices) %>% max()
      t_end <- spike_df %>% filter(Spike==(spike+1)) %>% pull(measurement_indices) %>% min()
      Quiet_Time_Next[spike] <- t_end - t_start    
    }
    
    setTxtProgressBar(prog, spike)
  }
  close(prog)
  
  # Summarize spike data and define motion events
  spike_summary <- spike_df %>%
    group_by(Spike) %>%
    summarize(Temporal_Length=length(measurement_indices)) %>%
    mutate(Quiet_Time_Before=Quiet_Time_Previous,
           Quiet_Time_After=Quiet_Time_Next) %>%
    mutate(Valid_Spike=(Temporal_Length >= l | (Quiet_Time_Before <= d | Quiet_Time_After <= d)))
  spike_summary$Valid_Spike[1] <- spike_summary$Temporal_Length[1] >= l | spike_summary$Quiet_Time_After[1] <= d
  spike_summary$Valid_Spike[n_spikes] <- spike_summary$Temporal_Length[n_spikes] >= l | spike_summary$Quiet_Time_Before[n_spikes] <= d
  
  if (nrow(spike_summary) < 2) {
    return(NA)
  }
  
  # Detect time between valid spikes
  accumulated <- 0
  quiet_time_accumulated <- integer(n_spikes)
  quiet_time_accumulated[1] <- 0
  for (spike in 2:n_spikes) {
    accumulated <- accumulated + spike_summary$Quiet_Time_Before[spike]
    if (spike_summary$Valid_Spike[spike]) {
      quiet_time_accumulated[spike] <- accumulated
      accumulated <- 0
    } else {
      quiet_time_accumulated[spike] <- accumulated
    }
  }
  
  # Detect new events
  spike_summary <- spike_summary %>%
    mutate(Quiet_Time_Accumulated=quiet_time_accumulated) %>%
    mutate(New_Event=(Valid_Spike & (Quiet_Time_Accumulated >= d | is.na(Quiet_Time_Before)))) %>%
    mutate(cumsum=cumsum(New_Event)) %>%
    mutate(Motion_Event=case_when(Valid_Spike ~ cumsum,
                                  !Valid_Spike ~ 0L)) %>%
    select(-cumsum) %>%
    mutate(Valid_Motion=(Motion_Event != 0))
  
  # Spike and motion vectors
  spike_vec <- motion_vec <- integer(nrow(df))
  valid_spike_vec <- valid_motion_vec <- rep(F, length.out=nrow(df))
  for (spike in 1:n_spikes) {
    spike_indices <- spike_df %>%
      filter(Spike==spike) %>%
      pull(measurement_indices)
    
    spike_vec[spike_indices] <- spike_summary %>% filter(Spike==spike) %>% pull(Spike)
    motion_vec[spike_indices] <- spike_summary %>% filter(Spike==spike) %>% pull(Motion_Event)
    valid_spike_vec[spike_indices] <- spike_summary %>% filter(Spike==spike) %>% pull(Valid_Spike)
    valid_motion_vec[spike_indices] <- spike_summary %>% filter(Spike==spike) %>% pull(Valid_Motion)
  }
  
  # Label spikes and motion events
  df_spike <- df %>% 
    mutate(Spike=spike_vec,
           Valid_Spike=valid_spike_vec,
           Motion_Event=motion_vec,
           Valid_Motion=valid_motion_vec)
  
  n_motions <- max(df_spike$Motion_Event, na.rm = T)
  if (n_motions >= 1) {
    Motion_Start_Time <- Motion_End_Time <- integer(n_motions)
    class(Motion_Start_Time) <- class(Motion_End_Time) <- c("POSIXct", "POSIXt")
    Motion_Start_Index <- Motion_End_Index <- integer(n_motions)
    motion_period <- integer(nrow(df))
    for (motion in 1:n_motions) {
      motion_start_time <- df_spike %>% filter(Motion_Event==motion) %>% pull(Time) %>% min()
      motion_end_time <- df_spike %>% filter(Motion_Event==motion) %>% pull(Time) %>% max()
      motion_start_index <- which(df_spike$Time==motion_start_time)
      motion_end_index <- which(df_spike$Time==motion_end_time)
      Motion_Start_Time[motion] <- as_datetime(motion_start_time)
      Motion_End_Time[motion] <- as_datetime(motion_end_time)
      Motion_Start_Index[motion] <- motion_start_index
      Motion_End_Index[motion] <- motion_end_index
      motion_period[motion_start_index:motion_end_index] <- motion
    }
    motion_period[motion_period==0] <- NA
    df_spike$Motion_Period <- motion_period
    
    # Summarize motion data
    motion_summary <- tibble(Motion_Event=1:n_motions, 
                             Motion_Start_Time=Motion_Start_Time, Motion_End_Time=Motion_End_Time,
                             Motion_Start_Index=Motion_Start_Index, Motion_End_Index=Motion_End_Index) %>%
      mutate(Motion_Length=(Motion_End_Index - Motion_Start_Index))
  } else {
    motion_summary <- tibble(Motion_Event=0, 
                             Motion_Start_Time=NA, Motion_End_Time=NA,
                             Motion_Start_Index=NA, Motion_End_Index=NA) %>%
      mutate(Motion_Length=0)
  }

  
  # Output
  out <- list(data=df_spike, spike_summary=spike_summary, motion_summary=motion_summary)
  
  if (debug) {
    # Plot spikes and motion event bands 
    df_spike %>%
      ggplot() +
      geom_rect(aes(xmin=Motion_Start_Time, xmax=Motion_End_Time, ymin=0, ymax=220), data=motion_summary) +
      geom_path(aes(x=Time, y=HeartRate), colour='grey50', alpha=0.5) +
      geom_path(aes(x=Time, y=Cadence, color=Valid_Spike, group=1)) + 
      scale_x_datetime(breaks = date_breaks("1 hour"), date_labels = "%b %d %H:%M") +
      scale_colour_manual(name="Removed\ncadence\nmeas.", values=c("black", "red")) +
      labs(title="Session 2") +
      theme_bw() + 
      theme(axis.text.x = element_text(angle=45, hjust=1))
    
    # Print plot
    print(plt)
    
    # Output debug
    out$debug <- plt
  }
  
  # Return
  return(out)
  
}

# Mean and variability for subsetting data
get_mean <- function(df, col="HeartRate", window=NA) {
  if (is.na(window)) {
    rolling_mean <- mean
  } else {
    rolling_mean <- rollify(mean, window=window)
  }
  out <- df %>%
    pull(col) %>%
    rolling_mean
  
  return(out)
}

get_variability_sd <- function(df, col="HeartRate", window=NA) {
  if (is.na(window)) {
    rolling_sd <- sd
  } else {
    rolling_sd <- rollify(sd, window=window)
  }
  out <- df %>%
    pull(col) %>%
    rolling_sd
  
  return(out)
}

get_variability_rms <- function(df, col="HeartRate", window=NA) {
  rms <- function(x) {
    return(sqrt(mean(x^2)))
  }
  
  if (is.na(window)) {
    rolling_rms <- rms
  } else {
    rolling_rms <- rollify(rms, window=window)
  }
  out <- df %>%
    pull(col) %>%
    rolling_rms
  
  return(out)
}

# Get measurement datetime
get_measurement_datetime <- function(df, measurement) {
  measurement_row <- tpinfo %>%
    filter(Measurement==measurement)
  dates <- unique(sort(date(df$Time)))
  measurement_datetime <- dates[measurement_row$Day]
  hour(measurement_datetime) <- hour(measurement_row$Time)
  out <- list(date=dates[measurement_row$Day], datetime=measurement_datetime)
  return(out)
}

# Subset data based on temporal window related to measurement
subset_session_hexodata <- function(df, measurement, period="W", window_length=7200) {
  measurement_dt <- get_measurement_datetime(df, measurement = measurement)
  if (startsWith(tolower(period), "n")) {
    t_start <- measurement_dt$date - 1
    t_end <- measurement_dt$date
    hour(t_start) <- 23
    hour(t_end) <- 09
  } else if (startsWith(tolower(period), "d")) {
    t_start <- measurement_dt$date
    t_end <- measurement_dt$date
    hour(t_start) <- 09
    hour(t_end) <- 23
  } else if (startsWith(tolower(period), "w")) {
    t_end <- measurement_dt$datetime
    t_start <- t_end - seconds(window_length)
  } else {
    t_start <- min(df$Time)
    t_end <- max(df$Time)
  }
  out <- df %>%
    filter(Time >= t_start, Time <= t_end)
  return(out)
}

# # Get sleep search range
# get_sleep_time_range <- function(df, sleep_session) {
#   dates <- unique(sort(date(df$Time)))
#   sleep_time_min <- as_date(dates[sleep_session])
#   wake_time_max <- as_date(sleep_time_min + 1)
#   hour(sleep_time_min) <- 22
#   hour(wake_time_max) <- 10
#   return(c(sleep_time_min, wake_time_max))
# }
# 
# get_sleep_estimate <- function(df, sleep_session) {
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
# }