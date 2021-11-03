library(tidyverse)
library(glue)
library(lubridate)

files <- list.files("data/raw/Hexoskin data updated", recursive = T, full.names = T, pattern = "*.csv")

df_list <- list()

for (f in files) {
  print(glue("Working on file: {f}"))
  
  metadata <- strsplit(gsub("data/raw/Hexoskin data updated/", "", f), "/")[[1]]
  
  meta_session <- as.integer(gsub("Session ", "", metadata[[1]]))
  meta_part <- as.integer(strsplit(metadata[[2]], " ")[[1]][[2]])
  meta_record <- as.integer(strsplit(metadata[[2]], "record_")[[1]][[2]])
  
  df <- read_csv(f) %>% 
    rename(Time=`time [s]`,
           HeartRate=`heart_rate [bpm] (/api/datatype/19/)`,
           BreathingRate=`breathing_rate [rpm] (/api/datatype/33/)`,
           Cadence=`cadence [spm] (/api/datatype/53/)cadence [spm] (/api/datatype/53/)`) %>%
    mutate(File=f, 
           Session=meta_session,
           Part=meta_part,
           Record=meta_record)
  
  df$Time <- as_datetime(sapply(strsplit(df$Time, ":"), function(x) {paste(x[1:3], collapse=":")}))

  
  
  df_list[[f]] <- df
}

df <- bind_rows(df_list)

write_csv(df, "data/parsed/hexodata.csv")
