library(tidyverse)
library(glue)
library(lubridate)
library(tibbletime)

median_filter <- rollify(mean, window = 7)

# Read data tables
cog <- read_csv("data/parsed/cognitive_parsed.csv")
hexo <- read_csv("data/parsed/hexodata.csv")
tpinfo <- read_csv("data/parsed/cognitive_timepoints.csv")
perminfo <- read_csv("data/parsed/permutations.csv")

# Reshape cog data
cog_reshaped <- cog %>% 
  gather(TestTimepoint, Score, IR_1:HR_5) %>%
  separate(TestTimepoint, into = c("Test", "Timepoint"), sep="_") %>%
  spread(Test, Score) %>%
  arrange(Session, Timepoint)

# Convert hexo time to datetime format
hexo

# Within each session, find latest timepoint after start and earliest timepoint 
s <- 4
d1 <- cog_reshaped %>% 
  filter(Session==s, Timepoint==1) %>%
  pull(Date_Start)

sleep_phase_1_sleep <- d1
sleep_phase_1_wake <- d1 + 1
sleep_phase_2_sleep <- d1 + 1
sleep_phase_2_wake <- d1 + 2

hour(sleep_phase_1_sleep) <- 22
hour(sleep_phase_1_wake) <- 10
hour(sleep_phase_2_sleep) <- 22
hour(sleep_phase_2_wake) <- 10

hexo %>%
  filter(Session==s) %>%
  arrange(Time) %>%
  filter(Time >= sleep_phase_1_sleep & Time <= sleep_phase_1_wake) %>%
  ggplot(aes(x=Time, y=Cadence)) +
  geom_path(aes(y=HeartRate), color='blue', size=0.5) +
  geom_path(size=1.5) + 
  theme_bw()

# PCA across all TP

