library(tidyverse)
library(glue)
library(lubridate)
library(ggrepel)
library(tibbletime)
library(ica)

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

# Session 2
source("code/helper_functions.R")
s <- 7
df <- hexo %>%
  filter(Session==s) %>%
  mutate(HeartRateValid=valid_measurement(HeartRate),
         BreathingRateValid=valid_measurement(BreathingRate)) %>%
  filter(HeartRateValid, BreathingRateValid)

df %>%
  ggplot(aes(x=Time, y=HeartRate)) +
  geom_path() + 
  theme_bw()

ica <- icafast(df %>% select(HeartRate, BreathingRate, Cadence), nc=3, center=TRUE)
df$ICA1 <- ica$S[,1]
df$ICA2 <- ica$S[,2]
df$ICA3 <- ica$S[,3]

df %>%
  ggplot(aes(x=Time, y=ICA3)) +
  geom_path() + 
  theme_bw()
