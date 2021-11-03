library(tidyverse)
library(glue)
library(lubridate)
library(tibbletime)

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