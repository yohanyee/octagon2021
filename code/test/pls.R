library(tidyverse)
library(glue)
library(lubridate)
library(ggrepel)

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

# Usable data
cog_reshaped_completecases <- cog_reshaped %>%
  filter(Hexodata_Exists) %>%
  mutate(index=1:nrow(.)) %>%
  drop_na()

cog_usable <- cog_reshaped_completecases %>%
  group_split(Permutation, Timepoint) %>%
  map(function(x) {
    n_total <- nrow(x)
    
    all_indices <- 1:n_total
    train_indices <- sort(sample(all_indices, size =as.integer(round(0.7*n_total)), replace = F))
    test_indices <- setdiff(all_indices, train_indices)
    
    group <- character(n_total)
    group[train_indices] <- "Training"
    group[test_indices] <- "Testing"
    
    x %>%
      mutate(Group=group)
  }) %>%
  bind_rows() %>%
  mutate(Time=case_when(Timepoint==1 ~ "Night",
                        Timepoint==2 ~ "Morning",
                        Timepoint==3 ~ "Afternoon",
                        Timepoint==4 ~ "Night",
                        Timepoint==5 ~ "Night"))

# PLS
library(plsdepot)

pls <- plsreg2(predictors = cog_usable %>% 
                 select(BG, BPD, BPS, HR), 
        responses = cog_usable %>% 
          select(DR, IR, Stroop), 
        comps = 3)

pls$expvar

# Random forest
library(caret)
library(randomForest)


cog_train <- cog_usable %>% 
  filter(Group=="Training")
cog_test <- cog_usable %>% 
  filter(Group=="Testing")

fit <- train(Stroop ~ BG + BPD + BPS + HR + Time, data=cog_train, method="rf")
summary(fit)
fit
plot(fit)

df <- cog_test %>%
  mutate(predicted = predict(fit, cog_test))

df %>%
  ggplot(aes(x=Stroop, y=predicted)) +
  geom_abline(slope=1, intercept = 0, lty=3, color='red') +
  geom_point() +
  coord_fixed(ratio=1) +
  theme_bw()


# LM

fit <- lm(DR ~ (BG + BPD + BPS + HR) * Time, data=cog_train)

df <- cog_test %>%
  mutate(predicted = predict(fit, cog_test))

df %>%
  ggplot(aes(x=DR, y=predicted)) +
  geom_abline(slope=1, intercept = 0, lty=3, color='red') +
  geom_point() +
  coord_fixed(ratio=1) +
  theme_bw()
