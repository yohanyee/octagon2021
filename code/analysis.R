library(tidyverse)
library(glue)
library(lubridate)
library(tibbletime)
library(elasticnet)
library(patchwork)

source("code/load_data.R")
source("code/session_functions.R")

sessions <- cog_reshaped %>%
  filter(Hexodata_Exists) %>%
  pull(Session) %>%
  unique

session_data_list <- list()
for (s in 1:length(sessions)) {
  session <- sessions[s]
  print(glue("Getting data for session: {session}"))
  
  df <- hexo %>%
    filter(Session==session) %>%
    mutate(HeartRateValid=valid_measurement(HeartRate),
           BreathingRateValid=valid_measurement(BreathingRate)) %>%
    filter(HeartRateValid, BreathingRateValid) %>%
    arrange(Time)
  
  session_data_list[[s]] <- get_all_data(df, window_length=7200)
  
}
session_data <- bind_rows(session_data_list)


# Inner join with other variables
dat <- session_data %>%
  left_join(cog_reshaped %>% 
              mutate(measurement=as.integer(Timepoint)) %>%
              select(Session, measurement, BG, BPD, BPS, HR, Date_Start, Date_Middle, Date_End, IR, DR, Stroop), 
            by=c("Session", "measurement")) %>%
  mutate(Measurement_Date=case_when(measurement == 1 ~ Date_Start,
                                    measurement == 2 ~ Date_Middle,
                                    measurement == 3 ~ Date_Middle,
                                    measurement == 4 ~ Date_Middle,
                                    measurement == 5 ~ Date_End)) %>%
  mutate(TimeCoarse=case_when(measurement == 1 ~ "Night",
                              measurement == 2 ~ "Morning",
                              measurement == 3 ~ "Afternoon",
                              measurement == 4 ~ "Night",
                              measurement == 5 ~ "Night")) %>%
  select(-Date_Start, -Date_Middle, -Date_End) %>%
  mutate(Day_of_Week=wday(Measurement_Date, label=T)) %>%
  mutate(Is_Weekday=case_when(Day_of_Week %in% c("Sat", "Sun") ~ FALSE,
                              TRUE ~ TRUE)) %>%
  select(Session:window_length, Measurement_Date, Day_of_Week, Is_Weekday, TimeCoarse, everything())


# Remove incomplete cases
dat_filtered <- dat %>%
  drop_na()

X <- dat_filtered %>%
  mutate(Is_Afternoon=case_when(TimeCoarse=="Afternoon" ~ 1,
                                TRUE ~ 0),
         Is_Weekday=case_when(Is_Weekday ~ 1,
                              TRUE ~ 0)) %>%
  select(Is_Afternoon, Is_Weekday, N_C_total:HR) %>%
  as.matrix()

Y <- dat_filtered %>% 
  select(IR, DR, Stroop)

# Setup for cross validation
cognitive_variable <- "IR"
lambda = 0




cvres <- 1:nrow(X) %>%
  map_dfr(function(cvrow) {
    fit <- enet(x=X[-cvrow,], y=Y[-cvrow, ][[cognitive_variable]], lambda = lambda, trace=TRUE)
    pred <- predict(fit, X[cvrow,])
    out <- tibble(cvrow=cvrow, lambda=lambda, s=pred$s, fraction=pred$fraction, cognitive_test=cognitive_variable, 
                  predicted=as.numeric(pred$fit), actual=Y[cvrow, ][[cognitive_variable]])
    out
  }) %>%
  mutate(sqerr=(actual - predicted)^2)

mean(cvres$sqerr)
sqrt(mean(cvres$sqerr))

plt3 <- cvres %>%
  mutate(frac_select=abs(fraction-1)) %>%
  group_by(cvrow) %>%
  filter(row_number()==n()) %>%
  ggplot(aes(x=predicted, y=actual)) +
  geom_abline(slope=1, intercept=0, lty=3, color='red') + 
  geom_point() +
  #facet_wrap(~ s) +
  coord_fixed(ratio=1) + 
  xlab(glue("Predicted {cognitive_variable} score")) +
  ylab(glue("Actual {cognitive_variable} score")) +
  theme_bw()

# Variable importance
coefdata <- 1:nrow(X) %>%
  map_dfr(function(cvrow) {
    fit <- enet(x=X[-cvrow,], y=Y[-cvrow, ][[cognitive_variable]], lambda = lambda, trace=TRUE)
    coefs <- tibble(predictors=fit$vn, coefficients=fit$beta.pure[nrow(fit$beta.pure),]) %>%
      arrange(desc(coefficients))
    
    out <- tibble(cvrow=cvrow, lambda=lambda, L1norm=fit$L1norm[length(fit$L1norm)], penalty=fit$penalty[length(fit$penalty)], mu=fit$mu,
                  cognitive_test=cognitive_variable) %>%
      cbind(coefs)
      
    out
  })

coeftable <- coefdata %>%
  group_by(predictors) %>%
  summarize(mean_coefficient=mean(coefficients)) %>%
  arrange(desc(mean_coefficient))



plt3 <- coefdata %>%
  mutate(predictors=factor(predictors, levels = coeftable$predictors)) %>%
  ggplot(aes(x=predictors, y=coefficients)) +
  geom_hline(yintercept = 0, lty=3, color='grey50') +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitter(width=0.2, height=0)) +
  xlab("Predictor variable") +
  ylab("Coefficient") +
  theme_bw() +
  labs(title=glue("{cognitive_variable}")) +
  theme(axis.text.x = element_text(angle=60, hjust=1))

print(plt1 / plt2 / plt3)




 %>%# Set train and test 
dat_usable <- dat_filtered %>%
  group_split(Activity, TimeCoarse, Is_Weekday) %>%
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
  bind_rows()

# Subset train and test
dat_train <- dat_usable %>%
  filter(Group=="Training")

dat_test <- dat_usable %>%
  filter(Group=="Testing")

# Setup inputs
x <- dat_train %>%
  mutate(Is_Afternoon=case_when(TimeCoarse=="Afternoon" ~ 1,
                                TRUE ~ 0),
         Is_Weekday=case_when(Is_Weekday ~ 1,
                              TRUE ~ 0)) %>%
  select(Is_Afternoon, Is_Weekday, N_C_total:HR) %>%
  as.matrix()

y_IR <- dat_train$IR 
y_DR <- dat_train$DR 
y_Stroop <- dat_train$Stroop

x_test <- dat_test %>%
  mutate(Is_Afternoon=case_when(TimeCoarse=="Afternoon" ~ 1,
                                TRUE ~ 0),
         Is_Weekday=case_when(Is_Weekday ~ 1,
                              TRUE ~ 0)) %>%
  select(Is_Afternoon, Is_Weekday, N_C_total:HR) %>%
  as.matrix()

y_IR_actual <- dat_test$IR 
y_DR_actual <- dat_test$DR 
y_Stroop_actual <- dat_test$Stroop

# Fit model
fit_IR <- enet(x=x, y=y_IR, lambda = 0.25, trace=TRUE)
fit_DR <- enet(x=x, y=y_DR, lambda = 0.25, trace=TRUE)
fit_Stroop <- enet(x=x, y=y_Stroop, lambda = 0.25, trace=TRUE)

# Predict
pred_IR <- predict(fit_IR, newx = x_test)
pred_DR <- predict(fit_DR, newx = x_test)
pred_Stroop <- predict(fit_Stroop, newx = x_test)

s <- 35
y_IR_predicted <- pred_IR$fit[, s]
y_DR_predicted <- pred_DR$fit[, s]
y_Stroop_predicted <- pred_Stroop$fit[, s]

qplot(y_IR_predicted, y_IR_actual) + geom_abline(slope=1, intercept=0, lty=3, color='red') + coord_fixed(ratio=1) + theme_bw()
qplot(y_DR_predicted, y_DR_actual) + geom_abline(slope=1, intercept=0, lty=3, color='red') + coord_fixed(ratio=1) + theme_bw()
qplot(y_Stroop_predicted, y_Stroop_actual) + geom_abline(slope=1, intercept=0, lty=3, color='red') + coord_fixed(ratio=1) + theme_bw()
