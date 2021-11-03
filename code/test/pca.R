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

# PCA across all TP
cog_reshaped_completecases <- cog_reshaped %>%
  filter(Hexodata_Exists) %>%
  mutate(index=1:nrow(.)) %>%
  drop_na()

pca <- prcomp(cog_reshaped_completecases %>% select(DR, IR, Stroop))  

print(pca)
plot(pca)
summary(pca)
biplot(pca)

scores <- as_tibble(pca$x)
scores$row <- 1:nrow(scores)
scores %>%
  ggplot(aes(x=PC1, y=PC2)) +
  geom_label_repel(aes(label=row)) +
  geom_point() + 
  theme_bw()  

scores %>%
  ggplot(aes(x=PC2, y=PC3)) +
  geom_label_repel(aes(label=row)) +
  geom_point() + 
  theme_bw()  

scores %>%
  ggplot(aes(x=PC1, y=PC3)) +
  geom_label_repel(aes(label=row)) +
  geom_point() + 
  theme_bw()  

similar_obs <- c(11,21,24,25,27,37,47,49,93,101,119,133)
scores %>%
  slice(similar_obs)

a <- cog_reshaped_completecases %>%
  slice(similar_obs) %>%
  select(Session, Timepoint, Permutation, DR, IR, Stroop, index)


cog_reshaped %>%
  ggplot(aes(x=IR, y=DR)) +
  geom_point(position=position_jitter(width=1, height=1)) +
  theme_bw()

# PCA at timepoint 1: Day 1, 11pm
pca1 <- cog_reshaped %>%
  filter(Hexodata_Exists, Timepoint==1) %>%
  select(DR, IR, Stroop) %>%
  drop_na() %>%
  prcomp()

print(pca1)
plot(pca1)
summary(pca1)
biplot(pca1)
