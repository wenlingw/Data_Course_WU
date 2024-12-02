library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(modelr)
library(easystats)
library(broom)
library(fitdistrplus)
#1
dat <- read.csv("../Exam_3/Exam_3/FacultySalaries_1995.csv")
View(dat)

str(dat)
head(dat)

dat1 <- dat %>%
  pivot_longer(cols = starts_with("Avg"), 
               names_to = c("Rank", "Metric"), 
               names_pattern = "Avg(.*)(Prof.*)", 
               values_to = "Value") %>% 
  filter(Rank %in% c("Full", "Assoc", "Assist")) %>%
  filter(Tier != "VIIB")  

view(dat1)

unique(dat1$Rank)

ggplot(dat1, aes(x = Rank, y = Value, fill = Rank)) +
  geom_boxplot(size = 1) +
  facet_wrap(~Tier,) +  
  scale_fill_manual(values = c("lightpink2", "seagreen3", "cadetblue")) + 
  labs(x = "Rank",
       y = "Salary",
       fill = "Rank") +
  theme_minimal() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

#2 
dat1$State <- as.factor(dat1$State)
dat1$Tier <- as.factor(dat1$Tier)
dat1$Rank <- as.factor(dat1$Rank)

mod <- aov(Value ~ State + Tier + Rank, data = dat1)
summary(mod)
report(mod)
