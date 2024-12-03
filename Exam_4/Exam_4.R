#I am Redoing Exam 1

getwd()
list.files(path = 'BIOL3100_Exams/Exam_1')
setwd('../BIOL3100_Exams/Exam_1')

library(tidyverse)

#1 
dat <- read_csv('cleaned_covid_data.csv')
view(dat)

#2
A_states <- dat %>% 
  filter(grepl("^A", Province_State))
view(A_states)

#3 
A_states %>% 
  ggplot(aes(x = Deaths, 
             y = Last_Update)) +
  geom_point() +
  geom_smooth(method = 'loess', se = FALSE) +
  facet_wrap(~Province_State, scales = 'free') +
  labs(x = 'Death', y = 'Time', title = 'Covid Data States ^A Death vs Time')

#4
State_max <- dat %>% 
  group_by(Province_State) %>% 
  summarise(Max_Fatality_Ratio = max(Case_Fatality_Ratio, na.rm = TRUE)) %>%
  arrange(desc(Max_Fatality_Ratio))
view(State_max)

#5
State_max %>% 
  mutate(Province_State = factor(Province_State, levels = Province_State)) %>% 
  ggplot(aes(x = Province_State, 
             y = Max_Fatality_Ratio)) +
  geom_bar(stat = 'identity', fill = 'lavenderblush3') + 
  labs(x = 'State', 
       y = 'Max Fatality Ratio', 
       title = "Cpvod Data State vs Max Fatality Ratio") + 
  theme_minimal() +
  theme(axis.text = element_text(angle = 90)) 

#bonus 
library(dplyr)
Cumulative_deaths <- dat %>%
  group_by(Last_Update) %>% 
  summarise(Total_deaths = sum(Deaths, na.rm = TRUE)) %>%
  arrange(Last_Update)
ggplot(Cumulative_deaths, aes(x = Last_Update, y = Total_deaths)) + 
  geom_line(color = 'lavenderblush3') +
  labs(x = 'Date', 
       y = 'Cumulative Deaths',
       title = 'Covid Data Cumulative Deaths States vs Time') +
  theme_minimal() +
  scale_x_date(date_labels = '%Y-%m_%d', date_breaks = "2 month")+
  theme(axis.text = element_text(angle = 90))
