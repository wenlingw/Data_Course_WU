library(tidyverse)
library(readxl)
getwd()

data <- read.csv("Utah_Religions_by_County.csv")
view(data)

head(data)

dat <- data %>%
  pivot_longer(cols = c("Assemblies.of.God", "Episcopal.Church", 
                        "Pentecostal.Church.of.God", "Greek.Orthodox", "LDS", 
                        "Southern.Baptist.Convention", "United.Methodist.Church", 
                        "Buddhism.Mahayana", "Catholic", "Evangelical", "Muslim", 
                        "Non.Denominational", "Orthodox"), 
               names_to = "religion", 
               values_to = "proportion")
view(dat)


library(ggplot2)

ggplot(dat, aes(x = Pop_2010, y = proportion, color = religion)) +
  geom_point() +
  labs(title = "Population vs Proportion of Religious Groups", 
       x = "County Population", y = "Proportion of Religious Group")

ggplot(dat, aes(x = proportion, fill = religion)) +
  geom_histogram(bins = 20, alpha = 0.7) +
  labs(title = "Distribution of Proportions for Different Religions", 
       x = "Proportion", y = "Count")


cor <- data %>%
  select(Pop_2010, Assemblies.of.God, Episcopal.Church, Pentecostal.Church.of.God, 
         Greek.Orthodox, LDS, Southern.Baptist.Convention, United.Methodist.Church, 
         Buddhism.Mahayana, Catholic, Evangelical, Muslim, Non.Denominational, Orthodox) %>%
  cor()

view(cor)
print(cor)

install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(cor, lab = TRUE)

ggplot(data, aes(x = Non.Religious, y = Assemblies.of.God)) +
  geom_point() +
  labs(title = "Non-religious Proportion vs Assemblies of God Proportion", 
       x = "Proportion of Non-religious People", 
       y = "Proportion of Assemblies of God")

ggplot(data, aes(x = Non.Religious, y = LDS)) +
  geom_point()

ggplot(data, aes(x = Non.Religious, y = Catholic)) +
  geom_point()


#1 Does population of a county correlate with the proportion of 
## any specific religious group in that county?

### There is no correlation between the county population and the specific 
### religious group in that county

cor1 <- data %>%
  select(Pop_2010, Assemblies.of.God, Episcopal.Church, Pentecostal.Church.of.God, 
         Greek.Orthodox, LDS, Southern.Baptist.Convention, United.Methodist.Church, 
         Buddhism.Mahayana, Catholic, Evangelical, Muslim, Non.Denominational, Orthodox) %>%
  cor()
view(cor1)


library(ggcorrplot)
ggcorrplot(cor1, lab = TRUE)


#2 Does proportion of any specific religion in a given county 
## correlate with the proportion of non-religious people?

### There is no correlation between the specific religion in a county and 
### the proportion of non-religious people 
cor2 <- data %>%
  select(Non.Religious, Assemblies.of.God, Episcopal.Church, Pentecostal.Church.of.God, 
         Greek.Orthodox, LDS, Southern.Baptist.Convention, United.Methodist.Church, 
         Buddhism.Mahayana, Catholic, Evangelical, Muslim, Non.Denominational, Orthodox) %>%
  cor()

view(cor2)

ggcorrplot(cor2, lab = TRUE)
