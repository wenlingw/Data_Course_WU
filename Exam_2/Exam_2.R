library(tidyverse)

#1 
df <- read.csv("unicef-u5mr.csv")
view(df)

#2
df_1 <- df %>%
  pivot_longer(cols = starts_with("U5MR"), 
               names_to = "Year", 
               values_to = "U5MR") %>%
  mutate(Year = as.integer(str_replace(Year, "U5MR.", "")))  

view(df_1)

#3&4
plot_1 <- ggplot(df_1, aes(x = Year, y = U5MR, color = CountryName)) +
  geom_line() +
  facet_wrap(~ Continent, scales = "fixed", labeller = label_value) +
  theme_minimal() +
  scale_color_viridis_d() + 
  labs(title = "U5MR over Time by Country and Continent", 
       x = "Year", 
       y = "U5MR") +
  theme(legend.position = "none") 
ggsave("WU_Plot_1.png", plot = plot_1)

#5
df_mean <- df_1 %>%
  group_by(Year, Continent) %>%
  summarise(mean_U5MR = mean(U5MR, na.rm = TRUE))

view(df_mean)

plot_2 <- ggplot(df_mean, aes(x = Year, y = mean_U5MR, color = Continent)) +
  geom_line(size = 2.5) +
  theme_minimal() +
  labs(title = "Mean U5MR by Continent Over Time", 
       x = "Year", 
       y = "Mean_U5MR") 

#6 
ggsave("WU_Plot_2.png", plot = plot_2)

#7 
mod1 <- lm(U5MR ~ Year, data = df_1)
mod2 <- lm(U5MR ~ Year + Continent, data = df_1)
mod3 <- lm(U5MR ~ Year * Continent, data = df_1)

summary(mod1)
summary(mod2)
summary(mod3)

#8
# I think mod3 is the best because it has the highest R-squared with the lowest mean square error. 
mse_mod1 <- mean(residuals(mod1)^2)
mse_mod2 <- mean(residuals(mod2)^2)
mse_mod3 <- mean(residuals(mod3)^2)

mse_mod1
mse_mod2
mse_mod3

#9 
df_pred <- df_1 %>%
  mutate(
    pred_mod1 = predict(mod1, newdata = df_1),  
    pred_mod2 = predict(mod2, newdata = df_1),  
    pred_mod3 = predict(mod3, newdata = df_1)   
  ) %>%
  pivot_longer(cols = starts_with("pred_mod"), 
               names_to = "Model", 
               values_to = "Prediction")
view(df_pred)

plot_3 <- ggplot(df_pred, aes(x = Year, y = Prediction, color = Continent)) +
  geom_line(size = 1.5) + 
  facet_wrap(~ Model, scales = "fixed", labeller = label_value) + 
  labs(title = "Model Predictions", 
       x = "Year", y = "Predicted U5MR") +
  theme_minimal() +
  theme(legend.position = "right") 

ggsave("WU_Plot_3.png", plot = plot_3)
