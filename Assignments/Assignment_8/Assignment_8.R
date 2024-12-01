library(modelr)
library(easystats)
library(broom)
library(tidyverse)
install.packages('fitdistrplus')
library(fitdistrplus)

data("mtcars")
glimpse(mtcars)

mod1 = lm(mpg ~ disp, data = mtcars)
summary(mod1)

ggplot(mtcars, aes(x=disp,y=mpg)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mod2 = lm(mpg ~ qsec, data = mtcars)
ggplot(mtcars, aes(x=disp,y=qsec)) + 
  geom_point() + 
  geom_smooth(method = "lm") +
  theme_minimal()

mean(mod1$residuals^2)
mean(mod2$residuals^2)

df <- mtcars %>% 
  add_predictions(mod1) 
df %>% dplyr::select("mpg","pred")

newdf = data.frame(disp = c(500,600,700,800,900))
pred = predict(mod1, newdata = newdf)

hyp_preds <- data.frame(disp = newdf$disp,
                        pred = pred)
df$PredictionType <- "Real"
hyp_preds$PredictionType <- "Hypothetical"
fullpreds <- full_join(df,hyp_preds)

ggplot(fullpreds,aes(x=disp,y=pred,color=PredictionType)) +
  geom_point() +
  geom_point(aes(y=mpg),color="Black") +
  theme_minimal()

mod3 <- glm(data=mtcars,
            formula = mpg ~ hp + disp + factor(am) + qsec)
mods <- list(mod1=mod1,mod2=mod2,mod3=mod3)
map(mods,performance) %>% reduce(full_join)
mtcars %>% 
  gather_residuals(mod1,mod2,mod3) %>% 
  ggplot(aes(x=model,y=resid,fill=model)) +
  geom_boxplot(alpha=.5) +
  geom_point() + 
  theme_minimal()

mtcars %>% 
  gather_predictions(mod1,mod2,mod3) %>% 
  ggplot(aes(x=disp,y=mpg)) +
  geom_point(size=3) +
  geom_point(aes(y=pred,color=model)) +
  geom_smooth(aes(y=pred,color=model)) +
  theme_minimal() +
  annotate("text",x=250,y=32,label=mod1$call) +
  annotate("text",x=250,y=30,label=mod2$call) +
  annotate("text",x=250,y=28,label=mod3$call)

report(mod3)

#Assignment
dat <- read.csv('C:/Users/maggi/Desktop/Biol3100/Data_Course_Wu/Data/mushroom_growth.csv')
glimpse(dat)
summary(dat)
view(dat)

ggplot(dat, aes(x = Light, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Light vs GrowthRate", x = "Light", y = "Growth Rate")

ggplot(dat, aes(x = Nitrogen, y = GrowthRate)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Nitrogen vs GrowthRate", x = "Nitrogen", y = "Growth Rate")

ggplot(dat, aes(x = Humidity, y = GrowthRate, fill = Humidity)) +
  geom_boxplot() +
  labs(title = "Humidity vs GrowthRate", x = "Humidity", y = "Growth Rate") +
  theme_minimal()

mod1 <- lm(GrowthRate ~ Nitrogen, data = dat)
summary(mod1)

mod2 <- lm(GrowthRate ~ Light + Nitrogen + Temperature + Humidity + Species, data = dat)
summary(mod2)

mod3 <- lm(GrowthRate ~ Light * Humidity + Nitrogen + Temperature + Species, data = dat)
summary(mod3)

mod4 <- lm(GrowthRate ~ Light + Nitrogen + Humidity, data = dat)
summary(mod4)

summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared
summary(mod3)$adj.r.squared
summary(mod4)$adj.r.squared

AIC(mod1, mod2, mod3, mod4)

anova(mod1, mod2, mod3, mod4)

calculate_mse <- function(model, data) {
  residuals <- model$residuals
  mse <- mean(residuals^2)
  return(mse)
}

mse_mod1 <- calculate_mse(mod1, dat)
mse_mod2 <- calculate_mse(mod2, dat)
mse_mod3 <- calculate_mse(mod3, dat)
mse_mod4 <- calculate_mse(mod4, dat)

cat("MSE for Model 1:", mse_mod1, "\n")
cat("MSE for Model 2:", mse_mod2, "\n")
cat("MSE for Model 3:", mse_mod3, "\n")
cat("MSE for Model 4:", mse_mod4, "\n")

mse_values <- c(mse_mod1, mse_mod2, mse_mod3, mse_mod4)
best_model_index <- which.min(mse_values)
best_model <- list(mod1, mod2, mod3, mod4)[[best_model_index]]
cat("The best model is Model", best_model_index, "\n")

par(mfrow = c(2, 2))
plot(mod1)
plot(mod2)
plot(mod3)
plot(mod4)


interaction.plot(x.factor = dat$Light, trace.factor = dat$Humidity, response = dat$GrowthRate,
                 fun = mean, col = c("red", "blue", "green"), lty = 1, legend = TRUE,
                 xlab = "Light", ylab = "Growth Rate", trace.label = "Humidity")

ggplot(dat, aes(x = Light, y = Humidity, fill = GrowthRate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "blue", high = "red") +
  labs(title = "Heatmap of GrowthRate by Light and Humidity",
       x = "Light", y = "Humidity", fill = "Growth Rate") +
  theme_minimal()

ndat <- data.frame(
  Light = c(5, 10, 15, 20),
  Nitrogen = c(5, 15, 25, 35),
  Humidity = c("Low", "Medium", "High", "Medium"),
  Temperature = c(20, 22, 24, 25)
)

ndat$Humidity <- factor(ndat$Humidity, levels = levels(dat$Humidity))
ndat$PredictedGrowthRate <- predict(best_model, ndat)

best_model_no_species <- lm(GrowthRate ~ Light + Nitrogen + Humidity + Temperature, data = dat)
ndat$PredictedGrowthRate <- predict(best_model_no_species, newdata = ndat)

print(ndat)

dat$Type <- "Real Data"
ndat$Type <- "Predicted Data"
combined_data <- rbind(
  dat[, c("Light", "Nitrogen", "Humidity", "Temperature", "GrowthRate", "Type")],
  data.frame(
    Light = ndat$Light,
    Nitrogen = ndat$Nitrogen,
    Humidity = ndat$Humidity,
    Temperature = ndat$Temperature,
    GrowthRate = ndat$PredictedGrowthRate,
    Type = ndat$Type
  )
)


ggplot(combined_data, aes(x = Nitrogen, y = GrowthRate, color = Type)) +
  geom_point(size = 3) +
  facet_wrap(~ Humidity) +
  labs(
    title = "Predicted vs Real Growth Rates",
    x = "Nitrogen Levels",
    y = "Growth Rate"
  ) +
  theme_minimal()

view(ndat)
head(ndat)
colnames(ndat)
sum(is.na(ndat))

