# Loading Packages
library(readr)
library(tidyverse)
library(texreg)
library(corrplot)

# Importing Data
sv <- read_csv("in-class-survey/Anonymous in-class survey (Responses) - Form responses 1.csv")

sv <-
sv %>% 
  # Rename variables
  rename(LRSCALE = `How would you rate yourself from 1-10 on the left-right ideological scale?`,
         PRIV_WORK = `Have you ever worked in the private sphere?`,
         PUB_WORK = `Have you ever worked in the public sphere?`,
         IPSCALE = `How would you rate yourself on interpretivist-positivist scale?`,
         NPR_PUB = `Have you ever published in a non-peer reviewed publication before?`,
         PR_PUB = `Have you ever published in a peer-reviewed publication before?`,
         FLUENT = `How many languages do you speak fluently?`,
         WOMAN = `In terms of gender, do you identify as a woman?`,
         POS_Q = `State your level of agreement with the following statement: We can make claims about the social world from doing social science in the same way that we can make claims about nature in the natural sciences.`,
         PID_STR = `To what degree do you identify with an electoral party where it matters to you.`) %>% 
  # creating an individual fixed effect variable
  mutate(fixed_effect = 1:nrow(sv))

# get correlations
cor(sv$IPSCALE, sv$LRSCALE)

# create initial simple regression plot
sv %>% 
ggplot(aes(x = LRSCALE, y = IPSCALE)) +
  geom_point(position = position_jitter(width = 0.05)) +
  # geom_point(position = position_nudge()) +
  geom_smooth(method = "lm", color = "blue") +
  theme_classic() +
  labs(x = "Left-Right Scale", y = "Int-Pos Scale")

# Run Multiple Regression

# Interpretevist-Positivist Scale
m1 <- lm(IPSCALE ~ LRSCALE + POS_Q + PRIV_WORK + PUB_WORK + NPR_PUB + PR_PUB + FLUENT + WOMAN + PID_STR, data = sv)
screenreg(m1)

# With individual fixed effects
m1_FE <- lm(IPSCALE ~ LRSCALE + PRIV_WORK + PUB_WORK + NPR_PUB + PR_PUB + FLUENT + WOMAN + PID_STR + 
              factor(fixed_effect), data = sv)
screenreg(m1_FE)

# Create hypothetical data to predict over 
pred_dat <- expand.grid(
  LRSCALE = 1:10,
  POS_Q = 3,
  PRIV_WORK = 1,
  PUB_WORK = 1,
  PR_PUB = 1,
  NPR_PUB = 1,
  FLUENT = 2,
  WOMAN = 2,
  PID_STR = 2
)

# Predict fitted values
plot_dat <- cbind(pred_dat, predict(m1, pred_dat, interval = "confidence"))

# plot fitted values vs. data and simple regression
ggplot() +
  geom_smooth(data = sv, aes(x = LRSCALE, y = IPSCALE), method = "lm") +
  geom_line(data = plot_dat, aes(x = LRSCALE, y = fit), color = "red") +
  geom_ribbon(data = plot_dat, aes(x = LRSCALE, ymax = upr, ymin = lwr), alpha = 0.2, color = "red") +
  geom_point(data = sv, aes(x = LRSCALE, y = IPSCALE), position = position_jitter(width = 0.05)) +
  theme_classic() +
  labs(x = "Left-Right Scale", y = "Int-Pos Scale")
