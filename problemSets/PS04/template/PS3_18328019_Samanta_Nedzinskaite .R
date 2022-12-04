install.packages('car')
library('car')
data(Prestige)
data <- Prestige
library('stargazer')
library(tidyverse)
library(dplyr)
library(ggplot2)
getwd()
#Question 1 
data$professional <- ifelse(data$type == "prof", 1, 0)
head(data)

sink(file = "data_head.txt")
head(data)
sink(file = NULL)

data %>%
  ggplot(aes(x = income , y = prestige, colour = professional)) +
  geom_point() +
  ggtitle('Prestige ~ Income + Professional')

Income_Prestige_Prof <- data %>%
  lm(formula = prestige ~ income + professional)
stargazer(Income_Prestige_Prof, type = "latex", out = "Income_Prestige_Prof.txt", title = "Prestige, Income and Professional")

sink(file = "regression_output.txt")
print(Income_Prestige_Prof)
sink(file = NULL)

Y=30.618334+0.001371×Income+22.757×X2
Y <- 30.618334+(0.001371*1000)+22.757
print(Y)

#NON PROFESSIONAL 
Y2 <- 30.618334+(0.001371*6000)
print(Y2)
#PROFESSIONAL 
Y2_2 <- 30.618334+(0.001371*6000)+22.757
print(Y2_2)

change_in_y = Y2 - Y2_2
print(change_in_y)

#Question 2
#Testing overall model fit with F-test
F <- (131 - 2 -1)/2*0.094/(1-0.094)
F
p_value<- 1- pf(F, 2, 128)
p_value

#testing individual regression coefficients 
#Presincts assigned lawn signs 
t = 0.042 / 0.016 
p_value = 2*(1-pt(t, 131-3))
p_value

#Presincts adjacent to law signs 
t2 = 0.042 / 0.013 
p_value2 = 2*(1-pt(t2, 131-3))
p_value2



