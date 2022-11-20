setwd("~/Documents/GitHub/StatsI_Fall2022/problemSets/PS03/template")
getwd()
library(tidyverse)
library(dplyr)
library(ggplot2)
install.packages('"stargazer"')
library(stargazer)
#Question 1
mydata <- read_csv("~/Documents/GitHub/StatsI_Fall2022/datasets/incumbents_subset.csv")
voteshare_model = lm(voteshare~difflog, data = mydata)
stargazer(voteshare_model, type = "html", out = "model1.html", title = "Voteshare and Difflog")

ggplot(mydata,aes(x = difflog, y = voteshare,color = voteshare)) +
  geom_point() +
  geom_smooth(method='lm', color = 'white') +
  labs(title= "Model 1: Voteshare and Difflog") 
  
model_residuals <- resid(voteshare_model)
head(model_residuals)

#Prediction equation 
Y = 0.041666 + 0.579031(X)

#Question 2
presidentialvote_model = lm(presvote~difflog, data = mydata)
summary(presidentialvote_model)

stargazer(presidentialvote_model, type = "html", out = "model2.html", 
          title = "Presidential Vote and Difflog")

ggplot(mydata,aes(x = difflog, y = presvote,color = presvote)) +
  geom_point() +
  geom_smooth(method='lm', color = 'white') +
  labs(title= "Model 2: Presidential Vote and Difflog")

model2_residuals <- resid(presidentialvote_model)
head(model2_residuals)

#Prediction equation 
Y = 0.023837 + 0.507583(X)

#Question 3

model3 = lm(voteshare~presvote, data = mydata)
summary(model3)

stargazer(model3, type = "html", out = "model3.html", 
          title = "Presidential Vote and Vote Share")

ggplot(mydata,aes(x = presvote, y = voteshare,color = voteshare)) +
  geom_point() +
  geom_smooth(method='lm', color = 'white') +
  labs(title= "Model 3: Presidential Vote and Vote Share ")


#Prediction equation 
Y = 0.388018 + 0.441330(X)

resreg = lm(model_residuals~model2_residuals)
summary(resreg)

stargazer(resreg, type = "html", out = "model4.html", 
          title = "Residual Regression")

ggplot(resreg,aes(x = model2_residuals, y = model_residuals)) +
  geom_point() +
  geom_smooth(method='lm', color = 'white') +
  labs(title= "Model 4: Residual Regression")

#Prediction equation 
Y = 2.569e-01 + (-5.207e-18)(X)

model5 <- lm(voteshare ~ difflog + presvote, data=mydata)
summary(fit) 

stargazer(model5, type = "html", out = "model4.html", 
          title = "Multi regression")


#Prediction Equation 
Y = 0.449 + 0.0355431(X1) + 0.2568770(X2)

#The residuals are identical...