tab <- matrix(c(14, 6, 7, 7, 7, 1), byrow = TRUE, nrow = 2, ncol = 3)
tab

#Step 1: finding expected frequencies 
expected_frequencies <- data.frame()

for (i in 1:2){
  expected_frequencies[i,1] <- (sum(tab[i,]) * sum(tab[,1])) / sum(tab)
  expected_frequencies[i,2] <- (sum(tab[i,]) * sum(tab[,2])) / sum(tab)
  expected_frequencies[i,3] <- (sum(tab[i,]) * sum(tab[,3])) / sum(tab)
}
expected_frequencies

rownames = c("Upper Class", "Lower Class")
colnames = c("Not Stopped", "Bribe requested", "Stopped/given warning")
names(expected_frequencies)[1:3] <- colnames
rownames(expected_frequencies)[1:2] <- rownames
expected_frequencies

#Step 2: finding test statistic 

test_statistic <- sum(((tab - expected_frequencies)^2/expected_frequencies))
test_statistic


df <- ((nrow(tab) -1)*(ncol(tab) -1))
alpha <- 0.1
pvalue <- pchisq(test_statistic, df = 2, lower.tail=FALSE)
pvalue
pvalue < alpha

#fail to reject null hypothesis because value is not less than 0.1.this suggests
#that we do not have enough evidence to claim that the...
#calculate standardized residual for each score and put in table below. 
stand_residual <- data.frame()

for (i in 1:2){
  stand_residual[i,1] <- (tab[i,1] - expected_frequencies[i, 1])/
    sqrt(expected_frequencies[i,1]*(1-sum(tab[i,])/sum(tab))*(1-sum(tab[,1])/sum(tab)))
  stand_residual[i,2] <- (tab[i,2] - expected_frequencies[i, 2])/
    sqrt(expected_frequencies[i,2]*(1-sum(tab[i,])/sum(tab))*(1-sum(tab[,2])/sum(tab)))
  stand_residual[i,3] <- (tab[i,3] - expected_frequencies[i, 3])/
    sqrt(expected_frequencies[i,3]*(1-sum(tab[i,])/sum(tab))*(1-sum(tab[,3])/sum(tab)))
}

stand_residual

rownames = c("Upper Class", "Lower Class")
colnames = c("Not Stopped", "Bribe requested", "Stopped/given warning")
names(stand_residual)[1:3] <- colnames
rownames(stand_residual)[1:2] <- rownames
stand_residual

#The standardizied residual measures the significance of the difference between 
#the observed and the expected values in a regression model. It is useful in
#interpreting results because it can help to identify outliers in the model. 
#Typically, a standardized residual with a value above 3 would be considered 
#as an outlier in the model. 






#QUESTION 2 

data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")

plot(data$water~data$reserved)
plot(data$water~jitter(data$reserved))
reg <- lm(data$water~data$reserved, data) 
reg
summary(reg)


data$D <- data$reserved == 1

plot(data$D, data$water,            
     pch = 20,                             
     cex = 0.5,                               
     col = "Steelblue",                       
     xlab = "Reservation policy",                 
     ylab = "Water facilities",
     main = "Dummy Regression")

data$D <- data$reserved == 1
dummy_model <- lm(water ~ D, data = data)
summary(dummy_model)

confint(dummy_model)

points(x = data$D, 
       y = predict(dummy_model), 
       col = "red", 
       pch = 20)

