#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
setwd("Users/samantanedzinskaite/Documents/GitHub/StatsI_Fall2022/problem_sets/PS1")


#####################
# Problem 1
#####################
#QUESTION 1

iqscores <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
#confidence interval of a mean = mean +- z-score * standard error 
mean <- mean(iqscores)
#finding standard error = standard deviation / sqrt of sample size 
n <- length(iqscores)
sd <- sd(iqscores)
se <- sd/sqrt(n) 

#z-score for 90% CI
conf.level <- 0.9
z <- qt((1+conf.level)/2, df=n-1)
ci <-z*se

conf_interval <- c(mean-ci, mean+ci)
conf_interval
#90% that the population mean is greater than 94 and less than 103
#we are 90% confident that 94 ≤ μ ≤ 103

#shortcut in r, checking my answer 
answer <- t.test(iqscores, conf.level = 0.90)
answer 

#QUESTION 2 
#Hypothesis: This school's students average IQ is higher than the 
#average IQ of 100 in all the other schools in the country.
#sample size = 25
#sample mean = 98.44
#sample standard deviation = 13.0928733795654
#Level of significance = α =0.05 
popmean <- 100
z_score <- (mean - popmean) / se 
z_score
#Our z-score is 0.6...which is smaller than our critical value 
#of 2 at the 5% level of significance. Therefore, we fail to 
#reject the null hypothesis. 



#####################
# Problem 2
#####################
file.choose
expenditure <- read.delim("/Users/samantanedzinskaite/Documents/GitHub/StatsI_Fall2022/datasets/expenditure.txt")
expenditure
#first plot
y1 <- as.data.frame(expenditure$Y)
min(expenditure$Y)
max(expenditure$Y)
pairs(~Y+X1+X2+X3,data=expenditure, 
      main="Scatterplot Matrix")
#there is no clear observable correlation between any of the variables. 
#The relationship between Y and X1 resembles correlation the most. 

#second plot
y <- expenditure$Y
x <- expenditure$Region
plot(y,x, xlab="per capita expenditure on shelter/housing assistance in state", ylab="Region")
#Region 4 - West - has the highest per capita expenditure on housing
#assistance. 

#third plot 
library(dplyr)
library(ggplot2)
install.packages("ggplot2")

plot(expenditure$Y, expenditure$X1, xlab="per capita expenditure on shelter/housing assistance in state", ylab="per capita personal income per state")


expenditure <- read.delim("/Users/samantanedzinskaite/Documents/GitHub/StatsI_Fall2022/datasets/expenditure.txt")
cols <- c("maroon","purple","light blue","orange")
pchs <- c(pch=15, pch=16, pch=17, pch=18)
plot(expenditure$Y, expenditure$X1, col= cols[expenditure$Region], pch=pchs[expenditure$Region],  xlab="per capita expenditure on shelter/housing assistance in state", ylab="per capita personal income per state")
legend("bottomright", inset=.02, title="Regions",
       c("NE","NC","South", "West"), fill=c(cols, pchs), horiz=TRUE, cex=0.8)
