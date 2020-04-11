
library(ggplot2)
library(cowplot)
library(GGally)
library(blorr)

## Smoker dataset

mydata = read.csv(file = 'GHW.csv')


mydata[mydata$Gender == 0,]$Gender = 'F'
mydata[mydata$Gender == 1,]$Gender = 'M'


gender = mydata$Gender
height = mydata$Height
smoker = mydata$Smoker
exercise = mydata$Exercise
age = mydata$Age
lungcap = mydata$Lung.Capacity..cc./100  


## Logistic regression model

mylm = glm(smoker~height + gender + exercise + age + lungcap,family = 'binomial')




## R2 & ADJ R2

r2 = blr_rsq_mcfadden(mylm)
adjr2 = blr_rsq_mcfadden_adj(mylm)


##p-value

lNull = mylm$null.deviance/-2  # null model likelihood
lModel = mylm$deviance/-2  # current model likelihood

pVal = 1 - pchisq(2*(lModel- lNull),df = (length(mylm$coefficients)-1))


## Correlation
mydata[mydata$Smoker == 0,]$Smoker = 'N'
mydata[mydata$Smoker == 1,]$Smoker = 'Y'
smoker = mydata$Smoker
ggpairs(mydata)

## summary of model
summary(mylm)



## Graph

myplot.data = data.frame(smoker = smoker, predicted = mylm$fitted.values)
myplot.data = myplot.data[order(myplot.data$predicted,decreasing = 'False'),]
myplot.data$rank = 1:nrow(myplot.data)
ggplot(data = myplot.data,aes(x=rank,y=predicted)) +
  geom_point(aes(color=smoker), alpha=1, shape=4, stroke=2) +
  xlab("Index") +
  ylab("Predicted probability")






