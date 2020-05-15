
heart.data <- read.csv("D:/R programms/CLEVE.csv")

#Naming the Columns 
names(heart.data) <- c("age", "sex", "cp", "trestbps", "choi", 
                       "fbs", "restecg", "thalach", "exang", "oldpeak",
                       "slope", "ca", "thai", "num")
#Dimensions of the data
dim(heart.data)

#Displaying the initial values 
head(heart.data)

#Assigning the value 1 to all the patients with the presence of heart disease.
#heart.data$num[heart.data$num > 1] <- 1
heart.data$num[heart.data$num > 0] <- 1

#Barplot
barplot(table(heart.data$num), main="Absence vs Presence of Heart Diseases", 
        col="red")

#Boxplot
boxplot(heart.data$age ~ heart.data$num, main="Patients by Age",
        ylab="Age",xlab="Heart disease")


#Correlation Matrix
library(GGally)
library(dplyr)
library(tidyr)
heart.data %>% ggcorr(high       = "#20a486ff",
                      low        = "#fde725ff",
                      label      = TRUE, 
                      hjust      = .75, 
                      size       = 3, 
                      label_size = 3,
                      nbreaks    = 5
)
#Pearson Correlation

#Summary of data
summary(heart.data)

sapply(heart.data, class)

heart.data <- transform(heart.data,
  age=as.integer(age),
  sex=as.factor(sex),
  cp=as.factor(cp),
  trestbps=as.integer(trestbps),
  choi=as.integer(choi),
  fbs=as.factor(fbs),
  restecg=as.factor(restecg),
  thalach=as.integer(thalach),
  exang=as.factor(exang),
  oldpeak=as.numeric(oldpeak),
  slope=as.factor(slope),
  ca=as.factor(ca),
  thai=as.factor(thai),
  num=as.factor(num)
)
sapply(heart.data, class)

summary(heart.data)

#Checking for missing values
heart.data[ heart.data == "?"] <- NA
colSums(is.na(heart.data))

library(caTools)

#Splitting the data
sample = sample.split(heart.data$num, SplitRatio = .75)
train = subset(heart.data, sample == TRUE)
test  = subset(heart.data, sample == FALSE)
dim(train)
dim(test)

#Checking the splitting 
nrow(train)/(nrow(test)+nrow(train))
#0.7524752

#RANDOM FOREST IMPLEMENTATION 
library(randomForest)
rf <- randomForest(num ~ ., data=train)
#Number of trees: 500
#No. of variables tried at each split: 3
#Estimate of  error rate: 17.11%

pred = predict(rf, newdata=test[-14])

#Printing the confusion matrix 
cm = table(test[,14], pred)
cm
#   0  1 
#0  28 3
#1  6  38

summary(cm)
#chi-sq=43.16
#df=1
#p-value = 5.053e-11

#LOGISTIC REGRESSION IMPLEMENTATION
heart.data$num <- factor(heart.data$num)
mylogit <- glm(num ~ ., data = heart.data, family = "binomial")
summary(mylogit)
'Deviance Residuals: 
  Min       1Q   Median       3Q      Max  
-2.9459  -0.2738   0.1012   0.4515   3.1248  

Coefficients:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)  0.179045   3.705420   0.048 0.961461    
age          0.027819   0.025428   1.094 0.273938    
sex1        -1.862297   0.570844  -3.262 0.001105 ** 
  cp1          0.864708   0.578000   1.496 0.134645    
cp2          2.003186   0.529356   3.784 0.000154 ***
  cp3          2.417107   0.719242   3.361 0.000778 ***
  trestbps    -0.026162   0.011943  -2.191 0.028481 *  
  choi        -0.004291   0.004245  -1.011 0.312053    
fbs1         0.445666   0.587977   0.758 0.448472    
restecg1     0.460582   0.399615   1.153 0.249089    
restecg2    -0.714204   2.768873  -0.258 0.796453    
thalach      0.020055   0.011859   1.691 0.090820 .  
exang1      -0.779111   0.451839  -1.724 0.084652 .  
oldpeak     -0.397174   0.242346  -1.639 0.101239    
slope1      -0.775084   0.880495  -0.880 0.378707    
slope2       0.689965   0.947657   0.728 0.466568    
ca1         -2.342301   0.527416  -4.441 8.95e-06 ***
  ca2         -3.483178   0.811640  -4.292 1.77e-05 ***
  ca3         -2.247144   0.937629  -2.397 0.016547 *  
  ca4          1.267961   1.720014   0.737 0.461013    
thai1        2.637558   2.684285   0.983 0.325808    
thai2        2.367747   2.596159   0.912 0.361759    
thai3        0.915115   2.600380   0.352 0.724901    
---

(Dispersion parameter for binomial family taken to be 1)

Null deviance: 417.64  on 302  degrees of freedom
Residual deviance: 179.63  on 280  degrees of freedom
AIC: 225.63'

confint(mylogit)
2.5 %       97.5 %
  '(Intercept) -6.520353992  7.612221779
age         -0.021711585  0.078569023
sex1        -3.033827937 -0.781456905
cp1         -0.241473349  2.046240514
cp2          1.000309167  3.089929630
cp3          1.061674680  3.903354655
trestbps    -0.050337381 -0.003275065
choi        -0.012556122  0.004308513
fbs1        -0.689249100  1.627272971
restecg1    -0.319484444  1.255945020
restecg2    -5.515509745  3.425174195
thalach     -0.002587503  0.044378501
exang1      -1.671509283  0.109615019
oldpeak     -0.889175726  0.065838403
slope1      -2.557985380  0.923381067
slope2      -1.240273929  2.519895332
ca1         -3.422221252 -1.342555106
ca2         -5.166226465 -1.968400734
ca3         -4.289449204 -0.551436689
ca4         -2.045138743  4.990919078
thai1       -2.045793732  7.625672944
thai2       -2.219334545  7.060581121
thai3       -3.653859155  5.642360044'

confint.default(mylogit)

library(InformationValue)
logitMod <- glm(num ~ ., data=train, family=binomial(link="logit"))
#predicted <- plogis(predict(logitMod, testData))  # predicted scores
predicted <- predict(logitMod, test, type="response")  

library(InformationValue)
optCutOff <- optimalCutoff(test$num, predicted)[1] 
optCutOff
#Prediction probability = 0.487106


#Sensitivity 
sensitivity(test$num, predicted, threshold = optCutOff)
# 0.9268293

#Specificty 
specificity(test$num, predicted, threshold = optCutOff)
# 0.7941176

#Printing the confusion matrix 

confusionMatrix(test$num, predicted, threshold = optCutOff)

'  0  1
0 27  3
1  7 38'
