Happiness <- read.delim("Happiness.txt", header = TRUE)
attach(Happiness)


Happiness$Household[Household==9]= NA
Happiness$Age[Age==98|Age==99]= NA
Happiness$Children[Children==9]= NA
Happiness$OwnHome[OwnHome == 9] = NA
Happiness$OwnHome[OwnHome == 0] = 8
Happiness$Health[ Health == 9] = NA
Happiness$Health[ Health == 0] = 8
Happiness$Instagram[Instagram == 9] = NA
Happiness$Instagram[Instagram == 0] = 8
Happiness$Marital[Marital == 9] = NA
Happiness$Education[Education == 97 | Education == 98 | Education == 99] = NA
Happiness$JobSat[JobSat == 8 | JobSat == 9] = NA
Happiness$Income[ Income ==999998 | Income == 999999] = NA
Happiness$WorkHrs[WorkHrs==999] =  NA
Happiness$WorkHrs[WorkHrs==(-1)] =  998
Happiness$Happy[Happy == 0 | Happy == 8 | Happy == 9 ] = NA



#Let's take a look at the multiple regression model without any transformations and examine the diagnostic plots.
#We need to show overwhleming evidence for making a transformation in the first place.

m1 <- lm(Happy ~ Household + Health + Age + Children + OwnHome + Instagram + Marital + Sex + Education + JobSat + Income + WorkHrs, na.action = na.exclude)
summary(m1)

par(mfrow = c(2,2))
plot(m1)


#Our Residuals vs. Fitted Values plot does not have random scatter about the horizontal line, so the average of the errors
#does not seem to be zero. There is definately a downward pattern that we see. This indictates that our model is not appropriate
#for the data that we have.

#Moreover, the Q-Q Plot does not seem to be that linear in form, and there are outliers present which are an issue for our model.


#Our Fitted Values vs. sqrt(Standardized Residuals) plot also does not look good. We see that our variance of the error terms
#changes quite a lot as scan from left to right

#there are many leverage values!

lev <- hat(model.matrix(m1))
sum(lev > ((2) * (13/nrow(Happiness)))) #105 leverage points


large.levs <- which((lev > ((2) * (13/nrow(Happiness)))))
#Let's look for outliers. We have a lot! These could pose a problem in our model.

our_outliers <- which(rstandard(m1) < (-2) | rstandard(m1) > 2)

#9   12   55  389  398  448  684  715  923 1001 1032 1253 1286 1518 1629 1813 1965 2360 

rstandard(m1)[which(rstandard(m1) < (-2) | rstandard(m1) > 2)]


sum(our_outliers %in% large.levs) #we have 6 BAD leverage points! not good

#We now have evidence that a regular multiple regression model is not appropriate. Therefore, we need a transformation.



Happiness$Children[Children == 0] =  0.01
Happiness$Income[Income == 0] = 0.01
Happiness$JobSat[JobSat == 0] = 0.01
Happiness$Education[Education == 0] = 0.01
Happiness$WorkHrs[WorkHrs == 0] = 0.01


attach(Happiness)

library(alr3)
#we will use the powerTransform which will use maximum likelihood estimation determine the best estimate of tranformation

pm1 <- powerTransform(cbind(Happy,WorkHrs,Health,OwnHome,Household,Age,Children,Marital,Sex,Education,JobSat,Income,Instagram)~1)
summary(pm1) #now we have the powers we can use

# bcPower Transformations to Multinormality 
# Est Power Rounded Pwr Wald Lwr bnd Wald Upr Bnd
# Happy        0.7328        0.73       0.6218       0.8438
# WorkHrs      1.4525        1.45       1.3863       1.5187
# Health       0.1951        0.20       0.1343       0.2559
# OwnHome     -0.1871       -0.19      -0.2472      -0.1269
# Household   -0.3244       -0.33      -0.4190      -0.2298
# Age          0.4648        0.50       0.3585       0.5712
# Children     0.3336        0.33       0.3107       0.3565
# Marital      0.0918        0.09       0.0084       0.1751
# Sex          1.0353        1.00       0.8293       1.2413
# Education    1.2063        1.21       1.0976       1.3150
# JobSat      -0.4434       -0.44      -0.4727      -0.4142
# Income       0.0439        0.04       0.0344       0.0534
# Instagram    0.3944        0.33       0.3238       0.4651



tHappy <- (Happy^(0.733))
tWorkHrs <- WorkHrs^(1.45)
tHealth <- Health^(0.195)
tOwnHome <- OwnHome^(-0.187)
tHousehold <- Household^(-0.324)
tAge <- Age^(0.465)
tChildren <- Children^(0.333)
tMarital <- Marital^(0.09)
tSex <- Sex^(1.035)
tEducation <- Education^(1.206)
tJobSat <- JobSat^(-0.443)
tIncome <- Income^(0.044)
tInstagram <- Instagram^(0.394)


#Now let's construct a model with our transformed Xi

m2 <- lm(tHappy ~ tHousehold + tHealth + tAge + tChildren + tOwnHome + tInstagram + tMarital + tSex + tEducation + tJobSat + tIncome + tWorkHrs, na.action = na.exclude)
summary(m2)

#Significant variables: tHousehold, tHealth, tChildren, tOwnHome, tMarital

par(mfrow = c(2,2))
plot(m2)

#let's look at our diagnostic plots


#Fitted Values vs. Residuals Plot also does not show random scatter, and there is definately a pattern
#Normal Q-Q plot looks roughly the same as m1
#Fitted Values and sqrt(Standardized Residuals) plot also shows nonconstant variance, so this is a problem.
#we end up having less leverage points, but more outliers so more BAD leverage points.

lev2 <- hat(model.matrix(m2))

sum(lev2 > (2 * (13)/nrow(Happiness)))

large.levs2 <- which((lev2 > (2 * (13)/nrow(Happiness))))

#we have 29 leverage points (could be good or bad). This is much less than before!


our_outliers2 <- which(rstandard(m2) < (-2) | rstandard(m2) > 2)

sum(our_outliers2 %in% large.levs2) #we now have 2 bad leverage points which is better.


library(car)
#Significant variables: tHousehold, tHealth, tChildren, tOwnHome, tMarital

avPlot(m2,variable=tHealth)
avPlot(m2,variable=tOwnHome)
avPlot(m2,variable=tHousehold)
avPlot(m2,variable=tAge)
avPlot(m2,variable=tChildren)
avPlot(m2,variable=tInstagram)
avPlot(m2,variable=tMarital)
avPlot(m2,variable=tSex)
avPlot(m2,variable=tEducation)
avPlot(m2,variable=tJobSat)
avPlot(m2,variable=tIncome)
avPlot(m2,variable=tWorkHrs)

#when we examine the individual added variable plots, we see that the added variable plots for the significant Xi's
#have a slightly better looking plot. The significant Xi's have a little more effect on tHappy
#the variables that are not signfiicant (ex. p-value of 0.80, 0.20, etc) have added-variable plots that are worse
#the model line also is not as steep


StanRes2 <- rstandard(m2)
par(mfrow=c(2,2))

#when a valid model  has been fit, a plot of standardized residuals
#against any predictor or any linear combination of the predictors will have
#random scatter of points around the horizontal axis
#and constant variability as we look along the horizontal axis 

plot(tHousehold,StanRes2, ylab="Standardized Residuals") #roughly random and constant variability
plot(tHealth,StanRes2, ylab="Standardized Residuals") #same
plot(tOwnHome,StanRes2, ylab="Standardized Residuals") #same
plot(tInstagram,StanRes2, ylab="Standardized Residuals") #same
plot(tMarital,StanRes2, ylab="Standardized Residuals") #same
plot(tSex,StanRes2, ylab="Standardized Residuals") #same
plot(tAge,StanRes2, ylab="Standardized Residuals") #same
plot(tChildren,StanRes2, ylab="Standardized Residuals") #same
plot(tEducation,StanRes2, ylab="Standardized Residuals") #same
plot(tJobSat,StanRes2, ylab="Standardized Residuals") #same
plot(tIncome,StanRes2, ylab="Standardized Residuals") #same
plot(tWorkHrs,StanRes2, ylab="Standardized Residuals") #same

attach(Happiness)

plot(Happy~fitted(m2))
#we do not see a linear relationship between Y and Y(hat)
#In the linear regression, you want the predicted values to be close to the actual values. 
#So to have a good fit, that plot should resemble a straight line at 45 degrees.
#we conclude that the model does not seem to provide an adequate fit and should be revised


#we will now resort to taking a certain subset of Xi's that will give us a better model
#we will use all possible subsets, forward stepwise, and backward stepwise

X <-cbind(tHappy,tWorkHrs,tHealth,tOwnHome,tHousehold,tAge,tChildren,tMarital,tSex,tEducation,tJobSat,tIncome,tInstagram)
vif(m2)

#we find that no variance inflation factors are greater than 5. we conclude that the associated regression
#coefficients are not necesarily poorly estimated due to multicolinearity
library(leaps)
newX <- na.exclude(X)


newX2 <- newX[,2:13]  #we focus on the Xi's
b <- regsubsets(as.matrix(newX2),newX[,1], nvmax = 12) 
rs <- summary(b)
rs$adjr2
rs
par(mfrow=c(1,1))
plot(1:12,rs$adjr2,xlab="Subset Size",ylab="Adjusted R-squared")

#these are all our possible subsets
om1 <- lm(tHappy ~ tMarital)
om2 <- lm(tHappy ~ tMarital + tIncome)
om3 <- lm(tHappy ~ tMarital + tHealth + tOwnHome)
om4 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat)
om5 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren)
om6 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren + tHousehold)
om7 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren + tHousehold + tIncome)
om8 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren + tHousehold + tIncome + tEducation)
om9 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren + tHousehold + tIncome + tEducation + tWorkHrs)
om10 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren + tHousehold + tIncome + tEducation + tWorkHrs + tSex)
om11 <- lm(tHappy ~ tMarital + tHealth + tOwnHome + tJobSat + tChildren + tHousehold + tIncome + tEducation + tWorkHrs + tSex + tInstagram)
om12 <- m2


#we will now create a matrix in which we have the AIC, AICc and BIC values for each subset.
#this will be used to determine which subset is most appropriate for our dataset


Rad <- rs$adjr2

#Subset size=1
n <- length(om1$residuals); p <- 1
#Calculate AIC
AIC1 <- extractAIC(om1,k=2)[2]
#Calculate AICc = AIC + 2k(k+1)/(n-k-1)
AICc1 <- extractAIC(om1,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC1 <-extractAIC(om1,k=log(n))[2]

#Subset size=2
p <-2
#Calculate AIC
AIC2 <- extractAIC(om2,k=2)[2]
#Calculate AICc
AICc2 <- extractAIC(om2,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC2 <- extractAIC(om2,k=log(n))[2]

#Subset size=3
p <- 3
#Calculate AIC
AIC3 <- extractAIC(om3,k=2)[2]
#Calculate AICc
AICc3 <- extractAIC(om3,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC3 <- extractAIC(om3,k=log(n))[2]

#Subset size=4
p <- 4
#Calculate AIC
AIC4 <- extractAIC(om4,k=2)[2]
#Calculate AICc
AICc4 <- extractAIC(om4,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC4 <- extractAIC(om4,k=log(n))[2]

#Subset size=5
p <- 5
#Calculate AIC
AIC5 <- extractAIC(om5,k=2)[2]
#Calculate AICc
AICc5 <- extractAIC(om5,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC5 <- extractAIC(om5,k=log(n))[2]

#Subset size=6
p <- 6
#Calculate AIC
AIC6 <- extractAIC(om6,k=2)[2]
#Calculate AICc = AIC + 2k(k+1)/(n-k-1)
AICc6 <- extractAIC(om6,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC6 <-extractAIC(om6,k=log(n))[2]

#Subset size=7
p <-7
#Calculate AIC
AIC7 <- extractAIC(om7,k=2)[2]
#Calculate AICc
AICc7 <- extractAIC(om7,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC7 <- extractAIC(om7,k=log(n))[2]

#Subset size=8
p <- 8
#Calculate AIC
AIC8 <- extractAIC(om8,k=2)[2]
#Calculate AICc
AICc8 <- extractAIC(om8,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC8 <- extractAIC(om8,k=log(n))[2]

#Subset size=9
p <- 9
#Calculate AIC
AIC9 <- extractAIC(om9,k=2)[2]
#Calculate AICc
AICc9 <- extractAIC(om9,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC9 <- extractAIC(om9,k=log(n))[2]

#Subset size=10
p <- 10
#Calculate AIC
AIC10 <- extractAIC(om10,k=2)[2]
#Calculate AICc
AICc10 <- extractAIC(om10,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC10 <- extractAIC(om10,k=log(n))[2]

#Subset size=11
p <- 11
#Calculate AIC
AIC11 <- extractAIC(om11,k=2)[2]
#Calculate AICc = AIC + 2k(k+1)/(n-k-1)
AICc11 <- extractAIC(om11,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC11 <-extractAIC(om11,k=log(n))[2]

#Subset size=12
p <-12
#Calculate AIC
AIC12 <- extractAIC(om12,k=2)[2]
#Calculate AICc
AICc12 <- extractAIC(om12,k=2)[2]+2*(p+2)*(p+3)/(n-p-1)
#Calculate BIC
BIC12 <- extractAIC(om12,k=log(n))[2]



AIC <- c(AIC1,AIC2,AIC3,AIC4,AIC5,AIC6,AIC7,AIC8,AIC9,AIC10,AIC11,AIC12)
AICc <- c(AICc1,AICc2,AICc3,AICc4,AICc5,AICc6,AICc7,AICc8,AICc9,AICc10,AICc11,AICc12)
BIC <- c(BIC1,BIC2,BIC3,BIC4,BIC5,BIC6,BIC7,BIC8,BIC9,BIC10,BIC11,BIC12)

table <- data.frame(Size=1:12, Radj2=Rad,AIC=AIC, AICc=AICc, BIC=BIC)
table

lapply(table, min)


#Below are the values of R^2 adjusted, AIC, AICc, and BIC that we examine for determining the best models
#from the All Possible Subsets method

# > table
# Size      Radj2       AIC      AICc       BIC
# 1     1 0.04614797 -4333.069 -4333.059 -4321.536
# 2     2 0.05548366 -4357.213 -4357.196 -4339.914
# 3     3 0.06705359 -4375.114 -4375.089 -4352.049
# 4     4 0.07949525 -4405.635 -4405.599 -4376.803
# 5     5 0.08460030 -4409.011 -4408.963 -4374.412
# 6     6 0.08817370 -4414.842 -4414.781 -4374.477
# 7     7 0.08954797 -4417.608 -4417.532 -4371.477
# 8     8 0.09007296 -4413.043 -4412.949 -4361.145
# 9     9 0.09037201 -4402.565 -4402.453 -4344.901
# 10   10 0.09044800 -4401.501 -4401.368 -4338.070
# 11   11 0.09055417 -4389.162 -4389.007 -4319.965
# 12   12 0.09018496 -4372.692 -4372.513 -4297.728


#we notice that for subset n = 4, BIC is the lowest
#we notice that for subset n = 7, AIC is the lowest, and AICC is also the lowest
#we notice that for subset n = 11, R^2 adjusted is the highest

summary(om4)


# > summary(om4)
# 
# Call:
#   lm(formula = tHappy ~ tMarital + tHealth + tOwnHome + tJobSat)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.82796 -0.39031  0.06058  0.23623  0.90165 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  0.226439   0.145924   1.552    0.121    
# tMarital     1.197303   0.122469   9.776  < 2e-16 ***
#   tHealth      0.377487   0.050103   7.534 6.97e-14 ***
#   tOwnHome    -0.585089   0.073657  -7.943 3.02e-15 ***
#   tJobSat      0.015542   0.002794   5.563 2.96e-08 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3914 on 2346 degrees of freedom
# (16 observations deleted due to missingness)
# Multiple R-squared:  0.08301,	Adjusted R-squared:  0.08145 
# F-statistic: 53.09 on 4 and 2346 DF,  p-value: < 2.2e-16


#We notice that all variables are significant here. This is good


summary(om7) #we notice that all variables here are also significant predictors
summary(om11) #we notice that there are certain variables that are not good predictors. We throw this model out

library(leaps)
HappinessSub = Happiness[complete.cases(Happiness), ]  #New Dataset without NAs
HappinessSub
dim(HappinessSub)
om1 = lm(I(Happy^(0.7328))~I(Health^(0.1951)), data=HappinessSub)
n = length(om1$residuals)

m3 = lm(I(Happy^0.7328)~I(Health^0.1951)+I(OwnHome^-0.1871)+I(Household^-0.3244)+I(Age^0.4648)+I(Children^0.3336)+I(Instagram^0.3944)+I(Marital^0.0918)+I(Sex^1.0353)+I(Education^1.2063)+I(JobSat^-0.4434)+I(Income^0.0439)+I(WorkHrs^1.452),data = HappinessSub, na.action=na.exclude)

#we will now approach the "final" model from two different sides, and then see which one is best (could give us same model)

#we will first start with the largest subset and then each time, we will drop by 1


backAIC <- step(m3,direction="backward", data=HappinessSub)
backBIC <- step(m3,direction="backward", data=HappinessSub, k=log(n))


mint <- lm((Happy^(0.7328))~1,data=HappinessSub)

#now, we will use forward selection

#in this process, a regressor added at an EARLIER step in the process may become redundant because of
#the relationship between it and those regressors added afterward. As a result, the final model may contain terms of little
#value which makes it discredible.



forwardAIC <- step(mint,scope=list(lower=~1, 
                                   upper=~I(Health^0.1951)+I(OwnHome^-0.1871)+I(Household^-0.3244)+I(Age^0.4648)+I(Children^0.3336)+I(Instagram^0.3944)+I(Marital^0.0918)+I(Sex^1.0353)+I(Education^1.2063)+I(JobSat^-0.4434)+I(Income^0.0439)+I(WorkHrs^1.452)),
                   direction="forward", data=HappinessSub)

forwardBIC <- step(mint,scope=list(lower=~1,                          upper=~I(Health^0.1951)+I(OwnHome^-0.1871)+I(Household^-0.3244)+I(Age^0.4648)+I(Children^0.3336)+I(Instagram^0.3944)+I(Marital^0.0918)+I(Sex^1.0353)+I(Education^1.2063)+I(JobSat^-0.4434)+I(Income^0.0439)+I(WorkHrs^1.452)),
                   direction="forward", data=HappinessSub,k=log(n))


#we notice that
BestSubset = lm(tHappy ~ tHealth + tOwnHome + tHousehold + 
                  tChildren + tMarital + 
                  tJobSat + tIncome, na.action = na.exclude)
summary(BestSubset) #all variables significant
plot(BestSubset)


lev_best <- hat(model.matrix(BestSubset))

sum(lev_best > (2 * (8)/nrow(Happiness)))

large.levs_best <- which((lev_best > (2 * (8)/nrow(Happiness))))

#we now have have 19 leverage points (could be good or bad) which is less than 29 (our un-subsetted model) 
#This is even better so we know we at least improved our model by a little


our_outliers_best <- which(rstandard(BestSubset) < (-2) | rstandard(BestSubset) > 2)

sum(our_outliers_best %in% large.levs_best)

#We now have 0 bad leverage points compared to 2 bad leverage points in the transformed, but non-subset model
#Now we know we are on the right track



StanRes3 <- rstandard(BestSubset)
par(mfrow=c(1,1))

#The following plots at the bottom look slightly better and improved than non-subsetted model


plot(tHealth,StanRes3, ylab="Standardized Residuals")
plot(tOwnHome,StanRes3, ylab="Standardized Residuals")
plot(tHousehold,StanRes3, ylab="Standardized Residuals")
plot(tChildren,StanRes3, ylab="Standardized Residuals")
plot(tMarital,StanRes3, ylab="Standardized Residuals")
plot(tJobSat,StanRes3, ylab="Standardized Residuals")
plot(tIncome,StanRes3, ylab="Standardized Residuals")



#added variable plots look a little better. The slope is steeper and the relationship
#is slightly more linear, especially in certain variables such as tHleath and tMarital


avPlot(BestSubset,variable=tHealth)
avPlot(BestSubset,variable=tOwnHome)
avPlot(BestSubset,variable=tHousehold)
avPlot(BestSubset,variable=tChildren)
avPlot(BestSubset,variable=tMarital)
avPlot(BestSubset,variable=tJobSat)
avPlot(BestSubset,variable=tIncome)


save.image("101A Project.RData")




























































