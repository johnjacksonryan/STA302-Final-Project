## Final Project ##
## John Ryan ##
## 1005024821 ##

## Model Selection ##

## If the package is not already installed then use ##
## install.packages('NHANES') ; install.packages('tidyverse') ; install.packages("UsingR") ;install.packages("scatterplot3d")
library(tidyverse)
library(NHANES)
library(UsingR)
library(scatterplot3d)
library(matrix)
library(lmtest)
library(car)
library(glmnet)
library(rms)
library(MASS)

small.nhanes <- na.omit(NHANES[NHANES$SurveyYr=="2011_12"
                               & NHANES$Age > 17,c(1,3,4,8:11,13,17,20,21,25,46,50,51,52,61)])
small.nhanes <- as.data.frame(small.nhanes %>%
                                group_by(ID) %>% filter(row_number()==1) )
nrow(small.nhanes)
## Checking whether there are any ID that was repeated. If not ##
## then length(unique(small.nhanes$ID)) and nrow(small.nhanes) are same ##
length(unique(small.nhanes$ID))

## Create training and test set ##
set.seed(1005024821)
train <- small.nhanes[sample(seq_len(nrow(small.nhanes)), size = 500),]
n <- nrow(train)
length(which(small.nhanes$ID %in% train$ID))
test <- small.nhanes[!small.nhanes$ID %in% train$ID,]
nrow(test)

## Predictors ##
BPSysAve <- train[,12]
Gender <- matrix(data = NA, ncol = 1, nrow = n)
G <- as.numeric(train[,2])
Age <- train[,3]
Race <- matrix(data = NA, ncol = 5, nrow = n)
R <- as.numeric(train[,4])
Education <- matrix(data = NA, ncol = 4, nrow = n)
E <- as.numeric(train[,5])
MaritalStatus <- matrix(data = NA, ncol = 5, nrow = n)
M <- as.numeric(train[,6])
Income <- matrix(data = NA, ncol = 11, nrow = n)
I <- as.numeric(train[,7])
Poverty <- train[,8]
Weight <- train[,9]
Height <- train[,10]
BMI <- train[,11]
Depressed <- matrix(data = NA, ncol = 2, nrow = n)
D <- as.numeric(train[,13])
SleepHrs <- train[,14]
SleepTrouble <- matrix(data = NA, ncol = 1, nrow = n)
S <- as.numeric(train[,15])
PhysActive <- matrix(data = NA, ncol = 1, nrow = n)
P <- as.numeric(train[,16])
SmokeNow <- matrix(data = NA, ncol = 1, nrow = n)
Smoke <- as.numeric(train[,17])

## Setting the Categorical Variables ##
for (i in 1:n){
  if(G[i] == 1) Gender[i,] = 1 else Gender[i,] = 0            ## Male - 0 Female - 1
  if(R[i] == 5) Race[i,] = c(0,0,0,0,0)                       ## White
  if(R[i] == 2) Race[i,] = c(1,0,0,0,0)                       ## Black
  if(R[i] == 3) Race[i,] = c(0,1,0,0,0)                       ## Hispanic
  if(R[i] == 4) Race[i,] = c(0,0,1,0,0)                       ## Mexican
  if(R[i] == 1) Race[i,] = c(0,0,0,1,0)                       ## Asian
  if(R[i] == 6) Race[i,] = c(0,0,0,0,1)                       ## Other
  if(E[i] == 5) Education[i,] = c(0,0,0,0)                    ## College Grad
  if(E[i] == 4) Education[i,] = c(1,0,0,0)                    ## Some College
  if(E[i] == 3) Education[i,] = c(0,1,0,0)                    ## High School
  if(E[i] == 2) Education[i,] = c(0,0,1,0)                    ## 9-11th Grade
  if(E[i] == 1) Education[i,] = c(0,0,0,1)                    ## 8th Grade
  if(M[i] == 3) MaritalStatus[i,] = c(0,0,0,0,0)              ## Married
  if(M[i] == 1) MaritalStatus[i,] = c(1,0,0,0,0)              ## Divorced
  if(M[i] == 2) MaritalStatus[i,] = c(0,1,0,0,0)              ## Live Partner
  if(M[i] == 4) MaritalStatus[i,] = c(0,0,1,0,0)              ## Never Married
  if(M[i] == 6) MaritalStatus[i,] = c(0,0,0,1,0)              ## Widowed
  if(M[i] == 5) MaritalStatus[i,] = c(0,0,0,0,1)              ## Separated
  if(I[i] == 1) Income[i,] = c(0,0,0,0,0,0,0,0,0,0,0)         ## 0-4999
  if(I[i] == 2) Income[i,] = c(1,0,0,0,0,0,0,0,0,0,0)         ## 5000-9999
  if(I[i] == 3) Income[i,] = c(0,1,0,0,0,0,0,0,0,0,0)         ## 10000-14999
  if(I[i] == 4) Income[i,] = c(0,0,1,0,0,0,0,0,0,0,0)         ## 15000-19999
  if(I[i] == 5) Income[i,] = c(0,0,0,1,0,0,0,0,0,0,0)         ## 20000-24999
  if(I[i] == 6) Income[i,] = c(0,0,0,0,1,0,0,0,0,0,0)         ## 25000-34999
  if(I[i] == 7) Income[i,] = c(0,0,0,0,0,1,0,0,0,0,0)         ## 35000-44999
  if(I[i] == 8) Income[i,] = c(0,0,0,0,0,0,1,0,0,0,0)         ## 45000-54999
  if(I[i] == 9) Income[i,] = c(0,0,0,0,0,0,0,1,0,0,0)         ## 55000-64999
  if(I[i] == 10) Income[i,] = c(0,0,0,0,0,0,0,0,1,0,0)        ## 65000-74999
  if(I[i] == 11) Income[i,] = c(0,0,0,0,0,0,0,0,0,1,0)        ## 75000-99999
  if(I[i] == 12) Income[i,] = c(0,0,0,0,0,0,0,0,0,0,1)        ## 99999+
  if(D[i] == 1) Depressed[i,] = c(0,0)                        ## None
  if(D[i] == 2) Depressed[i,] = c(1,0)                        ## Some
  if(D[i] == 3) Depressed[i,] = c(0,1)                        ## Most
  if(S[i] == 1) SleepTrouble[i,] = 0 else SleepTrouble[i,] = 1## No - 0 Yes - 1
  if(P[i] == 1) PhysActive[i,] = 0 else PhysActive[i,] = 1    ## No - 0 Yes - 1
  if(Smoke[i] == 1) SmokeNow[i,] = 0 else SmokeNow[i,] = 1    ## No - 0 Yes - 1
}

##### Step 1 ######
## Initial investigation of scatter plots ##
## Single variable models ##
## Correlation between variables ##
## Interaction considerations ##

## Full Multivariate Model ##
mvmodel.full <- lm(BPSysAve ~ Gender + Age + Race + Education + MaritalStatus + 
                     Income + Poverty + Weight + Height + BMI + Depressed + SleepHrs +
                     SleepTrouble + PhysActive + SmokeNow)
summary(mvmodel.full)

## Standardized Residuals and Q-Q ##
resid <- rstudent(mvmodel.full)
fitted <- predict(mvmodel.full)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", main = "Fitted Values vs. Standardized Residuals for the initial model",
     cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

plot(BPSysAve ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "BPSysAve", main = "Fitted Values vs. Outcome for the initial model",
     cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, BPSysAve), col = "blue")

## The Q-Q Plot shows a deviation from normality ##
## Also potential heterodasticity seen in fitted vs outcome ##

## Look at VIF ##
vif(mvmodel.full)

## Variables above cutoff are Poverty, Income, Weight, Height and BMI ##
## This makes sense as Poverty & Income as well as Height, Weight, and BMI ##
## are highly correlated based on how they are computed. ##

## Intially I will use single variable models to check for non-linearity ##
## and for the predictors single variable significance on the response. ##

## Hold pvalues ##
pvalues.single <- matrix(data=NA, ncol=2, nrow=42)
pvalues.single[,1] <- c("Gender", "Age", "White", "Black", "Hispanic", "Mexican", "Asian", "Other",
                        "College Grad", "Some College", "High School", "9-11th Grade", "8th Grade",
                        "Married", "Divorced", "Live Partner", "Never Married", "Widowed", "Separated",
                        "0-4999", "5000-9999", "10000-14999", "15000-19999", "20000-24999", "25000-34999", "35000-44999",
                        "45000-54999", "55000-64999", "65000-74999", "75000-99999", "99999+",
                        "Poverty", "Weight", "Height", "BMI","None", "Some", "Most", "SleepHrs",
                        "SleepTrouble", "PhysActive", "SmokeNow")

## Gender ##
gender_model <- lm(BPSysAve ~ Gender)
summary(gender_model)
pvalues.single[1,2] <- summary(gender_model)$coefficients[,4][2]
boxplot(BPSysAve ~ train[,2], xlab="Gender", main="Gender vs. BPSysAve")
## Significance is high, should be used ##

## Age ##
age_model <- lm(BPSysAve ~ Age)
summary(age_model)
pvalues.single[2,2] <- summary(age_model)$coefficients[,4][2]
plot(Age, BPSysAve, type = 'p', main="Age vs. BPSysAve")
abline(age_model, col = "red")

## Appears potentially heterodastic. Check Standard Residuals. ##
resid <- rstudent(age_model)
fitted <- predict(age_model)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

boxcox(age_model)

## Q-Q Plot shows a deviation from normality ##

## Race ##
race_model <- lm(BPSysAve ~ Race)
summary(race_model)
pvalues.single[3:8,2] <- summary(race_model)$coefficients[,4][1:6]
boxplot(BPSysAve ~ train[,4], xlab="Race", main="Race vs. BPSysAve")
## Significance is low for all categories ##

## Education ##
education_model <- lm(BPSysAve ~ Education)
summary(education_model)
pvalues.single[9:13,2] <- summary(education_model)$coefficients[,4][1:5]
boxplot(BPSysAve ~ train[,5], xlab="Education", main="Education vs. BPSysAve")
## Significance is low for most except for 8th grade. ##
## Based on the plot I will try one dummy, 8th grade and others ##

Education.R <- matrix(data = NA, ncol = 1, nrow = n)
for (i in 1:n){
  if(E[i] == 1) Education.R[i,] = 1 else Education.R[i,] = 0
}
education_model.R <- lm(BPSysAve ~ Education.R)
summary(education_model.R)
## Significance is much higher using this reduced model ##


## Marital Status ##
maritalstatus_model <- lm(BPSysAve ~ MaritalStatus)
summary(maritalstatus_model)
pvalues.single[14:19,2] <- summary(maritalstatus_model)$coefficients[,4][1:6]
boxplot(BPSysAve ~ train[,6], xlab="Marital Status", main="Marital Status vs. BPSysAve")
## Significance is high for LivePartner and Widowed. Widowed is very different ##
## Based on the box plot I will try a dummy variable for ##
## Widowed and Others. ##

MaritalStatus.R <- matrix(data = NA, ncol = 1, nrow = n)
for (i in 1:n){
  if(M[i] == 3) MaritalStatus.R[i,] = 0              ## Married
  if(M[i] == 1) MaritalStatus.R[i,] = 0              ## Divorced
  if(M[i] == 2) MaritalStatus.R[i,] = 0              ## Live Partner
  if(M[i] == 4) MaritalStatus.R[i,] = 0              ## Never Married
  if(M[i] == 6) MaritalStatus.R[i,] = 1              ## Widowed
  if(M[i] == 5) MaritalStatus.R[i,] = 0              ## Separated
}
maritalstatus_model.R <- lm(BPSysAve ~ MaritalStatus.R)
summary(maritalstatus_model.R)

## Significance is higher with reduced model ##
## Note that these differences may be due to Age ##

## Income ##
income_model <- lm(BPSysAve ~ Income)
summary(income_model)
pvalues.single[20:31,2] <- summary(income_model)$coefficients[,4][1:12]
boxplot(BPSysAve ~ train[,7], xlab="Income", main="Income vs. BPSysAve")
## Significance is low for all incomes, and no trend is apparent in the plot ##

## Poverty ##
poverty_model <- lm(BPSysAve ~ Poverty)
summary(poverty_model)
pvalues.single[32,2] <- summary(poverty_model)$coefficients[,4][2]
plot(Poverty, BPSysAve, type = 'p', main="Poverty vs. BPSysAve")
abline(poverty_model, col = "red")
## Significance is low, and no trend is apparent in the plot ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(poverty_model)
fitted <- predict(poverty_model)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

boxcox(poverty_model)

## Again the Q-Q Plot shows deviation from normality ##

## Weight ##
weight_model <- lm(BPSysAve ~ Weight)
summary(weight_model)
pvalues.single[33,2] <- summary(weight_model)$coefficients[,4][2]
plot(Weight, BPSysAve, type = 'p', main="Weight vs. BPSysAve")
abline(weight_model, col = "red")
## Significance is low, and no trend is apparent in the plot ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(weight_model)
fitted <- predict(weight_model)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

boxcox(weight_model)

## Again the Q-Q Plot shows deviation from normality ##

## Height ##
height_model <- lm(BPSysAve ~ Height)
summary(height_model)
pvalues.single[34,2] <- summary(height_model)$coefficients[,4][2]
plot(Height, BPSysAve, type = 'p', main="Height vs. BPSysAve")
abline(height_model, col = "red")
## Significance is low, and no trend is apparent in the plot ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(height_model)
fitted <- predict(height_model)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

boxcox(height_model)

## Again the Q-Q Plot shows deviation from normality ##

## BMI ##
BMI_model <- lm(BPSysAve ~ BMI)
summary(BMI_model)
pvalues.single[35,2] <- summary(BMI_model)$coefficients[,4][2]
plot(BMI, BPSysAve, type = 'p', main="BMI vs. BPSysAve")
abline(BMI_model, col = "red")
## Significance is moderate, but the slope is so low the R^2 is small ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(BMI_model)
fitted <- predict(BMI_model)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

boxcox(BMI_model)

## Again the Q-Q Plot shows deviation from normality ##

## Depressed ##
depressed_model <- lm(BPSysAve ~ Depressed)
summary(depressed_model)
pvalues.single[36:38,2] <- summary(depressed_model)$coefficients[,4][1:3]
boxplot(BPSysAve ~ train[,13], xlab="Depressed", main="Depression vs. BPSysAve")
## Significance is low, and no trend is apparent in the plot ##

## SleepHrs ##
sleephrs_model <- lm(BPSysAve ~ SleepHrs)
summary(sleephrs_model)
pvalues.single[39,2] <- summary(sleephrs_model)$coefficients[,4][2]
plot(SleepHrs, BPSysAve, type = 'p', main="SleepHrs vs. BPSysAve")
## Significance is low, and no trend is apparent in the plot ##

## SleepTrouble ##
sleeptrouble_model <- lm(BPSysAve ~ SleepTrouble)
summary(sleeptrouble_model)
pvalues.single[40,2] <- summary(sleeptrouble_model)$coefficients[,4][2]
boxplot(BPSysAve ~ train[,15], xlab="SleepTrouble", main="SleepTrouble vs. BPSysAve")
## Significance is low, and no trend is apparent in the plot ##

## PhysActive ##
physactive_model <- lm(BPSysAve ~ PhysActive)
summary(physactive_model)
pvalues.single[41,2] <- summary(physactive_model)$coefficients[,4][2]
boxplot(BPSysAve ~ train[,16], xlab="PhysActive", main="PhysActive vs. BPSysAve")
## Significance is high ##

## SmokeNow ##
smokenow_model <- lm(BPSysAve ~ SmokeNow)
summary(smokenow_model)
pvalues.single[42,2] <- summary(smokenow_model)$coefficients[,4][2]
boxplot(BPSysAve ~ train[,17], xlab="SmokeNow", main="Smoking vs. BPSysAve")
## Significance is high ##

pvalues.single

## The single variable models indicate that Gender, Age, Education (particularly ##
## 8th grade), MaritalStatus (particularly Widowed), PhysActive, and SmokeNow, are the ##
## most significant predictors (using a 0.05 level). ##

## Poverty and BMI are both of moderate significance ##

## Race, Income, Weight, Height, Depressed, SleepHrs, and SleepTrouble are the least significant. ##

## Almagamating all the data ##

fulldata <- matrix(data=NA, ncol=38, nrow=n)
fulldata[,1] <- Gender
fulldata[,2] <- Age
fulldata[,3:7] <- Race
fulldata[,8:11] <- Education
fulldata[,12:16] <- MaritalStatus
fulldata[,17:27] <- Income
fulldata[,28] <- Poverty
fulldata[,29] <- Weight
fulldata[,30] <- Height
fulldata[,31] <- BMI
fulldata[,32] <- BPSysAve
fulldata[,33:34] <- Depressed
fulldata[,35] <- SleepHrs
fulldata[,36] <- SleepTrouble
fulldata[,37] <- PhysActive
fulldata[,38] <- SmokeNow
labels <- c("Gender", "Age", "Black", "Hispanic", "Mexican", "Asian", "Other",
            "Some College", "High School", "9-11th Grade", "8th Grade",
            "Divorced", "Live Partner", "Never Married", "Widowed", "Separated",
            "5000-9999", "10000-14999", "15000-19999", "20000-24999", "25000-34999", "35000-44999",
            "45000-54999", "55000-64999", "65000-74999", "75000-99999", "99999+",
            "Poverty", "Weight", "Height", "BMI", "BPSysAve", "Some", "Most", "SleepHrs",
            "SleepTrouble", "PhysActive", "SmokeNow")


## Identifying Highly Correlated Variables ##
cor <- cor(fulldata)
print("Correlation above 0.5")
for (i in 1:38){
  for (j in 1:i) {
    if (cor[i, j] >= 0.5 && i!=j){
      print(c(labels[i], labels[j]))
    }
  }
}
print("Correlation above 0.3")
for (i in 1:38){
  for (j in 1:i) {
    if (0.5 > cor[i, j] && cor[i, j] >= 0.3 && i!=j){
      print(c(labels[i], labels[j]))
    }
  }
}
print("Correlation above 0.1")
for (i in 1:38){
  for (j in 1:i) {
    if (0.3 > cor[i,j] && cor[i,j] >= 0.1 && i!=j){
      print(c(labels[i], labels[j]))
    }
  }
}

## Investigation of highly correlated variables ##

## Pairs Scatter Plots ##

pairs(~ BPSysAve + Age + Poverty + Weight + Height + BMI + SleepHrs)

## Poverty and Income ##

plot(Poverty ~ train[,7], xlab = "Income")
model.Poverty.Income <- lm(Poverty ~ Income)
summary(model.Poverty.Income)
## Here the correlation between Income and Poverty is clear ##
## Only one or the other should be included in the model ##

model.BMI.HW <- lm(BMI ~ Height + Weight)
summary(model.BMI.HW)
## BMI is computed using height and weight, so the R^2 value is extremely high ##
## Thus either height and weight or BMI should be removed from the model. ##
## Compare models of BPSysAve vs BMI and one vs Weight and Height ##
HW_model <- lm(BPSysAve ~ Height + Weight)
summary(BMI_model)
summary(HW_model)
## BMI is more significant in these reduced models ##

## Consider Interaction Models via Scatter Plots ##

## Smoke Now ##
smokers <- which(SmokeNow == 1)
non_smokers <- which(SmokeNow == 0)

## PhysActive ##
active <- which(PhysActive == 1)
non_active <- which(PhysActive == 0)

## Gender ##
male <- which(Gender == 0)
female <- which(Gender == 1)

## Age and SmokeNow ##
model.smoker.age <- lm(BPSysAve[smokers] ~ Age[smokers])
model.non_smoker.age <- lm(BPSysAve[non_smokers] ~ Age[non_smokers])
plot(Age[smokers], BPSysAve[smokers], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "Age of smokers (blue) and non-smokers (red)", main = "SmokeNow on Age vs. Blood Pressure")
points(Age[non_smokers], BPSysAve[non_smokers], type = 'p', col = "red")
abline(model.smoker.age, col="blue")
abline(model.non_smoker.age, col = "red")

## Age and PhysActive ##
model.active.age <- lm(BPSysAve[active] ~ Age[active])
model.non_active.age <- lm(BPSysAve[non_active] ~ Age[non_active])
plot(Age[active], BPSysAve[active], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "Age of physically active (blue) and non physically active (red)", 
     main = "PhysActive on Age vs. Blood Pressure")
points(Age[non_active], BPSysAve[non_active], type = 'p', col = "red")
abline(model.active.age, col="blue")
abline(model.non_active.age, col = "red")

## Age and Gender ##
model.male.age <- lm(BPSysAve[male] ~ Age[male])
model.female.age <- lm(BPSysAve[female] ~ Age[female])
plot(Age[male], BPSysAve[male], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "Age of males (blue) and females (red)", main = "Gender on Age vs. Blood Pressure")
points(Age[female], BPSysAve[female], type = 'p', col = "red")
abline(model.male.age, col="blue")
abline(model.female.age, col = "red")

## BMI and SmokeNow ##
model.smoker.BMI <- lm(BPSysAve[smokers] ~ BMI[smokers])
model.non_smoker.BMI <- lm(BPSysAve[non_smokers] ~ BMI[non_smokers])
plot(BMI[smokers], BPSysAve[smokers], type = 'p', col = "blue", ylab = "BPSysAve",
     xlab = "BMI of smokers (blue) and non-smokers (red)", main = "SmokeNow on BMI vs. Blood Pressure")
points(BMI[non_smokers], BPSysAve[non_smokers], type = 'p', col = "red")
abline(model.smoker.BMI, col="blue")
abline(model.non_smoker.BMI, col = "red")

## BMI and PhysActive ##
model.active.BMI <- lm(BPSysAve[active] ~ BMI[active])
model.non_active.BMI <- lm(BPSysAve[non_active] ~ BMI[non_active])
plot(BMI[active], BPSysAve[active], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "BMI of physically active (blue) and non physically active (red)", 
     main = "PhysActive BMI vs. Blood Pressure")
points(BMI[non_active], BPSysAve[non_active], type = 'p', col = "red")
abline(model.active.BMI, col="blue")
abline(model.non_active.BMI, col = "red")

## BMI and Gender ##
model.male.BMI <- lm(BPSysAve[male] ~ BMI[male])
model.female.BMI <- lm(BPSysAve[female] ~ BMI[female])
plot(BMI[male], BPSysAve[male], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "BMI of males (blue) and females (red)", main = "Gender on BMI vs. Blood Pressure")
points(BMI[female], BPSysAve[female], type = 'p', col = "red")
abline(model.male.BMI, col="blue")
abline(model.female.BMI, col = "red")

## Poverty and SmokeNow ##
model.smoker.poverty <- lm(BPSysAve[smokers] ~ Poverty[smokers])
model.non_smoker.poverty <- lm(BPSysAve[non_smokers] ~ Poverty[non_smokers])
plot(Poverty[smokers], BPSysAve[smokers], type = 'p', col = "blue", ylab = "BPSysAve",
     xlab = "Poverty of smokers (blue) and non-smokers (red)", main = "SmokeNow on Poverty vs. Blood Pressure")
points(Poverty[non_smokers], BPSysAve[non_smokers], type = 'p', col = "red")
abline(model.smoker.poverty, col="blue")
abline(model.non_smoker.poverty, col = "red")

## Poverty and PhysActive ##
model.active.poverty <- lm(BPSysAve[active] ~ Poverty[active])
model.non_active.poverty <- lm(BPSysAve[non_active] ~ Poverty[non_active])
plot(Poverty[active], BPSysAve[active], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "Poverty of physically active (blue) and non physically active (red)", 
     main = "PhysActive on Poverty vs. Blood Pressure")
points(Poverty[non_active], BPSysAve[non_active], type = 'p', col = "red")
abline(model.active.poverty, col="blue")
abline(model.non_active.poverty, col = "red")

## Weight and Gender ##
model.male.weight <- lm(BPSysAve[male] ~ Weight[male])
model.female.weight <- lm(BPSysAve[female] ~ Weight[female])
plot(Weight[male], BPSysAve[male], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "Weight of males (blue) and females (red)", main = "Gender on Weight vs. Blood Pressure")
points(Weight[female], BPSysAve[female], type = 'p', col = "red")
abline(model.male.weight, col="blue")
abline(model.female.weight, col = "red")

## Weight and PhysActive ##
model.active.weight <- lm(BPSysAve[active] ~ Weight[active])
model.non_active.weight <- lm(BPSysAve[non_active] ~ Weight[non_active])
plot(Weight[active], BPSysAve[active], type = 'p', col = "blue", ylab = "BPSysAve", 
     xlab = "Weight of physically active (blue) and non physically active (red)",
     main = "PhysActive onWeight vs. Blood Pressure")
points(Weight[non_active], BPSysAve[non_active], type = 'p', col = "red")
abline(model.active.weight, col="blue")
abline(model.non_active.weight, col = "red")

## Weight and SmokeNow ##
model.smoker.weight <- lm(BPSysAve[smokers] ~ Weight[smokers])
model.non_smoker.weight <- lm(BPSysAve[non_smokers] ~ Weight[non_smokers])
plot(Weight[smokers], BPSysAve[smokers], type = 'p', col = "blue", ylab = "BPSysAve",
     xlab = "Weight of smokers (blue) and non-smokers (red)", main = "SmokeNow on Weight vs. Blood Pressure")
points(Weight[non_smokers], BPSysAve[non_smokers], type = 'p', col = "red")
abline(model.smoker.weight, col="blue")
abline(model.non_smoker.weight, col = "red")

## These plots indicate that there is potential interaction between the following: ##
## SmokeNow and Weight ##
## PhysActive and Poverty ##
## Gender and Age ##
## SmokeNow and Age ##

##### Step 2 #####
## Intial Model ##
## Diagnostics ##

## For the intial model I have chosen variables that were at least moderately significant ##
## in their single variable models and showed some correlation in the scatter plots ##
## Education and Marital Status have been reduced to their simpler dummies ## 
## I have also included potential interaction variables ##

## Initial Model ##
model.1 <- lm(BPSysAve ~ Gender + Age + Education.R + MaritalStatus.R + 
                Poverty + BMI + PhysActive + SmokeNow + Age*SmokeNow + Age*Gender +
                Poverty*PhysActive + Weight*SmokeNow)
summary(model.1)

## Diagnostic Check ##
resid <- rstudent(model.1)
fitted <- predict(model.1)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Q-Q Plot Indicates Deviation from Normality ##

boxcox(model.1)

## Try an inverse transformation on the outcome ##
inv.BPSysAve <- 1/(BPSysAve)
model.2 <- lm(inv.BPSysAve ~ Gender + Age + Education.R + MaritalStatus.R + 
                             Poverty + BMI + PhysActive + SmokeNow + Age*SmokeNow + Age*Gender +
                             Poverty*PhysActive + Weight*SmokeNow)
summary(model.2)

## Diagnostic Check ##
resid <- rstudent(model.2)
fitted <- predict(model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Q-Q Plot Indicates a much better pattern for the transformed data ##

## VIF of transformed model ##
vif(model.2)

## Variables above the threshold are those in interaction with one another ##
## I will ignore this for now ##

## Check for linearity between predictors and transformed outcome ##

## Gender ##
gender_model.2 <- lm(inv.BPSysAve ~ Gender)
summary(gender_model.2)
boxplot(inv.BPSysAve ~ train[,2], xlab="Gender", main="Gender vs. inverse BPSysAve")
## Significance is high, should be used ##

## Age ##
age_model.2 <- lm(inv.BPSysAve ~ Age)
summary(age_model.2)
plot(Age, inv.BPSysAve, type = 'p', main="Age vs. inverse BPSysAve")
abline(age_model.2, col = "red")

## Standard Residuals. ##
resid <- rstudent(age_model.2)
fitted <- predict(age_model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Race ##
race_model.2 <- lm(inv.BPSysAve ~ Race)
summary(race_model.2)
boxplot(inv.BPSysAve ~ train[,4], xlab="Race", main="Race vs. inverse BPSysAve")
## Significance is low for all categories ##

## Education ##
education_model.2 <- lm(inv.BPSysAve ~ Education)
summary(education_model.2)
boxplot(inv.BPSysAve ~ train[,5], xlab="Education", main="Education vs. inverse BPSysAve")
## Significance is low for most except for 8th grade. ##
## Based on the plot I will try one dummy, 8th grade and others ##

education_model.R.2 <- lm(inv.BPSysAve ~ Education.R)
summary(education_model.R.2)
## Significance is much higher using this reduced model ##

## Marital Status ##
maritalstatus_model.2 <- lm(inv.BPSysAve ~ MaritalStatus)
summary(maritalstatus_model.2)
boxplot(inv.BPSysAve ~ train[,6], xlab="Marital Status", main="Marital Status vs. inverse BPSysAve")
## Significance is high for LivePartner and Widowed. ##
## Based on the box plot I will try a three category dummy variable for ##
## LivePartner, Widowed and Others. ##

maritalstatus_model.R.2 <- lm(inv.BPSysAve ~ MaritalStatus.R)
summary(maritalstatus_model.R.2)

## Significance is higher with reduced model ##
## Note that these differences may be due to Age ##

## Income ##
income_model.2 <- lm(inv.BPSysAve ~ Income)
summary(income_model.2)
boxplot(inv.BPSysAve ~ train[,7], xlab="Income", main="Income vs. inverse BPSysAve")
## Significance is low for all incomes, and no trend is apparent in the plot ##

## Poverty ##
poverty_model.2 <- lm(inv.BPSysAve ~ Poverty)
summary(poverty_model.2)
plot(Poverty, inv.BPSysAve, type = 'p', main="Poverty vs. inverse BPSysAve")
abline(poverty_model.2, col = "red")
## Significance is low, and no trend is apparent in the plot ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(poverty_model.2)
fitted <- predict(poverty_model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Weight ##
weight_model.2 <- lm(inv.BPSysAve ~ Weight)
summary(weight_model.2)
plot(Weight, inv.BPSysAve, type = 'p', main="Weight vs. inverse BPSysAve")
abline(weight_model.2, col = "red")
## Significance is moderate ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(weight_model.2)
fitted <- predict(weight_model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Height ##
height_model.2 <- lm(inv.BPSysAve ~ Height)
summary(height_model.2)
plot(Height, inv.BPSysAve, type = 'p', main="Height vs. inverse BPSysAve")
abline(height_model.2, col = "red")
## Significance is low, and no trend is apparent in the plot ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(height_model.2)
fitted <- predict(height_model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## BMI ##
BMI_model.2 <- lm(inv.BPSysAve ~ BMI)
summary(BMI_model.2)
plot(BMI, inv.BPSysAve, type = 'p', main="BMI vs. inverse BPSysAve")
abline(BMI_model.2, col = "red")
## Significance is moderate ##

## Standardized Residuals and Q-Q ##
resid <- rstudent(BMI_model.2)
fitted <- predict(BMI_model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Depressed ##
depressed_model.2 <- lm(inv.BPSysAve ~ Depressed)
summary(depressed_model.2)
boxplot(inv.BPSysAve ~ train[,13], xlab="Depressed", main="Depression vs. inverse BPSysAve")
## Significance is low, and no trend is apparent in the plot ##

## SleepHrs ##
sleephrs_model.2 <- lm(inv.BPSysAve ~ SleepHrs)
summary(sleephrs_model.2)
plot(SleepHrs, inv.BPSysAve, type = 'p', main="SleepHrs vs. inverse BPSysAve")
## Significance is low, and no trend is apparent in the plot ##

## SleepTrouble ##
sleeptrouble_model.2 <- lm(inv.BPSysAve ~ SleepTrouble)
summary(sleeptrouble_model.2)
boxplot(inv.BPSysAve ~ train[,15], xlab="SleepTrouble", main="SleepTrouble vs. inverse BPSysAve")
## Significance is low, and no trend is apparent in the plot ##

## PhysActive ##
physactive_model.2 <- lm(inv.BPSysAve ~ PhysActive)
summary(physactive_model.2)
boxplot(inv.BPSysAve ~ train[,16], xlab="PhysActive", main="PhysActive vs. inverse BPSysAve")
## Significance is high ##

## SmokeNow ##
smokenow_model.2 <- lm(inv.BPSysAve ~ SmokeNow)
summary(smokenow_model.2)
boxplot(inv.BPSysAve ~ train[,17], xlab="SmokeNow", main="Smoking vs. inverse BPSysAve")
## Significance is high ##

## Upon inspecting relationships between each predictor and the transformed outcome ## 
## I notice no difference than in the relationships that existed initially ##
## Thus I will not change model.2 and proceed ##

##### Step 3 #####
## Now that I have selected an initial model ##
## I will do diagnostics and remove bad leverages ##

p <- dim(model.matrix(model.2))[2] - 1

## Diagnostics ##
resid <- rstudent(model.2)
fitted <- predict(model.2)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Fitted Values vs. Outcome ##
plot(inv.BPSysAve ~ fitted, type = "p", xlab = "Fitted Values",
     ylab = "Inverse BPSysAve", cex.lab = 1.2, col = "red")
lines(lowess(fitted, inv.BPSysAve), col = "blue")

## Sqrt Standardized Residuals vs Fitted Values ##
sqrt_resid <- abs(resid)^(1/2)
plot(sqrt_resid ~ fitted, type = "p", col = "red", xlab = "Fitted Values", 
     ylab = "Square root standardized residuals", main = "Scale-Location")
lines(lowess(fitted, sqrt_resid), col = "blue")

## Residuals vs Predictors ##
## Age ##
plot(resid ~ Age, type = "p", xlab = "Age", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(Age, resid), col = "blue")

## Poverty ##
plot(resid ~ Poverty, type = "p", xlab = "Poverty", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(Poverty, resid), col = "blue")

## BMI ##
plot(resid ~ BMI, type = "p", xlab = "BMI", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(BMI, resid), col = "blue")

## Points of Influence ##

## Hat Values ###
h <- hatvalues(model.2)
cutoff <- 2*((p+1)/n)
plot(resid ~ h, type = "p", xlab = "Leverage",
     ylab = "Standardized Residuals", cex.lab = 1.2, col = "red")
lines(lowess(h, resid), col = "blue")
abline(v=cutoff, lwd = 3, lty = 2)

hat.bad <- which(h > cutoff)
high.lev <- which(h > 0.1)

## Cooks Distance ##
cooks <- cooks.distance(model.2)
cooks.bad <- which(cooks > qf(0.5, p+1, n-p-1))

## DFFITS ##
dfits <- dffits(model.2)
dfits.bad <- which(abs(dfits) > 2*sqrt((p+1)/n))

## DFBETAS ##
dfb <- dfbetas(model.2)
dfb.mat <- matrix(data=NA, ncol = p+1, nrow = n)
dfb.pts <- matrix(data=NA, ncol = 1, nrow = n)
for (i in 1:n){
  for (j in 1:(p+1)){
    if (abs(dfb[i,j] > 2/sqrt(n))){
      dfb.mat[i,j] <- 1
      dfb.pts[i] <- 1
    }
  }
}
dfb.bad <- which(dfb.pts == 1)

outliers <- which(abs(resid) > 2)

outliers
hat.bad
cooks.bad
dfits.bad
dfb.bad

bad.lev <- union(intersect(hat.bad , outliers), high.lev)
fits.betas <- intersect(dfits.bad, dfb.bad)
pts <- union(bad.lev, fits.betas)

plot(resid ~ h, type = "p", xlab = "Leverage",
     ylab = "Standardized Residuals", main = "Standardized Residuals vs Leverages",
     cex.lab = 1.2, col = "red")
points(resid[fits.betas] ~ h[fits.betas], type = "p", cex.lab = 1.2, col = "blue")
points(resid[bad.lev] ~ h[bad.lev], type = "p", cex.lab = 1.2, col = "green")
lines(lowess(h, resid), col = "blue")
abline(v=cutoff, lwd = 3, lty = 2)

avg.BP <- mean(BPSysAve)
sd.BP <- sqrt(var(BPSysAve))
avg.BP
sd.BP

train[pts,]
info.infl <- train[pts,c(2,3,5,6,8,9,11,12,16,17)]
old <- which(info.infl[,2]>49)
young <- which(info.infl[,2]<50)

low <- which(info.infl[,8] < avg.BP)
high <- which(info.infl[,8] > avg.BP)
old.low <- info.infl[intersect(old, low),]
old.high <- info.infl[intersect(old, high),]
young.low <- info.infl[intersect(young, low),]
young.high <- info.infl[intersect(young, high),]

old.low
old.high
young.low
young.high

avg.old.BP <- mean(BPSysAve[which(Age > 49)])
avg.young.BP <- mean(BPSysAve[which(Age < 50)])
avg.old.BP
avg.young.BP

## Try fitting the model without influence points ##
inv.BPSysAve.new <- inv.BPSysAve[-pts] 
Age.new <- Age[-pts]
Gender.new <- Gender[-pts,]
Education.R.new <- Education.R[-pts,]
MaritalStatus.R.new <- MaritalStatus.R[-pts,]
Poverty.new <- Poverty[-pts]
BMI.new <- BMI[-pts]
Weight.new <- Weight[-pts]
PhysActive.new <- PhysActive[-pts,]
SmokeNow.new <- SmokeNow[-pts,]

model.3 <- lm(inv.BPSysAve.new ~ Gender.new + Age.new + Education.R.new + 
                MaritalStatus.R.new + Poverty.new + BMI.new + PhysActive.new + 
                SmokeNow.new + Age.new*SmokeNow.new + Age.new*Gender.new + 
                Poverty.new*PhysActive.new + Weight.new*SmokeNow.new)
summary(model.3)

## Diagnostic Check ##
resid <- rstudent(model.3)
fitted <- predict(model.3)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Fitted Values vs. Outcome ##
plot(inv.BPSysAve.new ~ fitted, type = "p", xlab = "Fitted Values",
     ylab = "Inverse BPSysAve", cex.lab = 1.2, col = "red")
lines(lowess(fitted, inv.BPSysAve.new), col = "blue")

## Sqrt Standardized Residuals vs Fitted Values ##
sqrt_resid <- abs(resid)^(1/2)
plot(sqrt_resid ~ fitted, type = "p", col = "red", xlab = "Fitted Values", 
     ylab = "Square root standardized residuals", main = "Scale-Location")
lines(lowess(fitted, sqrt_resid), col = "blue")

## Residuals vs Predictors ##
## Age ##
plot(resid ~ Age.new, type = "p", xlab = "Age", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(Age.new, resid), col = "blue")

## Poverty ##
plot(resid ~ Poverty.new, type = "p", xlab = "Poverty", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(Poverty.new, resid), col = "blue")

## BMI ##
plot(resid ~ BMI.new, type = "p", xlab = "BMI", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(BMI.new, resid), col = "blue")

p <- dim(model.matrix(model.3))[2] - 1
n.new <- n - length(pts)

## Hat Values ###
h <- hatvalues(model.3)
cutoff <- 2*((p+1)/n.new)
plot(resid ~ h, type = "p", xlab = "Leverage",
     ylab = "Standardized Residuals", cex.lab = 1.2, col = "red")
lines(lowess(h, resid), col = "blue")
abline(v=cutoff, lwd = 3, lty = 2)

##### Step 4 #####
## Model Selection ##
## Model Validation ##

## Stepwise Variable AIC Selection ##
step.aic <- step(model.3, trace = 0, k = 2, direction = "both") 
var.aic<- attr(terms(step.aic), "term.labels")   
var.aic

## Stepwise Variable BIC Selection ##
step.bic <- step(model.3, trace = 0, k = log(n), direction = "both") 
var.bic<- attr(terms(step.bic), "term.labels")   
var.bic

## LASSO Selection ##
set.seed(1005024821)
predictors <- cbind(Gender.new, Age.new, Education.R.new, MaritalStatus.R.new,
                    Poverty.new, BMI.new, PhysActive.new, SmokeNow.new, 
                    Age.new*SmokeNow.new, Age.new*Gender.new, Poverty.new*PhysActive.new,
                    Weight.new*SmokeNow.new)
colnames(predictors) <- c("Gender.new", "Age.new", "Education.R.new", "MaritalStatus.R.new",
                          "Poverty.new","BMI.new", "PhysActive.new", "SmokeNow.new",
                          "Age.new:SmokeNow.new", "Age.new:Gender.new", "Poverty.new:PhysActive.new",
                          "Weight.new:SmokeNow.new")
cv.out <- cv.glmnet(x = predictors, y = inv.BPSysAve.new, standardize = T, alpha = 1)
plot(cv.out)
best.lambda <- cv.out$lambda.1se
best.lambda
co<-coef(cv.out, s = "lambda.1se")

thresh <- 0.00
inds <- which(abs(co) > thresh )
variables <- row.names(co)[inds]
var.lasso <- variables[!(variables %in% '(Intercept)')]
var.lasso

## Elastic Net Selection ##
set.seed(1005024821)
cv.out <- cv.glmnet(x = predictors, y = inv.BPSysAve.new, standardize = T, alpha = 0.5)
plot(cv.out)
best.lambda <- cv.out$lambda.1se
best.lambda
co<-coef(cv.out, s = "lambda.1se")

thresh <- 0.00
inds <- which(abs(co) > thresh )
variables <- row.names(co)[inds]
var.elastic <- variables[!(variables %in% '(Intercept)')]
var.elastic

## Notice the LASSO and Elastic Net models produce the same model ##

## Models to be considered ## 
## AIC Stepwise Model ##
model.aic <- lm(inv.BPSysAve.new ~ Gender.new + Age.new + Education.R.new + 
                SmokeNow.new + Age.new*SmokeNow.new + Age.new*Gender.new)
summary(model.aic)

## BIC Stepwise Model ##
model.bic <- lm(inv.BPSysAve.new ~ Gender.new + Age.new + SmokeNow.new + 
                  Age.new*SmokeNow.new + Age.new*Gender.new)
summary(model.bic)

## LASSO Model ##
model.lasso <- lm(inv.BPSysAve.new ~ Gender.new + Age.new + Education.R.new + 
                    Poverty.new + PhysActive.new + Age.new*Gender.new
                  + Weight.new*SmokeNow.new)
summary(model.lasso)

data <- as.data.frame(cbind(predictors, inv.BPSysAve.new))
## Validation ##
## Cross Validation of AIC based selection ##
ols.aic <- ols(inv.BPSysAve.new ~ Gender.new + Age.new + Education.R.new + 
                 SmokeNow.new + Age.new*SmokeNow.new + Age.new*Gender.new, 
               data = data[,which(colnames(data) %in% c(var.aic, "inv.BPSysAve.new"))], 
               x=T, y=T, model = T)

## Cross Validation for AIC, B=10 ##    
aic.cross <- calibrate(ols.aic, method = "crossvalidation", B = 10)
## Calibration plot ##
plot(aic.cross, las = 1, xlab = "Predicted inverse BPSysAve", main = "Cross-Validation calibration with AIC")

## Cross Validation of BIC based selection ##
ols.bic <- ols(inv.BPSysAve.new ~ Gender.new + Age.new + SmokeNow.new + 
                 Age.new*SmokeNow.new + Age.new*Gender.new, 
               data = data[,which(colnames(data) %in% c(var.bic, "inv.BPSysAve.new"))], 
               x=T, y=T, model = T)

## Cross Validation for BIC, B=10 ##    
bic.cross <- calibrate(ols.bic, method = "crossvalidation", B = 10)
## Calibration plot ##
plot(bic.cross, las = 1, xlab = "Predicted inverse BPSysAve", main = "Cross-Validation calibration with BIC")

## Cross Validation of LASSO based selection ##
ols.lasso <- ols(inv.BPSysAve.new ~ Gender.new + Age.new + Education.R.new + 
                   Poverty.new + PhysActive.new + Age.new*Gender.new + Weight.new*SmokeNow.new, 
               data = data[,which(colnames(data) %in% c(var.lasso, "inv.BPSysAve.new"))], 
               x=T, y=T, model = T)

## Cross Validation for LASSO, B=10 ##    
lasso.cross <- calibrate(ols.lasso, method = "crossvalidation", B = 10)
## Calibration plot ##
plot(lasso.cross, las = 1, xlab = "Predicted inverse BPSysAve", main = "Cross-Validation calibration with LASSO")

## Test Set must be reformatted ##
t <- nrow(test)
BPSysAve.test <- test[,12]
Gender.test <- matrix(data = NA, ncol = 1, nrow = t)
G.test <- as.numeric(test[,2])
Age.test <- test[,3]
Education.R.test <- matrix(data = NA, ncol = 1, nrow = t)
E.test <- as.numeric(test[,5])
Poverty.test <- test[,8]
Weight.test <- test[,9]
PhysActive.test <- matrix(data = NA, ncol = 1, nrow = t)
P.test <- as.numeric(test[,16])
SmokeNow.test <- matrix(data = NA, ncol = 1, nrow = t)
Smoke.test <- as.numeric(test[,17])

for (i in 1:t){
  if(G.test[i] == 1) Gender.test[i,] = 1 else Gender.test[i,] = 0                          
  if(E.test[i] == 1) Education.R.test[i,] = 1 else Education.R.test[i,] = 0                   
  if(P.test[i] == 1) PhysActive.test[i,] = 0 else PhysActive.test[i,] = 1    
  if(Smoke.test[i] == 1) SmokeNow.test[i,] = 0 else SmokeNow.test[i,] = 1  
}
inv.BPSysAve.test <- 1/BPSysAve.test
test.data <- cbind(Gender.test, Age.test, Education.R.test,
                   Poverty.test, PhysActive.test, SmokeNow.test, 
                   Age.test*SmokeNow.test, Age.test*Gender.test, Poverty.test*PhysActive.test,
                   Weight.test*SmokeNow.test, Weight.test, Weight.test*Gender.test, inv.BPSysAve.test)
colnames(test.data) <- c("Gender.new", "Age.new", "Education.R.new",
                          "Poverty.new", "PhysActive.new", "SmokeNow.new", 
                          "Age.new * SmokeNow.new", "Gender.new * Age.new", "Poverty.new:PhysActive.new",
                          "Weight.new * SmokeNow.new", "Weight.new", "Weight.new * Gender.new", "inv.BPSysAve.new")

## Test Error for AIC ##
pred.aic <- predict(ols.aic, newdata = 
                      as.data.frame(test.data[, c("Gender.new", "Age.new", "Education.R.new",
                                                  "SmokeNow.new", "Age.new * SmokeNow.new",
                                                  "Gender.new * Age.new")]))
## Prediction Error ##
pred.error.AIC <- mean((inv.BPSysAve.test - pred.aic)^2)

## Test Error for BIC ##
pred.bic <- predict(ols.bic, newdata = 
                      as.data.frame(test.data[, c("Gender.new", "Age.new",
                                                  "SmokeNow.new", "Age.new * SmokeNow.new",
                                                  "Gender.new * Age.new")]))
## Prediction Error ##
pred.error.BIC <- mean((inv.BPSysAve.test - pred.bic)^2)

## Test Error for LASSO ##
pred.lasso <- predict(ols.lasso, newdata = 
                      as.data.frame(test.data[, c("Gender.new", "Age.new",
                                                  "Education.R.new", "Poverty.new", "PhysActive.new", "Weight.new",
                                                  "SmokeNow.new", "Gender.new * Age.new", "Weight.new * SmokeNow.new")]))
## Prediction Error ##
pred.error.LASSO <- mean((inv.BPSysAve.test - pred.lasso)^2)

## Comparing Errors ##
pred.error.AIC
pred.error.BIC
pred.error.LASSO

summary(model.bic)
summary(model.lasso)

## The BIC model has the most accurate cross validation plot ##
## The LASSO model has the least prediction error ##
## Between the two I will choose BIC because it is significantly more simple ##
## and it actually has a higher adjusted R^2 value ##

## Diagnostics ##

resid <- rstudent(model.bic)
fitted <- predict(model.bic)

plot(resid ~ fitted, type = "p", xlab = "Fitted Values", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(fitted, resid), col = "blue")

qqnorm(resid)
qqline(resid)

## Fitted Values vs. Outcome ##
plot(inv.BPSysAve.new ~ fitted, type = "p", xlab = "Fitted Values",
     ylab = "Inverse BPSysAve", cex.lab = 1.2, col = "red")
lines(lowess(fitted, inv.BPSysAve.new), col = "blue")

## Sqrt Standardized Residuals vs Fitted Values ##
sqrt_resid <- abs(resid)^(1/2)
plot(sqrt_resid ~ fitted, type = "p", col = "red")

## Residuals vs Predictors ##
## Age ##
plot(resid ~ Age.new, type = "p", xlab = "Age", 
     ylab = "Standardized Residual", cex.lab = 1.2,
     col = "red")
lines(lowess(Age.new, resid), col = "blue")

p <- dim(model.matrix(model.bic))[2] - 1
n.new <- n - length(pts)

## Hat Values ###
h <- hatvalues(model.bic)
cutoff <- 2*((p+1)/n.new)
plot(resid ~ h, type = "p", xlab = "Leverage",
     ylab = "Standardized Residuals", cex.lab = 1.2, col = "red")
lines(lowess(h, resid), col = "blue")
abline(v=cutoff, lwd = 3, lty = 2)

hat.bad <- which(h > cutoff)
high.lev <- which(h > 0.1)

## Cooks Distance ##
cooks <- cooks.distance(model.bic)
cooks.bad <- which(cooks > qf(0.5, p+1, n.new-p-1))

## DFFITS ##
dfits <- dffits(model.bic)
dfits.bad <- which(abs(dfits) > 2*sqrt((p+1)/n.new))

## DFBETAS ##
dfb <- dfbetas(model.bic)
dfb.mat <- matrix(data=NA, ncol = p+1, nrow = n.new)
dfb.pts <- matrix(data=NA, ncol = 1, nrow = n.new)
for (i in 1:n.new){
  for (j in 1:(p+1)){
    if (abs(dfb[i,j] > 2/sqrt(n.new))){
      dfb.mat[i,j] <- 1
      dfb.pts[i] <- 1
    }
  }
}
dfb.bad <- which(dfb.pts == 1)

outliers <- which(abs(resid) > 2)

outliers
hat.bad
cooks.bad
dfits.bad
dfb.bad

bad.lev <- union(intersect(hat.bad , outliers), high.lev)
fits.betas <- intersect(dfits.bad, dfb.bad)
pts.new <- union(bad.lev, fits.betas)

plot(resid ~ h, type = "p", xlab = "Leverage",
     ylab = "Standardized Residuals", cex.lab = 1.2, col = "red")
points(resid[pts.new] ~ h[pts.new], type = "p", xlab = "Leverage",
       ylab = "Standardized Residuals", cex.lab = 1.2, col = "blue")
lines(lowess(h, resid), col = "blue")
abline(v=cutoff, lwd = 3, lty = 2)

