
library(psych)

df<-read.csv("Depression_GPA_data.csv", header=T, sep=",", encoding='utf-8-rom')


# What type of data do we have? -------------------------------------------

str(df)

# Male/Female is a factor (i.e., nominal data)
# So we'll make it a factor instead of numeric (i.e., interval/ratio)
df$Male<-as.factor(df$Male)



# Is there room in the data? -- Variance ----------------------------------

describe(df$Depression)
hist(df$Depression)

describe(df$GPA)
hist(df$GPA)

describe(df$PreCollegeAnxiety)
hist(df$PreCollegeAnxiety)



# Are GPA and Depression associated with one another? ---------------------

lowerCor(df)



# GLM Step 1: Specify the Statistical Hypothesis --------------------------

# H0: b = 0 - Null Hypothesis: GPA is unrelated to Depression
# Ha: b < 0 - Alternative Hypothesis: Higher GPA will predict Less Depression


# GLM Step 2: Specify the statistical model -------------------------------

fit<-lm(Depression ~ GPA, data = df)



# GLM Step 3, 4, & 5:  ----------------------------------------------------
# Step 3: Estimate parameters of the model 
# Step 4: Assess Goodness of fit
# Step 5: Test hypotheses on the model parameters

summary(fit)




# Checking our Math -------------------------------------------------------


# Correlation

cor.plot(df[,1:3])
lowerCor(df[,1:3])

# Slope and Intercept

a = 17.1769
b = -1.7474

xbar<-mean(df$GPA)
ybar<-mean(df$Depression)

df$Xdev<-df$GPA-xbar
df$Ydev<-df$Depression-ybar

df$b_numerator <- df$Xdev*df$Ydev

b = sum(df$b_numerator) / sum(df$Xdev^2)

print(b)

a = ybar - (b * xbar)

print(a)




# GLM Step 6: Assess the adequacy of the model ----------------------------

par(mfrow=c(2,2))
plot(fit)


#################
## ASSUMPTIONS ##
#################


#1: Independence of observations (study design)
#2: No measurement error in X (reliable / valid measures used given target pop)
#3: Model is correctly specified

#4: Linear Relationship between X & Y
#     Before running regression

plot(Y, X , xlab = "X label", ylab = "y label", main= "label on top of graph")

####################### NOT LINEAR?: Look into exponential or quadradic regression (i.e., poloynomial regression)


fit<-lm(Y~X)
summary(fit)

abline(lm(Y~X), col="red")


#5: Homoskedasticity of residuals
#     Post-Regression
#           Look at the plot(fit) matrix
#             Uniform distribution of residuals around Y (0) in Resid. vs. Fitted?
#             Scale-Location graph's line sticking close to center (up & down's ok)?

####################### IF NOT: Look into Weighted Least Sq. Regression (WLS)

par(mfrow=c(2,2))

plot(fit)


#6: Independence of Residuals - also taps into #1
#     Durbin-Watson test must be NON-SIG (test is sensitive in with high n's)


library(car)
durbinWatsonTest(lm(Y~X))



#7: Normally Distributed Residuals
#     Q-Q plot from plot(fit) should not deviate much from the line
#           Often happens at the tail ends.
###################### IF DEVIATING TOO MUCH: Increase Sample size, Transform the data and re-test, use WLS

plot(fit)


################
### OUTLIERS ###
################

################ Leverage: once modeled, the deviations on the X-axis

#     Create new variable with residuals LEV1

data$LEV1 = hatvalues(fit)
?hatvalues

# ADD a column for participant number
data$participantNUMBER <- seq.int(nrow(data))

#the AB line is out criterion
plot(data$participantNUMBER, Profs2$LEV1 , xlab = "case", ylab = "leverage")

# CALCULATE CRITERIA
#     small n: (3k/n)
#     large n: (2k/n)

print(CRITERIA) # 3k/n

abline(h=0.133 # <- set this to your criteria (then delete the text after the #)
       , col="blue")

plot(hatvalues(fit), type = "h")




################ Discrepancy: once modeled, the deviations along the y-axis

# studentized deleted residuals
#     Create new variable SDR1

library(MASS)
data$SDR_1 <- studres(fit)

install.packages('olsrr')
library(olsrr)


# olsrr package: studentized deleted residuals
#
#
#  Threshold is for large samples by default, research a way
#   to change it to the more conservative value "2" (small n) "3-4"(n>100)

ols_plot_resid_stud(fit)


################ Influence: Both leverage & discrepancy.



# Standardized difference in fit (DFFIT)
# olsrr package:
#
#
# The default threshold is ALSO based on the large data set
# Research how to change this to the value of 1 (even tho it's very close)


data$SDF1 <- dffits(fit)
ols_plot_dffits(fit)


# Cook's Distance

data$COO1 <- cooks.distance(fit)
ols_plot_cooksd_bar(fit)

# Standardized DFBETA:

dfbetas = dfbetas(fit)
data = cbind(Profs2 , dfbetas)
View(dfbetas)
ols_plot_dfbetas(fit)






# Step 7: Make a Decision! ------------------------------------------------

# Interpret the coefficients and communicate results










# BONUS ROUND! Testing Nested Models --------------------------------------

fit<-lm(Depression ~ GPA, data = df)
summary(fit)


fit1<-lm(Depression ~ GPA + Male, data = df)
summary(fit1)


fit2<-lm(Depression ~ GPA + Male + PreCollegeAnxiety, data = df)
summary(fit2)


# Incremental Improvement in Model Fit
anova(fit, fit1)

anova(fit1, fit2)






