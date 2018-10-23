library(car)
library(leaps)
library(alr3)
library(MASS)

sleep <- read.table("/Users/blakeshaw/Downloads/sleep.txt", header = TRUE)
sleep <- na.omit(sleep)

#Exploration of Data
  plot(sleep$TotalSleep ~ sleep$BodyWt)
plot(sleep$TotalSleep ~ sleep$BrainWt)
plot(sleep$TotalSleep ~ sleep$NonDreaming)
plot(sleep$TotalSleep ~ sleep$Dreaming)
plot(sleep$TotalSleep ~ sleep$LifeSpan)
plot(sleep$TotalSleep ~ sleep$Gestation)
plot(sleep$TotalSleep ~ sleep$Predation)
plot(sleep$TotalSleep ~ sleep$Exposure)
plot(sleep$TotalSleep ~ sleep$Danger)

# Fitting Initial Model
  m1 <- lm(sleep$TotalSleep ~ sleep$BodyWt + sleep$BrainWt + sleep$NonDreaming + sleep$Dreaming + sleep$LifeSpan + sleep$Gestation + sleep$Predation + sleep$Exposure + sleep$Danger)
summary(m1)

#Checking Diagnostics of Initial Model
  X <- cbind(sleep$BodyWt, sleep$BrainWt, sleep$NonDreaming, sleep$Dreaming, sleep$LifeSpan, sleep$Gestation, sleep$Predation, sleep$Exposure, sleep$Danger)
c <- cor(X)
round(c, 3)
vif(m1)
par(mfrow = c(2,2))
plot(m1)
b <- regsubsets(as.matrix(X), sleep$TotalSleep)
rs <- summary(b)
par(mfrow = c(1,2))

# Starting Variable Selection
  plot(1:8, rs$adjr2, xlab = "Subset Size", ylab = "Adjusted R-squared")
subsets(b, statistic = c("adjr2"))
plot(1:8, rs$bic, xlab = "Subset Size", ylab = "BIC")
subsets(b, statistic = c("bic"))

# Fitting Transformation of Initial Model
  m2 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$BodyWt) + sqrt(sleep$BrainWt) + sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$LifeSpan) + sqrt(sleep$Gestation) + sqrt(sleep$Predation) + sqrt(sleep$Exposure) + sqrt(sleep$Danger))
summary(m2)

# Checking Diagnostics of Transformed Model
  Y <- cbind(sqrt(sleep$BodyWt), sqrt(sleep$BrainWt), sqrt(sleep$NonDreaming), sqrt(sleep$Dreaming), sqrt(sleep$LifeSpan), sqrt(sleep$Gestation), sqrt(sleep$Predation), sqrt(sleep$Exposure), sqrt(sleep$Danger))
c <- cor(Y)
round(c, 3)
vif(m2)
par(mfrow = c(2,2))
plot(m2)

#Eliminating Outliers and Exceedingly High Leverage Points
  sleep <- sleep[!sleep$BodyWt > 1000,]
sleep <- sleep[!sleep$BrainWt > 1000,]
sleep <- sleep[!sleep$Dreaming == 0.0,]
sleep <- sleep[-c(10, 38, 39),]
sleep <- sleep[-c(21),]

# Re-fitting without Trouble Points
  m2 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$BodyWt) + sqrt(sleep$BrainWt) + sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$LifeSpan) + sqrt(sleep$Gestation) + sqrt(sleep$Predation) + sqrt(sleep$Exposure) + sqrt(sleep$Danger))
summary(m2)

# Checking Diagnostics
  Y <- cbind(sqrt(sleep$BodyWt), sqrt(sleep$BrainWt), sqrt(sleep$NonDreaming), sqrt(sleep$Dreaming), sqrt(sleep$LifeSpan), sqrt(sleep$Gestation), sqrt(sleep$Predation), sqrt(sleep$Exposure), sqrt(sleep$Danger))
c <- cor(Y)
round(c, 3)
vif(m2)
plot(m2)

#Starting Variable Selection in Transformed Model
  c <- regsubsets(as.matrix(Y), sqrt(sleep$TotalSleep))
rs1 <- summary(c)
par(mfrow = c(1,2))
plot(1:8, rs1$adjr2, xlab = "Subset Size", ylab = "Adjusted R-squared")
subsets(c, statistic = c("adjr2"))
plot(1:8, rs1$bic, xlab = "Subset Size", ylab = "BIC")
subsets(c, statistic = c("bic"))
rs1$adjr2
im1 <- lm(sleep$TotalSleep ~ sleep$NonDreaming)
im2 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming)
im3 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming + sleep$BodyWt)
im4 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming + sleep$BodyWt + sleep$LifeSpan)
im5 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming + sleep$BodyWt + sleep$LifeSpan + sleep$Gestation)
im6 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming + sleep$BodyWt + sleep$LifeSpan + sleep$Gestation + sleep$Danger)
im7 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming + sleep$BodyWt + sleep$LifeSpan + sleep$Gestation + sleep$Danger + sleep$Exposure)
im8 <- lm(sleep$TotalSleep ~ sleep$NonDreaming + sleep$Dreaming + sleep$BodyWt + sleep$LifeSpan + sleep$Gestation + sleep$Danger + sleep$Exposure + sleep$Predation)
im9 <- m1
n <- length(im1$coefficients)
npar <- length(im1$coefficients) + 1
extractAIC(im1, k = 2)
extractAIC(im1, k = log(n))
npar <- length(im2$coefficients) + 1
extractAIC(im2, k = 2)
extractAIC(im2, k = log(n))
npar <- length(im3$coefficients) + 1
extractAIC(im3, k = 2)
extractAIC(im3, k = log(n))
npar <- length(im4$coefficients) + 1
extractAIC(im4, k = 2)
extractAIC(im4, k = log(n))
npar <- length(im5$coefficients) + 1
extractAIC(im5, k = 2)
extractAIC(im5, k = log(n))
npar <- length(im6$coefficients) + 1
extractAIC(im6, k = 2)
extractAIC(im6, k = log(n))
npar <- length(im7$coefficients) + 1
extractAIC(im7, k = 2)
extractAIC(im7, k = log(n))
npar <- length(im8$coefficients) + 1
extractAIC(im8, k = 2)
extractAIC(im8, k = log(n))
npar <- length(im9$coefficients) + 1
extractAIC(im9, k = 2)
extractAIC(im9, k = log(n))
summary(im1)
summary(im2)
summary(im3)
summary(im4)
summary(im5)
summary(im6)
summary(im7)
summary(im8)
summary(im9)
backAIC <- step(m1, direction = "backward", data = sleep)
backBIC <- step(m1, direction = "backward", data = sleep, k=log(n))
mint <- lm(sleep$TotalSleep ~ 1, data = sleep)
forwardAIC <- step(mint, scope = list(lower = ~ 1, 
                                      upper = ~ sleep$BodyWt + sleep$BrainWt + sleep$NonDreaming + sleep$Dreaming + sleep$LifeSpan + sleep$Gestation + sleep$Predation + sleep$Exposure + sleep$Danger),
                   direction = "forward", data = sleep)
forwardBIC <- step(mint, scope = list(lower = ~ 1, 
                                      upper = ~ sleep$BodyWt + sleep$BrainWt + sleep$NonDreaming + sleep$Dreaming + sleep$LifeSpan + sleep$Gestation + sleep$Predation + sleep$Exposure + sleep$Danger),
                   direction = "forward", data = sleep, k = log(n))
om1 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming))
om2 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming))
om3 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$Exposure))
om4 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$Exposure) + sqrt(sleep$BodyWt))
om5 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$BodyWt) + sqrt(sleep$Gestation) + sqrt(sleep$Danger))
om6 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$BodyWt) + sqrt(sleep$Gestation) + sqrt(sleep$Danger) + sqrt(sleep$LifeSpan))
om7 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$BodyWt) + sqrt(sleep$Gestation) + sqrt(sleep$Danger) + sqrt(sleep$BrainWt) + sqrt(sleep$Predation))
om8 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$BodyWt) + sqrt(sleep$Gestation) + sqrt(sleep$Danger) + sqrt(sleep$BrainWt) + sqrt(sleep$Predation) + sqrt(sleep$Exposure))
om9 <- m2
n <- length(om1$coefficients)
npar <- length(om1$coefficients) + 1
extractAIC(om1, k = 2)
extractAIC(om1, k = log(n))
npar <- length(om2$coefficients) + 1
extractAIC(om2, k = 2)
extractAIC(om2, k = log(n))
npar <- length(om3$coefficients) + 1
extractAIC(om3, k = 2)
extractAIC(om3, k = log(n))
npar <- length(om4$coefficients) + 1
extractAIC(om4, k = 2)
extractAIC(om4, k = log(n))
npar <- length(om5$coefficients) + 1
extractAIC(om5, k = 2)
extractAIC(om5, k = log(n))
npar <- length(om6$coefficients) + 1
extractAIC(om6, k = 2)
extractAIC(om6, k = log(n))
npar <- length(om7$coefficients) + 1
extractAIC(om7, k = 2)
extractAIC(om7, k = log(n))
npar <- length(om8$coefficients) + 1
extractAIC(om8, k = 2)
extractAIC(om8, k = log(n))
npar <- length(om9$coefficients) + 1
extractAIC(om9, k = 2)
extractAIC(om9, k = log(n))
summary(om1)
summary(om2)
summary(om3)
summary(om4)
summary(om5)
summary(om6)
summary(om7)
summary(om8)
summary(om9)
backAIC <- step(m2, direction = "backward", data = sleep)
backBIC <- step(m2, direction = "backward", data = sleep, k=log(n))
mint <- lm(sqrt(sleep$TotalSleep) ~ 1, data = sleep)
forwardAIC <- step(mint, scope = list(lower = ~ 1, 
                                      upper = ~ sqrt(sleep$BodyWt) + sqrt(sleep$BrainWt) + sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$LifeSpan) + sqrt(sleep$Gestation) + sqrt(sleep$Predation) + sqrt(sleep$Exposure) + sqrt(sleep$Danger)),
                   direction = "forward", data = sleep)
forwardBIC <- step(mint, scope = list(lower = ~ 1, 
                                      upper = ~ sqrt(sleep$BodyWt) + sqrt(sleep$BrainWt) + sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$LifeSpan) + sqrt(sleep$Gestation) + sqrt(sleep$Predation) + sqrt(sleep$Exposure) + sqrt(sleep$Danger)),
                   direction = "forward", data = sleep, k = log(n))
mReduced <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$BodyWt) + sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming) + sqrt(sleep$Gestation) + sqrt(sleep$Danger))
summary(mReduced)

# Checking Diagnostics of Selected Model
  vif(mReduced)
par(mfrow = c(1,1))
avPlot(mReduced, variable = sqrt(sleep$BodyWt), ask = FALSE)
avPlot(mReduced, variable = sqrt(sleep$NonDreaming), ask = FALSE)
avPlot(mReduced, variable = sqrt(sleep$Dreaming), ask = FALSE)
avPlot(mReduced, variable = sqrt(sleep$Gestation), ask = FALSE)
avPlot(mReduced, variable = sqrt(sleep$Danger), ask = FALSE)
par(mfrow = c(2,2))
plot(mReduced)
anova(mReduced)

# Checking Potential Alternate Model
  mReduced1 <- lm(sqrt(sleep$TotalSleep) ~ sqrt(sleep$NonDreaming) + sqrt(sleep$Dreaming))
summary(mReduced1)
vif(mReduced1)
plot(mReduced1)
