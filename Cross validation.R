
#cv1
library(cvTools)
library(robustbase)
data(coleman)
call <- call("lmrob", formula = Y~.)

folds <- cvFolds(nrow(coleman), K = 5, R = 10 )
require(cvTools)
cvTools(call, data = coleman, y = coleman$Y, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))

tuning <- list(tuning.psi=seq(2., 6., 20))
cvFitsLmrob <- cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))

cvFitsLmrob
aggregate(cvFitsLmrob, summary)

#cv2

library(MASS)
data("mammals")
mamals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals,mamals.glm)$delta)
(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)

muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.esterr <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))

library(boot)
data(nodal)

cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)
nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)


#cv3 

install.packages("robustbase")
install.packages("cvTools")
library("robustbase")
require(cvTools)
data("coleman")
set.seed(1234) 
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLts50 <- ltsReg(Y~.,data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both",trim = 0.1)

fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)



cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts


ncv(cvFitLts50)
nfits(cvFitLts50)
cvNames(cvFitLts50)
cvNames(cvFitLts50) <- c("improved", "initial")
fits(cvFitLts50)
cvFitLts50

ncv(cvFitsLts)
nfits(cvFitsLts)
cvNames(cvFitsLts)
cvNames(cvFitsLts) <- c("improved", "initial")
fits(cvFitsLts)
fits(cvFitsLts) <- 1:2
cvFitsLts


# cv4 

set.seed(4321) 
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)
cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
aggregate(cvFitLts50, summary)
aggregate(cvFitsLts, summary)

tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))
call <- call("lmrob", formula = Y ~ .)
cvFitsLmrob <- cvTuning(call, data = coleman,y = coleman$Y, tuning = tuning, cost = rtmspe,folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob
aggregate(cvFitsLmrob, summary)


# cv5 

folds <- cvFolds(nrow(coleman), K = 5, R = 50)
fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)

cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits

bwplot(cvFitLmrob)

bwplot(cvFits)

fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)


fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
bwplot(cvFitsLts)

##cv6 
fit <- lmrob(Y ~ ., data=coleman)

cvFit(fit, data = coleman, y = coleman$Y, cost = rtmspe,K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

cvFit(lmrob, formula = Y ~ ., data = coleman, cost = rtmspe,K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)

call <- call("lmrob", formula = Y ~ .)

cvFit(call, data = coleman, y = coleman$Y, cost = rtmspe,K = 5, R = 10, costArgs = list(trim = 0.1), seed = 1234)


##cv7 
set.seed(2143) 
cvFolds(20, K = 5, type = "random")
cvFolds(20, K = 5, type = "consecutive")
cvFolds(20, K = 5, type = "interleaved")
cvFolds(20, K = 5, R = 10)


#cv8
fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe, K = 5, R = 10,fit = "both", trim = 0.1, seed = 1234)
cvFitLts
cvReshape(cvFitLts)

#cv9

folds <- cvFolds(nrow(coleman), K = 5, R = 10)

fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)

cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)


fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)


fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)


cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)


#cv 10

## evaluate MM regression models tuned for 85% and 95% efficiency
tuning <- list(tuning.psi = c(3.443689, 4.685061))

cvTuning(lmrob, formula = Y ~ ., data = coleman, tuning = tuning,cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),seed = 1234)

call <- call("lmrob", formula = Y ~ .)

#vTuning(call, data = coleman, y = coleman$Y, tuning = tuning,6 densityplot.cv, cost = rtmspe, K = 5, R = 10, costArgs = list(trim = 0.1),seed = 1234)


#cv 11
set.seed(1234) 

call <- call("lmrob", formula = Y ~ .)
folds <- cvFolds(nrow(coleman), K = 5, R = 10)
cvTool(call, data = coleman, y = coleman$Y, cost = rtmspe,folds = folds, costArgs = list(trim = 0.1))

#cv12

set.seed(1234) 
folds <- cvFolds(nrow(coleman), K = 5, R = 50)

fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe,folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)

cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits

densityplot(cvFitLmrob)

densityplot(cvFits)

# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)
# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)

cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
densityplot(cvFitsLts)

# cv13 

set.seed(1234)

folds <- cvFolds(nrow(coleman), K =5, R = 10)

fitLm <- lm(Y~., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)

cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
dotplot(cvFits)

fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)


cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitLts
dotplot(cvFitLts)

# cv14

set.seed(1234)
folds <- cvFolds(nrow(coleman), K =5, R =50)
fitLm <- lm(Y ~., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)


fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)

cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits

plot(cvFits, method = "bw")
plot(cvFits, method = "density")
plot(cvFits, method = "xy")
plot(cvFits, method = "dot")

fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)


cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitLts

plot(cvFitsLts, method = "bw")
plot(cvFitsLts, method = "density")
plot(cvFitsLts, method = "xy")
plot(cvFitsLts, method = "dot")

#cv 15

set.seed(1234)
folds <- cvFolds(nrow(coleman), K =5, R =10)

fitLm <- lm(Y ~ ., data = coleman)
repCV(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman)
repCV(fitLmrob, cost = rtmspe, folds = folds, trim = 0.1)


fitLts <- ltsReg(Y ~ ., data = coleman)
repCV(fitLts, cost = rtmspe, folds = folds, trim = 0.1)
repCV(fitLts, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)

# cv16

set.seed(1234)
folds <- cvFolds(nrow(coleman), K =5, R =10)

fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)

fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds,fit = "both", trim = 0.1)

cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts

summary(cvFitLts50)

summary(cvFitsLts)

tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))

call <- call("lmrob",formula = Y ~.)

cvFistLmrob <- cvTuning(call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
cvFistLmrob

summary(cvFistLmrob)

# cv 17

set.seed(1234) 
folds <- cvFolds(nrow(coleman), K = 5, R = 10)

fitLm <- lm(Y ~ ., data = coleman)
cvFitLm <- cvLm(fitLm, cost = rtmspe, folds = folds, trim = 0.1)

fitLmrob <- lmrob(Y ~ ., data = coleman, k.max = 500)
cvFitLmrob <- cvLmrob(fitLmrob, cost = rtmspe,folds = folds, trim = 0.1)

fitLts <- ltsReg(Y ~ ., data = coleman)
cvFitLts <- cvLts(fitLts, cost = rtmspe,folds = folds, trim = 0.1)

cvFits <- cvSelect(LS = cvFitLm, MM = cvFitLmrob, LTS = cvFitLts)
cvFits
xyplot(cvFits)

# 50% subsets
fitLts50 <- ltsReg(Y ~ ., data = coleman, alpha = 0.5)
cvFitLts50 <- cvLts(fitLts50, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

# 75% subsets
fitLts75 <- ltsReg(Y ~ ., data = coleman, alpha = 0.75)
cvFitLts75 <- cvLts(fitLts75, cost = rtmspe, folds = folds, fit = "both", trim = 0.1)

cvFitsLts <- cvSelect("0.5" = cvFitLts50, "0.75" = cvFitLts75)
cvFitsLts
xyplot(cvFitsLts)


tuning <- list(tuning.psi=c(3.14, 3.44, 3.88, 4.68))

cvFitsLmrob <- cvTuning(fitLmrob$call, data = coleman, y = coleman$Y, tuning = tuning, cost = rtmspe, folds = folds, costArgs = list(trim = 0.1))
cvFitsLmrob

xyplot(cvFitsLmrob)

#cv18

require(boot)

data(mammals, package="MASS")
mammals.glm <- glm(log(brain) ~ log(body), data = mammals)
(cv.err <- cv.glm(mammals, mammals.glm)$delta)
(cv.err.6 <- cv.glm(mammals, mammals.glm, K = 6)$delta)


muhat <- fitted(mammals.glm)
mammals.diag <- glm.diag(mammals.glm)
(cv.err <- mean((mammals.glm$y - muhat)^2/(1 - mammals.diag$h)^2))



cost <- function(r, pi = 0) mean(abs(r-pi) > 0.5)

nodal.glm <- glm(r ~ stage+xray+acid, binomial, data = nodal)
(cv.err <- cv.glm(nodal, nodal.glm, cost, K = nrow(nodal))$delta)
(cv.11.err <- cv.glm(nodal, nodal.glm, cost, K = 11)$delta)
