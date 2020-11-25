v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors =3 )
factanal(m1,factors = 3, rotation = "promax")
prcomp(m1)
factanal(~v1+v2+v3+v4+v5+v6, factors = 3, scores = "Bartlett")


install.packages("Hmisc")
library(Hmisc)
AthleticsData <- spss.get("AthleticsData.sav")
attach(AthleticsData)
names(AthleticsData)
cor(AthleticsData)
prcomp(AthleticsData)
fit.2 <- factanal(AthleticsData, factors = 2, rotation = "varimax")
print(fit.2)

fit.3 <- factanal(AthleticsData, factors = 3, rotation = "varimax")
print(fit.3)
print(fit.3, digits = 2, cutoff = .2, sort = TRUE)

install.packages("GPArotation")
library(GPArotation)

fit <- principal(AthleticsData, nfactors = 3, rotate =,Äùvarimax,Äù)

fit



fit.3.promax <- update(fit.3,rotation = "promax")
colnames(fit.3.promax$loadings) <- c("Endurance","Strength","Hand-Eye")
print(loadings(fit.3.promax), digits =2, cutoff = .2, sort = TRUE)
AssignFactorNames <- function(fit.object,names) {
  colnames(fit.object$promax.loadings) <- names
  colnames(fit.object$varimax.loadings) <- names 
  rownames(fit.object$corr.factors) <- names
  colames(fit.object$corr.factors) <- names 
}

fit.3.Enzmann <- fa.promax(AthleticsData, factors = 3, digits =2, sort = TRUE) 
AssignFactorNames(fit.3.Enzmann,factor.names)
fit.3.Enzmann

data(epi)
epi.keys <- make.keys(epi,list(E = c(1, 3, -5, 8, 10, 13, -15, 17, -20, 22, 25, 27,
                                     -29, -32, -34, -37, 39, -41, 44, 46, 49, -51, 53, 56),
                               N=c(2, 4, 7, 9, 11, 14, 16, 19, 21, 23, 26, 28, 31, 33, 35, 38, 40,
                                   43, 45, 47, 50, 52, 55, 57),
                               L = c(6, -12, -18, 24, -30, 36, -42, -48, -54),
                               I =c(1, 3, -5, 8, 10, 13, 22, 39, -41), 
                               S = c(-11, -15, 17, -20, 25, 27, -29, -32, -37, 44, 46, -51, 53)))


scores <- scoreItems(epi.keys,epi)
N <- epi[abs(epi.keys[,"N"]) >0]
E <- epi[abs(epi.keys[,"E"]) >0]
fa.lookup(epi.keys[,1:3],epi.dictionary)



set.seed(1.234)
N <- 200                             
P <- 6                               
Q <- 2                               

Lambda <- matrix(c(0.7,-0.4, 0.8,0, -0.2,0.9, -0.3,0.4, 0.3,0.7, -0.8,0.1),
                 nrow=P, ncol=Q, byrow=TRUE)

library(mvtnorm)                      
FF  <- rmvnorm(N, mean=c(5, 15), sigma=diag(Q))    
E   <- rmvnorm(N, rep(0, P), diag(P)) 
X   <- FF %*% t(Lambda) + E           
Xdf <- data.frame(X)                  

library(psych) 
fa(X, nfactors=2, rotate="varimax")$loadings    

Xdi    <- lapply(Xdf, function(x) cut(x, breaks=c(-Inf, median(x), Inf), ordered=TRUE))
Xdidf  <- do.call("data.frame", Xdi) 
XdiNum <- data.matrix(Xdidf)         

library(polycor)                     
pc <- hetcor(Xdidf, ML=TRUE)         

faPC <- fa(r=pc$correlations, nfactors=2, n.obs=N, rotate="varimax")
faPC$loadings
faPCdirect <- fa.poly(XdiNum, nfactors=2, rotate="varimax")    
faPCdirect$fa$loadings        
factor.plot(faPCdirect$fa, cut=0.5)
fa.diagram(faPCdirect)
fa.parallel.poly(XdiNum)    
vss(pc$correlations, n.obs=N, rotate="varimax") 
library(random.polychor.pa) 
random.polychor.pa(data.matrix=XdiNum, nrep=5, q.eigen=0.99)







