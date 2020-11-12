data("USArrests")
states = row.names(USArrests)
states
names(USArrests)
apply(USArrests,2,mean)
apply(USArrests, 2, var)
pr.out = prcomp(USArrests, scale = TRUE)
names(pr.out)
pr.out$center
pr.out$scale
pr.out$rotation
dim(pr.out$x)
biplot(pr.out,scale=0)
pr.var = pr.out$sdev^2
pr.var
pve = pr.var/sum(pr.var)
pve


data("iris")
head(iris)
irisdata1 <- iris[,1:4]
irisdata1
head(irisdata1)
help("princomp")
principal_components <- princomp(irisdata1,cor=TRUE, score=TRUE)
summary(principal_components)
plot(principal_components)
plot(principal_components, type = "l")

help("biplot")
biplot(principal_components)
