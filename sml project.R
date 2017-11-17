library(tidyverse)
k=read.csv("/Users/shivayogibiradar/Desktop/Projects/train.csv")
l=read.csv("/Users/shivayogibiradar/Desktop/Projects/test.csv")
t=sapply(k, function(x) sum(is.na(x)))
t=t[t>0]
t=sort(t)
barplot(t,col="Blue",cex.names = 0.75,las=2,horiz = TRUE)

hist(log(k$SalePrice))
boxplot(log(k$SalePrice))
hist(log(k$SalePrice+1))

t=sapply(l, function(x) sum(is.na(x)))
t=t[t>0]
t=sort(t)
barplot(t,col="Blue",cex.names = 0.75,las=2,horiz = TRUE)
install.packages("regr")






#sqf=(k$X1stFlrS+k$X2ndFlrSF)



boxplot(k$SalePrice)


none=function(x){

  x=ifelse(is.na(x), "None", x)
return(x)

}

k$PoolQC=none(k$PoolQC)

names=colnames(k)[colSums(is.na(k)) > 600]
none(k[names])

#####for test.csv

names=colnames(l)[colSums(is.na(l)) > 600]
none(l[names])

str(k$MasVnrArea)
?sapply()
##########coding missing values
k[names]=sapply(k[names],function(x) none(x))
l[names]=sapply(l[names],function(x) none(x))

names1=colnames(k)[colSums(is.na(k)) > 2 &colSums(is.na(k)) <200]

k[names1]=sapply(k[names1],function(x) none(x))

z=k %>% select(LotFrontage,Neighborhood) %>%
  group_by(Neighborhood)%>% summarize(median=median(LotFrontage,na.rm=TRUE))

t=k[which(is.na(k$LotFrontage)),] %>% select(LotFrontage,Neighborhood)

q=left_join(t,z,by="Neighborhood")

k$LotFrontage[which(is.na(k$LotFrontage))]=q$median

k$Electrical[1380]="SBrkr"

k=k %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF)
##############modelling
unique(k$Neighborhood)

k %>% group_by(Neighborhood) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(Neighborhood,-median),y=median,fill=Neighborhood))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))
  

k %>% group_by(SaleCondition) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(SaleCondition,-median),y=median,fill=SaleCondition))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))


k %>% group_by(KitchenQual) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(KitchenQual,-median),y=median,fill=KitchenQual))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

k %>% group_by(MSSubClass) %>% summarize(median=median(SalePrice)) %>% ggplot()+geom_col(aes(x=reorder(MSSubClass,-median),y=median,fill=MSSubClass))+theme(axis.text.x=element_text(angle=45,hjust=1,vjust=0.5))

k %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF) %>% ggplot()+geom_point(aes(totalsqft,SalePrice,color=SaleCondition))+geom_smooth(aes(totalsqft,SalePrice))


k %>% mutate(totalsqft=TotalBsmtSF+X1stFlrSF+X2ndFlrSF) %>% filter(totalsqft>6000 & SalePrice<500000)

k$PoolQC=as.factor(k$PoolQC)
k$MSSubClass=as.factor(k$MSSubClass)

#cols=c("MSZoning",'Street',"Alley","LotShape","LandContour","Utilities","LotConfig","LandSlope","Neighborhood","")
#######remember
k = k %>% mutate_if(is.character,funs(factor(.)))
(which(is.factor(k)))
k$MasVnrArea=as.numeric(k$MasVnrArea)

col=("MoSold")
k[col]=lapply(k[col], factor)

z=Filter(is.factor, k)
ncol(z)
ncol(k)
z1=Filter(is.numeric, k)
ncol(z1)

?as.Date
#######fitting regreeesion line
install.packages("Metrics")
library(Metrics)
fit1=lm(data=k[,-1],SalePrice~.)
summary(fit1)
##########checking residuals
residuals(fit1)

plot(residuals(fit1),predict(fit1,k))

hist(log(k$SalePrice))
qqnorm(fit1$residuals, main = "Normal qqplot of residuals")
qqline(fit1$residuals)
#################Transforming sale price to to log
k$lsaleprice=log(k$SalePrice)
k1=k[,-81]
k1=k1[,-1]

fit2=lm(data=k1,lsaleprice~.)
fit3=lm(data=train,lsaleprice~.)
summary(fit2)
summary(fit)
qqnorm(fit2$residuals, main = "Normal qqplot of residuals")
qqline(fit2$residuals)

plot(residuals(fit2),predict(fit2,k))

head(sort(fit2$residuals,decreasing = TRUE),n=10)
rmse(predict(fit3,test),test$lsaleprice)
#######Residuals column
q=c(826,971,89,689,1146,739,329,14,682,775)
k[q,]
rmse(fit2$fitted.values,k$lsaleprice)
##################Split dtaset
seed=789
ratio=sample(1:nrow(k1),size=0.7*nrow(k1))
train=k1[ratio,]
test=k1[-ratio,]
###########subset selection taking toolong
#######################PCREGRESSION
install.packages("pls")
library(pls)
set.seed(123)
pca_fit=pcr(lsaleprice~.,data=train,validation="CV")
scale(train)
scale(train)
train1=data.frame(model.matrix(~.,train)[,-1])
train1=as.data.frame(train1)
summary(pca_fit)
validationplot(pca_fit)
validationplot(pca_fit, val.type = "R2")
predplot(pca_fit)
qqnorm(pca_fit$residuals, main = "Normal qqplot of residuals")
qqline(pca_fit$residuals)
rmse(predict(pca_fit,test),test$lsaleprice)
###########################################################
library(glmnet)
x <- model.matrix(lsaleprice~.,train)[,-1]
y <- train$lsaleprice
x_prime <- as.data.frame(x)
y_prime <- y
Training_prime <- cbind(y_prime, x_prime)
x <- model.matrix(y_prime~.,Training_prime)[,-1]
y <- Training_prime$y_prime

grid = 10^seq(15,-2, length = 100)

ridge.mode = glmnet(x, y, alpha=0, lambda = grid)
plot(ridge.mode, main = "Ridge regression",label = TRUE, xvar = "lambda", xlim = c(-5,20))

cv.out <- cv.glmnet(x,y, alpha = 0)
plot(cv.out)
bestlam.ridge = cv.out$lambda.min
bestlam.ridge
log(bestlam.ridge)
ridge.mode <- glmnet(x, y, alpha=0, lambda = bestlam.ridge)
predict(ridge.mode, s = bestlam.ridge, type = "coefficients")


lasso.mod <- glmnet(x,y, alpha = 1, lambda = grid)
plot(lasso.mod, main = "Lasso regression", label = TRUE, xvar = "lambda", xlim = c(-5,15))

cv.out <- cv.glmnet(x,y,alpha = 1)
plot(cv.out)

bestlam.lasso <- cv.out$lambda.min
bestlam.lasso
test1 <- model.matrix(lsaleprice~.,test)[,-1]
newx=data.matrix(test1)
lasso.mode <- glmnet(x, y, alpha=1, lambda = bestlam.lasso)
predict(lasso.mode, s = bestlam.lasso, type = "coefficients")
rmse(predict(lasso.mode,newx=newx),test$lsaleprice)
rmse(predict(ridge.mode,newx=newx),test$lsaleprice)
residuals=(predict(ridge.mode,newx=x)-train$lsaleprice)
qqnorm(residuals, main = "Normal qqplot of residuals")
qqline(residuals)
head(sort(residuals[,1],decreasing = TRUE),n=10)
d=c(496,633,1325,917,969,463,729,875,534,589)
k[d,]

