library(caret)#converting categorical to numerical
library(randomForest) # classification algorithm
library(dplyr)
library(ggplot2)
train=read.csv("C:/Users/manub/Downloads/DM/project/house prices/train.csv", stringsAsFactors = F)


test= read.csv('C:/Users/manub/Downloads/DM/project/house prices/test.csv', stringsAsFactors = F)
Id=test$Id
y=bind_rows(train, test) # bind training & test data

#replacing meaningful NAs
y$Alley[is.na(y$Alley)]="NoAlley"
y$MiscFeature[is.na(y$MiscFeature)]="None"
y$Fence[is.na(y$Fence)]="NoFence"
y$PoolQC[is.na(y$PoolQC)]="NoPool"
y$GarageCond[is.na(y$GarageCond)]="NoGarage"
y$GarageQual[is.na(y$GarageQual)]="NoGarage"
y$GarageFinish[is.na(y$GarageFinish)]="NoGarage"
y$GarageType[is.na(y$GarageType)]="NoGarage"
y$FireplaceQu[is.na(y$FireplaceQu)]="NoFireplace"
y$BsmtFinType2[is.na(y$BsmtFinType2)]="NoBasement"
y$BsmtFinType1[is.na(y$BsmtFinType1)]="NoBasement"
y$BsmtExposure[is.na(y$BsmtExposure)]="NoBasement"
y$BsmtCond[is.na(y$BsmtCond)]="NoBasement"
y$BsmtQual[is.na(y$BsmtQual)]="NoBasement"
s=sapply(y, function(x) sum(is.na(x)))
p=subset(s,s>0)

Mode <- function(x){
  
  u=unique(x)
  u[is.na(u)]=0
  u[which.max(tabulate(match(x, u)))]
  
}

y=sapply(y, function(x) replace(x,is.na(x),Mode(x)))

train=data.frame(y[1:1460,])
test=data.frame(y[1461:2919,])
o=sapply(train,function(x) length(unique(x)))
subset(o,o>25)
#binning
qplot(y = SalePrice, data = train, colour = 'blue')
sp_bins=cut(unclass(train$SalePrice),20,include.lowest=T)
la_bins=cut(unclass(train$LotArea),20,include.lowest=T)
bfs1_bins=cut(unclass(train$BsmtFinSF1),20,include.lowest=T)
mva_bins=cut(unclass(train$MasVnrArea),20,include.lowest=T)
bfs2_bins=cut(unclass(train$BsmtFinSF2),20,include.lowest=T)
bus_bins=cut(unclass(train$BsmtUnfSF),20,include.lowest=T)
tbsf_bins=cut(unclass(train$TotalBsmtSF),20,include.lowest=T)
x1fs_bins=cut(unclass(train$X1stFlrSF),20,include.lowest=T)
x2fs_bins=cut(unclass(train$X2ndFlrSF),20,include.lowest=T)
gla_bins=cut(unclass(train$GrLivArea),20,include.lowest=T)
gyb_bins=cut(unclass(train$GarageYrBlt),20,include.lowest=T)
ga_bins=cut(unclass(train$GarageArea),20,include.lowest=T)
wds_bins=cut(unclass(train$WoodDeckSF),20,include.lowest=T)
ops_bins=cut(unclass(train$OpenPorchSF),20,include.lowest=T)
ep_bins=cut(unclass(train$EnclosedPorch),20,include.lowest=T)
scrnp_bins=cut(unclass(train$ScreenPorch),20,include.lowest=T)
X3_bins=cut(unclass(train$X3SsnPorch),20,include.lowest=T)
lf_bins=cut(unclass(train$LotFrontage),20,include.lowest=T)
yb_bins=cut(unclass(train$YearBuilt),20,include.lowest=T)

# bin_creation = function (x,labelVal) {
#   a= cut(unclass(train$x),20,include.lowest=T)
#   b = as.vector(a)
#   train$labelVal = b
# }
# bin_creation()



train$sp_bins=sp_bins
train$la_bins = la_bins
train$bfs1_bins=bfs1_bins
train$mva_bins=mva_bins
train$bfs2_bins=bfs2_bins
train$bus_bins=bus_bins
train$tbsf_bins=tbsf_bins
train$x1fs_bins=x1fs_bins
train$x2fs_bins=x2fs_bins
train$gla_bins=gla_bins
train$gyb_bins=gyb_bins
train$ga_bins=ga_bins
train$wds_bins=wds_bins
train$ops_bins=ops_bins
train$ep_bins=ep_bins
train$scrnp_bins=scrnp_bins
train$X3_bins=X3_bins
train$lf_bins=lf_bins
train$yb_bins=yb_bins

#random forest
train$SalePrice=NULL
train$X3SsnPorch=NULL
train$LotArea=NULL
train$BsmtFinSF1 =NULL
train$MasVnrArea =NULL
train$BsmtFinSF2 =NULL
train$BsmtUnfSF =NULL
train$TotalBsmtSF =NULL
train$X1stFlrSF =NULL
train$X2ndFlrSF =NULL
train$GrLivArea =NULL
train$GarageYrBlt =NULL
train$GarageArea =NULL
train$WoodDeckSF =NULL
train$OpenPorchSF =NULL
train$ScreenPorch =NULL
train$EnclosedPorch = NULL
train$LotFrontage=NULL
train$YearRemodAdd=NULL
train$YearBuilt=NULL

trnsfrmd_data=train
trans_names=names(trnsfrmd_data)
table(trnsfrmd_data$SalePrice)/nrow(trnsfrmd_data)
trans_names<-trans_names[!trans_names %in% c("sp_bins")]
trans_names<-trans_names[!trans_names %in% c("Id")]
trans_names1 <- paste(trans_names, collapse = "+")
rf.formulas <- as.formula(paste("sp_bins", trans_names1, sep = " ~ "))
trans.rf<- randomForest(rf.formulas,trnsfrmd_data, importance=TRUE, ntree=500)
#trans
plot(trans.rf)
# Variable Importance Plot
varImpPlot(trans.rf,sort = T,main="Variable Importance",n.var=30)
importance    <- importance(trans.rf)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))

# dummify the data
dummy_data=dummyVars(" ~ .", data = train)
#trnsfrmd_data=data.frame(predict(dummy_data, newdata = train))

#linear regression
lr<- lm(SalePrice~OverallQual+GrLivArea+GarageCars+TotalBsmtSF+ExterQual,data=train)
summary(lr) 

#writing data to a file
final_prediction=data.frame(Id=Id,SalePrice=ceiling(lr$fitted.values[1:1459]))
write.csv(final_prediction,file='C:/Users/manub/Downloads/DM/project/house prices/HP_solution.csv',row.names=F)