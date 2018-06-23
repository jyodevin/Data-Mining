#Reading the data
dataset=read.csv("C:/Users/manub/Downloads/DM/project/house prices/train_new.csv", stringsAsFactors = F)
test_data= read.csv('C:/Users/manub/Downloads/DM/project/house prices/test.csv', stringsAsFactors = F)
saleprice=dataset$SalePrice
#binding the train and test data for replacement
library(dplyr)
y=bind_rows(dataset[-81], test_data) 

#checking for null values
s=sapply(y, function(x) sum(is.na(x)))
p=subset(s,s>0)

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

#defining mode
Mode <- function(x){
  
  u=unique(x)
  u[is.na(u)]=0
  u[which.max(tabulate(match(x, u)))]
  
}

#Taking care of missing data
y$LotFrontage = ifelse(is.na(y$LotFrontage),
                     ave(y$LotFrontage, FUN = function(x) mean(x, na.rm = TRUE)),
                     y$LotFrontage)

y$MasVnrType = ifelse(is.na(y$MasVnrType),
                        ave(y$MasVnrType, FUN = function(x) Mode(x)),
                        y$MasVnrType)

y$MasVnrArea = ifelse(is.na(y$MasVnrArea),
                             ave(y$MasVnrArea, FUN = function(x) mean(x, na.rm = TRUE)),
                             y$MasVnrArea)

y$Electrical = ifelse(is.na(y$Electrical),
                            ave(y$Electrical, FUN = function(x) Mode(x)),
                            y$Electrical)

y$MSZoning = ifelse(is.na(y$MSZoning),
                       ave(y$MSZoning, FUN = function(x) Mode(x)),
                       y$MSZoning)

y$Utilities = ifelse(is.na(y$Utilities),
                      ave(y$Utilities, FUN = function(x) Mode(x)),
                      y$Utilities)

y$Exterior1st = ifelse(is.na(y$Exterior1st),
                      ave(y$Exterior1st, FUN = function(x) Mode(x)),
                      y$Exterior1st)
y$Exterior2nd = ifelse(is.na(y$Exterior2nd),
                       ave(y$Exterior2nd, FUN = function(x) Mode(x)),
                       y$Exterior2nd)


y$BsmtFinSF1 = ifelse(is.na(y$BsmtFinSF1),
                      ave(y$BsmtFinSF1, FUN = function(x) mean(x, na.rm = TRUE)),
                      y$BsmtFinSF1)

y$BsmtFinSF2 = ifelse(is.na(y$BsmtFinSF2),
                       ave(y$BsmtFinSF2, FUN = function(x) mean(x, na.rm = TRUE)),
                       y$BsmtFinSF2)

y$BsmtUnfSF = ifelse(is.na(y$BsmtUnfSF),
                      ave(y$BsmtUnfSF, FUN = function(x) mean(x, na.rm = TRUE)),
                      y$BsmtUnfSF)


y$TotalBsmtSF = ifelse(is.na(y$TotalBsmtSF),
                      ave(y$TotalBsmtSF, FUN = function(x) mean(x, na.rm = TRUE)),
                      y$TotalBsmtSF)

y$BsmtFullBath = ifelse(is.na(y$BsmtFullBath),
                      ave(y$BsmtFullBath, FUN = function(x) Mode(x)),
                      y$BsmtFullBath)

y$BsmtHalfBath = ifelse(is.na(y$BsmtHalfBath),
                    ave(y$BsmtHalfBath, FUN = function(x) Mode(x)),
                    y$BsmtHalfBath)

y$KitchenQual = ifelse(is.na(y$KitchenQual),
                     ave(y$KitchenQual, FUN = function(x) Mode(x)),
                     y$KitchenQual)

y$Functional = ifelse(is.na(y$Functional),
                       ave(y$Functional, FUN = function(x) Mode(x)),
                       y$Functional)

y$GarageCars = ifelse(is.na(y$GarageCars),
                       ave(y$GarageCars, FUN = function(x) Mode(x)),
                       y$GarageCars)

y$GarageArea = ifelse(is.na(y$GarageArea),
                      ave(y$GarageArea, FUN = function(x) mean(x, na.rm = TRUE)),
                      y$GarageArea)
y$SaleType = ifelse(is.na(y$SaleType),
                      ave(y$SaleType, FUN = function(x) Mode(x)),
                      y$SaleType)
y$GarageYrBlt = NULL




s=sapply(y, function(x) sum(is.na(x)))
p=subset(s,s>0)


# Encoding categorical data
y$MSZoning = factor(y$MSZoning,
                    levels =  unique(y$MSZoning),
                    labels = c(1:length( unique(y$MSZoning))))

y$Street = factor(y$Street,
                          levels =  unique(y$Street),
                          labels = c(1:length( unique(y$Street))))

y$Alley = factor(y$Alley,
                          levels =  unique(y$Alley),
                          labels = c(1:length( unique(y$Alley))))

y$LotShape = factor(y$LotShape,
                         levels =  unique(y$LotShape),
                         labels = c(1:length( unique(y$LotShape))))
y$LandContour = factor(y$LandContour,
                          levels =  unique(y$LandContour),
                          labels = c(1:length( unique(y$LandContour))))

y$Utilities = factor(y$Utilities,
                        levels =  unique(y$Utilities),
                        labels = c(1:length( unique(y$Utilities))))

y$LotConfig = factor(y$LotConfig,
                       levels =  unique(y$LotConfig),
                       labels = c(1:length( unique(y$LotConfig))))

y$LandSlope = factor(y$LandSlope,
                          levels =  unique(y$LandSlope),
                          labels = c(1:length( unique(y$LandSlope))))

y$Neighborhood = factor(y$Neighborhood,
                          levels =  unique(y$Neighborhood),
                          labels = c(1:length( unique(y$Neighborhood))))

y$Condition1  = factor(y$Condition1 ,
                        levels =  unique(y$Condition1 ),
                        labels = c(1:length( unique(y$Condition1 ))))

y$Condition2 = factor(y$Condition2,
                       levels =  unique(y$Condition2),
                       labels = c(1:length( unique(y$Condition2))))

y$BldgType = factor(y$BldgType,
                          levels =  unique(y$BldgType),
                          labels = c(1:length( unique(y$BldgType))))
y$HouseStyle = factor(y$HouseStyle,
                             levels =  unique(y$HouseStyle),
                             labels = c(1:length( unique(y$HouseStyle))))

y$OverallQual = factor(y$OverallQual,
                           levels =  unique(y$OverallQual),
                           labels = c(1:length( unique(y$OverallQual))))

y$OverallCond = factor(y$OverallCond,
                           levels =  unique(y$OverallCond),
                           labels = c(1:length( unique(y$OverallCond))))

y$YearBuilt = factor(y$YearBuilt,
                           levels =  unique(y$YearBuilt),
                           labels = c(1:length( unique(y$YearBuilt))))

y$YearRemodAdd = factor(y$YearRemodAdd,
                          levels =  unique(y$YearRemodAdd),
                          labels = c(1:length( unique(y$YearRemodAdd))))

y$RoofStyle = factor(y$RoofStyle,
                        levels =  unique(y$RoofStyle),
                        labels = c(1:length( unique(y$RoofStyle))))

y$RoofMatl = factor(y$RoofMatl,
                       levels =  unique(y$RoofMatl),
                       labels = c(1:length( unique(y$RoofMatl))))

y$Exterior1st = factor(y$Exterior1st,
                          levels =  unique(y$Exterior1st),
                          labels = c(1:length( unique(y$Exterior1st))))
y$Exterior2nd = factor(y$Exterior2nd,
                             levels =  unique(y$Exterior2nd),
                             labels = c(1:length( unique(y$Exterior2nd))))

y$MasVnrType = factor(y$MasVnrType,
                           levels =  unique(y$MasVnrType),
                           labels = c(1:length( unique(y$MasVnrType))))

y$ExterQual = factor(y$ExterQual,
                           levels =  unique(y$ExterQual),
                           labels = c(1:length( unique(y$ExterQual))))

y$ExterCond = factor(y$ExterCond,
                           levels =  unique(y$ExterCond),
                           labels = c(1:length( unique(y$ExterCond))))

y$Foundation = factor(y$Foundation,
                              levels =  unique(y$Foundation),
                              labels = c(1:length( unique(y$Foundation))))

y$BsmtQual  = factor(y$BsmtQual ,
                             levels =  unique(y$BsmtQual ),
                             labels = c(1:length( unique(y$BsmtQual ))))

y$BsmtCond = factor(y$BsmtCond,
                            levels =  unique(y$BsmtCond),
                            labels = c(1:length( unique(y$BsmtCond))))

y$BsmtExposure = factor(y$BsmtExposure,
                          levels =  unique(y$BsmtExposure),
                          labels = c(1:length( unique(y$BsmtExposure))))
y$BsmtFinType1 = factor(y$BsmtFinType1,
                            levels =  unique(y$BsmtFinType1),
                            labels = c(1:length( unique(y$BsmtFinType1))))

y$BsmtFinType2 = factor(y$BsmtFinType2,
                             levels =  unique(y$BsmtFinType2),
                             labels = c(1:length( unique(y$BsmtFinType2))))

y$Heating = factor(y$Heating,
                             levels =  unique(y$Heating),
                             labels = c(1:length( unique(y$Heating))))

y$HeatingQC = factor(y$HeatingQC,
                           levels =  unique(y$HeatingQC),
                           labels = c(1:length( unique(y$HeatingQC))))

y$CentralAir = factor(y$CentralAir,
                          levels =  unique(y$CentralAir),
                          labels = c(1:length( unique(y$CentralAir))))

y$Electrical = factor(y$Electrical,
                        levels =  unique(y$Electrical),
                        labels = c(1:length( unique(y$Electrical))))

y$KitchenQual = factor(y$KitchenQual,
                       levels =  unique(y$KitchenQual),
                       labels = c(1:length( unique(y$KitchenQual))))

y$Functional = factor(y$Functional,
                          levels =  unique(y$Functional),
                          labels = c(1:length( unique(y$Functional))))
y$FireplaceQu = factor(y$FireplaceQu,
                             levels =  unique(y$FireplaceQu),
                             labels = c(1:length( unique(y$FireplaceQu))))

y$GarageType = factor(y$GarageType,
                           levels =  unique(y$GarageType),
                           labels = c(1:length( unique(y$GarageType))))

y$GarageFinish = factor(y$GarageFinish,
                           levels =  unique(y$GarageFinish),
                           labels = c(1:length( unique(y$GarageFinish))))

y$GarageQual = factor(y$GarageQual,
                           levels =  unique(y$GarageQual),
                           labels = c(1:length( unique(y$GarageQual))))

y$GarageCond = factor(y$GarageCond,
                              levels =  unique(y$GarageCond),
                              labels = c(1:length( unique(y$GarageCond))))

y$PavedDrive  = factor(y$PavedDrive ,
                             levels =  unique(y$PavedDrive ),
                             labels = c(1:length( unique(y$PavedDrive ))))

y$PoolQC = factor(y$PoolQC,
                            levels =  unique(y$PoolQC),
                            labels = c(1:length( unique(y$PoolQC))))

y$Fence = factor(y$Fence,
                          levels =  unique(y$Fence),
                          labels = c(1:length( unique(y$Fence))))
y$MiscFeature = factor(y$MiscFeature,
                            levels =  unique(y$MiscFeature),
                            labels = c(1:length( unique(y$MiscFeature))))

y$MoSold = factor(y$MoSold,
                             levels =  unique(y$MoSold),
                             labels = c(1:length( unique(y$MoSold))))

y$YrSold = factor(y$YrSold,
                             levels =  unique(y$YrSold),
                             labels = c(1:length( unique(y$YrSold))))

y$SaleType = factor(y$SaleType,
                           levels =  unique(y$SaleType),
                           labels = c(1:length( unique(y$SaleType))))

y$SaleCondition = factor(y$SaleCondition,
                              levels =  unique(y$SaleCondition),
                              labels = c(1:length( unique(y$SaleCondition))))


#replacing the data sets
dataset = y[1:1460,]
test_data = y[1461:2919,]
dataset$SalePrice = saleprice
dataset2 = dataset
#binning
la_bins=cut(unclass(dataset$LotArea),20,include.lowest=T)
bfs1_bins=cut(unclass(dataset$BsmtFinSF1),20,include.lowest=T)
mva_bins=cut(unclass(dataset$MasVnrArea),20,include.lowest=T)
bfs2_bins=cut(unclass(dataset$BsmtFinSF2),20,include.lowest=T)
bus_bins=cut(unclass(dataset$BsmtUnfSF),20,include.lowest=T)
tbsf_bins=cut(unclass(dataset$TotalBsmtSF),20,include.lowest=T)
x1fs_bins=cut(unclass(dataset$X1stFlrSF),20,include.lowest=T)
x2fs_bins=cut(unclass(dataset$X2ndFlrSF),20,include.lowest=T)
gla_bins=cut(unclass(dataset$GrLivArea),20,include.lowest=T)
#gyb_bins=cut(unclass(y$GarageYrBlt),20,include.lowest=T)
ga_bins=cut(unclass(dataset$GarageArea),20,include.lowest=T)
wds_bins=cut(unclass(dataset$WoodDeckSF),20,include.lowest=T)
ops_bins=cut(unclass(dataset$OpenPorchSF),20,include.lowest=T)
ep_bins=cut(unclass(dataset$EnclosedPorch),20,include.lowest=T)
scrnp_bins=cut(unclass(dataset$ScreenPorch),20,include.lowest=T)
X3_bins=cut(unclass(dataset$X3SsnPorch),20,include.lowest=T)
lf_bins=cut(unclass(dataset$LotFrontage),20,include.lowest=T)
yb_bins=cut(unclass(dataset$YearBuilt),20,include.lowest=T)

# for(col in y[-80]){
#   if(length(unique(col))>53){count=count+1}}
#y$sp_bins=sp_bins
dataset$la_bins = la_bins
dataset$bfs1_bins=bfs1_bins
dataset$mva_bins=mva_bins
dataset$bfs2_bins=bfs2_bins
dataset$bus_bins=bus_bins
dataset$tbsf_bins=tbsf_bins
dataset$x1fs_bins=x1fs_bins
dataset$x2fs_bins=x2fs_bins
dataset$gla_bins=gla_bins
#y$gyb_bins=gyb_bins
dataset$ga_bins=ga_bins
dataset$wds_bins=wds_bins
dataset$ops_bins=ops_bins
dataset$ep_bins=ep_bins
dataset$scrnp_bins=scrnp_bins
dataset$X3_bins=X3_bins
dataset$lf_bins=lf_bins
dataset$yb_bins=yb_bins

dataset$X3SsnPorch=NULL
dataset$LotArea=NULL
dataset$BsmtFinSF1 =NULL
dataset$MasVnrArea =NULL
dataset$BsmtFinSF2 =NULL
dataset$BsmtUnfSF =NULL
dataset$TotalBsmtSF =NULL
dataset$X1stFlrSF =NULL
dataset$X2ndFlrSF =NULL
dataset$GrLivArea =NULL
dataset$GarageYrBlt =NULL
dataset$GarageArea =NULL
dataset$WoodDeckSF =NULL
dataset$OpenPorchSF =NULL
dataset$ScreenPorch =NULL
dataset$EnclosedPorch = NULL
dataset$LotFrontage=NULL
dataset$YearRemodAdd=NULL
dataset$YearBuilt=NULL




# Splitting the y into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$SalePrice, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

train_id=training_set$Id
training_set$Id=NULL
# Fitting Random Forest Regression to the y
# install.packages('randomForest')
library(randomForest)
set.seed(1234)
rf = randomForest(x = training_set[-80],
                         y = training_set$SalePrice,
                         ntree = 50)


# Variable Importance Plot
varImpPlot(rf,sort = T,main="Variable Importance",n.var=30)
# Fitting Multiple Linear Regression to the Training set
lr = lm(formula = SalePrice ~ OverallCond+GarageCars+Neighborhood+ExterQual+GrLivArea+BsmtQual+TotalBsmtSF+X1stFlrSF+GarageArea+KitchenQual+BsmtFinSF1+Exterior2nd+GarageType,
               data = training_set)


#summary(lr)
# Predicting the Test set results
y_pred = predict(lr, newdata = test_set)

#Rmse
#RMSE=sqrt(mean((y_pred-test_set$SalePrice)^2))
r2=1 - (sum(( y_pred-test_set$SalePrice)^2)/sum((test_set$SalePrice-mean(test_set$SalePrice))^2))
r2

#final prediction
final_lr = lm(formula = SalePrice ~ OverallCond+GarageCars+Neighborhood+ExterQual+GrLivArea+BsmtQual+TotalBsmtSF+X1stFlrSF+GarageArea+KitchenQual+BsmtFinSF1+Exterior2nd+GarageType,
              data = dataset2)
ypred_final = predict(final_lr, newdata = test_data)
final_prediction=data.frame(Id=test_data$Id,SalePrice=ypred_final)
write.csv(final_prediction,file='C:/Users/manub/Downloads/DM/project/house prices/HP_solution.csv',row.names=F)





