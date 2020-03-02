# After running file ames tree model
# can only a few features help us predict price per square foot using
# a random forest?


head(houses)
dft <- houses %>%
  dplyr::select(., OverallQual, GrLivArea, Neighborhood, YearRemodAdd, YrSold, SalePrice)
dft <-dft %>%
  mutate(., Ppsq = SalePrice/GrLivArea)
dft <- dft %>%
  mutate(., YrSince = YrSold - YearRemodAdd)
head(dft)


# investigating price per square foot 

ddf <- dft %>%
  dplyr::select(., OverallQual, Neighborhood, YrSince, GrLivArea, SalePrice)
head(ddf)
ddf<-ddf %>%
  mutate(., Ppsq = SalePrice/GrLivArea)
head(ddf)

ddf<- ddf %>%
  dplyr::select(., OverallQual, Neighborhood, YrSince, Ppsq)

ddf <- slice(ddf, -523)
ddf <- slice(ddf, -1297)

set.seed(0)
train = sample(1:nrow(ddf), 8*nrow(ddf)/10) #Training indices.
ddf.test = ddf[-train, ] #Test dataset.
Ppsq.test = ddf$Ppsq[-train] #Test resp

ddf.train = ddf[train, ] # training set is all indices that match our sample
ddf.test = ddf[-train, ] # test set is everything else

set.seed(0)
rf.sq = randomForest(Ppsq ~ ., data = ddf.train, importance = T,
                       do.trace = 50, mtry = 1)
rf.sq

importance(rf.sq)
varImpPlot(rf.sq)


#Training the tree to predict price per square foot
tree.ames = tree(Ppsq ~ ., ddf, subset = train)
summary(tree.ames)

#Visually inspecting the regression tree.
plot(tree.ames)
text(tree.ames, pretty = 0)

#Performing cross-validation.
set.seed(0)
cv.ames = cv.tree(tree.ames)
par(mfrow = c(1, 2))
plot(cv.ames$size, cv.ames$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv.ames$k, cv.ames$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")


#Calculating and assessing the MSE of the test data on the overall tree.
yhat = predict(tree.ames, newdata = ddf[-train, ])
yhat
ames.test = ddf[-train, "Ppsq"]
ames.test
plot(yhat, ames.test)
abline(0, 1)
mean((yhat - ames.test)^2)



set.seed(0)
oob.err = numeric(3)
for (mtry in 1:3) {
  fit = randomForest(Ppsq ~ ., data = ddf[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:3, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.boston)
varImpPlot(rf.boston)



table(predict(rf.sq, ddf.test, type = "regression"), ddf.test$Ppsq)
(118 + 55)/nrow(ddf.test)

set.seed(0)
rf.2var = randomForest(Ppsq ~ ., data = ddf.train, mtry = 3) #fitting with our best hyperparameter
table(predict(rf.2var, ddf.test, type = "response"), ddf.test$Ppsq)
(118 + 55)/nrow(ddf.test)

model = lm(dist ~ speed, data = cars) #Use the linear model function lm() to
#conduct the simple linear regression.

summary(model)




model.saturated = lm(Ppsq ~ ., data = ddf.train)

summary(model.saturated)

plot(model.saturated) #Assessing the assumptions of the model.

library(car) #Companion to applied regression.
influencePlot(model.saturated)

vif(model.saturated) #Assessing the variance inflation factors for the variables
#in our model.

#Added variable plots for assessing the contribution of each additional variable.
avPlots(model.saturated) #Distinct patterns are indications of good contributions

#to the model; absent patterns usually are pointers to
#variables that could be dropped from the model.

#We note that Illiteracy has a large VIF, an insignificant p-value in the overall
#regression, and no strong distinct pattern in the added-variable plot. What
#happens when we remove it from the model?
model2 = lm(Life.Exp ~ . - Illiteracy, data = states)

summary(model2) #R^2 adjusted went up, model still significant, etc.

plot(model2) #No overt additional violations.

influencePlot(model2) #No overt additional violations; Hawaii actually lowers
#its hat value (leverage).

vif(model2) #VIFs all decrease.

#We can compare these two models using a partial F-test using the anova function.
#Here, the first model we supply is the reduced model, and the second is the full
#model.
anova(model2, model.saturated)


AIC(model.saturated)
BIC(model.saturated)

#We can use stepwise regression to help automate the variable selection process.
#Here we define the minimal model, the full model, and the scope of the models
#through which to search:
model.empty = lm(Ppsq ~ 1, data = ddf.train) #The model with an intercept ONLY.
model.full = lm(Ppsq ~ ., data = ddf.train) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))



summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)
confint(forwardAIC)

predict(forwardAIC, ddf.test, interval = "confidence") #Construct confidence intervals
#for the average value of an
#outcome at a specific point.

predict(forwardAIC, ddf.test, interval = "prediction")





model2_df <- read.csv("model2_df.csv")

head(model2_df)

set.seed(0)
train = sample(1:nrow(ddf), 8*nrow(ddf)/10) #Training indices.

model2_df.train = model2_df[train, ] # training set is all indices that match our sample
model2_df.test = model2_df[-train, ] # test set is everything else


model.saturated = lm(log_sales ~ ., data = model2_df.train)

summary(model.saturated) #Many predictor variables are not significant, yet the
#overall regression is significant.

plot(model.saturated) #Assessing the assumptions of the model.


library(car) #Companion to applied regression.
influencePlot(model.saturated)

vif(model.saturated) #Assessing the variance inflation factors for the variables
#in our model.

#Added variable plots for assessing the contribution of each additional variable.
avPlots(model.saturated) #Distinct patterns are indications of good contributions

#to the model; absent patterns usually are pointers to
#variables that could be dropped from the model.

#We note that Illiteracy has a large VIF, an insignificant p-value in the overall
#regression, and no strong distinct pattern in the added-variable plot. What
#happens when we remove it from the model?
model2 = lm(Life.Exp ~ . - Illiteracy, data = states)

summary(model2) #R^2 adjusted went up, model still significant, etc.

plot(model2) #No overt additional violations.

influencePlot(model2) #No overt additional violations; Hawaii actually lowers
#its hat value (leverage).

vif(model2) #VIFs all decrease.

#We can compare these two models using a partial F-test using the anova function.
#Here, the first model we supply is the reduced model, and the second is the full
#model.
anova(model2, model.saturated)


AIC(model.saturated)
BIC(model.saturated)

#We can use stepwise regression to help automate the variable selection process.
#Here we define the minimal model, the full model, and the scope of the models
#through which to search:
model.empty = lm(log_sales ~ 1, data = model2_df.train) #The model with an intercept ONLY.
model.full = lm(log_sales ~ ., data = model2_df.train) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

library(MASS) #The Modern Applied Statistics library.

#Stepwise regression using AIC as the criteria (the penalty k = 2).
forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
backwardAIC = step(model.full, scope, direction = "backward", k = 2)
bothAIC.empty = step(model.empty, scope, direction = "both", k = 2)
bothAIC.full = step(model.full, scope, direction = "both", k = 2)

#Stepwise regression using BIC as the criteria (the penalty k = log(n)).
forwardBIC = step(model.empty, scope, direction = "forward", k = log(50))
backwardBIC = step(model.full, scope, direction = "backward", k = log(50))
bothBIC.empty = step(model.empty, scope, direction = "both", k = log(50))
bothBIC.full = step(model.full, scope, direction = "both", k = log(50))



summary(forwardAIC)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)

confint(forwardAIC)



newdata = model2_df.test %>%
  dplyr::select(., Total_size, LotArea, Neighborhood, BldgType_clean, 
                OverallQual, Age_at_sale, ExterQual, Outdoor_Porch_Size, 
                YrSold, KitchenQual, Fireplaces)
head(newdata)
y_test = model2_df.test %>%
  dplyr::select(., log_sales)
head(y_test)

predict(forwardAIC, newdata, interval = "confidence") #Construct confidence intervals
#for the average value of an
#outcome at a specific point.

predict(forwardAIC, newdata, interval = "prediction")


predictions = predict(model.saturated, newdata)
confusionMatrix(predictions, y_test)
library(caret)

performance(predictions, model = forwardAIC )


rsquared = (y_test$log_sales - predictions) ^ 2
mse^(.5)
4^(.5)
rsquared
summary(forwardAIC)

rsq = rSquared(y_test$log_sales, resid = y_test$log_sales - predictions)
rsq
mse <- mean(residuals(model.simple)^2)
mse

sqrt(mse)
