# running MLR on our heavy feature list

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
