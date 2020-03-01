# running MLR on our light feature list


model2light_df <- read.csv("model2light_df.csv")

model2light_df <- slice(model2light_df, -524)
model2light_df <- slice(model2light_df, -1298)

plot(model2light_df)

set.seed(0)
train = sample(1:nrow(model2light_df), 8*nrow(model2light_df)/10) #Training indices.

model2light_df.train = model2light_df[train, ] # training set is all indices that match our sample
model2light_df.test = model2light_df[-train, ] # test set is everything else

model.saturated = lm(log_sales ~ ., data = model2light_df)
summary(model.saturated)

influencePlot(model.saturated) 


vif(model.saturated)


newdata = model2light_df.test %>%
  dplyr::select(., Total_size, LotArea, Neighborhood,
                OverallQual, Age_at_sale, 
                YrSold)
head(newdata)
y_test = model2light_df.test %>%
  dplyr::select(., log_sales)

predictions = predict(forwardAIC, newdata)

rsq = rSquared(y_test$log_sales, resid = y_test$log_sales - predictions)
rsq




model.empty = lm(log_sales ~ 1, data = model2light_df) #The model with an intercept ONLY.
model.full = lm(log_sales ~ ., data = model2light_df) #The model with ALL variables.
scope = list(lower = formula(model.empty), upper = formula(model.full))

forwardAIC = step(model.empty, scope, direction = "forward", k = 2)
28

summary(forwardAIC)
summary(model.full)
plot(forwardAIC)
influencePlot(forwardAIC)
vif(forwardAIC)
avPlots(forwardAIC)


# loading in our test data 

bigtest <- read.csv("testlight_df.csv")
head( bigtest)

predictions = predict(model.simple, bigtest)
SalePrice = exp(predictions)
type(predictions)
class(predictions)

Id = c(1461:2919)

# formatting csv correctly for submission and imputing missing values

submit_df = data.frame(Id, SalePrice)
head(submit_df)1117
slice(submit_df, 1117)
mean(SalePrice, na.rm = T)

write.csv(submit_df, "submission3.csv", row.names = F)

submit_df[1117, "SalePrice"] = mean(SalePrice, na.rm = T)
head(submit_df)

submit_df <- submit_df %>% arrange(Id)



# OLR with just log sale price versus total size

model.simple = lm(log_sales ~ Total_size, data = model2light_df)
summary(model.simple)

simplest <- read.csv("../../train.csv")
simplest <- simplest %>%
  mutate(., log_sales = log(SalePrice))

model2.simple = lm(log_sales ~ GrLivArea, data = simplest)
summary(model2.simple)

