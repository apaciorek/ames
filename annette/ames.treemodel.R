library(tree)
library(dplyr)
houses <- read.csv("../../train.csv")
modtree_df <- houses %>%
  select(., OverallQual, GrLivArea, YearBuilt, KitchenQual, Fireplaces,
         GarageArea, Neighborhood, YearRemodAdd, HeatingQC, SalePrice)
modtree_df <- modtree_df %>%
  mutate(., SalePrice = exp(SalePrice))
head(modtree_df)
summary(modtree_df)

tree.modtree_df = tree(SalePrice ~ . - SalePrice, split = "gini", data = modtree_df)
summary(tree.modtree_df)

plot(tree.modtree_df)
text(tree.modtree_df, pretty = 0)
tree.modtree_df

#Splitting the data into training and test sets by an 80% - 20% split.
set.seed(0)
train = sample(1:nrow(modtree_df), 8*nrow(modtree_df)/10) #Training indices.
modtree.test = modtree_df[-train, ] #Test dataset.
SalePrice.test = modtree_df$SalePrice[-train] #Test response.

#Ftting and visualizing a classification tree to the training data.
tree.modtree_df = tree(SalePrice ~ . - SalePrice, data = modtree_df, subset = train)
plot(tree.modtree_df)
text(tree.modtree_df, pretty = 0)
summary(tree.modtree_df)
tree.modtree_df

#Using the trained decision tree to classify the test data.
tree.pred = predict(tree.modtree_df, modtree.test, type = "vector")
tree.pred

df2 <- as.data.frame(lapply(SalePrice.test, unlist))
df2[order(df2[, 1]), ]

df3<- as.data.frame(lapply(tree.pred, unlist))


View(table(tree.pred, SalePrice.test))
(60 + 42)/120

### Regression Tree ###


#regression tree to predict sale price (log transformed)
tree1.ames = tree(SalePrice ~ ., modtree_df, subset = train)
summary(tree1.ames)
plot(tree1.ames)
text(tree1.ames, pretty = 0)

# Performing ccross-validation
set.seed(0)
cv1.ames = cv.tree(tree1.ames)
par(mfrow = c(1, 2))
plot(cv1.ames$size, cv1.ames$dev, type = "b",
     xlab = "Terminal Nodes", ylab = "RSS")
plot(cv1.ames$k, cv1.ames$dev, type  = "b",
     xlab = "Alpha", ylab = "RSS")

#Calculating and assessing the MSE of the test data on the overall tree.
yhat = predict(tree1.ames, newdata = modtree_df[-train, ])
yhat
ames1.test = modtree_df[-train, "SalePrice"]
ames1.test
plot(yhat, ames1.test)
abline(0, 1)
mean((yhat - ames1.test)^2)

### Bagging and Random Forests ###

library(randomForest)
set.seed(0)
rf.ames = randomForest(SalePrice ~ ., data = modtree_df, subset = train, importance = T)
rf.ames

#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(9)
for(mtry in 1:9) {
  fit = randomForest(SalePrice ~., data = modtree_df[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}




#Visualizing the OOB error.
plot(1:9, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates \nby # of Variables")

# see error is lowest when # of variables at each split = 4


#Can visualize a variable importance plot.
importance(rf.ames)
varImpPlot(rf.ames)


### Boosting ###

library(gbm)
#Fitting 10,000 trees with a depth of 4.
set.seed(0)
boost.ames = gbm(SalePrice ~ ., data = modtree_df[train, ],
                   distribution = "gaussian",
                   n.trees = 10000,
                   interaction.depth = 4)

#Inspecting the relative influence.
par(mfrow = c(1, 1))
summary(boost.ames)

#Let’s make a prediction on the test set. With boosting, the number of trees is
#a tuning parameter; having too many can cause overfitting. In general, we should
#use cross validation to select the number of trees. Instead, we will compute the
#test error as a function of the number of trees and make a plot for illustrative
#purposes.
n.trees = seq(from = 50, to = 1000, by = 10)
predmat = predict(boost.ames, newdata = modtree_df[-train, ], n.trees = n.trees)

dim(predmat)

#Calculating the boosted errors.
par(mfrow = c(1, 1))
berr = with(modtree_df[-train, ], apply((predmat - SalePrice)^2, 2, mean))
plot(n.trees, berr, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

#Include the best OOB error from the random forest.
abline(h = min(oob.err), col = "red")

#Increasing the shrinkage parameter; a higher proportion of the errors are
#carried over.
set.seed(0)
boost.ames2 = gbm(SalePrice ~ ., data = modtree_df[train, ],
                    distribution = "gaussian",
                    n.trees = 10000,
                    interaction.depth = 4,
                    shrinkage = 0.1)
predmat2 = predict(boost.ames2, newdata = modtree_df[-train, ], n.trees = n.trees)

berr2 = with(modtree_df[-train, ], apply((predmat2 - SalePrice)^2, 2, mean))
plot(n.trees, berr2, pch = 16,
     ylab = "Mean Squared Error",
     xlab = "# Trees",
     main = "Boosting Test Error")

