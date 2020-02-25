library(tree)
library(dplyr)
houses <- read.csv("../../train.csv")
modtree_df <- houses %>%
  select(., OverallQual, GrLivArea, YearBuilt, KitchenQual, Fireplaces, 
         GarageArea, Neighborhood, YearRemodAdd, HeatingQC, SalePrice)
modtree_df <- modtree_df %>%
  mutate(., SalePrice = log(SalePrice))
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


