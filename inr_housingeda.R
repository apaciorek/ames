house<-read.csv("train.csv")
annette <- house %>%
  select(., SalePrice,41:60)
head(annette)
summarise(annette)
summary(annette)
names(annette)
net <- annette %>%
  mutate(., sumsf = X1stFlrSF + X2ndFlrSF)
net <- net %>%
  select(., sumsf, GrLivArea)
plot(net)
# notice 25 properties where sumsf underestimates grlivarea
ne2 <- annette %>%
  select(., GrLivArea, TotRmsAbvGrd, Fireplaces, SalePrice)
plot(ne2)
ne3 <- annette %>%
  select(., SalePrice, HeatingQC, Electrical, GrLivArea, Fireplaces)
plot(ne3)
ne4 <- ne3 %>% select(., SalePrice, Electrical)
plot(ne4)
summary(ne4$Electrical)

ne5 <- annette %>%
  select(., SalePrice, KitchenAbvGr, KitchenQual)
plot(ne5)
ne6 <- annette %>%
  select(., SalePrice, BsmtFullBath, BsmtHalfBath, FullBath, HalfBath)
ne6 <- ne6 %>%
  mutate(., FullBtotal = BsmtFullBath + FullBath, 
         HalfBtotal = BsmtHalfBath + HalfBath)
plot(ne6)

ne7 <- ne6 %>%
  select(., SalePrice, FullBtotal, FullBath)
plot(ne7)
ne8 <- annette %>%
  select(., SalePrice, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea)
ne9 <- ne8 %>%
  mutate(., sumtot = X1stFlrSF + X2ndFlrSF + LowQualFinSF)
ne10 <- ne9 %>%
  select(., sumtot, GrLivArea, SalePrice)
plot(ne10)
plot(ne8)

# notice GrLiveArea = X1st + X2nd + LowQual and none of these columns seem to have
# a stronger relationship on the sale price than GrLiveArea
# so we can disregard these three columns

ne11 <- annette %>%
  select(., GarageYrBlt, SalePrice)
plot(ne11)


# garage year built may be correlated with a higher price 
# this information may already exist elsewhere in the dataframe

ne8 <- annette %>%
  select(., SalePrice, X1stFlrSF, X2ndFlrSF, LowQualFinSF, GrLivArea)
ne9 <- ne8 %>%
  mutate(., sumtot = X1stFlrSF + X2ndFlrSF + LowQualFinSF)
plot(ne9)
ne12 <- annette %>%
  select(., SalePrice, TotRmsAbvGrd, BedroomAbvGr, KitchenAbvGr)
plot(ne12)
ne13 <- ne12 %>%
  mutate(., totsum = BedroomAbvGr + KitchenAbvGr)
plot(ne13)
