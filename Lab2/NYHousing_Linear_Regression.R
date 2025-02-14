library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("D:/data_analytics/lab2/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point()

## filter data
dataset <- dataset[dataset$PRICE<195000000,]
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]
dataset <- dataset[dataset$PROPERTYSQFT<=2100,]
dataset <- dataset[dataset$BEDS<=12,]
dataset <- dataset[dataset$BATH<=12,]
dataset <- dataset[dataset$BATH!=2.3738608579684373,]
dataset$PROPERTYSQFT[dataset$BROKERTITLE=="Brokered by Douglas Elliman - 575 Madison Ave"][85]

## column names
names(dataset)

## fit linear model
lmod <- lm(PRICE~PROPERTYSQFT, data = dataset)

lmod <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)

## print model output
summary(lmod)

## scatter plot of 2 variables
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod)

plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## Beds Information
ggplot(dataset, aes(x = log10(BEDS), y = log10(PRICE))) +
  geom_point()

## column names
names(dataset_beds)

## fit linear model
lmod_bd <- lm(PRICE~PROPERTYSQFT+BEDS, data = dataset)

lmod_bd <- lm(log10(PRICE)~log10(PROPERTYSQFT)+BEDS, data = dataset)

## print model output
summary(lmod_bd)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE, color = factor(BEDS))) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE, color = factor(BEDS))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE), color = factor(BEDS))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

## Bath Information
ggplot(dataset, aes(x = log10(BATH), y = log10(PRICE))) +
  geom_point()

## fit linear model
lmod_ba <- lm(PRICE~PROPERTYSQFT+BATH, data = dataset)

lmod_ba <- lm(log10(PRICE)~log10(PROPERTYSQFT) + BEDS + BATH, data = dataset)

## print model output
summary(lmod_ba)

## scatter plot of 2 variables
ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE, color=factor(BATH))) +
  geom_point()

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE, color=factor(BATH))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")

ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE), color=factor(BATH))) +
  geom_point() +
  stat_smooth(method = "lm", col="red")