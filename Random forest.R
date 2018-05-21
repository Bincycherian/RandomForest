real_estate <- read.csv("real_estate_data_prepn.csv")
View(real_estate)
dataset = real_estate

# 
# install.packages('caTools')
library(caTools)

set.seed(123)
split = sample.split(dataset$tx_price, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)


# install.packages("randomForest")
library(randomForest)
set.seed(1234)
regressor = randomForest(x = training_set,
                         y = training_set$tx_price,
                         ntree = 10)

regressor$rsq

y_pred = predict(regressor, newdata = test_set)
y_pred

y_mae <- mean(abs(y_pred-test_set$tx_price))
y_mae

y_pred = predict(regressor, data.frame(beds=2,baths=1,twobedtwobath=0,sqft=768,lot_size=0,beauty_spas=10,median_age=35,married=29,college_grad=73,property_tax=192,insurance=58,num_schools=3,age_of_property=56,year_flag=0))

y_pred


summary(y_pred)


# Visualising the Random Forest Regression results (higher resolution)
install.packages('ggplot2')
library(ggplot2)
x_grid = seq(min(dataset$Level), max(dataset$Level), 0.01)
ggplot() +
geom_point(aes(x = dataset$Level, y = dataset$Salary),
colour = 'red') +
geom_line(aes(x = x_grid, y = predict(regressor, newdata = data.frame(Level = x_grid))),
colour = 'blue') +
ggtitle('Truth or Bluff (Random Forest Regression)') +
xlab('Level') +
ylab('Salary')

