# Logistic regression is used when outcome variable is a binary variable

library(tidyverse)
library(ggplot2)
library(gridExtra)

breast_data <- 
  read_csv("C:/Users/rohan/OneDrive/Desktop/Winter 2023/MSCI 718 (Statistics for Data Analytics) - MC 4020/Assignments/Assignment 4/data.csv")

breast_data
view(breast_data_main)
str(breast_data_main)
glimpse(breast_data_main)
sum(is.na(breast_data)) # 0
summary(breast_data)

# subset() used to drop the column from our dataset. -c() means remove this column
breast_data <- subset(breast_data, select = -c(...33))
# diagnosis -> convert char to factor so that it can be used in glm() as the outcome variable
breast_data$diagnosis <- as.factor(breast_data$diagnosis)
levels(breast_data$diagnosis)

#####
# correlation plot using ggcorrplot for all the "*mean*" variables
# all of these have values below 0.5 so we can see they're not highly correlated with each other.
#####
install.packages("ggcorrplot")
library(ggcorrplot)

breast_data_cor <- breast_data %>%
  select(contains("mean"))
cor <- round(cor(breast_data_cor), 1)
ggcorrplot(cor, title = "Correlation plot - breast cancer dataset", lab=TRUE)

########
# outcome variable: diagnosis
# predictor variable: area_mean ; smoothness_mean ; symmetry_mean
########

# Plotting data

## area
breast_data %>%
  ggplot(aes(x=area_mean, y=diagnosis))+geom_jitter(width = 0.1, height = 0.25)

breast_data %>%
  ggplot(aes(x=seq(area_mean), y=area_mean))+geom_point()

breast_data %>% 
  ggplot(aes(area_mean))+geom_histogram()

breast_data %>% 
  ggplot(aes(area_mean))+geom_boxplot()

## smoothness
breast_data %>%
  ggplot(aes(x=smoothness_mean, y=diagnosis))+geom_jitter(width = 0.1, height = 0.25)

breast_data %>%
  ggplot(aes(x=seq(smoothness_mean), y=smoothness_mean))+geom_point()

breast_data %>% 
  ggplot(aes(smoothness_mean))+geom_histogram()

breast_data %>% 
  ggplot(aes(smoothness_mean))+geom_boxplot()

## symmetry
breast_data %>%
  ggplot(aes(x=symmetry_mean, y=diagnosis))+geom_jitter(width = 0.1, height = 0.25)

breast_data %>%
  ggplot(aes(x=seq(symmetry_mean), y=symmetry_mean))+geom_point()

breast_data %>% 
  ggplot(aes(symmetry_mean))+geom_histogram()

breast_data %>% 
  ggplot(aes(symmetry_mean))+geom_boxplot()

#####
# Logistic Regression Model formation
#####
bd_model <- glm(diagnosis ~ area_mean + smoothness_mean + symmetry_mean, data = breast_data, family = binomial())
bd_model
summary(bd_model)
# The smaller the AIC, the better the fit (when comparing multiple models).
# The difference between Null deviance and Residual deviance tells us that the model is a good fit. 
# Greater the difference better the model. Null deviance is the value when you only have intercept 
# in your equation with no variables and Residual deviance is the value when you are taking all the
# variables into account. It makes sense to consider the model good if that difference is big enough.

#####
# Checking linearity assumption by checking x*log(x) [interaction effects]
#####
log.area <- breast_data$area_mean * log(breast_data$area_mean)
log.area

log.smoothness <- breast_data$smoothness_mean * log(breast_data$smoothness_mean)
log.smoothness

log.symmetry <- breast_data$symmetry_mean * log(breast_data$symmetry_mean)
log.symmetry

# In logistic regression, we check linearity in a different way - we add in interaction effects 
# with the log of each predictor variable, and see if that is significant. In this case, none of the
# x*log.*variables are significant, so we do not reject the assumption of linearity for these variables.
# Note: Upon test, I could see logit of texture_mean was significant (p<0.05) thus denoting non-linearity. Hence, it was removed from the model.

bd_model1 <- glm(diagnosis ~ area_mean + smoothness_mean + symmetry_mean + log.area + 
                   log.smoothness + log.symmetry, data = breast_data, family = binomial())
bd_model1
summary(bd_model1)

####
# Checking if the continuous independent variables have linearity against the log odds of the dependent variable
# Could also use the Box-Tidwell test
####
# create probabilities and logit variables

probabilities <- predict(bd_model, type="response")
probabilities

logit <- log(probabilities/(1-probabilities))
logit

# scatterplots for our continuous variables for visual inspection
# we see all 3 continuous variables appear to have a fairly linear relationship

area_plot <- ggplot(breast_data, aes(logit, area_mean))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()

smoothness_plot <- ggplot(breast_data, aes(logit, smoothness_mean))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()

symmetry_plot <- ggplot(breast_data, aes(logit, symmetry_mean))+
  geom_point(size=0.5, alpha=0.5)+
  geom_smooth(method = "loess")+
  theme_bw()

# Grid arrange of scatterplots for our 3 continuous variables against logit
grid.arrange(area_plot, smoothness_plot, symmetry_plot, nrow=1, top="Scatterplots for our 3 continuous variables against logit")

####
confint(bd_model)
exp(confint(bd_model))
# The coefficient of radius_mean is 2.72 to 4.31, which is greater than 1.
# This means that, with radius, you are significantly more likely to be Malignant(specifically 2.7 to 4.3
# times more likely), at 5% level of significance.

# Wald z test
# here also we can see radius is a significant predictor for Malign
install.packages("aod")
library(aod)
wald.test( Sigma = vcov(bd_model), b = coef(bd_model), Terms = 1:1)
wald.test( Sigma = vcov(bd_model), b = coef(bd_model), Terms = 2:2)
wald.test( Sigma = vcov(bd_model), b = coef(bd_model), Terms = 3:3)

####
# checking multicollinearity using pearson's correlation since our variables are continuous in nature
# value < 0.7 means fine, else highly correlated
####
cor.test(breast_data$area_mean, breast_data$smoothness_mean, method = "pearson")
cor.test(breast_data$smoothness_mean, breast_data$symmetry_mean, method = "pearson")
cor.test(breast_data$area_mean, breast_data$symmetry_mean, method = "pearson")

# No perfect multicollinearity(predictor variables should not correlate highly)-visual inspection looked good
library(ggplot2)
ggplot(breast_data, aes(y=area_mean, x=smoothness_mean)) + geom_point();
ggplot(breast_data, aes(y=smoothness_mean, x=symmetry_mean)) + geom_point();
ggplot(breast_data, aes(y=symmetry_mean, x=area_mean)) + geom_point();

#VIF
# values below 10 denote no multicollinearity exists between the variables
vif(bd_model)
mean(vif(bd_model))
1/vif(bd_model)

# We inspected the VIF (Variance Inflation Factor) to investigate multicollinearity. 
# The largest VIF was 1.730124, less than 10; the average vif was 1.54804, close to 1. 
# The lowest tolerance (1/VIF) was 0.5779934, much greater than 0.1 (which would indicate a serious 
# problem) and 0.2 (which indicates a potential problem).
# We thus conclude that there is no collinearity in our data.


# independence of errors ????
# Vinamra suggested to use same dublinWatson test for autocorrelation here also
library(car)
durbinWatsonTest(bd_model)

#The Durbin-Watson test for independent errors was significant at the 5% level of significance (d=1.72, p=0). 
#As d is a little away from 2, it shows some degree of positive autocorrelation; it means the model could
#be fine tuned, but since D-W statistic is between 1 and 3, we fail to reject the null hypothesis 
#that the errors are independent, and continue with the assumption of independence met.

# Residuals
plot(bd_model)

# GGpairs plot
view(breast_data)
breast_data <- breast_data %>% 
  select(diagnosis, area_mean, smoothness_mean, symmetry_mean)

library(GGally)
ggpairs(breast_data)

# Also check any other assumptions mentioned in the textbook

# Outliers removal? 
# influential points - cooks test
breast_data.cooks <- cooks.distance(bd_model)
plot(sort(breast_data.cooks, decreasing=TRUE))
max(breast_data.cooks)

# Outliers in residuals

breast_data.fitted <- bd_model$fitted
breast_data.residuals <- bd_model$residuals
breast_data.standardized.residuals <- rstandard(bd_model)

possible.outliers <- subset(breast_data, breast_data.standardized.residuals < -1.96 | breast_data.standardized.residuals > 1.96)
possible.outliers # 12
nrow(breast_data) # 568
# 12/568 = 2.1%
# We found 12 residuals are above or below 1.96 standard deviations. 
# As this represents 2.1% of the observations, expected if the residuals 
# are normal (5% of data is expected to be outside of 2 standard deviations),
# we do not consider any of these observations as outliers and continued 
# with all 568 observations included in the model.

######
# Conclusion

# We built a logistic regression model predicting whether cancer diagnosis is benign or malignant from 
# mean values of area, smoothness and symmetry features of a cell nuclei present in the digitized image of a 
# fine needle aspirate (FNA) of a breast mass. 
# All assumptions of the logistic model were met.

# From this model, we conclude that area, smoothness, and symmetry of the cell nuclei are all significantly involved 
# in predicting whether cancer is benign or malignant. In addition, we can predict the influence of these variables on 
# prediction - for instance, if nothing else is changed, each unit increase in smoothness, nuclei is 1.48e+20 to 
# 9.90e+46 times more likely to be malignant, at 5% level of significance.