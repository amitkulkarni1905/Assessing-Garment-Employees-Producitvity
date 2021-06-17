#loading libraries
library(dplyr)
library(ggcorrplot)

#reading the dataset
df <- read.csv("C:/Users/Amit/Downloads/garments_worker_productivity.csv")

#calculating correlation matrix with precision of 2 decimal places
corr_matrix <- round(cor(df),2)

# visualizing the correlation matrix to observe the strength of association between 

# Dependent variable : actual_productivity(The actual % of productivity that was delivered by the workers) 

# &

# Independent Variables :
# smv(Standard Minute Value)
# no_of_style_change(Number of changes in the style of a particular product)
# team(Associated team number with the instance)
# idle_men(The number of workers who were idle due to production interruption)
ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE)

# Constructing a linear model 
model <- lm(actual_productivity ~ smv + no_of_style_change + team + idle_men, data = df)

# Getting intercepts & coefficients 
a <- coef(model)[1]

X_smv <- coef(model)[2]

X_no_of_style_change <- coef(model)[3]

X_team <- coef(model)[4]

X_idle_men <- coef(model)[5]

# Applying multiple linear regression equation with other values to predict new value of actual_productivity
Y <- a + X_smv*(15.26) + X_no_of_style_change*(0) + X_team*(6) + X_idle_men*(10) 

cat("Calculated actual_productivity : ",(Y*100),"%")

# Assessing the performance of the model with Residual Standard Error estimate
rse <- (sigma(model)/mean(df$actual_productivity))
cat("RSE : ",(rse*100),"%")