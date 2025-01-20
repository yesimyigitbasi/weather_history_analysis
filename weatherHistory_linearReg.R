
data <- read.csv("weatherHistory_cleaned.csv")

# scatter plot for Temperature and Humidity
with(data, plot(Temperature..C., Humidity,
                main = "Scatter Plot of Temperature vs Humidity",
                xlab = "Temperature (°C)",
                ylab = "Humidity",
                pch = 19, col = "blue", cex = 0.1))

# histogram for Temperature and Humidity
hist(data$Temperature..C.,
     main = "Histogram of Temperature",
     xlab = "Temperature (°C)",
     col = "lightblue",
     border = "black")
hist(data$Humidity,
     main = "Histogram of Humidity",
     xlab = "Humidity",
     col = "lightgreen",
     border = "black")


# find outliers for Temperature and Humidity and mark outliers on scatter plot
temperature_outliers <- which(data$Temperature..C. %in% boxplot.stats(data$Temperature..C., coef = 1.4)$out)
humidity_outliers <- which(data$Humidity %in% boxplot.stats(data$Humidity, coef = 1.4)$out)
outliers <- union(temperature_outliers, humidity_outliers)
points(data[outliers, "Temperature..C."],
       data[outliers, "Humidity"],
       col = "red", pch = "x", cex = 1.0)

library(MASS)

# correlation between Temperature and Humidity 
correlation <- cor(data$Temperature..C., data$Humidity, use = "complete.obs")
print(paste("Correlation coefficient (rho):", correlation))

# fit a linear regression model and add the regression line to the plot
temp_humidity_model <- lm(Humidity ~ Temperature..C., data = data)
summary(temp_humidity_model)
abline(temp_humidity_model, lwd = 2)

# Residual plot
plot(temp_humidity_model$residuals, main = "Residual Plot",
     xlab = "Index", ylab = "Residuals", pch = 16, col = "red")

# confidence intervals for regression coefficients
conf_int <- confint(temp_humidity_model, level = 0.95)
print(conf_int)

# make predictions for 10 and 30 degrees
pred_temp <- data.frame(Temperature..C. = c(10, 30))
predicted_ci <- predict(temp_humidity_model, newdata = pred_temp, interval = "confidence", level = 0.95)
predicted_pi <- predict(temp_humidity_model, newdata = pred_temp, interval = "prediction", level = 0.95)
print(predicted_ci)
print(predicted_pi)

# Plot predictions
plot(data$Humidity ~ data$Temperature..C., xlim = c(0, 40), ylim = c(0.4, 1.0),
     xlab = "Temperature (°C)", ylab = "Humidity", pch = 16, col = "blue", cex = 0.5)
abline(temp_humidity_model, lwd = 2)