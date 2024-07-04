dataset <- dados$eq.space.0.25[[1]]

model <- resultado_ajustes$mods$eq.space.0.25[[1]]

# Make predictions
new_x_values <- seq(0, 12, length.out = 100)
predictions <- predict(model, newdata = data.frame(x = new_x_values))

# Get model summary
model_summary <- summary(model)

# Extract coefficients and their standard errors
coefficients <- coef(model)
se_coefficients <- coef(summary(model))[, "Std. Error"]

# Calculate standard error of predictions (assuming a simple model for example)
new_data <- data.frame(x = new_x_values)
J <- model.matrix(~ x, data = new_data)  # Jacobian matrix
sigma <- model_summary$sigma  # Residual standard error

# Variance-covariance matrix of the coefficients
vcov_matrix <- vcov(model)

# Standard error of the fitted values
se_fit <- sqrt(diag(J %*% vcov_matrix %*% t(J)))

# Prediction intervals
critical_value <- qt(0.975, df = df.residual(model))  # 95% CI
lower_bound <- predictions - critical_value * se_fit
upper_bound <- predictions + critical_value * se_fit

# Combine the results
prediction_intervals <- data.frame(
  x = new_x_values,
  fit = predictions,
  lwr = lower_bound,
  upr = upper_bound
)

# Display the prediction intervals
print(prediction_intervals)
