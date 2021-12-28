# Δx

# Data
l = 0.8
a = c( 0.16, 0.08, 0.04, 0.02 ) / 1000
x = c( 0.343, 0.676, 1.335, 2.674 ) / 100

# Laser wavelength
lambda = mean( x * a / l )
lambda

# Linear regression
model = lm( x ~ I(l/a) )
summary(model)

# Plots with regression line
plot(l/a, x)
abline(model)

# Residuals
res = resid(model)

# Residual VS Fitted plot
plot(fitted(model), res)
abline(0,0)

# Shapiro-Wilk
shapiro.test(res)
shapiro.test(x)

# QQ-plot 
qqnorm(res)
qqline(res)
qqnorm(x)
qqline(x)

# Density
plot(density(res))
plot(density(x))