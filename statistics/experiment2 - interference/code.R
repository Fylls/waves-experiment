# Δx

# Data
l = 0.8
d = c( 0.5, 0.25, 0.5, 0.25 ) / 1000
x = c( 0.109, 0.199, 0.111, 0.216 ) / 100

# Laser wavelength
lambda = mean( x * d / l )
lambda

# Linear regression
model = lm( x ~ I(l/d) )
summary(model)

# Plots with regression line
plot(l/d, x)
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