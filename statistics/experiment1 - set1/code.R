# Data
frequency = c( 10, 20, 30, 40 )
current1 = c( 2.803, 2.709, 2.593, 2.453 ) * 0.0506
voltage2 = c( 0.017, 0.028, 0.041, 0.055 )

# Linear regression
model = lm( voltage2 ~ I( current1 * frequency * 2 * pi ) )
summary(model)

# Plots with regression line
plot(current1 * frequency * 2 * pi, voltage2)
abline(model)

# Residuals
res = resid(model)

# Residual VS Fitted plot
plot(fitted(model), res)
abline(0,0)

# Shapiro-Wilk
shapiro.test(res)
shapiro.test(current1)
shapiro.test(voltage2)

# QQ-plot 
qqnorm(res)
qqline(res)
qqnorm(current1)
qqline(current1)
qqnorm(voltage2)
qqline(voltage2)

# Density
plot(density(res))
plot(density(current1))
plot(density(voltage2))