# Data
frequency = c( 10, 20, 30, 40 )
current1 = c( 5.748, 5.561, 5.280, 4.953 ) * 0.0506
voltage2 = c( 0.034, 0.062, 0.081, 0.112 )

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