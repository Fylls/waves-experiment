# Magnetic permittivity of free space
mu = 4 * pi * 10^-7

# Data
radius1 = 0.105
radius2 = 0.015
windings1 = 200
windings2 = 2000
frequency = c(10,20,30,40)
current1 = c(2.874,2.780,2.664,2.523) * 0.0506 # non ho capito se voltaggio o
voltage2 = c(0.024,0.037,0.041,0.049)

# mutual inductance
M = (mu * windings1 * windings2 * radius2^2) / (2 * radius1)

# Linear regression
model = lm( voltage2 ~ I( current1 * frequency  * 2 * pi ) )
summary(model)

# Plots with regression line
plot(current1 * frequency  * 2 * pi, voltage2)
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