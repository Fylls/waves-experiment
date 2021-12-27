# magnetic permittivity of free space
mu = 4 * pi * 10^-7

# Data
radius1 = 0.105
radius2 = 0.015
windings1 = 200
windings2 = 2000
frequencies = c(10,20,30,40)
voltage1 = c(5.748,5.561,5.280,4.953) * 0.0506 # non ho capito se voltaggio o
voltage2 = c(0.034,0.062,0.081,0.112)

# mutual inductance
M = (mu * windings1 * windings2 * radius2^2) / (2 * radius1)

# Linear regression
model = lm( voltage2 ~ I( voltage1 * frequencies  * 2 * pi ) )
summary(model)

# Plots with regression line
plot(voltage1 * frequencies  * 2 * pi, voltage2)
abline(model)

# Residuals
res = resid(model)

# Residual VS Fitted plot
plot(fitted(model), res)
abline(0,0)

# Shapiro-Wilk
shapiro.test(res)
shapiro.test(voltage1)
shapiro.test(voltage2)

# QQ-plot 
qqnorm(res)
qqline(res)
qqnorm(voltage1)
qqline(voltage1)
qqnorm(voltage2)
qqline(voltage2)

# Density
plot(density(res))
plot(density(voltage1))
plot(density(voltage2))