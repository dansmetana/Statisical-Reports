setwd('C:/Users/dsmet/OneDrive/Desktop/MABA Program/Coursework/MABA6121 Practical Stats for Business Applications/data_files')

#Read in the data
library(readxl)
moose_associates <- read_excel("Paper2.xlsx", na="NA", col_names = TRUE)

#Convert Region to a factor variable
region_factor <- factor(moose_associates$Region)

#Attach the dataframe for future use
attach(moose_associates)

#Region subset dataframes
moose_region1 <-subset(moose_associates, Region == 1)
moose_region2 <-subset(moose_associates, Region == 2)
moose_region3 <-subset(moose_associates, Region == 3)

#Summary Data
summary(moose_associates)
summary(moose_region1)
summary(moose_region2)
summary(moose_region3)

#Confidence intervals for Revenue, Offices, Partners & Employees
t.test(moose_associates$Revenue, conf.level = .95)
t.test(moose_associates$Offices, conf.level = .95)
t.test(moose_associates$Partners, conf.level = .95)
t.test(moose_associates$Employees, conf.level = .95)

#Linear Model between Revenue and Offices
linefit_offices <-lm(Revenue ~ Offices)
summary(linefit_offices)
#B-Hat confints
confint(linefit_offices, level = .95)

#Polynomial Model between Revenue and Offices... Hypothesis is B2 = 0? 
linefit_offices_poly <- lm(Revenue ~ Offices+ I(Offices^2))
summary(linefit_offices_poly)

#Linear Model between Revenue and Partners
linefit_partners <- lm(Revenue ~ Partners)
summary(linefit_partners)
#B-Hat confints
confint(linefit_partners, level = .95)

#Linear Model between Revenue and Employees
linefit_employees <- lm(Revenue ~ Employees)
summary(linefit_employees)
#B-Hat confints
confint(linefit_employees, level = .95)

#Multi-Linear Models
#Region 2 as the base
linefit_mult_reg2 <- lm(Revenue ~ Offices + Partners + Employees + (region_factor ==1) +(region_factor==3))
summary(linefit_mult_reg2)

#Region 2 as the base... Second Order
linefit_multso_reg2 <- lm(Revenue ~ Offices + I(Offices^2) + Partners + Employees + (region_factor ==1) +(region_factor==3))
summary(linefit_multso_reg2)


#Collinearity?
cor(moose_associates[,2:4])


#Std Residuals normal, mean 0 and constant sd?

#Linear Offices
# Standardized residual plot
linefit_offices.stres <- rstandard(linefit_offices)
plot(moose_associates$Offices, linefit_offices.stres, pch = 16, main = "Standardized Residual Plot",  xlab = "Number of Offices", ylab = "Standarized Residuals")
abline(0,0, lty=2, col="red")
# Shapiro-Wilk test
shapiro.test(linefit_offices.stres)
# Histogram with normal curve
h <- hist(linefit_offices.stres)
# code to add normal curve
x <- linefit_offices.stres
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
# Normal probability plot
qqnorm(linefit_offices.stres, main = "Normal Probability Plot")
qqline(linefit_offices.stres, col = "red")
# Shapiro-Wilk test
shapiro.test(linefit_offices.stres)


#Linear Partners
linefit_partners.stres <- rstandard(linefit_partners)
plot(moose_associates$Partners, linefit_partners.stres, pch = 16, main = "Standardized Residual Plot", xlab = "Number of Partners", ylab = "Standarized Residuals")
abline(0,0, lty=2, col="red")
# Histogram with normal curve
h <- hist(linefit_partners.stres)
# code to add normal curve
x <- linefit_partners.stres
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
# Normal probability plot
qqnorm(linefit_partners.stres, main = "Normal Probability Plot")
qqline(linefit_partners.stres, col = "red")
# Shapiro-Wilk test
shapiro.test(linefit_partners.stres)


#Linear Employees
linefit_employees.stres <- rstandard(linefit_employees)
plot(moose_associates$Employees, linefit_employees.stres, pch = 16, main = "Standardized Residual Plot", xlab = "Number of Employees", ylab = "Standarized Residuals")
abline(0,0, lty=2, col="red")
# Shapiro-Wilk test
shapiro.test(linefit_employees.stres)
# Histogram with normal curve
h <- hist(linefit_employees.stres)
# code to add normal curve
x <- linefit_employees.stres
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
# Normal probability plot
qqnorm(linefit_employees.stres, main = "Normal Probability Plot")
qqline(linefit_employees.stres, col = "red")
# Shapiro-Wilk test
shapiro.test(linefit_employees.stres)


#Multi 1st order
linefit_mult_reg2.stres <- rstandard(linefit_mult_reg2)
plot(linefit_mult_reg2$fitted.values, linefit_mult_reg2.stres, pch = 16, main = "Standardized Residual Plot", ylab = "Standarized Residuals")
abline(0,0, lty=2, col="red")
# Histogram with normal curve
h <- hist(linefit_mult_reg2.stres)
# code to add normal curve
x <- linefit_mult_reg2.stres
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
# Normal probability plot
qqnorm(linefit_mult_reg2.stres, main = "Normal Probability Plot")
qqline(linefit_mult_reg2.stres, col = "red")
# Shapiro-Wilk test
shapiro.test(linefit_mult_reg2.stres)


#Multi 2nd order
linefit_multso_reg2.stres <- rstandard(linefit_multso_reg2)
plot(linefit_multso_reg2$fitted.values, linefit_multso_reg2.stres, pch = 16, main = "Standardized Residual Plot", ylab = "Standarized Residuals")
abline(0,0, lty=2, col="red")
# Histogram with normal curve
h <- hist(linefit_multso_reg2.stres)
# code to add normal curve
x <- linefit_multso_reg2.stres
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
# Normal probability plot
qqnorm(linefit_multso_reg2.stres, main = "Normal Probability Plot")
qqline(linefit_multso_reg2.stres, col = "red")
# Shapiro-Wilk test
shapiro.test(linefit_multso_reg2.stres)


#Poly Offices
# Standardized residual plot
linefit_offices_poly.stres <- rstandard(linefit_offices_poly)
plot(linefit_offices_poly$fitted.values, linefit_offices_poly.stres, pch = 16, main = "Standardized Residual Plot", ylab = "Standarized Residuals")
abline(0,0, lty=2, col="red")
# Shapiro-Wilk test
shapiro.test(linefit_offices_poly.stres)
# Histogram with normal curve
h <- hist(linefit_offices_poly.stres)
# code to add normal curve
x <- linefit_offices_poly.stres
xfit <- seq(min(x), max(x), length = 40)
yfit <- dnorm(xfit, mean = mean(x), sd = sd(x))
yfit <- yfit*diff(h$mids[1:2])*length(x)
lines(xfit, yfit, col="blue")
# Normal probability plot
qqnorm(linefit_offices_poly.stres, main = "Normal Probability Plot")
qqline(linefit_offices_poly.stres, col = "red")
# Shapiro-Wilk test
shapiro.test(linefit_offices_poly.stres)