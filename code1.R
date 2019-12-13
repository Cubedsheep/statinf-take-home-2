# R-code for take-home 2 for the course "Statistical inference and data analysis"
# author: Pieter Luyten

############################################################
#  ___       _ _   _       _ _          _   _              #
# |_ _|_ __ (_) |_(_) __ _| (_)______ _| |_(_) ___  _ __   #
#  | || '_ \| | __| |/ _` | | |_  / _` | __| |/ _ \| '_ \  #
#  | || | | | | |_| | (_| | | |/ / (_| | |_| | (_) | | | | #
# |___|_| |_|_|\__|_|\__,_|_|_/___\__,_|\__|_|\___/|_| |_| #
#							   #
############################################################

library(ggplot2)
# used to output tikz code to render graphs in latex
library(tikzDevice)

# the linewidth of the latex-document in Inches. Used to correctly scale the
# graphs for the report so font sizes are consistent.
LINEWIDTH = 6.02872


#################################################
#   ___                  _   _               _  #
#  / _ \ _   _  ___  ___| |_(_) ___  _ __   / | #
# | | | | | | |/ _ \/ __| __| |/ _ \| '_ \  | | #
# | |_| | |_| |  __/\__ \ |_| | (_) | | | | | | #
#  \__\_\\__,_|\___||___/\__|_|\___/|_| |_| |_| #
#						#
#################################################
print("Question 1:")

# read the data and save it to a dataframe
# read the data in ex1.txt and save it to a dataframe
data = read.csv("ex1.txt", sep=" ");
X1 = data$V1
Y = data$V2
X = cbind(rep(1, length(X1)), X1)

#####
# a #
#####
print("q1-a:")

# fit a linear relation with data in column V1 as explanatory variable and 
# data in column V2 as dependent
# get the matrix XtX
beta = solve(t(X)%*%X) %*% t(X)%*%Y	# P. 190

# make a plot of the fit
# initalization
tikz(file = "fit_1a.tex", width=0.9*LINEWIDTH, height = 0.7*LINEWIDTH);
par(mfrow=c(1,1))

# plot the data
plot(X1, Y, xlab="V1", ylab="V2")
# plot the line fit
x = seq(min(X1)-0.5, max(X1)+0.5, length.out=20)
coeffs = beta

# print the fitted coefficients
print("coefficienten fit:")
print(coeffs)
y = coeffs[1] + coeffs[2]*x
lines(x, y)
dev.off()


####
# b #
#####
print("q1-b")
Df = length(X1) - 2		# degrees of freedom of the data
X_inv = solve(t(X)%*%X)		# calculate inverse of the matrix x
C = c(0, 1)			# we are interested in the second variable (the slope)
RSS = t(Y-X%*%beta) %*% (Y-X%*%beta)	# calculate the sum of squares of the residuals
S2 = RSS/(Df)			# estimate for the variance of the errors

# calculate the value to test, formula P. 200
test_val = coeffs[2]/sqrt(t(C) %*% X_inv %*% C*S2)
# calculate the 99% confidence region
alpha = 0.01
x_max = qt(1-alpha/2, Df, 0)

# do the test
clevel = pt(test_val, Df, 0)	# chance of x being smaller than test_val. 
sprintf("p-value: %f", (1-clevel)*2)	# (1-clevel)*2 is the chance of |x| > test_val
sprintf("test value: %f", test_val)
sprintf("alpha=%f confidence region: [%f, %f]", alpha*100, -x_max, x_max)


#####
# c #
#####
print("q1-c")

# get the residuals
res = Y - X%*%beta
# qq-plot of the residuals
tikz(file = "qq-plot.tex", width = 0.9*LINEWIDTH, height = 0.7*LINEWIDTH)
par(mfrow=c(1, 1))
qqnorm(res)
qqline(res)

# save the plot
dev.off()

#####
# d #
#####
print("q1-d")

#####
# e #
#####
print("q1-e")

# test the assumption of normality of the errors with the shapiro-wilk test
print(shapiro.test(res))
# p-value of 0.14, we accept normality.
# the confidence level and corresponding t-value
alpha = 0.01
t_val = qf(1-alpha, 2, Df)
S2 = RSS

sprintf("test value: %.10f", t_val)
print("S^2: %.9f", S2)
print("matrix X^TX: ")
print(t(X)%*%X)
##print(eigen(t(X)%*%X))
