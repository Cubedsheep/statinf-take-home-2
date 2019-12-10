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
X2 = data$V2


#####
# a #
#####
print("q1-a:")

# fit a linear relation with data in column V1 as explanatory variable and 
# data in column V2 as dependent
fit = lm(V2~V1, data, x=TRUE)
# get the matrix XtX
X = t(fit$x) %*% fit$x


# print a summary table of the fit
print("summary of linear fit through data in ex1.txt")
print(anova(fit))


# make a plot of the fit
# initalization
tikz(file = "fit_1a.tex", width=0.9*LINEWIDTH, height = 0.7*LINEWIDTH);
par(mfrow=c(1,1))

# plot the data
plot(X1, X2, xlab="V1", ylab="V2")
# plot the line fit
x = seq(min(X1)-0.5, max(X1)+0.5, length.out=20)
coeffs = fit$coefficients

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
X_inv = solve(X)	# calculate inverse of the matrix x
C = c(0, 1)		# we ar interested in the second variable (the slope)
S2 = anova(fit)$Sum[2]	# estimate for the variance of the errors
Df = anova(fit)$Df[2]	# degrees of freedom of the student-t distribution
# calculate the value to test, formula P. 200
test_val = coeffs[2]/sqrt(t(C) %*% X_inv %*% C*S2)

# do the test
clevel = pt(test_val, Df, 0)
sprintf("p-value: %f", (1-clevel))
sprintf("test value: %f", test_val)


#####
# c #
#####
print("q1-c")

# get the residuals
res = summary(fit)$residuals
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
print("q2-e")

#####################################################
#   ___                  _   _               ____   #
#  / _ \ _   _  ___  ___| |_(_) ___  _ __   |___ \  #
# | | | | | | |/ _ \/ __| __| |/ _ \| '_ \    __) | #
# | |_| | |_| |  __/\__ \ |_| | (_) | | | |  / __/  #
#  \__\_\\__,_|\___||___/\__|_|\___/|_| |_| |_____| #
# 						    #
#####################################################



#####################################################
#   ___                  _   _               _____  #
#  / _ \ _   _  ___  ___| |_(_) ___  _ __   |___ /  #
# | | | | | | |/ _ \/ __| __| |/ _ \| '_ \    |_ \  #
# | |_| | |_| |  __/\__ \ |_| | (_) | | | |  ___) | #
#  \__\_\\__,_|\___||___/\__|_|\___/|_| |_| |____/  #
#						    #
#####################################################


#####################################################
#  ___                  _   _               _  _    #
# / _ \ _   _  ___  ___| |_(_) ___  _ __   | || |   #
#| | | | | | |/ _ \/ __| __| |/ _ \| '_ \  | || |_  #
#| |_| | |_| |  __/\__ \ |_| | (_) | | | | |__   _| #
# \__\_\\__,_|\___||___/\__|_|\___/|_| |_|    |_|   #
#						    #
#####################################################



