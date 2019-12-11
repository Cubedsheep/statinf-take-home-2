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

#####################################################
#   ___                  _   _               _____  #
#  / _ \ _   _  ___  ___| |_(_) ___  _ __   |___ /  #
# | | | | | | |/ _ \/ __| __| |/ _ \| '_ \    |_ \  #
# | |_| | |_| |  __/\__ \ |_| | (_) | | | |  ___) | #
#  \__\_\\__,_|\___||___/\__|_|\___/|_| |_| |____/  #
#						    #
#####################################################

# import the data
data = read.csv("ex3.txt", sep=" ")
X1 = data$V1
X2 = data$V2

#####
# a #
#####

# fit the model without taking the size of the errors into account
fit = lm(V2~V1 + I(V1^2) + I(V1^3), data, x=TRUE)
print(fit)

# make a plot of the data and the fitted model
# initalization
tikz(file = "fit_3.tex", width=0.9*LINEWIDTH, height = 0.7*LINEWIDTH);
par(mfrow=c(1,1))

# plot the data
plot(X1, X2, xlab="V1", ylab="V2", type="p", pch=20, col="green")

coeffs = fit$coefficients
X = seq(min(X1)-0.1, max(X1)+0.1, length.out=100)
Y = coeffs[1] + coeffs[2]*X + coeffs[3]*X*X + coeffs[4]*X*X*X
lines(X, Y, col="red")


#####
# b #
#####
print("q-3b")

# calculate the weights
w = (X1 < 4/3)*(X1^2) + (X1 >=4/3 )*(X1<= 2)*4*(X1 - 2)^2

# fit the model without taking the size of the errors into account
fit = lm(V2~V1 + I(V1^2) + I(V1^3), data, x=TRUE, weights=1/w^2)
print(fit)

coeffs = fit$coefficients
X = seq(min(X1)-0.1, max(X1)+0.1, length.out=100)
Y = coeffs[1] + coeffs[2]*X + coeffs[3]*X*X + coeffs[4]*X*X*X
lines(X, Y, col="blue")
legend(0, -1, legend=c("data", "OLS fit", "WLS fit"), col=c("green", "red", "blue"), 
       lty=c(20, 1, 1), pch=20)
dev.off()






















