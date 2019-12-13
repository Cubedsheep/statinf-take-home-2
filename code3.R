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
Y = data$V2
X = cbind(rep(1, length(X1)), X1, X1^2, X1^3)

#####
# a #
#####
print("ex-3a:")
# fit the model without taking the size of the errors into account
beta = solve(t(X)%*%X) %*% t(X)%*%Y	# P. 190
print(beta)

# make a plot of the data and the fitted model
# initalization
tikz(file = "fit_3.tex", width=0.9*LINEWIDTH, height = 0.7*LINEWIDTH);
par(mfrow=c(1,1))
# plot the data
plot(X1, Y, xlab="V1", ylab="V2", pch=20)

coeffs = beta
x = seq(min(X1)-0.1, max(X1)+0.1, length.out=100)
y = coeffs[1] + coeffs[2]*x + coeffs[3]*x^2 + coeffs[4]*x^3
lines(x, y, col="blue")


#####
# b #
#####
print("ex-3b:")
# calculate the vector of weights
sigma = (X1 < 4/3)*(X1^2) + (X1>= 4/3)*4*(X1-2)^2
W = diag(sigma^-2)

beta = solve(t(X)%*%W%*%X) %*% t(X)%*%W%*%Y
print(beta)

coeffs = beta
x = seq(min(X1)-0.1, max(X1)+0.1, length.out=100)
y = coeffs[1] + coeffs[2]*x + coeffs[3]*x^2 + coeffs[4]*x^3
lines(x, y, col="red")
dev.off()


