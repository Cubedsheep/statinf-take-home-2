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


#####################################################
#   ___                  _   _               ____   #
#  / _ \ _   _  ___  ___| |_(_) ___  _ __   |___ \  #
# | | | | | | |/ _ \/ __| __| |/ _ \| '_ \    __) | #
# | |_| | |_| |  __/\__ \ |_| | (_) | | | |  / __/  #
#  \__\_\\__,_|\___||___/\__|_|\___/|_| |_| |_____| #
#      						    #
#####################################################

data = read.csv("ex2.txt", sep=" ");
X1 = data$V1
Y = data$V2
X = cbind(rep(1, length(X1)), X1)

# plot the data
tikz(file="fit-2.tex", width=0.9*LINEWIDTH, height=0.7*LINEWIDTH);
par(mfrow=c(1,1))
plot(X1, Y, xlab="V1", ylab="V2", pch=20)

#####
# a #
#####
print("q2-a:")

beta = solve(t(X)%*%X) %*% t(X)%*%Y
print(beta)
# calculate the RSS
RSS = t(Y-X%*%beta) %*% (Y-X%*%beta)
print(RSS)

# plot the fit
lines(X1, X%*%beta, col="red")

# print parameters to plot the confidence region
print("parameters for the confidence region:")
sprintf("S2: %.9f", RSS/(length(X1)-2))
print(t(X)%*%X)
sprintf("t_val: %.9f", qf(1-0.01, 2, length(X1)-2))
sprintf("Df: %d", length(X1)-2)

#####
# c #
#####
print("q2-c:")

# construct the diagonal matrix
n = length(data$V1)
m = matrix(nrow=n, ncol=n)
# fill the matrix
sigma = 25/9
rho = 0.8
for (i in 1:(n-2)) {
	diag(m[(i+1):(n),(1):(n-i)]) = rep(sigma*rho^(i), n-i)
	diag(m[(1):(n-i),(i+1):(n)]) = rep(sigma*rho^(i), n-i)
}
m[1,n] = sigma*rho^(n-1)
m[n,1] = sigma*rho^(n-1)
diag(m) = rep(sigma, n)
U = eigen(m)$vec
Lambda = diag(eigen(m)$val)
##print(eigen(m)$val)
##print(U%*%Lambda%*%t(U)-m)
A = sqrt(solve(Lambda))%*%t(U)
##print(Pi %*% m %*% P)
##print(P %*% Pi)
# this is (almost) the identity matrix
Xprime = A %*% X
Yprime = A %*% Y
##errors = A %*% rep(sqrt(sigma), n)
##errors = errors^2
##print(P)
##print(X2)
##print(Y2)
##print(errors)
i
# do the linear regression
beta = solve(t(Xprime)%*%Xprime) %*% t(Xprime)%*%Yprime
beta2 = solve(t(X)%*%solve(m)%*%X) %*% t(X)%*%solve(m)%*%Y
#print(df)
print(beta)
print(beta2)
# calculate the RSS
RSS = t(Yprime-Xprime%*%beta2) %*% (Yprime-Xprime%*%beta2)
print(RSS)

# print parameters to plot the confidence region
print("parameters for the confidence region:")
sprintf("S2: %.9f", RSS/(length(X1)-2))
print(t(Xprime)%*%Xprime)


# plot the fit
lines(X1, X%*%beta, col="blue")
dev.off()

