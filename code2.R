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

#####
# a #
#####

fit = lm(V2~V1, data)
print(fit$coefficients)
print(anova(fit))

#####
# c #
#####

# construct the diagonal matrix
n = length(data$V1)
m = matrix(nrow=n, ncol=n)
# fill the matrix
sigma = 25/9
rho = 0.8
for (i in 1:(n-2)) {
	diag(m[(i+1):(n),(1):(n-i)]) = rep(sigma*rho^(i-1), n-i)
	diag(m[(1):(n-i),(i+1):(n)]) = rep(sigma*rho^(i-1), n-i)
}
m[1,n] = sigma*rho^(n-1)
m[n,1] = sigma*rho^(n-1)
diag(m) = rep(sigma, n)
P = eigen(m)$vec
Pi = t(P)
#print(P %*% Pi)
# this is (almost) the identity matrix
X2 = P %*% data$V1
Y2 = P %*% data$V2
errors = P %*% rep(sqrt(sigma), n)
errors = errors^2
#print(P)
#print(X2)
#print(Y2)
#print(errors)

# do the linear regression
df = data.frame("V1"=X2, "V2"=Y2)
#print(df)
fit = lm(V2 ~ V1, df)
print(fit$coefficients)
print(anova(fit))
