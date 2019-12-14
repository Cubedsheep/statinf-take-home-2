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
library(plot3D)

# the linewidth of the latex-document in Inches. Used to correctly scale the
# graphs for the report so font sizes are consistent.
LINEWIDTH = 6.02872

#####################################################
#  ___                  _   _               _  _    #
# / _ \ _   _  ___  ___| |_(_) ___  _ __   | || |   #
#| | | | | | |/ _ \/ __| __| |/ _ \| '_ \  | || |_  #
#| |_| | |_| |  __/\__ \ |_| | (_) | | | | |__   _| #
# \__\_\\__,_|\___||___/\__|_|\___/|_| |_|    |_|   #
#						    #
#####################################################

#####
# a #
#####

# the variance-covariance matrix
Sigma = matrix(c(1.25, 1.50, 0.5, 1.50, 5.25, 3.5, 0.5, 3.5, 3.0), nrow=3, ncol=3)
mu = c(1,1,-2)
# calculate the matrix A from the matrix Sigma_X
U = eigen(Sigma)$vec		# the unitary transformation from Sigma to a diagonal matrix
L = diag(eigen(Sigma)$val)	# the diagonal matrix with eigenvalues on the diagonal
A = U %*% sqrt(L)		# the matrix A

# take 3x200 samples from a standard normal distribution and transform them
samples_standard = matrix(rnorm(3*200), nrow=3, ncol=200)
samples = A %*% samples_standard + mu

tikz(file = "sample-4a-xy.tex", width=0.9*LINEWIDTH, height = 0.7*LINEWIDTH);
par(mfrow=c(2,2))
plot(samples[1,], samples[2,], pch=20, xlab="X1", ylab="X2")
plot(samples[2,], samples[3,], pch=20, xlab="X2", ylab="X3")
plot(samples[3,], samples[1,], pch=20, xlab="X3", ylab="X1")
points3D(samples[1,], samples[2,], samples[3,], pch=".", xlab="X1", ylab="X2", zlab="X3")

# save the projection on the xy-plane
dev.off()

#####
# b #
#####

# calculate the necessary matrices for the conditional distributions
# for the first conditional distribution:
Sigma11 = Sigma[1:1,1:1]
Sigma12 = Sigma[1:1,2:3]
Sigma21 = Sigma[2:3,1:1]
Sigma22 = Sigma[2:3,2:3]

x2 = c(1, -2)

muprime = mu[1] + Sigma12 %*% solve(Sigma22) %*% (x2-mu[2:3])
sprintf("mu_1:")
print(muprime)
sigmaprime = Sigma11 - Sigma12%*%solve(Sigma22)%*%Sigma21
sprintf("sigma_prime:")
print(sigmaprime)

# for the first conditional distribution
# first do the transformation to the random vetor Y:
A = cbind(c(1,0), c(0,1), c(0,1))
mu2 = A %*% mu
Sigma2 = A %*% Sigma %*% t(A)

# define the submatrices (just numbers in this case as Sigma2 is a 2x2 matrix)
# (but use series indexing to preserve the matrix structure for the following calculations)
Sigma11 = Sigma2[1:1,1:1]
Sigma12 = Sigma2[1:1,2:2]
Sigma21 = Sigma2[2:2,1:1]
Sigma22 = Sigma2[2:2,2:2]
 
x2 = c(-1)
 
muprime = mu2[1:1] + Sigma12 %*% solve(Sigma22) %*% (x2-mu2[2:2])
sprintf("mu_2:")
print(muprime)
sigmaprime = Sigma11 - Sigma12%*%solve(Sigma22)%*%Sigma21
sprintf("sigma_prime2:")
print(sigmaprime)

#####
# c #
#####
A = cbind(c(1,0), c(0,1), c(0,0))
Sigma3 = A %*% Sigma %*% t(A)
mu3 = A %*% mu
print(Sigma3)
print(mu3)

print(qchisq(0.95, df=2))

