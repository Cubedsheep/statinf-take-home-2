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
par(mfrow=c(1,1))
plot(samples[1,], samples[2,])
# save the projection on the xy-plane
dev.off()
