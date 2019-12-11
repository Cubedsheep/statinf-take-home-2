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

fit = lm(V2~V1, data)
print(fit$coefficients)
print(anova(fit))
