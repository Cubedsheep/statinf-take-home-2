library(ggplot2)
library(tikzDevice)

#################################################
#   ___                  _   _               _  #
#  / _ \ _   _  ___  ___| |_(_) ___  _ __   / | #
# | | | | | | |/ _ \/ __| __| |/ _ \| '_ \  | | #
# | |_| | |_| |  __/\__ \ |_| | (_) | | | | | | #
#  \__\_\\__,_|\___||___/\__|_|\___/|_| |_| |_| #
#################################################

# read the data in ex1.txt and save it to a dataframe
data = read.csv("ex1.txt", sep=" ");

# fit a linear relation with data in column V1 as explanatory variable and 
# data in column V2 as dependent
fit = lm(V2~V1, data)
# print a summary table of the fit
print("summary of linear fit through data in ex1.txt")
print(anova(fit))

print(names(summary(fit)))
res = summary(fit)$residuals
print(res)

# qq-plot of the residuals
tikz(file = "qq-plot.tex", width = 5, height = 5)
par(mfrow=c(1, 1))
qqnorm(res)
qqline(res)

# save the plot
dev.off()
