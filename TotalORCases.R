##OR total cases

install.packages("tidyverse")
library(readr)
library(dplyr)

or_total <- oregon_new %>% select(Week,sum)  %>% arrange(desc(Week), .by_group = FALSE)
or_total

## Adding count column

count <- 1:361
or <- data.frame(or_total, count)
or <- or[-c(52:361),] ##erase NA columns
or

##plotting 
plot(or$count, or$sum, type = "b", xlab="Week", ylab="Total New Cases",xlim=c(0,50), ylim=c(0,12000)) ## OR total pop
par(new=T)
plot(or$count, or$sum, type = "b", col="red", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Oregon General Population Cases Over Time")
legend("topleft", legend=c("Total New Cases", "Log(y)"),lty= c(2,1), pch=c(1,1), col=c("black", "red"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, subset = count < 6, data=or)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1

