##Texas total cases

install.packages("tidyverse")
library(readr)
library(dplyr)

texas <- texas_new %>% select(Week,Sum) %>% arrange(desc(Week), .by_group = FALSE)
texas

## Adding count column

count <- 1:362
tx <- data.frame(texas, count)
tx <- tx[-c(53:362),] ##erase NA columns
tx

##plotting 
plot(tx$count, tx$Sum, type = "b", xlab="Week", ylab="Total New Cases",xlim=c(0,50), ylim=c(0,200000)) ## TX total pop
par(new=T)
plot(tx$count, tx$Sum, type = "b", col="red", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Texas General Population Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(2,1), pch=c(1,1), col=c("black", "red"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(Sum)~count, subset = count < 5, data=tx)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(Sum)~count, data=tx)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1