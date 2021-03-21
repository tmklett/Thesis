##OH total cases

install.packages("tidyverse")
library(readr)
library(dplyr)

oh_total <- ohio_new %>% select(Week,sum) %>% arrange(desc(Week), .by_group = FALSE)
oh_total

## Adding count column

count <- 1:360
oh <- data.frame(oh_total, count)
oh <- oh[-c(52:360),] ##erase NA columns
oh

##plotting 
plot(oh$count, oh$sum, type = "b", xlab="Week", ylab="Total New Cases",xlim=c(0,50), ylim=c(0,90000)) ## OH total pop
par(new=T)
plot(oh$count, oh$sum, type = "b", col="red", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Ohio General Population Cases Over Time")
legend("topleft", legend=c("Total New Cases", "Log(y)"),lty= c(3,1), pch=c(1,1), col=c("black", "red"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, subset = count < 5, data=oh)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1
