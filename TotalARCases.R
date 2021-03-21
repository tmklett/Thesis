##AR total cases

install.packages("tidyverse")
library(readr)
library(dplyr)

ar_total <- arkansas_new %>% select(Week,sum) %>% arrange(desc(Week), .by_group = FALSE)
ar_total

## Adding count column

count <- 1:359
ar <- data.frame(ar_total, count)
ar <- ar[-c(51:359),] ##erase NA columns
ar

##plotting 
plot(ar$count, ar$sum, type = "b", xlab="Week", ylab="Total New Cases",xlim=c(0,50), ylim=c(0,30000)) ## AR total pop
par(new=T)
plot(ar$count, ar$sum, type = "b", col="red", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Arkansas General Population Cases Over Time")
legend("topleft", legend=c("Total New Cases", "Log(y)"),lty= c(3,1), pch=c(1,1), col=c("black", "red"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, subset = count < 3, data=ar)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1


##special
ar_special <- ar %>% filter(count != 1, count != 2, count > 3, count < 6)

ar_special

plot(ar_special$count, ar_special$sum, type = "b", xlab="Week", ylab="Total Cases", xlim=c(0,7), ylim=c(0,9000))
par(new=T)
plot(ar_special$count, ar_special$sum, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Arkansas Gen Pop Cases Weeks 4 - 5")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, data=ar_special)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1

