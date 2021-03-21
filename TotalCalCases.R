##California total cases

install.packages("tidyverse")
library(readr)
library(dplyr)

california_new <- california_totalss %>% select(Week,sum) %>% arrange(desc(Week), .by_group = FALSE)
california_new

## Adding week column which is named count

count <- 1:361
CA <- data.frame(california_new, count)
CA <- CA[-c(53:365),] ##erase NA columns
CA


##plotting 
plot(CA$count, CA$sum, type = "b", xlab="Week", ylab="Total New Cases",xlim=c(0,55), ylim=c(0,400000)) ## CA total pop
par(new=T)
plot(CA$count, CA$sum, type = "b", col="red", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total California General Population Cases Over Time")
legend("topleft", legend=c("Total New Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "red"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, subset = count < 4, data=CA)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, data=CA)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1