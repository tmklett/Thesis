##MI total cases

install.packages("tidyverse")
library(readr)
library(dplyr)

mi_total <- michigan_new %>% select(Week,sum) %>% arrange(desc(Week), .by_group = FALSE)
mi_total

## Adding count column

count <- 1:364
mi <- data.frame(mi_total, count)
mi <- mi[-c(53:364),] ##erase NA columns
mi

##plotting 
plot(mi$count, mi$sum, type = "b", xlab="Week", ylab="Total New Cases",xlim=c(0,51), ylim=c(0,70000)) ## MI total pop
par(new=T)
plot(mi$count, mi$sum, type = "b", col="red", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Michigan General Population Cases Over Time")
legend("topleft", legend=c("Total New Cases", "Log(y)"),lty= c(4,1), pch=c(1,1), col=c("black", "red"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(sum)~count, subset = count < 3, data=mi)
r = fit$coef["count"]
V=c(0.557, 0)
V*r+1

