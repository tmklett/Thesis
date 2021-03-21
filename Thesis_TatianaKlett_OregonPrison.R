install.packages("tidyverse")
library(readr)
library(dplyr)

covid

Oregon <- covid %>% filter(abbreviation =="OR", as_of_date != "01/05/2021", as_of_date != "01/12/2021", as_of_date != "01/19/2021") %>% select(name, total_prisoner_cases, as_of_date) %>% arrange((total_prisoner_cases))
Oregon

## adding week column 
week <- 1:41
oregon_week <- data.frame(Oregon, week)
oregon_week <- oregon_week[-c(1:2),]
oregon_week

##plotting 
plot(oregon_week$week, oregon_week$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases") 
par(new=T)
plot(oregon_week$week, oregon_week$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Oregon Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, subset = week < 5, data=oregon_week)
r = fit$coef["week"]
V=c(0.557, 0)
V*r+1

##plot FIRST SECTION

or_week33 <- oregon_week %>% filter(week <16)

or_week33

plot(or_week33$week, or_week33$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(0,16), ylim=c(0,200))
par(new=T)
plot(or_week33$week, or_week33$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Oregon Cases Weeks 3 - 16")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=or_week33)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 17 - 37
or_week17 <- oregon_week %>% filter(week > 16, week < 37)

or_week17

plot(or_week17$week, or_week17$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(15,40), ylim=c(300,1500)) 
par(new=T)
plot(or_week17$week, or_week17$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Oregon Cases Weeks 17 - 36")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=or_week17)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 17 - 37
or_week41 <- oregon_week %>% filter(week > 36)

or_week41

plot(or_week41$week, or_week41$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(35,45), ylim=c(1500,2500)) 
par(new=T)
plot(or_week41$week, or_week41$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Oregon Cases Weeks 37 - 41")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")

## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=or_week41)
r = fit$coef["week"]
V=c(0.557)
V*r+1
