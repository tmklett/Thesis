install.packages("tidyverse")
library(readr)
library(dplyr)

covid

Ohio <- covid %>% filter(abbreviation =="OH", as_of_date != "01/05/2021", as_of_date != "01/12/2021", as_of_date != "01/19/2021") %>% select(name, total_prisoner_cases, as_of_date) %>% arrange((total_prisoner_cases))
Ohio

## adding week column 
week <- 1:43
ohio_week <- data.frame(Ohio, week)
ohio_week <- ohio_week[-c(1:2),]
ohio_week

##plotting 
plot(ohio_week$week, ohio_week$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases") 
par(new=T)
plot(ohio_week$week, ohio_week$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Ohio Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, subset = week < 5, data=ohio_week)
r = fit$coef["week"]
V=c(0.557, 0)
V*r+1

##plot FIRST SECTION

oh_week5 <- ohio_week %>% filter(week > 3, week < 6)

oh_week5

plot(oh_week5$week, oh_week5$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(0,10), ylim=c(0,4000))
par(new=T)
plot(oh_week5$week, oh_week5$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Ohio Cases Weeks 4 - 5")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=oh_week5)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 6 +
oh_week6 <- ohio_week %>% filter(week > 5)

oh_week6

plot(oh_week6$week, oh_week6$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(5,45), ylim=c(3000,9500)) 
par(new=T)
plot(oh_week6$week, oh_week6$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Ohio Cases Weeks 6 - 43")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=oh_week6)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 17 - 37
or_week41 <- oregon_week %>% filter(week > 37)

or_week41

plot(or_week41$week, or_week41$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(35,45), ylim=c(1500,2500)) 
par(new=T)
plot(or_week41$week, or_week41$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Oregon Cases Weeks 38 - 41")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")

## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=or_week41)
r = fit$coef["week"]
V=c(0.557)
V*r+1


