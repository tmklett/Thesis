## total cases in Prisons

install.packages("tidyverse")
library(readr)
library(dplyr)

covid

arkansas <- covid %>% filter(abbreviation == "AR", as_of_date != "01/05/2021", as_of_date != "01/12/2021", as_of_date != "01/19/2021") %>% select(name, total_prisoner_cases, as_of_date) %>% arrange((total_prisoner_cases))
arkansas 

## adding week column and deleting 0 values
week <- 1:41
ark_week <- data.frame(arkansas, week)
ark_week <- ark_week[-c(1:3),]
ark_week

##plotting 
plot(ark_week$week, ark_week$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases") ## AR plot
par(new=T)
plot(ark_week$week, ark_week$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Arkansas Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, subset = week < 7, data=ark_week)
r = fit$coef["week"]
V=c(0.557, 0)
V*r+1

##plot FIRST SECTION

ark_week4 <- ark_week %>% filter(week > 3, week < 6)

ark_week4

plot(ark_week4$week, ark_week4$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(0,7), ylim=c(0,800))
par(new=T)
plot(ark_week4$week, ark_week4$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Arkansas Cases Weeks 4 - 5")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, data=ark_week4)
r = fit$coef["week"]
V=c(0.557, 0)
V*r+1


## plot third section, weeks 6 - 41
ark_week6 <- ark_week %>% filter(week > 5)

ark_week6

plot(ark_week6$week, ark_week6$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(5,45), ylim=c(900,11000)) 
par(new=T)
plot(ark_week6$week, ark_week6$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Arkansas Cases Weeks 6 - 41")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=ark_week6)
r = fit$coef["week"]
V=c(0.557)
V*r+1



