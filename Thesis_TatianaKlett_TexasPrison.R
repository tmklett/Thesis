install.packages("tidyverse")
library(readr)
library(dplyr)

covid

texas <- covid %>% filter(abbreviation =="TX", as_of_date != "01/05/2021", as_of_date != "01/12/2021", as_of_date != "01/19/2021") %>% select(name, total_prisoner_cases, as_of_date) %>% arrange((total_prisoner_cases))
texas

## adding week column 
week <- 1:41
texas_week <- data.frame(texas, week)
texas_week

##plotting 
plot(texas_week$week, texas_week$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases") ## tx plot
par(new=T)
plot(texas_week$week, texas_week$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Texas Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, subset = week < 5 , data=texas_week)
r = fit$coef["week"]
V=c(0.557, 0)
V*r+1


##plot FIRST SECTION

##new table-- section 1
tx_week9 <- texas_week %>% filter( week <= 9)
tx_week9

plot(tx_week9$week, tx_week9$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(1,10), ylim=c(0,2500)) 
par(new=T)
plot(tx_week9$week, tx_week9$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Texas Cases Weeks 1 - 9")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, data=tx_week9)
r = fit$coef["week"]
V=c(0.557)
V*r+1

##plot Second section, weeks 10 - 33
tx_week33 <- texas_week %>% filter( week > 9, week <= 33)

tx_week33

plot(tx_week33$week, tx_week33$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(9,35), ylim=c(4000,24000))
par(new=T)
plot(tx_week33$week, tx_week33$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Texas Cases Weeks 10 - 33")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=tx_week33)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 34 - 41
tx_week34 <- hello_week %>% filter(week > 33)

tx_week34

plot(tx_week34$week, tx_week34$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(30,45), ylim=c(15000,40000)) 
par(new=T)
plot(tx_week34$week, tx_week34$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Texas Cases Weeks 34 - 41")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=tx_week34)
r = fit$coef["week"]
V=c(0.557)
V*r+1



