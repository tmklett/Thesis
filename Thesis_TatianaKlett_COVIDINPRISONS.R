install.packages("tidyverse")
library(readr)
library(dplyr)

covid

hello <- covid %>% filter(abbreviation =="CA", as_of_date != "01/05/2021", as_of_date != "01/12/2021", as_of_date != "01/19/2021") %>% select(name, total_prisoner_cases, as_of_date) %>% arrange((total_prisoner_cases))

## adding week column 
week <- 1:41
hello_week <- data.frame(hello, week)
hello_week


##plot entire pandemic
plot(hello_week$week, hello_week$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(1,41), ylim=c(0,40000)) ## california plot
par(new=T)
plot(hello_week$week, hello_week$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total California Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks for weeks <4
hello4 <- hello_week %>% filter( week < 4)

hello4

fit = lm(log(total_prisoner_cases)~week, data=hello4)
r = fit$coef["week"]
V=c(0.557)
V*r+1

##plot FIRST SECTION

##new table-- section 1
hello1 <- hello_week %>% filter( week <= 10)

hello1

plot(hello1$week, hello1$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(1,10), ylim=c(0,1500)) ## california plot
par(new=T)
plot(hello1$week, hello1$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("California Cases Weeks 1 - 10")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, data=hello1)
r = fit$coef["week"]
V=c(0.557)
V*r+1

##plot Second section, weeks 11 - 34

Weeks_11 <- hello_week %>% filter( week > 10, week <= 34)

Weeks_11

plot(Weeks_11$week, Weeks_11$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(10,40), ylim=c(1500,20000)) ## california plot
par(new=T)
plot(Weeks_11$week, Weeks_11$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("California Cases Weeks 11 - 34")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=Weeks_11)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 35 - 41
Weeks_35 <- hello_week %>% filter(week > 34)

Weeks_35

plot(Weeks_35$week, Weeks_35$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(30,45), ylim=c(15000,40000)) ## california plot
par(new=T)
plot(Weeks_35$week, Weeks_35$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("California Cases Weeks 35 - 41")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=Weeks_35)
r = fit$coef["week"]
V=c(0.557)
V*r+1


