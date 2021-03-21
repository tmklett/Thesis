## total cases in Prisons

install.packages("tidyverse")
library(readr)
library(dplyr)

covid

michigan <- covid %>% filter(abbreviation == "MI", as_of_date != "01/05/2021", as_of_date != "01/12/2021", as_of_date != "01/19/2021") %>% select(name, total_prisoner_cases, as_of_date) %>% arrange((total_prisoner_cases))
michigan 

## adding week column 
week <- 1:41
mich_week <- data.frame(michigan, week)
mich_week

##plotting 
plot(mich_week$week, mich_week$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases") ## MI plot
par(new=T)
plot(mich_week$week, mich_week$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
axis(side=4)
mtext(side=4, line = 0, "Log y")
title("Total Michigan Cases Over Time")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))


##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, subset = week < 5 , data=mich_week)
r = fit$coef["week"]
V=c(0.557, 0)
V*r+1

##plot FIRST SECTION

##new table-- section 1
mi_week8 <- mich_week %>% filter( week <= 8)
mi_week8

plot(mi_week8$week, mi_week8$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(1,8), ylim=c(0,2500)) 
par(new=T)
plot(mi_week8$week, mi_week8$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Michigan Cases Weeks 1 - 8")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)

##First, solving for Rnaught, serial intervals = 3.9 days --> 0.557 weeks
fit = lm(log(total_prisoner_cases)~week, data=mi_week8)
r = fit$coef["week"]
V=c(0.557)
V*r+1

##plot Second section, weeks 10 - 33
mi_week36 <- mich_week %>% filter( week > 8, week < 36)

mi_week36

plot(mi_week36$week, mi_week36$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(9,40), ylim=c(3000,9763))
par(new=T)
plot(mi_week36$week, mi_week36$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Michigan Cases Weeks 9 - 35")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext("Log y", side=4, line=0)


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=mi_week36)
r = fit$coef["week"]
V=c(0.557)
V*r+1

## plot third section, weeks 34 - 41
mi_week37 <- mich_week %>% filter(week > 35)

mi_week37

plot(mi_week37$week, mi_week37$total_prisoner_cases, type = "b", xlab="Week", ylab="Total Cases", xlim=c(30,45), ylim=c(12000,22000)) 
par(new=T)
plot(mi_week37$week, mi_week37$total_prisoner_cases, type = "b", col="blue", axes = FALSE, xlab=NA, ylab=NA, log="y")
title("Michigan Cases Weeks 36 - 41")
legend("topleft", legend=c("Total Cases", "Log(y)"),lty= c(1,1), pch=c(1,1), col=c("black", "blue"))
axis(side=4)
mtext(side=4, line =0, "Log y")


## solve for R naught
fit = lm(log(total_prisoner_cases)~week, data=mi_week37)
r = fit$coef["week"]
V=c(0.557)
V*r+1



