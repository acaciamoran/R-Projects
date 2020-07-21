# 417 Project 
library(survival)
library(eha)
library(tidyverse)

# n is number of samples, pct is percent censored
changenpct <- function(n, pct){
  numcen <- ceiling(pct*n)
  expvar <- c(disttype, n, pct)
  
  # all at the beginning 
  cenbeg <- c(rep(0, numcen), rep(1, n-numcen))
  # all at end
  cenend <- c(rep(1, n-numcen), rep(0, numcen))
  # random throughout 
  cenrand <- sample (c(rep(0, numcen), rep(1, n-numcen)), size = n, replace = F)
  
  # Censored Data
  cdata <- as.data.frame(cbind(values, cenbeg, cenend, cenrand))
  
  changecen <- function(censor){
    KM.obj <- survfit(Surv(values, censor)~1, conf.type = "log-log")
    # Median
    getmedian <- capture.output(KM.obj)[4]
    getmedian <- unlist(str_split(str_squish(getmedian)," "))
    medlower <- getmedian[4]
    medupper <- getmedian[5]
    median <- getmedian[3]
    # Mean  
    getmean <- capture.output(print(KM.obj, print.rmean=TRUE))[4]
    getmean <- unlist(str_split(str_squish(getmean), " "))
    mean <- as.numeric(getmean[3])
    sd <- as.numeric(getmean[4])                
    meanlower <- mean - 1.96*sd
    meanupper <- mean + 1.96*sd
    # Combine values to get output 
    output <- c(median, medlower, medupper, mean, meanlower, meanupper)
    return(output)
  }
  
  # CIs for beginning
  beg <- changecen(cenbeg)
  beg <- c(expvar, "Beginning", beg)
  # CIs for end
  end <- changecen(cenend)
  end <- c(expvar, "End", end)
  # CIs for random
  rand <- changecen(cenrand)
  rand <- c(expvar, "Random", rand)
  
  # Output 3 rows, one for each censoring type
  output <- rbind(beg, end, rand)
  return(output)
}

n <- c(50, 100, 200, 1000)
pct <- c(0, .1, .25, .5, .85, 1)

# Exponential 
disttype <- "Exponential"
lambda = 10

expgroup <- as.data.frame(matrix(seq(1:10), nrow = 1))
for (i in seq_along(n)){
  values <- sort(rexp(n[i], rate = lambda))
  for (j in seq_along(pct)){
    expgroup <- rbind(expgroup, changenpct(n[i],pct[j]))
  }
}
expgroup <- expgroup[-1,]
names(expgroup) <- c("Distribution","Sample Size","% Censored","Censor Location",
                     "Median","Med Lower","Med Upper","Mean","Mean Lower","Mean Upper")

# Gompertz 
disttype <- "Gompertz"
theta = .01
alpha= .25

gomgroup <- as.data.frame(matrix(seq(1:10), nrow = 1))
for (i in seq_along(n)){
  values <- sort(rgompertz(n[i], alpha, theta))
  for (j in seq_along(pct)){
    gomgroup <- rbind(gomgroup, changenpct(n[i],pct[j]))
  }
}
gomgroup <- gomgroup[-1,]
names(gomgroup) <- c("Distribution","Sample Size","% Censored","Censor Location",
                     "Median","Med Lower","Med Upper","Mean","Mean Lower","Mean Upper")

# Weibual 
disttype <- "Weibual"
beta= 1.6
lambda= 8

wgroup <- as.data.frame(matrix(seq(1:10), nrow = 1))
for (i in seq_along(n)){
  values <- sort(rweibull(n[i],beta, lambda))
  for (j in seq_along(pct)){
    wgroup <- rbind(wgroup, changenpct(n[i],pct[j]))
  }
}
wgroup <- wgroup[-1,]
names(wgroup) <- c("Distribution","Sample Size","% Censored","Censor Location",
                   "Median","Med Lower","Med Upper","Mean","Mean Lower","Mean Upper")

data <- rbind(expgroup, gomgroup, wgroup)

head(data)