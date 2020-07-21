#I created different time series plots for each Activity
Exercise= ts(my.data$Exercise)
plot.ts(Exercise)
Other= ts(my.data$Other)
plot.ts(Other)
Stats= ts(my.data$Stats)
plot.ts(Stats)
#I combined the differnt time series plots into one time series plot
my.data7 = cbind(Exercise,Other,Stats)
plot.ts(my.data7, main = "Time Spent on Stats, Other Classes, and Exercise", xlab = "Time (days)", ylab = "Minutes spent per day", legend=TRUE)
#Here I combined all the time series plots on to one x and y axis and made the graph look better
ts.plot(my.data7,
        gpars=list(main = "Minutes Spent on Stats, Other Classes, and Exercise", xlab="Days", ylab="Number of Minutes Spent", lty=c(1)),col= c("Exercise"="coral","Other"= "purple", "Stats"= "green"), xlim=c(1.6,42), ylim = c(11.5,300))
  #here I added a legend
   legend("topleft", title="Activities",
legend=c("Stats","Other","Exercise"), col=c("green","purple","coral"),lty=1, cex=1)
#here I found the means of each activity
mean(my.data$Stats)
mean(my.data$Other)
mean(my.data$Exercise)
