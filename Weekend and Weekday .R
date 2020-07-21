#here I read in the data
my.data = read.csv("/Users/williamsteier/Desktop/Minutes.csv")
my.data
#here I singled out the Exercise data and day of the month
my.data2 = subset(my.data, select = c(X,Exercise))
#here I seperated out days of the weekday and weekend days
weekday.data1 <- my.data2[1:5,]
weekday.data2 <- my.data2[8:12,]
weekday.data3 <- my.data2[15:19,]
weekday.data4 <- my.data2[22:26,]
weekday.data5 <- my.data2[29:33,]
weekday.data6 <- my.data2[36:40,]
weekend.data1 <- my.data2[6:7,]
weekend.data2 <- my.data2[13:14,]
weekend.data3 <- my.data2[20:21,]
weekend.data4 <- my.data2[27:28,]
weekend.data5 <- my.data2[34:35,]
weekend.data6 <- my.data2[41:42,]
#here I combined all the weekday data into one data set
weekday = rbind(weekday.data1,weekday.data2,weekday.data3, weekday.data4,weekday.data5,weekday.data6)
#here I combined all the weekedn data into one data set
weekend=rbind(weekend.data1<my.data2[6:7,]
,weekend.data2,weekend.data3
,weekend.data4
,weekend.data5,
weekend.data6)
#here I made a boxplot of the weekend vs weekday data
boxplot(weekend$Exercise,weekday$Exercise, main= "Exercise Time Between Weekdays and Weekends", xlab = "Day of the Week", ylab = "Exercise in minutes", col= c("weekday"="coral","weekend"="darkblue"),legend=TRUE)
#here I labeled the axis with weekday and weekend
axis(1,1:2,c("Weekend","Weekday"))
#here I found the summary statistics of the weekend and weekday data
summary(weekday$Exercise)
summary(weekend$Exercise)
sd(weekday$Exercise)
sd(weekend$Exercise)