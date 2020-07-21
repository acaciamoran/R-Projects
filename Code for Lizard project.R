
library(readxl)
Lizard <- read_excel("/Users/acaciamoran/Downloads/Lizard microhabitat updated.xlsx")
model=aov(centeredtemp~microhabitat, data=Lizard)
View(Lizard)
summary(model)
TukeyHSD(model)


#here we can see that only the difference between shurb and burrow and shrub and open
#were significantly different.
library("ggpubr")
ggline(Lizard, x = "microhabitat", y = "avgtemp", 
       add = c("mean_se", "jitter"), 
       ylab = "Avg Temp in C", xlab = "Microhabitat")

#Assumptions
#Assumption 2
hist(Lizard$centeredtemp)
qqnorm(model$residuals)
qqline(model$residuals)
#We can see that there is some slight departure from the line along the tail 
#ends which might indicate that the data is not normally distributed, 
#however it fits the line pretty well.
shapiro.test(newLizard$centeredtemp)
shapiro.test(Lizard$centeredtemp)
#The Shapiro-Wilk Test tests the null hypothesis that the samples come 
#from a normal distribution vs. the alternative hypothesis that the 
#samples do not come from a normal distribution. In this case, the 
#p-value of the test is 3.292e-08, which is less than the alpha level of 0.05. 
#This suggests that the samples do not come from a normal distribution.
#However, because we have such a large sample size it is ok that this assumption
#is violated.

#Assumption 1
#Points 610, 646, and 754 are detected as outliers, which can severely affect normality 
#and homogeneity of variance. So we will try removing them from the data set.
plot(model, 1)
newLizard<- Lizard[-c(610,646, 754),]
newLizard
ggboxplot(newLizard, x = "microhabitat", y = "centeredtemp", 
          color = "microhabitat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Avg Temp in C ajusting for Ambient Temperature", xlab = "Microhabitat")
bartlett.test(centeredtemp ~ microhabitat, data=newLizard)

ggboxplot(Lizard, x = "microhabitat", y = "centeredtemp", 
          color = "microhabitat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          ylab = "Avg Temp in C ajusting for Ambient Temperature", xlab = "Microhabitat")
bartlett.test(centeredtemp ~ microhabitat, data=Lizard)
#Bartlett’s Test tests the null hypothesis that the samples have equal variances 
#vs. the alternative hypothesis that the samples do not have equal variances. 
#I ran the data with the outliers removed and without them removed and it did not
#make a difference as the assumption was still violated.
#In this case, the p-value of the test is <2.2e-16, which is less than the alpha 
#level of 0.05. This suggests that the samples do not all have equal variances.
#Since we have the sampl sample size in each microhabitat it is ok that 
#the equal variance assumption is violated.

#Assumption 3
#The observations in each group are independent of the observations in every 
#other group and that the observations within each group were obtained by a 
#random sample.
#We will assume here that the observations are independent and that one measuremet
#of temperature did not effect the other microhabitat temperatures.
#While she dd not ramdomly pick locations for placing the piece to measure the 
#temperature, she did say she walked around placing them. So we will proceed
#with caution.
morningopen <- Lizard[ which(Lizard$time2=='Morning' 
                         & Lizard$microhabitat == "open"), ]

afternoonopen <- Lizard[ which(Lizard$time2=='Afternoon' 
                             & Lizard$microhabitat == "open"), ]
eveningopen <- Lizard[ which(Lizard$time2=='Evening' 
                             & Lizard$microhabitat == "open"), ]
nightopen <- Lizard[ which(Lizard$time2=='Night' 
                             & Lizard$microhabitat == "open"), ]

morningshrub <- Lizard[ which(Lizard$time2=='Morning' 
                             & Lizard$microhabitat == "shrub"), ]
afternoonshrub <- Lizard[ which(Lizard$time2=='Afternoon' 
                               & Lizard$microhabitat == "shrub"), ]
eveningshrub <- Lizard[ which(Lizard$time2=='Evening' 
                             & Lizard$microhabitat == "shrub"), ]
nightshrub <- Lizard[ which(Lizard$time2=='Night' 
                           & Lizard$microhabitat == "shrub"), ]

morningburrow <- Lizard[ which(Lizard$time2=='Morning' 
                              & Lizard$microhabitat == "burrow"), ]
afternoonburrow <- Lizard[ which(Lizard$time2=='Afternoon' 
                                & Lizard$microhabitat == "burrow"), ]
eveningburrow <- Lizard[ which(Lizard$time2=='Evening' 
                              & Lizard$microhabitat == "burrow"), ]
nightburrow <- Lizard[ which(Lizard$time2=='Night' 
                            & Lizard$microhabitat == "burrow"), ]
mean(morningopen$centeredtemp)
mean(morningshrub$centeredtemp)
mean(morningburrow$centeredtemp)
mean(afternoonburrow$centeredtemp)
mean(afternoonopen$centeredtemp)
mean(afternoonshrub$centeredtemp)
mean(eveningburrow$centeredtemp)
mean(eveningopen$centeredtemp)
mean(eveningshrub$centeredtemp)
mean(nightburrow$centeredtemp)
mean(nightopen$centeredtemp)
mean(nightshrub$centeredtemp)
ggplot(data=morningopen, aes(x=centeredtemp), color_palette("#00AFBB"), add("mean"))+geom_histogram(bins=15, fill= "yellow")+ labs(title= "Morning Open", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=morningshrub, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "yellow")+  labs(title= "Morning Shrub", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=morningburrow, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "yellow")+  labs(title= "Morning Burrow", x= "Temperature adjusted for Ambient Temperature")

ggplot(data=afternoonopen, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "Orange")+ labs(title= "Afternoon Open", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=afternoonshrub, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "Orange")+  labs(title= "Afternoon Shrub", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=afternoonburrow, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "Orange")+  labs(title= "Afternoon Burrow", x= "Temperature adjusted for Ambient Temperature")

ggplot(data=eveningopen, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "red")+ labs(title= "Evening Open", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=eveningshrub, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "red")+  labs(title= "Evening Shrub", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=eveningburrow, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "red")+  labs(title= "Evening Burrow", x= "Temperature adjusted for Ambient Temperature")

ggplot(data=nightopen, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "purple")+ labs(title= "Night Open", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=nightshrub, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "purple")+  labs(title= "Night Shrub", x= "Temperature adjusted for Ambient Temperature")
ggplot(data=nightburrow, aes(x=centeredtemp), color_palette("#00AFBB"))+geom_histogram(bins=15, fill= "purple")+  labs(title= "Night Burrow", x= "Temperature adjusted for Ambient Temperature")

#Question 2
mean(morningopen$avgtemp)
mean(morningshrub$avgtemp)
mean(morningburrow$avgtemp)
mean(afternoonburrow$avgtemp)
mean(afternoonopen$avgtemp)
mean(afternoonshrub$avgtemp)
mean(eveningburrow$avgtemp)
mean(eveningopen$avgtemp)
mean(eveningshrub$avgtemp)
mean(nightburrow$avgtemp)
mean(nightopen$avgtemp)
mean(nightshrub$avgtemp)

ggplot(Lizard, aes(x=Lizard$'Ta/C', y=avgtemp, color=microhabitat)) +
geom_point()+labs(title="Average temp compared to Ambient Temp", x= "Ambient Temperature (C)", y= "Average Temperature(C)")+
geom_smooth(method=lm, se=FALSE, fullrange=TRUE)



