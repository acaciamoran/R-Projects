#Lab 2 Bus 450
library(tidyverse)
install.packages("ggthemes")
library(ggthemes)
install.packages("ggLinearModel")
library(ggLinearModel)
install.packages("ISLR")
library(ISLR)
#1
collegeclean <- read.csv("/Users/acaciamoran/Downloads/colleges_clean.csv")
#2
head(collegeclean$ADM_RATE)
summary(collegeclean$ADM_RATE)

collegeclean %>%
  ggplot(aes(y = ADM_RATE)) +
  geom_boxplot() +
  ggtitle("Admission Rate in the U.S") +
  ylab("Admission Rate (%)")

#3
head(collegeclean)
#Admission Rate vs. TUITION_DIFF
lm(ADM_RATE~TUITION_DIFF, collegeclean)
collegeclean %>%
  ggplot(aes(y = ADM_RATE, x = TUITION_DIFF)) +
  geom_point() +
  ggtitle("Admission Rate vs. Differece in Tuition") +
  ylab("Admission Rate (%)")+
  xlab("Tuition difference ($)")
#Admission Rate vs. Tuition in State
lm(ADM_RATE~TUITIONFEE_IN, collegeclean)
collegeclean %>%
  ggplot(aes(y = ADM_RATE, x = TUITIONFEE_IN)) +
  geom_point() +
  ggtitle("Admission Rate vs. Tuition in State") +
  ylab("Admission Rate (%)")+
  xlab("Tuition in state ($)")
#Admission Rate vs. Tuition out of State
lm(ADM_RATE~TUITIONFEE_OUT, collegeclean)
collegeclean %>%
  ggplot(aes(y = ADM_RATE, x = TUITIONFEE_OUT)) +
  geom_point() +
  ggtitle("Admission Rate vs. Tuition out of State") +
  ylab("Admission Rate (%)")+
  xlab("Tuition out of state ($)")
#Admission Rate vs. SAT score
lm(ADM_RATE~SAT_AVG, collegeclean)
collegeclean %>%
  ggplot(aes(y = ADM_RATE, x = SAT_AVG)) +
  geom_point() +
  ggtitle("Admission Rate vs. SAT score") +
  ylab("Admission Rate (%)")+
  xlab("SAT Score")

#Admission Rate vs. Number of Undergraduate's
lm(ADM_RATE~UGDS, collegeclean)
collegeclean %>%
  ggplot(aes(x = ADM_RATE, y = UGDS)) +
  geom_point() +
  ggtitle("Admission Rate vs. Number of Undergraduate's") +
  xlab("Admission Rate (%)")+
  ylab("Number of Undergraduates (students)")

#4
newcollegeclean <- mutate(collegeclean, LOCATION=ifelse(REGION %in% 1:2, "Northeast",
                                     ifelse(REGION %in% 3:4, "Midwest", 
                                     ifelse(REGION %in% 5:7, "South", "West"))))
#5
#Here we are looking at them numerically
West <-subset(newcollegeclean, LOCATION == "West")
summary(West$ADM_RATE)
South <-subset(newcollegeclean, LOCATION == "South")
summary(South$ADM_RATE)
Midwest <-subset(newcollegeclean, LOCATION == "Midwest")
summary(Midwest$ADM_RATE)
Northeast <-subset(newcollegeclean, LOCATION == "Northeast")
summary(Northeast$ADM_RATE)

#here we are looking at them as a graph
newcollegeclean %>%
  ggplot(aes(y = ADM_RATE, x = LOCATION)) +
  geom_boxplot() +
  ggtitle("Admission Rate vs. LOCATION") +
  ylab("Admission Rate (%)")

#6
#Admission Rate vs. TUITION_DIFF
a<- lm(ADM_RATE~TUITION_DIFF, collegeclean)
summary(a)
#this is significant
#Admission Rate vs. Tuition in State
b<- lm(ADM_RATE~TUITIONFEE_IN, collegeclean)
summary(b)
#this is significant
#Admission Rate vs. Tuition out of State
c<- lm(ADM_RATE~TUITIONFEE_OUT, collegeclean)
summary(c)
#this is significant
#Admission Rate vs. SAT score
d<- lm(ADM_RATE~SAT_AVG, collegeclean)
summary(d)
collegeclean %>%
  ggplot(aes(y = ADM_RATE, x = SAT_AVG)) +
  geom_point() +
  ggtitle("Admission Rate vs. SAT score") +
  ylab("Admission Rate (%)")+
  xlab("SAT Score")
#this is significant
#here I think the best one is SAT_AVG because it has the highest R squared.Also it
#seems to have a linear relationship with Admission rate.

#7
newcollegeclean %>%
  ggplot(aes(y = TUITIONFEE_IN, x = LOCATION)) +
  geom_boxplot() +
  ggtitle("In-State Tuition Cost vs. LOCATION") +
  xlab("Admission Rate (%)")+
  ylab("Instate Tuition ($)")
#Here the visualization looks pretty good. We can see a difference in the
#median in state tuition cost between the different locations.
