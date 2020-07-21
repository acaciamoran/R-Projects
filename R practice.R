#Lab 1
#1. No this data set is not tidy, we need to use the gather fucntion to change how the data is organized. 
#2. Some variables that I would expect to see are year, because they are tracking the data over the years.
population <- read_csv(file.choose())
income <- read_csv(file.choose())
life <- read_csv(file.choose())
#these gather the data so that its tidy
populationtidy <- population %>%
  gather(key = "Year", value = "population", -country)
incometidy <- income %>%
  gather(key = "Year", value = "income", -country)
lifetidy <- life %>%
  gather(key = "Year", value = "life", -country)
#this prints out the first few and last of each dataset
head(populationtidy)
head(incometidy)
head(lifetidy)
tail(populationtidy)
tail(incometidy)
tail(lifetidy)
#this mergeres the three data sets
merger1 <- merge(populationtidy, incometidy, by=c("Year", "country"))
merger2 <- merge(merger1, lifetidy, by=c("Year", "country"))

#this seperates out the year 2010
#this works too year2010 <- merger2[which( merger2$Year == 2010),]
year2010 <- filter(merger2, Year== 2010)
# step 4A
#This gets the lowest, highest, furthest to the right, and left
lowest <-arrange(year2010, life)
highest <- arrange(year2010, desc(life))
furthestleft <- arrange(year2010, income)
furthestright <- arrange(year2010, desc(income))
highest[1,]
lowest[1,]
furthestright[1,]
furthestleft[1,]

#step 4B
bottom10 <- arrange(year2010, income, life)
head(bottom10, n=10)
top10 <- arrange(year2010, desc(income), desc(life))
head(top10, n=10)

#step 5
ggplot(year2010, aes(x = income, y = life, size = population)) +
  geom_point()+
  scale_x_log10()

#task 2
energy <- read_csv(file.choose())
exports <- read_csv(file.choose())
imports <- read_csv(file.choose())
#these gather the data so that its tidy
energytidy <- energy %>%
  gather(key = "Year", value = "energy", -country)
exportstidy <- exports %>%
  gather(key = "Year", value = "exports", -country)
importstidy <- imports %>%
  gather(key = "Year", value = "imports", -country)

merger3 <- merge(energytidy, exportstidy, by=c("Year", "country"))
merger4 <- merge(merger3, importstidy, by=c("Year", "country"))

#this seperates out the year 1997
year1997 <- filter(merger4, Year== 1997)
# step 4A
#This gets the lowest, highest, furthest to the right, and left
lowest1997 <-arrange(year1997, imports)
highest1997 <- arrange(year1997, desc(imports))
furthestleft1997 <- arrange(year1997, exports)
furthestright1997 <- arrange(year1997, desc(exports))
highest1997[1,]
lowest1997[1,]
furthestrigh1997t[1,]
furthestleft1997[1,]
#step 4B
bottom101997 <- arrange(year1997, exports, imports)
head(bottom101997, n=10)
top101997 <- arrange(year1997, desc(exports), desc(imports))
head(top101997, n=10)

#step 5
ggplot(year1997, aes(x = exports, y = imports, size = energy)) +
  geom_point()+
  scale_x_log10()

#task 3
#this seperates out the year 2001
income <- read_csv(file.choose())
internet <- read_csv(file.choose())
GDP <- read_csv(file.choose())
incometidy2 <- income %>%
  gather(key = "Year", value = "income", -country)
internettidy <- internet %>%
  gather(key = "Year", value = "internet", -country)
GDPtidy <- GDP %>%
  gather(key = "Year", value = "GDP", -country)

merger5 <- merge(incometidy, internettidy, by=c("Year", "country"))
merger6 <- merge(merger5, GDPtidy, by=c("Year", "country"))


year2001 <- filter(merger6, Year== 2001)
# step 4A
#This gets the lowest, highest, furthest to the right, and left
lowest2001 <-arrange(year2001, GDP)
highest2001 <- arrange(year2001, desc(GDP))
furthestleft2001 <- arrange(year2001, internet)
furthestrigh2001t <- arrange(year2001, desc(internet))
highest2001[1,]
lowest2001[1,]
furthestright2001[1,]
furthestleft2001[1,]

#step 4B
bottom102001 <- arrange(year2001, internet, GDP)
head(bottom102001, n=10)
top102001 <- arrange(year2001, desc(internet), desc(GDP))
head(top102001, n=10)

#step 5
ggplot(year2001, aes(x = internet, y = GDP, size = income)) +
  geom_point()+
  scale_x_log10()

