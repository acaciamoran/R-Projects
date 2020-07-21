#Acacia Moran
#Lab 2

#1a Plot the series. Describe any seasonality or trend evident in the series.
#Series A
time.index = 1:500

y.A = 15 + time.index/100 + rnorm(500)

y.A = ts(y.A)

plot(y.A)

#The data seems to have a upward trend and there seems to be no
#seasonality in the data because there are no consistant spikes in or dips in the data.

#1b Plot the ACF for the series. Describe how the ACF is reflecting 
#the trend (if any) or seasonality (if any) in the series.
acf(y.A)
#here we can see that tha ACF shows that there is a strong linear trend by having all positive ACF
#values. They are all good predictions of the one before. Because the first are all large and positive
#that means that there is a trend to the data

#1c Now fit a linear trend to the series (there should be code to help out with this). Does 
#the linear trend fit well? Explain.
#fit linear trend to y.A
reg.A = lm(y.A~time.index)
plot(y.A)
lines(time.index,reg.A$fitted,col="red")
#The linear trend does fit the data well, but there is alot of error in the data so there
#might be better way to fit the data.

#1d Now plot the residuals of the linear trend (again, there should be code to help out). 
#Do the residuals, which we can view as a detrended version of our series have any trend?
plot(time.index,reg.A$resid,type="l")
#the residauls do not seem to have a trend.

#1e Plot the ACF for the “detrended” (residual) series. Describe how the ACF is reflecting 
#characteristics evident in the time series plot
acf(reg.A$resid)

#Because the data points are scattered above and below the data. The residual before the next is not
#a good predictor of the next one. Which makes sense compared to the time series above.

#2
y.B = 15 + 5*sin(time.index/100) + rnorm(500)

y.B = ts(y.B)

plot(y.B)


#2a Plot the series. Describe any seasonality or trend evident in the series.
y.B = 15 + 5*sin(time.index/100) + rnorm(500)

y.B = ts(y.B)

plot(y.B)

#there is a trend, but the trend is not linear. Also there is no seasonality.

#2b
acf(y.B)
#here we can see that tha ACF shows that there is a strong linear trend by having all positive ACF
#values. They are all good predictions of the one before. Because the first are all large and positive
#that means that there is a trend to the data.

#2c Now fit a linear trend to the series (there should be code to help out with this). Does the linear trend fit well? Explain
reg.B = lm(y.B~time.index)
plot(y.B)
lines(time.index,reg.B$fitted,col="red")
# the linear trend does not fit the data very well because of the curve.The data curves above and below the data.

#2d Now plot the residuals of the linear trend (again, there should be code to help out). Do the residuals, which we can view as
#a “detrended” version of our series have any trend? Why would removing a linear trend from this dataset not be sufficient
#for detrending the series?
plot(time.index,reg.B$resid,type="l")
# The residuals also seem to have a trend that is not linear.

#2e Plot the ACF for the “detrended” (residual) series. Describe how the ACF is reflecting characteristics evident in the time
#series plot.
acf(reg.B$resid)
##Because the data points are scattered above and below the data. The residual before the next is not
#a good predictor of the next one. Which makes sense compared to the time series above.


#3
#Series C

y.C = 15 + 5*sin(2*pi*time.index/12) + rnorm(500)

y.C = ts(y.C)

#3a Plot the series. Describe any seasonality or trend evident in the series.
plot(y.C)
#There is no trend and there is seasonality in the data.

#3b Plot the ACF for the series. Describe how the ACF is reflecting the trend 
#(if any) or seasonality (if any) in the series.
acf(y.C)
#The ACF is showing that there is seasonality in the data by seeing that there are some positive
#ACf values and than some low ACf values.

#4

#Series D

y.D = 15 + 3*time.index/100 + 5*sin(2*pi*time.index/12) + rnorm(500)

y.D = ts(y.D)

#4a Plot the series. Describe any seasonality or trend evident in the series.
plot(y.D)
#here we see that there is a positive linear trend, and there is some seasonality in the data
#set. This is because the points consistantly move up and down

#4b Plot the ACF for the series. Describe how the ACF is reflecting the trend (if any) or 
#seasonality (if any) in the series.
acf(y.D)
# here we can see that the acf is showing both the trend and the seasonality in the data.
#since the Acf has some values that are very large and positive there is a trend reflected 
# and because the Acf shows it can strongly reflect at some points and not well at others
#it reflects the seasonality. Also at a lag of 60 we can see that there is a slight decrease 
#in both the peaks and throfs.

#4c Fit a linear trend and use the residuals as a “detrended” version of the series. How does this change the ACF? Are there
#any features now evident which were not evident in the original series? Explain why.
reg.D = lm(y.D~time.index)
plot(y.D)
lines(time.index,reg.D$fitted,col="red")
plot(time.index,reg.D$resid,type="l")
acf(reg.D$resid)
#this makes the values lower, but we still see the sesonality in the graph with the peaks,but
#no longer see the linear trend. Some of the ACF values are negative and no longer reflect the 
#the trend.

#5
Spirits.url = "http://statweb.calpoly.edu/srein/stat416fall2018/Data/SPIRITS.DAT"

x = read.table(Spirits.url)

Spirits = ts(x$V1,freq=12)

plot(Spirits)

#5a ) Create a time series plot of the original series. Create the ACF for this original series. Comment on how the overall trend and
#seasonality are reflected in the ACF.
acf(Spirits, lag.max = 60)
#here we see that there is some evidence of seasonality in the data and that there is trend 
#in the data, the ACF shows large positive spikes which reflect the trend and if you extend the lag
#we see a slight decrease in the plot. Also the seasonality is reflected by the ACF above and below
#the data.

#5b Perform a seasonal difference to calculate yt-yt-12. Create the ACF for this differenced data. What does this ACF say about the
#seasonally differenced data? Be sure to comment both on seasonality and trend. Are there any other features of the ACF that
#seem noticeable?
difference2<- diff(x$V1,lag=12,type =l)
acf(difference2, lag.max = 50)
#the acf no longer shows no periodic behavior therefore no seasonality and no trend. This explaines 
#why there is no longer linear decay.
