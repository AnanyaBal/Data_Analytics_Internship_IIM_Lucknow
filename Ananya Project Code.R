# Project Title: Hotel Room Pricing In The Indian Market
# NAME: Ananya Bal
# EMAIL: ananyabal3098@gmail.com
# COLLEGE: VIT Vellore 

#1 Reading the data into a dataframe and viewing it
ch.df<-read.csv(paste("Cities42.csv",sep=""))
View(ch.df)

#2 Summarising variables
summary(ch.df)
summary(ch.df$Airport)
summary(ch.df$RoomRent)
summary(ch.df$StarRating)
summary(ch.df$HasSwimmingPool)
summary(ch.df$IsTouristDestination)

max(ch.df$RoomRent)
h.df<-subset(ch.df, RoomRent==322500)
View(h.df)

min(ch.df$RoomRent)
h.df<-subset(ch.df, RoomRent==299)
View(h.df)

#Visualisations
plot(StarRating~RoomRent, data=ch.df)        #Roomrent increases with the star rating
boxplot(RoomRent~IsNewYearEve, data=ch.df) 
boxplot(RoomRent~FreeWifi, data=ch.df, ylim=c(0,50000))
boxplot(RoomRent~FreeBreakfast, data=ch.df)
boxplot(RoomRent~IsTouristDestination, data=ch.df,ylim=c(0,50000))
boxplot(RoomRent~IsMetroCity, data=ch.df,ylim=c(0,50000))
library(car)
scatterplot(ch.df$RoomRent,ch.df$HotelCapacity,main="RoomRent of Hotels  with Hotel capacity",
            ylab = "Hotel Capacity in rooms", xlab="RoomRent in INR",cex=1.1)
plot(jitter(ch.df$RoomRent),jitter(ch.df$HasSwimmingPool),
     main="RoomRent of Hotels  with HasSwimmingPool",ylab = "Has Swimmng Pool ", xlab="RoomRent",cex=1.1)

boxplot(RoomRent~IsWeekend, data=ch.df)
plot(RoomRent~Airport, data=ch.df, ylim=c(0,50000),xlim=c(0,50))
boxplot(RoomRent~Population, data=ch.df, ylim=c(0,50000))
boxplot(RoomRent~HasSwimmingPool, data=ch.df, ylim=c(0,50000))
library(lattice)
bwplot(RoomRent~CityName, data=ch.df, ylim=c(0,50000))
bwplot(RoomRent~CityRank, data=ch.df)

library(car)
scatterplotMatrix(~RoomRent+IsTouristDestination+HasSwimmingPool+StarRating, data=ch.df)
scatterplotMatrix(~RoomRent+Airport+FreeWifi+StarRating, data=ch.df)


#Hypothesis and testing
t.test(RoomRent~FreeWifi, data=ch.df)                 # roomrent does not depend on availability of free wifi

t.test(RoomRent~FreeBreakfast, data=ch.df)            # roomrent does not depend on availability of free breakfast

t.test(RoomRent~IsTouristDestination, data=ch.df)     # roomrent depends on tourist destination
sub1.df<-subset(ch.df, IsTouristDestination==1)
mean(sub1.df$RoomRent)
sub2.df<-subset(ch.df, IsTouristDestination==0)
mean(sub2.df$RoomRent)


t.test(RoomRent~IsWeekend, data=ch.df)                # roomrent does not depend on whether the booking is for a weekend or not

t.test(RoomRent~IsMetroCity, data=ch.df)              # roomrent depends on metro status

t.test(RoomRent~HasSwimmingPool, data=ch.df)          # roomrent depends on swimming pool
sub1.df<-subset(ch.df, HasSwimmingPool==1)
mean(sub1.df$RoomRent)
sub2.df<-subset(ch.df, HasSwimmingPool==0)
mean(sub2.df$RoomRent)


mytable1<- xtabs(~RoomRent+Airport, data=ch.df)
chisq.test(mytable1)                                  #roomrent depends on airport  

mytable2 <- xtabs(~RoomRent+CityName, data=ch.df)
chisq.test(mytable2)                                  #roomrent depends on city

mytable3 <- xtabs(~RoomRent+CityRank, data=ch.df)     #roomrent depends on city rank
chisq.test(mytable3)

mytable4 <- xtabs(~RoomRent+StarRating, data=ch.df)   #roomrent depends on star rating of the hotel
chisq.test(mytable4)

#4 and 5 Finding out the independent and the dependent attributes
model1<-lm(RoomRent~Population+IsMetroCity+IsTouristDestination+
             IsWeekend+IsNewYearEve+StarRating+Airport+FreeWifi
           +FreeBreakfast+HasSwimmingPool+HotelCapacity, data=ch.df)
summary(model1)
#Multiple R-squared:  0.1906,  p-value: < 2.2e-16
#The dependent attribute (y) is the room rent 
#The the most important independent attributes are StarRating, IsTouristDestination, Hotel capacity and HasSwimmingPool

model2<-lm(RoomRent~IsMetroCity+IsTouristDestination+
             StarRating+Airport+HasSwimmingPool+HotelCapacity, data=ch.df)
summary(model2)
#Multiple R-squared:  0.1868,  p-value: < 2.2e-16
#The dependent attribute (y) is the room rent 
#All attributes are significant

model3<-lm(RoomRent~StarRating+IsTouristDestination+HasSwimmingPool-1, data=ch.df)
summary(model3)
#Multiple R-squared:  0.447,  p-value: < 2.2e-16
#The dependent attribute (y) is the room rent 
#All attributes are significant


#6 Individual visualizations of y and x1,x2,x3
boxplot(ch.df$RoomRent, ylim=c(0,20000))
mytable4<-table(ch.df$StarRating)
mytable4

mytable5<-table(ch.df$IsTouristDestination)
mytable5

mytable6<-table(ch.df$HasSwimmingPool)
mytable6

#7 Scatterplots of y and x1,x2,x3
plot(RoomRent~StarRating, ch.df)
plot(RoomRent~IsTouristDestination, ch.df)
plot(RoomRent~HasSwimmingPool, ch.df)

#8 Corrgram of y and x1,x2,x3
mt.df<-subset(ch.df, select=c(6,11,12,20))
View(mt.df)
library(corrgram)
corrgram(mt.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

#9 Variance covariance matrix of y and x1,x2,x3
cor(mt.df)
r1<-lm(RoomRent~StarRating+IsTouristDestination+HasSwimmingPool-1, data=mt.df)
summary(r1)
r1$coefficients
#Multiple R-squared:  0.447 and all factors are very significant
vcov(r1)