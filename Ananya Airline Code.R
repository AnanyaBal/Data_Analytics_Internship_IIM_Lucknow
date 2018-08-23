# Analysis of Airline Ticket Pricing
# NAME: Ananya Bal
# EMAIL: ananyabal3098@gmail.com
# COLLEGE: VIT Vellore

# 1 Reading and viewing the dataset after setting the working directory

airline.df<-read.csv(paste("SixAirlines.csv",sep=""))
View(airline.df)


# 2 Summarising the various fields of the dataset
summary(airline.df)


# 3 Visualizing using plots
boxplot(airline.df$PRICE_PREMIUM~airline.df$AIRLINE)
boxplot(airline.df$PRICE_ECONOMY~airline.df$AIRLINE)
boxplot(airline.df$PRICE_PREMIUM~airline.df$MONTH)
boxplot(airline.df$PRICE_ECONOMY~airline.df$MONTH)

boxplot(airline.df$PRICE_PREMIUM~airline.df$PITCH_PREMIUM)
boxplot(airline.df$PRICE_ECONOMY~airline.df$PITCH_ECONOMY)
boxplot(airline.df$PRICE_PREMIUM~airline.df$WIDTH_PREMIUM)
boxplot(airline.df$PRICE_ECONOMY~airline.df$WIDTH_ECONOMY)

boxplot(airline.df$PRICE_RELATIVE~airline.df$QUALITY, xlab="Quality",ylab="relative price")        # As quality increases, relative price increases
boxplot(airline.df$PRICE_PREMIUM~airline.df$INTERNATIONAL,xlab="International",ylab="relative price")  # Large difference between domestic and international flights


# 4 Categorising followed by bivarikate plotting
f1<-factor(airline.df$FLIGHT_DURATION)  
boxplot(airline.df$PRICE_RELATIVE~f1, ylab="Relative price", xlab="FLight duration")   #No pattern

f2<-factor(airline.df$SEATS_PREMIUM) 
boxplot(airline.df$PRICE_RELATIVE~f2,ylab="Relative price", xlab="No of premium seats" )   #No pattern

f3<-factor(airline.df$SEATS_ECONOMY)
boxplot(airline.df$PRICE_RELATIVE~f3, ylab="Relative price", xlab="No of economy seats")   #No pattern

f4<-factor(airline.df$PITCH_ECONOMY)
boxplot(airline.df$PRICE_RELATIVE~f4, ylab="Relative price", xlab="Economy pitch")  # as economy pitch increases, the relative price decreases

f5<-factor(airline.df$PITCH_PREMIUM)
boxplot(airline.df$PRICE_RELATIVE~f5, ylab="Relative price", xlab="Premium pitch")  # as premium pitch increases, the relative price increases

f6<-factor(airline.df$WIDTH_ECONOMY)
boxplot(airline.df$PRICE_RELATIVE~f6, ylab="Relative price", xlab="Economy width")  # no pattern

f7<-factor(airline.df$WIDTH_PREMIUM)
boxplot(airline.df$PRICE_RELATIVE~f7, ylab="Relative price", xlab="Premium width")  # as premium width increases,the relative price increases

library(car)
scatterplot(airline.df$PRICE_RELATIVE,airline.df$SEATS_ECONOMY)    # As seats increase, the relative price decreases


# Visualization using ggvis
library(ggvis)
ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=airline.df)
ggvis(~PRICE_ECONOMY,~PRICE_PREMIUM,fill=~AIRLINE, data=airline.df)
ggvis(~WIDTH_PREMIUM,~PRICE_PREMIUM,fill=~PRICE_RELATIVE,data=airline.df)
ggvis(~PRICE_ECONOMY,~PRICE_RELATIVE,fill=~PRICE_PREMIUM,data=airline.df)


#5 Corrgram, correlation matrix 
library(corrgram)
corrgram(airline.df, order=TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt)

cor(airline.df[,-c(1)])                     #Removing the aphabetical column


#6 and 7
#Hypothesis 1: Cost of Premium seats in Boeing aircraft is less than Airbus aircraft 
t.test(PRICE_PREMIUM~AIRCRAFT,data=airline.df) # We dont reject this hypothesis as p>0.05

#Hypothesis 2: Cost of Premium seats is more on domestic flights than international flights 
t.test(PRICE_PREMIUM~INTERNATIONAL,data=airline.df) #we reject the hypothesis as p<0.05


#8 and 9 REGRESSION MODELS
#1.Regression model to estimate the price of economy seat(Y) from predictors FLIGHT_DURATION,SEATS_ECONOMY,PITCH_ECONOMY,WIDTH_ECONOMY,AIRLINE,AIRCRAFT,QUALITY,MONTH, INTERNATIONAL
#Y=B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7+B8*X8+B9*X9
model1<-lm(PRICE_ECONOMY~FLIGHT_DURATION+SEATS_ECONOMY+PITCH_ECONOMY+WIDTH_ECONOMY+AIRLINE+AIRCRAFT+QUALITY+MONTH+INTERNATIONAL,data=airline.df)
summary(model1)
#Most significant factors are FLIGHT_DURATION, INTERNATIONAL, SEATS_ECONOMY AND AIRLINE

#How economy price is fitted into model
fitted(model1)
#coefficiants of regression model
model1$coefficients
#residuals of regression model
model1$residuals


#2.Regression model to estimate the price of PREMIUM seat(Y) from predictors FLIGHT_DURATION,SEATS_PREMIUM,PITCH_PREMIUM,INTERNATIONAL,WIDTH_PREMIUM,QUALITY,MONTH,AIRCRAFT and AIRLINE
#Y=B0+B1*X1+B2*X2+B3*X3+B4*X4+B5*X5+B6*X6+B7*X7+B8*X8+B9*X9
model2<-lm(PRICE_PREMIUM~FLIGHT_DURATION+SEATS_PREMIUM+PITCH_PREMIUM+INTERNATIONAL+WIDTH_PREMIUM+QUALITY+MONTH+AIRCRAFT+AIRLINE,data=air.df)
summary(model2)
#Most significant factors are FLIGHT_DURATION, AIRCRAFT AND AIRLINE


#How relative price is fitted into model
fitted(model2)
#coefficiants of regression model
model2$coefficients
#residuals of regression model
model2$residuals


#10
# These regression models help to estimate the price of seat(premium or economy) in any month, of any width, of any pitch, of any aircraft, of any airline, of any quality
# The factors influencing the cost of economy seats the most are FLIGHT_DURATION, INTERNATIONAL, SEATS_ECONOMY AND AIRLINE
# The factors influencing the cost of premium seats the most are FLIGHT_DURATION, AIRCRAFT AND AIRLINE



