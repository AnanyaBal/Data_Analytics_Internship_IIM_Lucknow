data.df<-read.csv(paste("dm.csv",sep=","))
View(data.df)

library(rpart)
train<-data.df[1:500,]
View(train)

library(rpart)

library(rpart.plot)

binary.fit<-rpart(Status ~ Age+Gender+AwaitingTime+Alcoolism + HiperTension + 
             Scholarship + Smokes + Sms_Reminder, data = train, method = "class",
             minsplit=50, minbucket=1, cp=0.001)
plot(binary.fit)
text(binary.fit)
rpart.plot(binary.fit)
summary(binary.fit)

pred <- predict(binary.fit, train)
pred

library(lattice)
library(ggplot2)
library(caret)
library(gmodels)

t<-table(train$Status, pred[,1]>0.5)
t

T<-CrossTable(pred)
#need confusion mtrix for fp,fn,tp,tn
#need to show model accuracy
#
pred <- as.data.frame(pred)
predicted_values <- ifelse(pred[,2] < 0.5, "No-Show", "Show-up")
predicted_values <- as.factor(predicted_values)
actual_values <- train$Status
library(gmodels)
CrossTable(actual_values, predicted_values)