library(jsonlite)
library(dplyr)
library(car)
#businessData<-fromJSON(file = "yelp_academic_dataset_business.json")
businessData<-stream_in(file("yelp_academic_dataset_business.json"))
businessDataFlatten<-flatten(businessData)
userData<-stream_in(file("yelp_academic_dataset_user.json"))
userDataFlatten<-flatten(userData)
reviewData<-stream_in(file("yelp_academic_dataset_review.json"))
reviewDataFlatten<-flatten(reviewData)
tipData<-stream_in(file("yelp_academic_dataset_tip.json"))
tipDataFlatten<-flatten(tipData)
checkinData<-stream_in(file("yelp_academic_dataset_checkin.json"))
checkinDataFlatten<-flatten(checkinData)
#outdoor seating
#Noise Level
#Has TV
#Good For Dancing
#Smoking
#BYOB
#Open 24 Hours
#Parking.Valet
#Music.live

reqIndexes<-sapply(businessDataFlatten$categories,function(x){is.element('Restaurants',x) | is.element('Bars',x)})
reqBusinessDataFlatten<-businessDataFlatten[reqIndexes,]
joinedData<-inner_join(reqBusinessDataFlatten,reviewDataFlatten,by='business_id')
attributesData<-select(joinedData,starts_with("attributes"),stars.y)
consideredCols<-sapply(attributesData,function(x){(sum(!is.na(x))/length(x))>0.8})
reqData<-attributesData[,consideredCols]
consideredRows<-sapply(1:nrow(reqData),function(x){ if(((x-1) %% 10000) == 0) { cat("\nDone for ",x-1)} 
return(!any(is.na(reqData[x,])))
})

consideredData<-reqData[consideredRows,]
consideredData$rating<-consideredData$stars.y
consideredData$stars.y<-NULL
consideredData$`attributes.Accepts Credit Cards`<-NULL
consideredData$`attributes.Take-out`<-NULL
consideredData$`attributes.Good For.dessert`<-NULL

fit<-lm(rating ~ .,data = consideredData)


plot(predict(fit),resid(fit),xlab="Fitted Values"
     ,ylab="Residuals",main="Residual Plot")

v<-vif(fit)

c<-coef(fit)
c<-c[names(c) != "(Intercept)"]
pc<-c[c>0]
nc<-c[c<0]
nc<-nc*-1
sortedPc<-sort(pc,decreasing = TRUE)
topPc<-sortedPc[1:10]
topPcFrame<-data.frame(attribute=names(topPc),effect=topPc)

sortedNc<-sort(nc,decreasing = TRUE)
topNc<-sortedNc[1:10]
topNcFrame<-data.frame(attribute=names(topNc),effect=topNc)




 
 



