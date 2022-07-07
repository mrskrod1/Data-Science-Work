library("caret")
library("magrittr")
library("dplyr")
library("tidyr")
library("lmtest")
library("popbio")
install.packages("e1071")
install.packages("caret")
install.packages("popbio")


#Run basic Logistic Model
mylogit<- glm(Gold~ Antimony, data=minerals, family="binomial")

#Make Predictions
probabilities<- predict(mylogit, type= "response")
minerals$Predicted<-ifelse(probabilities>.5, "pos", "neg")

#Recode Predicted Variables
minerals$PredictedR<- NA
minerals$PredictedR[minerals$Predicted =='pos']<-1
minerals$PredictedR[minerals$Predicted=='neg']<-0

#Convert Variables to Factors
minerals$PredictedR <- as.factor(minerals$PredictedR)
minerals$Gold <- as.factor(minerals$Gold)

#Create Confusion Matrix
conf_mat <- caret::confusionMatrix(minerals$PredictedR, minerals$Gold)
conf_mat

#Logit Linearity
minerals1<- minerals %>%
dplyr::select_if(is.numeric)

predictors <- colnames(minerals1)
minerals1<- minerals1 %>%
mutate(logit=log(probabilities/(1-probabilities))) %>%
gather(key= "predictors", value="predictor.value", -logit)

#Logit Graph
ggplot(minerals1, aes(logit, predictor.value))+
geom_point(size=.5, alpha=.5)+
geom_smooth(method= "loess")+
theme_bw()+
facet_wrap(~predictors, scales="free_y")

#Graphing for Errors
plot(mylogit$residuals)
#SCREENING FOR OUTLIERS
infl <- influence.measures(mylogit)
summary(infl)
summary(mylogit)
#Graphing Logistic Model
logi.hist.plot(minerals$Antimony,minerals$Gold, boxp=FALSE, type="hist", col="gray")
