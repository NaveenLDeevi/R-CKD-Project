getwd()
setwd("D:\\Study Related (VIMP)\\Naveen MSBA\\HealthCare Analytics\\Assiginment_3\\Transformed Data")
train_x = read.csv("Data_set_transformation_1.csv",header = TRUE,na.strings = c(""))
summary(train_x)
train_x$Educ[which(is.na(train_x$Educ))] <- 1
train_x$Unmarried[which(is.na(train_x$Unmarried))]<-0
train_x$Income[which(is.na(train_x$Income))]<-0
train_x$Insured[which(is.na(train_x$Insured))] <- 1
train_x$Weight[which(is.na(train_x$Weight))]<-mean(train_x$Weight, na.rm = 1)
train_x$Height[which(is.na(train_x$Height))]<-mean(train_x$Height, na.rm = 1)
train_x$BMI[which(is.na(train_x$BMI))]<- mean(train_x$BMI, na.rm=1)
train_x$Waist[which(is.na(train_x$Waist))]<-mean(train_x$Waist, na.rm = 1)
train_x$SBP[which(is.na(train_x$SBP))]<-mean(train_x$SBP, na.rm = 1)
train_x$DBP[which(is.na(train_x$DBP))]<-mean(train_x$DBP, na.rm = 1)
train_x$HDL[which(is.na(train_x$HDL))]<-mean(train_x$HDL, na.rm = 1)
train_x$LDL[which(is.na(train_x$LDL))]<-mean(train_x$LDL, na.rm = 1)
train_x$LDL[which(is.na(train_x$LDL))]<-mean(train_x$LDL, na.rm = 1)
train_x$Total.Chol[which(is.na(train_x$Total.Chol))]<-mean(train_x$Total.Chol, na.rm = 1)
train_x$Activity[which(is.na(train_x$Activity))] <- 1
train_x$PoorVision[which(is.na(train_x$PoorVision))] <- 0
train_x$Hypertension[which(is.na(train_x$Hypertension))] <- 0
train_x$Stroke[which(is.na(train_x$Stroke))] <- 0
train_x$CVD[which(is.na(train_x$CVD))] <- 0
train_x$Fam.CVD[which(is.na(train_x$Fam.CVD))] <- 0
train_x$CHF[which(is.na(train_x$CHF))] <- 0
train_x$Diabetes[which(is.na(train_x$Diabetes))] <- 0
train_x$Anemia[which(is.na(train_x$Anemia))] <- 0
train_x$Obese[which(is.na(train_x$Obese))] <- 0
summary(train_x)
############################################################################################
train_x$Female <- factor(train_x$Female)
train_x$White <- factor(train_x$White)
train_x$Black<- factor(train_x$Black)
train_x$Hispanic<- factor(train_x$Hispanic)
train_x$Other<- factor(train_x$Other)
train_x$Educ <- factor(train_x$Educ)
train_x$Unmarried <- factor(train_x$Unmarried)
train_x$Income <- factor(train_x$Income)
train_x$Insured <- factor(train_x$Insured)
train_x$Obese <- factor(train_x$Obese)
train_x$Dyslipidemia <- factor(train_x$Dyslipidemia)
train_x$PVD <- factor(train_x$PVD)
train_x$PoorVision <- factor(train_x$PoorVision)
train_x$Smoker <- factor(train_x$Smoker)
train_x$Hypertension <- factor(train_x$Hypertension)
train_x$Fam.Hypertension <- factor(train_x$Fam.Hypertension)
train_x$Diabetes <- factor(train_x$Diabetes)
train_x$Fam.Diabetes <- factor(train_x$Fam.Diabetes)
train_x$Stroke <- factor(train_x$Stroke)
train_x$CVD <- factor(train_x$CVD)
train_x$Fam.CVD <- factor(train_x$Fam.CVD)
train_x$CHF <- factor(train_x$CHF)
train_x$Anemia <- factor(train_x$Anemia)
train_x$Activity <- factor(train_x$Activity)
train_x$CKD <- factor(train_x$CKD)

###################Descriptive Statistics############################################
plot(train_x$CKD, train_x$Age, main = "Age vs CKD", xlab = "CKD", ylab = "Age", col = c("light green","darksalmon"))

Sex.counts <- table(train_x$CKD,train_x$Female)
barplot(Sex.counts, main = "CKD effects on gender", xlab = "Gender\nGreen:Non-CKD, Red:CKD", ylab = "Frequency", col = c("light green","darksalmon"))

race.count <- table(train_x$CKD, train_x$Race_White)
barplot(race.count, main = "Effect of CKD\n on Race", xlab = "Race(1-white,0-others)", ylab = "Frequency", col = c("light green","darksalmon"))

Educ.count <- table(train_x$CKD, train_x$Educ)
barplot(Educ.count, main = "CKD vs Education", xlab = "Education", ylab = "Frequency", col = c("light green","darksalmon") )

Unmarried.count <- table(train_x$CKD, train_x$Unmarried)
barplot(Unmarried.count, main = "CKD vs Unmarried", xlab = "Unmarried", ylab = "Frequency", col = c("light green","darksalmon") )

Income.count <- table(train_x$CKD, train_x$Income)
barplot(Income.count, main = "CKD vs Income", xlab = "Income", ylab = "Frequency", col = c("light green","darksalmon") )

CareSource.count <- table(train_x$CKD, train_x$CareSource)
barplot(CareSource.count, main = "CKD vs Caresource", xlab = "Care Source", ylab = "Frequnecy", col = c("light green","darksalmon") )

Insured.count <- table(train_x$CKD, train_x$Insured)
barplot(Insured.count, main = "CKD vs Caresource",xlab = "Insured", ylab = "Frequency", col = c("light green","darksalmon") )

plot(train_x$CKD, train_x$Height, main = "Height vs CKD", xlab = "CKD", ylab = "Height", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$Weight, main = "Weight vs CKD", xlab = "CKD", ylab = "Weight", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$BMI, main = "BMI vs CKD", xlab = "CKD", ylab = "BMI", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$Waist, main = "Waist vs CKD", xlab = "CKD", ylab = "Waist", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$SBP, main = "SBP vs CKD", xlab = "CKD", ylab = "SBP", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$DBP, main = "DBP vs CKD", xlab = "CKD", ylab = "DBP", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$HDL, main = "HDL vs CKD", xlab = "CKD", ylab = "HDL", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$LDL, main = "LDL vs CKD", xlab = "CKD", ylab = "LDL", col = c("light green","darksalmon"))
plot(train_x$CKD, train_x$Total.Chol, main = "Total.cholestrol vs CKD", xlab = "CKD", ylab = "Total.chol", col = c("light green","darksalmon"))

Dyslpm.count <- table(train_x$CKD, train_x$Dyslipidemia)
barplot(Dyslpm.count, main = "CKD vs Dyslipidemia",xlab = "Dyslipidemia", ylab = "Frequency", col = c("light green","darksalmon") )

PVD.count <- table(train_x$CKD, train_x$PVD)
barplot(PVD.count, main = "CKD vs PVD",xlab = "PVD", ylab = "Frequency", col = c("light green","darksalmon") )

Activity.count <- table(train_x$CKD, train_x$Activity)
barplot(Activity.count, main = "CKD vs Activity",xlab = "Activity", ylab = "Frequency", col = c("light green","darksalmon") )

PoorVision.count <- table(train_x$CKD, train_x$PoorVision)
barplot(PoorVision.count, main = "CKD vs PoorVision",xlab = "PoorVision", ylab = "Frequency", col = c("light green","darksalmon") )

Smoker.count <- table(train_x$CKD, train_x$Smoker)
barplot(Smoker.count, main = "CKD vs Smoker",xlab = "Smoker", ylab = "Frequency", col = c("light green","darksalmon") )

Hypertension.count <- table(train_x$CKD, train_x$Hypertension)
barplot(Hypertension.count, main = "CKD vs Hypertension",xlab = "Hypertension", ylab = "Frequency", col = c("light green","darksalmon") )

Fam.Hypertension.count <- table(train_x$CKD, train_x$Fam.Hypertension)
barplot(Fam.Hypertension.count, main = "CKD vs Fam.Hypertension",xlab = "CKD", ylab = "Fam.Hypertension", col = c("light green","darksalmon") )

Diabetes.count <- table(train_x$CKD, train_x$Diabetes)
barplot(Diabetes.count, main = "CKD vs Diabetes",xlab = "Diabetes", ylab = "Frequency", col = c("light green","darksalmon") )

Fam.Diabetes.count <- table(train_x$CKD, train_x$Fam.Diabetes)
barplot(Fam.Diabetes.count, main = "CKD vs Fam.Diabetes",xlab = "CKD", ylab = "Fam.Diabetes", col = c("light green","darksalmon") )

Stroke.count <- table(train_x$CKD, train_x$Stroke)
barplot(Stroke.count, main = "CKD vs Stroke",xlab = "CKD", ylab = "Stroke", col = c("light green","darksalmon") )

CVD.count <- table(train_x$CKD, train_x$CVD)
barplot(CVD.count, main = "CKD vs CVD",xlab = "CKD", ylab = "CVD", col = c("light green","darksalmon") )

Fam.CVD.count <- table(train_x$CKD, train_x$Fam.CVD)
barplot(Fam.CVD.count, main = "CKD vs Fam.CVD",xlab = "CKD", ylab = "Fam.CVD", col = c("light green","darksalmon") )

CHF.count <- table(train_x$CKD, train_x$CHF)
barplot(CHF.count, main = "CKD vs CHF",xlab = "CKD", ylab = "CHF", col = c("light green","darksalmon") )

Anemia.count <- table(train_x$CKD, train_x$Anemia)
barplot(Anemia.count, main = "CKD vs Anemia",xlab = "CKD", ylab = "Anemia", col = c("light green","darksalmon") )
#################################Processing or Data Analysis#########################################
trainn_x <- train_x[1:4000,]
valid_x <- train_x[4001:5000,]
test_x <- train_x[5001:6000,]
#################################Automated AIC based backward selection process#####################
library(rms)
lm.full = glm(CKD~Age+Female+White+Hispanic+Black+Other+Educ+Unmarried+Income+Insured
             +Weight+Height+BMI+Obese+Waist+SBP+DBP+HDL+LDL+Total.Chol+Dyslipidemia
             +PVD+Activity+PoorVision+Smoker+Hypertension+Fam.Hypertension+Diabetes
             +Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia, family = "binomial",data=trainn_x)
model.aic.backward <- step(lm.full, direction = "backward",trace=1)

AIC_backward_model = glm(CKD~Age+PVD+HDL+Anemia+Female+Diabetes+Hispanic+CVD
                         +Height+Hypertension+Activity+Total.Chol+Fam.CVD+Fam.Hypertension
                         +CHF+Weight+Smoker, family = "binomial", data=trainn_x)
summary(AIC_backward_model)                                                         

lm.null<-glm(CKD~1, family="binomial",data=trainn_x)
model.aic.forward <- step(lm.null, direction = "forward",trace=1, scope=~Age+Female+White+Hispanic+Black+Other+Educ+Unmarried+Income+Insured
             +Weight+Height+BMI+Obese+Waist+SBP+DBP+HDL+LDL+Total.Chol+Dyslipidemia
             +PVD+Activity+PoorVision+Smoker+Hypertension+Fam.Hypertension+Diabetes
             +Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia)
summary(model.aic.forward)

model.aic.both <- step(lm.null, direction = "both",trace=1,scope=~Age+Female+White+Hispanic+Black+Other+Educ+Unmarried+Income+Insured
             +Weight+Height+BMI+Obese+Waist+SBP+DBP+HDL+LDL+Total.Chol+Dyslipidemia
             +PVD+Activity+PoorVision+Smoker+Hypertension+Fam.Hypertension+Diabetes
             +Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia)
logistic_model = glm(CKD ~ Age + Weight + CVD + PVD + Diabetes + Hispanic + Anemia + 
                        Hypertension + LDL + Activity + HDL + CHF, family=binomial(link='logit'),data=trainn_x)
summary(logistic_model)
log_valid <- predict.glm(logistic_model,newdata=valid_x)
log_test <- predict.glm(logistic_model, newdata = test_x)
log_valid <- ifelse(log_valid>-0.5,1,0)
log_test <- ifelse(log_test>0.5,1,0)
logist_confusion_matrix <- table(log_valid,valid_x$CKD)
logist_confusion_matrix_testdata <- table(log_test,test_x$CKD)
#Accuracy is 91.5% but the true positive rate is really low#
#Accuracy is 91.1% on test data#
class(trainn_x$CKD)
#predict(lda.sample,newdata=valid_x)$class
##Best model from both forward and backward selection''CKD ~ Age + Weight + CVD + PVD + Diabetes + Hispanic + Anemia + 
    #Hypertension + LDL + Activity + HDL + CHF''###

###########Linear discriminant Analysis########
install.packages("MASS")
library(MASS)
lda.sample <- lda(CKD~Age+Female+White+Hispanic+Black+Other+Educ+Unmarried+Income+Insured
             +Weight+Height+BMI+Obese+Waist+SBP+DBP+HDL+LDL+Total.Chol+Dyslipidemia
             +PVD+Activity+PoorVision+Smoker+Hypertension+Fam.Hypertension+Diabetes
             +Fam.Diabetes+Stroke+CVD+Fam.CVD+CHF+Anemia,data = trainn_x)
summary(lda.sample)
lda.sample$prior
lda.sample$counts
lda.sample$means
lda.sample$svd
prop = lda.sample$svd^2
prop
lda.sample$scaling
data_valid <- predict(lda.sample,newdata=valid_x)$class
data_test <- predict(lda.sample,newdata = test_x)$class
length(data_valid)
LDA_all_variables_confusion_matrix <- table(data_valid,valid_x$CKD)
LDA_all_variables_confusion_matrix_test <- table(data_test,test_x$CKD)
#Accuracy is 90.75% on validation data#
#Accuracy is 90.2 on test data#
length(data_valid)
length(valid_x$CKD)
####LDA with select variables#####
lda.select.variabs <- lda(CKD ~ Age + Weight + CVD + PVD + Diabetes + Hispanic + Anemia + 
                        Hypertension + LDL + Activity + HDL + CHF, data= trainn_x)
lda.select.variabs.valid <- predict(lda.select.variabs,newdata = valid_x)$class
lda.select.test <- predict(lda.select.variabs,newdata = test_x)$class
LDA_selected_variables <- table(lda.select.variabs.valid,valid_x$CKD)
LDA_selected_variables_test <- table(lda.select.test,test_x$CKD)
LDA_selected_variables_test
#Accuracy is 91% on validation data#
#Accuracy is 90.5% on test data#
##########Using SVM on the data########
install.packages("e1071")
install.packages("rpart")
library(e1071)
library(rpart)
svm_model <- svm(CKD~Age + Weight + CVD + PVD + Diabetes + Hispanic + Anemia + 
                        Hypertension + LDL + Activity + HDL + CHF, data= trainn_x,kernel ="polynomial",cost=50, gamma=0.5)
svm.pred <- predict(svm_model, newdata = valid_x)
svm.pred_high_cost <- table(svm.pred,valid_x$CKD)
svm.pred.test <- predict(svm_model , newdata=test_x)
svm.pred_high_cost_test <- table(svm.pred.test,test_x$CKD)
########Validation accuracy is 90.6%######
###Test data accuracy is 90.2######
#########K-NN #######
library(class)
summary(trainn_x)
myvars <- c("Age","Female","White","Hispanic","Black","Other","Educ","Unmarried"
            ,"Income","Insured","Weight","Height","BMI","Obese"
            ,"Waist","SBP","DBP","HDL","LDL","Total.Chol","Dyslipidemia","PVD","Activity","PoorVision",
           "Smoker","Hypertension","Diabetes","Fam.Diabetes","Stroke","CVD","Fam.CVD","CHF","Anemia")
num.vars <- sapply(train_x[myvars], is.numeric)
train_x$CKD <- factor(train_x$CKD)

knn_orig_data <- train_x[myvars]
nrow(knn_orig_data)
train_knn <- knn_orig_data[1:4000,]
validation_knn <- knn_orig_data[4001:5000,]
test_knn <- knn_orig_data[5001:6000,]
train_knn_target <- train_x_knn$CKD[1:4000]
validation_knn_true_labels <- train_x$CKD[4001:5000]
summary(validation_knn_true_labels)
summary(train_knn_target)
sum(is.na(train_knn))
sapply(train_x[myvars], is.numeric)
knn_prediction <- knn(train_knn,validation_knn,train_knn_target,k=3)
length(knn_prediction)
table(knn_prediction,validation_knn_true_labels)




write.csv(fitted.results, file = "D:\\Study Related (VIMP)\\Naveen MSBA\\HealthCare Analytics\\Assiginment_3\\Transformed Data\\Valid_results.csv")
table(fitted.results,test_x$CKD)
missed <- mean(fitted.results!=test_x$CKD)
print(paste('Accuracy',1-missed))

