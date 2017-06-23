#' Assume all projects have same difficulty.	
#' 	
#' A Function to install and load the libraries.	
#' 	
#' 	
load_package <- function(packages){	
  new_package <- packages[!(packages %in% installed.packages()[, "Package"])]	
  if (length(new_package)) 	
    install.packages(new_package, dependencies = TRUE)	
  sapply(packages, require, character.only = TRUE)	
}	
	
	
packages <- c("ggplot2", "leaps", "lars", "ROCR", "rpart", "randomForest",	
              "pROC", "e1071", "caret", "ROCR", "dplyr", "corrplot",	
              "rattle", "rpart.plot", "RColorBrewer", "gbm", "GGally")	
load_package(packages)	
	
	
HR_comma_sep <- read.csv("/Users/Naveen/Downloads/Studies/Semester2/DataPrep/final project/HR_comma_sep.csv", header = TRUE)	
HR_comma_sep<-data.frame(HR_comma_sep)	
HR_comma_sep$salary_f = factor(HR_comma_sep$salary, levels=c('low','medium','high'))	
#' 	
#' 	
#' Loading the dataset.	
#' 	
#' 	
HR_comma_sep <- read.csv("/Users/Naveen/Downloads/Studies/Semester2/DataPrep/final project/HR_comma_sep.csv", header = TRUE)	
HR_comma_sep$salary_f = factor(HR_comma_sep$salary, levels=c('low','medium','high'))	
#' 	
#' 	
#' Renaming the predictors to appropriate names to improve readability.	
#' 	
#' 	
colnames(HR_comma_sep)[9]<-"Department"	
#' 	
#' 	
#' Adding unique identifier to each employee.	
#' 	
#' 	
HR_comma_sep["ID"]<-seq.int(nrow(HR_comma_sep))	
#' 	
#' 	
#' Finding the NA values in the table.	
#' 	
#' 	
sum(is.na(HR_comma_sep))	
#' 	
#' 	
#' Data Visualizations:	
#' 	
#' 	
layout(matrix(c(1,1,2,2,3,3,	
                      0,4,4,5,5,0), nrow = 2, byrow = TRUE))	
for(i in 2:6){	
  hist(HR_comma_sep[,i],xlab=names(HR_comma_sep)[i],main = paste0("Histogram of ",names(HR_comma_sep)[i]))	
  }	
#' 	
#' 	
#' 	
l1<-HR_comma_sep$left	
#' 	
#' 	
#' Coverting the variables to proper data type.	
#' 	
#' 	
to_factor <- c("left", "salary", "Work_accident", "Department", "promotion_last_5years")	
	
HR_comma_sep[to_factor] <- lapply(HR_comma_sep[to_factor], function(x) as.factor(x))	
#' 	
#' 	
#' Finding the descriptive statistics.	
#' 	
#' 	
summary(HR_comma_sep)	
#' 	
#' 	
#' Finding the correlation between variables.	
#' 	
#' 	
nums<-sapply(HR_comma_sep,is.numeric)	
cor_matrix<-cor(HR_comma_sep[,nums])	
HR_Corr<-HR_comma_sep %>% select(satisfaction_level:promotion_last_5years)	
#' 	
#' 	
#' 	
par(mfrow = c(1,1))	
ggcorr(cor_matrix, nbreaks = 4, palette = "RdGy", label = TRUE, label_size = 6, label_color = "white")	
#' 	
#' 	
#' 	
ggplot(HR_comma_sep,aes(factor(left),average_montly_hours))+geom_boxplot(outlier.colour = "green", outlier.size = 3)+ggtitle("Average Monthly Hours Spent Vs Left Employees")	
ggplot(HR_comma_sep,aes(factor(left),time_spend_company))+geom_boxplot(outlier.colour = "green", outlier.size = 3)+xlab("Left")+ylab("Time Spend Company")+ggtitle("Time Spend Company Vs Left Employees")	
ggplot(HR_comma_sep,aes(Department))+geom_bar(aes(fill=factor(left)),position='dodge')	
ggplot(HR_comma_sep,aes(Department))+geom_bar(aes(fill=factor(time_spend_company)),position='dodge')	
#' 	
#' 	
#' From the above plots we can observe that there are few outliers in the data where the employees have experience of more than 8 years. To find the total number of observation who spend in company more than 8 years.	
#' 	
#' 	
attach(HR_comma_sep)	
sum(HR_comma_sep$time_spend_company>=8)	
#' 	
#' 	
#' There are 376 employees who spend more than 8 years in the company. We cannot ignore these observations as majority employees are from Sales and Management departments which is crutial in this company.	
#' 	
#' Creating new data sets, for the employees who has 'left' and 'not left' into two seperate tables.	
#' 	
#' 	
left_employees<-HR_comma_sep[(HR_comma_sep$left==1),]	
non_left_employees<-HR_comma_sep[(HR_comma_sep$left==0),]	
#' 	
#' 	
#' Histograms for 'time spend in the company' using left and non-left data tables.	
#' 	
#' 	
ggplot(left_employees,aes(time_spend_company))+geom_histogram(binwidth = 0.5)+xlab("Time Spend at the company")+ylab("Number of Observations")+ggtitle("left")	
	
ggplot(non_left_employees,aes(time_spend_company))+geom_histogram(binwidth = 0.5)+xlab("Time Spend at the company")+ylab("Number of Observations")+ggtitle("Not left")	
#' 	
#' 	
#' Important Facts about Data:	
#' 	
#'   1. People who spend more than 6 years and who just spend 2 years at the company are less likely to go.	
#' 	
#'   2. People are more likely to leave when they have spent 3-5 years.	
#' 	
#' 	
ggplot(HR_comma_sep,aes(x=time_spend_company,y=left,fill=factor(promotion_last_5years),colour=factor(promotion_last_5years)))+geom_bar(position='stack', stat='identity')+xlab("Time Spend in Company")+ylab("promotion in last 5 years")+ggtitle("Time Spend Compnay Vs Promotion in Last 5 years")	
#' 	
#' 	
#' Very less people got promoted for the last 5 years despite spending more years in the company.	
#' 	
#' 	
ggplot(HR_comma_sep,aes(x=salary,y=time_spend_company,fill=factor(left),colour=factor(left)))+geom_boxplot(outlier.colour = NA)+xlab("salary")+ylab("Time Spend Company")+ggtitle("Time Spend Company Vs Salaray")	
	
ggplot(HR_comma_sep, aes(x = last_evaluation, y = average_montly_hours, col = factor(left))) + 	
    geom_point(alpha = 0.5) + facet_wrap(~factor(salary_f)) + ylab("Average monthly hours worked") + 	
    xlab("Last evaluation rating") + ggtitle("Average monthly hrs vs Last Evaluation w.r.t. salary") + 	
    theme(plot.title = element_text(size = 16, face = "bold"))	
#' 	
#' 	
#' From the above plot we can conclude that low and medium salaried employees are leaving the company more than emplyees with high salary.	
#' 	
#' 	
ggplot(HR_comma_sep,aes(x=salary,y=satisfaction_level,fill=factor(left),colour=factor(left)))+geom_boxplot(outlier.colour = "black")+xlab("salary")+ylab("Satisfaction Level")	
	
ggplot(HR_comma_sep,aes(x=factor(time_spend_company),y=average_montly_hours,fill=factor(left),colour=factor(left)))+	
  geom_boxplot(outlier.colour = NA)+xlab("Time Spend Company")+ylab("Average Monthly Hours")	
	
ggplot(HR_comma_sep,aes(x=salary,y=last_evaluation,fill=factor(left),colour=factor(left)))+geom_boxplot(outlier.colour = "black")+xlab("salary")+ylab("Last Evaluation")	
	
ggplot(HR_comma_sep,aes(x=number_project,y=last_evaluation,fill=factor(left),colour=factor(left)))+geom_bar(position='stack', stat='identity')+xlab("Number of projects")+ylab("Last Evaluation")	
	
ggplot(HR_comma_sep,aes(x=time_spend_company,y=l1)) + geom_jitter(height = 0.1, width = 0.25) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(~salary_f) + ggtitle("Time spent in company before by retention and salary") + ylab("Has the employee left? (binary)") + xlab("Time spent in the company (years)") + theme(plot.title = element_text(size = 16, face = "bold"))	
	
ggplot(HR_comma_sep,aes(x=average_montly_hours,y=l1)) + geom_jitter(height = 0.1, width = 0.25) + geom_smooth(method = "glm", method.args = list(family = "binomial")) + facet_wrap(~salary_f) + ggtitle("Average monthly hours spent by an employee by salary and retention") + ylab("Has the employee left? (binary)") + xlab("Average monthly hours worked") + theme(plot.title = element_text(size = 16, face = "bold"))	
#' 	
#' 	
#' 	
HR_comma_sep['avg_hr_prj_range']<-cut(HR_comma_sep$average_montly_hours,4,labels = c(1:4))	
#' 	
#' 	
#' Finding the total valuable employees.	
#' 	
#' 	
b1<-HR_comma_sep$last_evaluation > 0.7	
b2<- HR_comma_sep$avg_hr_prj_range==3| HR_comma_sep$avg_hr_prj_range==4	
sum(b1 & b2)	
#' 	
#' 	
#' Rule for Valueable Employees:	
#' 	
#' Valuable employees can be decided based on their deptartment statistics. Inour case, we have taken the mean of each department for average monthly hours and last evaluation.	
#' 	
#' Total valueable Employees according to above rule: 4754	
#' 	
#' Total number of employees :14999.	
#' 	
#' 	
HR_comma_sep['valuedEmployee']<-0	
for (i in (1: nrow(HR_comma_sep))){	
  b1<-(HR_comma_sep[i,'last_evaluation'] > 0.7)	
  b2<-((HR_comma_sep[i,'avg_hr_prj_range']==3) | (HR_comma_sep[i,'avg_hr_prj_range']==4))	
  if(b1 & b2){	
    HR_comma_sep[i,'valuedEmployee'] = 1	
  }   	
}	
	
lev<-levels(as.factor(HR_comma_sep$Department))	
for(i in (1:length(lev))){	
      lev[i]<-sum(grepl(lev[i],HR_comma_sep$Department))	
}	
lev_list<-as.data.frame(levels(as.factor(HR_comma_sep$Department)))	
colnames(lev_list)<-"Department"	
lev_list['number_of_employees']<-lev	
	
left_employees<-HR_comma_sep[(HR_comma_sep$left==1),]	
lev_left<-levels(as.factor(left_employees$Department))	
for(i in (1:length(lev_left))){	
       lev_left[i]<-sum(grepl(lev_left[i],left_employees$Department))	
   }	
lev_list['number_of_employees_left']<-lev_left	
	
f<-function(x,y) as.numeric(x)/as.numeric(y)	
p<-as.data.frame(mapply(f,lev_list[,2],lev_list[,3]))	
lev_list['percent']<-p[,1]	
#' 	
#' 	
#' 	
lev_list	
#' 	
#' 	
#' From the above table we can say that "management' and 'R and D' people are leaving the company more when compared to other departments. 	
#' 	
#' Feature Selection:	
#' 	
#' 1. Cp Model:	
#' 	
#' 	
model.mat<-model.matrix(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+Department+salary,data=HR_comma_sep,sep="_")	
sb<-leaps(x=model.mat[,2:19],y=HR_comma_sep[,7],method = 'Cp')	
#' 	
#' 	
#' Plotting the cp method to find the best subset model.	
#' 	
#' 	
plot(sb$size,sb$Cp,pch=19)	
cp_model1<-sb$which[which(sb$Cp==min(sb$Cp)),]	
cp_df<-data.frame(cp_model1)	
colnames(cp_df)<-"CpVariables"	
cp_df$IndependentVars<-colnames(model.mat)[-1]	
vars<-cp_df %>% filter(CpVariables==TRUE)	
Ind_vars<-vars$IndependentVars	
Ind_vars	
#' 	
#' 	
#' 2. Forward selection:	
#' 	
#' 	
forward_fit<-glm(left~1,data = HR_comma_sep,family=binomial(link="logit"))	
fit.forward<-step(forward_fit,scope = list(lower=left~1,upper=left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+Department+salary),direction = 'forward')	
#' 	
#' 	
#' Finding the summary for forward selection.	
#' 	
#' 	
summary(fit.forward)	
#' 	
#' 	
#' 3. Backward Selection:	
#' 	
#' 	
fit.backward<-glm(left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+Department+salary,data = HR_comma_sep,family=binomial(link="logit"))	
fit.back<-step(fit.backward,scope = list(lower=left~1,upper=left~satisfaction_level+last_evaluation+number_project+average_montly_hours+time_spend_company+Work_accident+promotion_last_5years+Department+salary),direction = 'backward')	
#' 	
#' 	
#' 	
summary(fit.back)	
#' 	
#' 	
#' 4. LASSO:	
#' 	
#' 	
YVars<-as.numeric(HR_comma_sep[,7])	
	
fit.lasso<-lars(x=as.matrix(model.mat[,2:19]),y=as.matrix(YVars),type = 'lasso')	
#' 	
#' 	
#' Plotting the LASSO,	
#' 	
#' 	
plot(fit.lasso)	
#' 	
#' 	
#' From the above feature selection techniques, it is evident that all the 9 variables are important.	
#' 	
#' Stratified Sampling	
#' 	
xvars=c('satisfaction_level','last_evaluation','number_project','average_montly_hours','time_spend_company','Work_accident','promotion_last_5years','Department','salary')	
yvars='left'	
HR_comma_sep <- read.csv("/Users/Naveen/Downloads/Studies/Semester2/DataPrep/final project/HR_comma_sep.csv")	
p1<-0.8	
set.seed(12345)	
inTrain<-createDataPartition(y=HR_comma_sep[,yvars],p=p1,list=FALSE)	
train_HR<-HR_comma_sep[inTrain,]	
test_HR<-HR_comma_sep[-inTrain,]	
stopifnot(nrow(train_HR)+nrow(test_HR)==nrow(HR_comma_sep))	
#' 	
#' 	
#' Modelling techniques:	
#' 	
#' 1. Genalized Linear Model:	
#' 	
#' 	
glm.fit<-glm(left~.,data=train_HR,family = binomial(link="logit"))	
#' 	
#' 	
#' summary statistics	
#' 	
#' 	
summary(glm.fit)	
#' 	
#' 	
#' Predicting the values for test data:	
#' 	
#' 	
glm_link <- predict(glm.fit, test_HR, type="link")	
glm_response <- predict(glm.fit, test_HR, type="response")	
	
score_data <- data.frame(link=glm_link, 	
                         response=glm_response,	
                         left=test_HR$left,	
                         stringsAsFactors=FALSE)	
score_data %>% 	
  ggplot(aes(x=link, y=response, col=left)) + 	
  geom_point() + 	
  geom_rug() + 	
  ggtitle("Both link and response scores put cases in the same order")	
	
test_HR[,'Yhat']<-predict(glm.fit,newdata=test_HR)	
fitted.values<-test_HR[,'Yhat']	
test_HR$Yhat<-ifelse( test_HR$Yhat>0.5,1,0)	
conf<-confusionMatrix(test_HR$Yhat,test_HR$left)	
#' 	
#' 	
#' 	
conf	
roc <- roc(test_HR$left, test_HR$Yhat)	
auc <- auc(roc)	
auc	
#' 	
#' 	
#' ROC Curve	
#' 	
#' 	
fit_values<-prediction(fitted.values,test_HR$left)	
p<-performance(fit_values,measure = 'tpr',x.measure = 'fpr')	
#' 	
#' 	
#' 	
plot(p, main = 'ROC', col= 'blue')	
abline(0, 1, lty = 2)	
#' 	
#' 	
#' 2. Decision Tree:	
#' 	
#' 	
cart.fit<-rpart(left~.,data=train_HR,method='class')	
#' 	
#' Fancy plot for decision tree:	
#' 	
#' 	
fancyRpartPlot(cart.fit)	
#' 	
#' 	
#' 	
summary(cart.fit)	
#' 	
#' 	
#' predicting for Test data using CART model:	
#' 	
#' 	
fit.values.cart<-predict(cart.fit,newdata = test_HR)	
fit.val1<-ifelse(fit.values.cart[,1]>0.5,1,0)	
fit.val2<-ifelse(fit.values.cart[,2]>0.5,1,0)	
conf.cart<-confusionMatrix(fit.val2,test_HR$left)	
#' 	
#' 	
#' 	
conf.cart	
#' 	
#' 	
#' ROC Curve:	
#' 	
#' 	
fit.values.cart1<-predict(cart.fit,newdata = test_HR,type="prob")	
p.cart<-prediction(fit.values.cart[,2],test_HR$left)	
p.cart<-performance(p.cart,measure = 'tpr',x.measure = 'fpr')	
#' 	
#' 	
#' 	
plot(p.cart)	
abline(0,1,lty=2)	
#' 	
#' 	
#' 3. Random Forest Algorithm:	
#' 	
#' 	
fit_rf<-randomForest(as.factor(left)~.,data=train_HR,importance=TRUE,ntree=1000)	
#' 	
#' 	
#' 	
fit_rf$confusion	
#' 	
#' 	
#' Variable Importance for Random Forest Model.	
#' 	
#' 	
varImpPlot(fit_rf)	
#' 	
#' 	
#' predicting for Test data using Random Forest model.	
#' 	
#' 	
fitted.values.rf<-predict(fit_rf,newdata = test_HR,type='class')	
fitted.values.rf1<-predict(fit_rf,newdata = test_HR,type='prob')	
conf.rf<-confusionMatrix(fitted.values.rf,test_HR$left)	
#' 	
#' 	
#' 	
conf.rf	
#' 	
#' 	
#' ROC Curve:	
#' 	
#' 	
HR.rf<-roc(test_HR$left, fitted.values.rf1[,2])	
#' 	
#' 	
#' 	
plot(HR.rf, print.auc=TRUE, auc.polygon=TRUE)	
#' 	
#' 	
#' 4. Support Vector Machine	
#' 	
#' 	
svm_model<-svm(left~.,data=train_HR,type='C-classification')	
svm_model1<-svm(left~.,data=train_HR,type='C-classification',probability = TRUE)	
#' 	
#' 	
#' 	
summary(svm_model)	
#' 	
#' 	
#' predicting for Test data using SVM:	
#' 	
#' 	
pred<-predict(svm_model,newdata = test_HR)	
pred.prob<-predict(svm_model1,newdata = test_HR,type='prob',probability = TRUE)	
conf.svm<-confusionMatrix(pred,test_HR$left)	
#' 	
#' 	
#' 	
conf.svm	
#' 	
#' 	
#' ROC Curve:	
#' 	
#' 	
p.svm<-prediction(attr(pred.prob,"probabilities")[,1],test_HR$left)	
svm.perf<-performance(p.svm,measure = 'tpr',x.measure = 'fpr')	
#' 	
#' 	
#' 	
plot(svm.perf)	
#' 	
#' 	
#' 5. Gradient Boosting	
#' 	
#' 	
con <- trainControl(method = "cv", number = 10)	
model_GB <- train(as.factor(left) ~ ., data = train_HR, method = "gbm", trControl = con)	
#' 	
#' 	
#' Predicting for test data using Gradient Boosting	
#' 	
#' 	
predicted_GB = predict(model_GB, test_HR)	
Actual <- test_HR$left	
#' 	
#' 	
#' 	
confusionMatrix(reference = Actual, data = predicted_GB)	
#' 	
#' 	
#' ROC Curve for Gradiant Boosting	
#' 	
#' 	
 predicted_GB_prob = predict(model_GB, test_HR, type = "prob")	
 pr <- prediction(predicted_GB_prob[, 2], test_HR$left)	
 prf <- performance(pr, measure = "tpr", x.measure = "fpr")	
#' 	
#' 	
#' 	
plot(prf)	
#' 	
#'  	
#' Comparing all mdoels using ROC Curve	
#' 	
#' 	
#Logistic Regression	
Final_roc <- plot(roc(test_HR$left, fitted.values), print.auc = TRUE, col = "blue")	
 	
#Decision Tree	
 Final_roc <- plot(roc(test_HR$left, fit.values.cart[,2]), print.auc = TRUE, col = "Green", print.auc.y = 0.6, add = TRUE)	
 	
# Random Forest	
 Final_roc <- plot(roc(test_HR$left, fitted.values.rf1[,2]), print.auc = TRUE, col = "Purple", print.auc.y = 0.8, add = TRUE)	
 	
# Gradient Boosting	
 Final_roc <- plot(roc(test_HR$left, predicted_GB_prob[, 2]), print.auc = TRUE, col = "Red", print.auc.y = 0.9, add = TRUE)	
 	
# SVM	
Final_roc <- plot(roc(test_HR$left, attr(pred.prob,"probabilities")[,1]), print.auc = TRUE, col = "Black", print.auc.y = 0.1, add = TRUE)	
	
legend(x='bottomright',legend=c("GLM", "Decision Tree","Random Forest","Gradient Boosting","SVM"),col=c("blue","Green", "Purple","Red","Black"),lwd=2,xjust = 1, yjust = 1)	
#' 	
