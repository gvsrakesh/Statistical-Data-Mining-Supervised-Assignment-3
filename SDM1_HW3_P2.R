library(ggplot2)
library(data.table)
library(MASS)
library(dplyr)
library(gmodels)
library(leaps)
library(caret)
library(klaR)
library(class)
library(plotly)
library(MMST)
library(mltools)
library(DataExplorer)
library(GGally)
library(lattice)

getwd()
setwd("/Users/rakeshguduru/Documents/UB Grad stuff/SDM 1/hw3/")

#### Importing dataset from the environment window######
#save(diabetes_data,file = "diabetes.RData")
load("diabetes.RData")
df = diabetes_data[,-(1:3)]
names(df) = c("obs_num","glucose.area","insulin.area", "SSPG", "relative.weight","fasting.plasma.glucose","class_num")
df$class_num = as.factor(df$class_num)
df_final= df[,-1]
##### Pairwise scatterplots #########
summary(df_final)
df_1h = one_hot(as.data.table(df[,-(1)]))
plot_correlation(df_1h)
ggpairs(df_final, aes(color=class_num)) + ggtitle("Diabetes Data with 3 classes")

######## Forming the datasets ###################################

set.seed(420)
training_samples_cont = df$class_num %>% createDataPartition(p = 0.632, list = FALSE)
train_data_cont = df[training_samples_cont, ]
test_data_cont = df[-training_samples_cont, ]

####### Applying Linear Discrimant Analysis to the dataset ##############

lda_fit <- lda(class_num~., data = train_data_cont)
lda_fit
plot(lda_fit, col=as.integer(train_data_cont$class_num))
plot(lda_fit,dimen = 1,type="b")
lda_pred_train = predict(lda_fit, newdata = train_data_cont)
lda_train_pred_values = lda_pred_train$class
lda_pred_test = predict(lda_fit, newdata = test_data_cont)
lda_test_pred_values = lda_pred_test$class

############################## Confusion Matrix Train Data Linear Discrimant Analysis #############################

output_predict_train_lda = as.factor(lda_train_pred_values)
levels(output_predict_train_lda)
train_lm_table = CrossTable(x=train_data_cont$class_num, y = output_predict_train_lda, prop.chisq = FALSE)
train_lda_table_error = train_lm_table$prop.tbl[1,2] + train_lm_table$prop.tbl[1,3] + train_lm_table$prop.tbl[2,1] + train_lm_table$prop.tbl[2,3] + train_lm_table$prop.tbl[3,1] + train_lm_table$prop.tbl[3,2]
train_lda_table_accuracy = 1 - train_lda_table_error
train_lda_table_accuracy
############################## Confusion Matrix Test Data Linear Discrimant Analysis  ###########################

output_predict_test = as.factor(lda_test_pred_values)
levels(output_predict_test)
test_lm_table = CrossTable(x=test_data_cont$class_num, y = output_predict_test, prop.chisq = FALSE)
test_lda_table_error = test_lm_table$prop.tbl[1,2] + test_lm_table$prop.tbl[1,3] + test_lm_table$prop.tbl[2,1] + test_lm_table$prop.tbl[2,3] + test_lm_table$prop.tbl[3,1] + test_lm_table$prop.tbl[3,2]
test_lda_table_accuracy = 1 - test_lda_table_error
test_lda_table_accuracy
####### Applying Quadratic Discrimant Analysis to the dataset ##############

qda_fit <- qda(class_num~., data = train_data_cont)
qda_fit
qda_pred_train = predict(qda_fit, newdata = train_data_cont)
qda_train_pred_values = qda_pred_train$class
qda_pred_test = predict(qda_fit, newdata = test_data_cont)
qda_test_pred_values = qda_pred_test$class

############################## Confusion Matrix Train Data Quadratic Discrimant Analysis #############################

output_predict_train_qda = as.factor(qda_train_pred_values)
levels(output_predict_train_qda)
train_lm_table = CrossTable(x=train_data_cont$class_num, y = output_predict_train_qda, prop.chisq = FALSE)
train_qda_table_error = train_lm_table$prop.tbl[1,2] + train_lm_table$prop.tbl[1,3] + train_lm_table$prop.tbl[2,1] + train_lm_table$prop.tbl[2,3] + train_lm_table$prop.tbl[3,1] + train_lm_table$prop.tbl[3,2]
train_qda_table_accuarcy = 1 - train_qda_table_error
train_qda_table_accuarcy
############################## Confusion Matrix Test Data Quadratic Discrimant Analysis  ###########################

output_predict_test = as.factor(qda_test_pred_values)
levels(output_predict_test)
test_lm_table = CrossTable(x=test_data_cont$class_num, y = output_predict_test, prop.chisq = FALSE)
test_qda_table_error = test_lm_table$prop.tbl[1,2] + test_lm_table$prop.tbl[1,3] + test_lm_table$prop.tbl[2,1] + test_lm_table$prop.tbl[2,3] + test_lm_table$prop.tbl[3,1] + test_lm_table$prop.tbl[3,2]
test_qda_table_accuarcy = 1 - test_qda_table_error
test_qda_table_accuarcy
############# Prediction for a new data point for the models ######################

names(test_data_cont)
new_data = data.frame(t(data.frame(c(146,0.98,122,544,186,184,1))))
names(new_data) = names(test_data_cont)
qda_pred_test_case = predict(qda_fit, newdata = new_data)
qda_test_pred_values_cases = as.numeric(qda_pred_test_case$class)

qda_test_pred_values_cases

lda_pred_test_case = predict(lda_fit, newdata = new_data)
lda_test_pred_values_cases = as.numeric(lda_pred_test_case$class)

lda_test_pred_values_cases
