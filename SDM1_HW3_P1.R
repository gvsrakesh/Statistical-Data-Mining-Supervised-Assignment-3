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
library(GGally)

getwd()
setwd("/Users/rakeshguduru/Documents/UB Grad stuff/SDM 1/hw3/")

df_Boston = Boston
median_crime = median(df_Boston$crim)
df_Boston$output_variable = if_else(df_Boston$crim >= median_crime,1,0)
df_Boston$output_variable = as.factor(df_Boston$output_variable)
df_Boston = df_Boston[,-1]

set.seed(420)
training_samples_cont = df_Boston$output_variable %>% createDataPartition(p = 0.632, list = FALSE)
train_data_cont = df_Boston[training_samples_cont, ]
test_data_cont = df_Boston[-training_samples_cont, ]

####################### Logistic Regression ###################################################

logistic_fit <- glm(output_variable~., data = train_data_cont, family = "binomial")
summary(logistic_fit)

##### Prediction using Logistic regression ################

logistic_probs_train <- predict(logistic_fit, newdata = train_data_cont, type = "response")
output_predict_train <- round(logistic_probs_train)
logistic_probs_test <- predict(logistic_fit, newdata = test_data_cont, type = "response")
output_predict_test <- round(logistic_probs_test)

############################## Confusion Matrix Train Data Logistic Regression #####################################

output_predict_train = as.factor(output_predict_train)
levels(output_predict_train)
train_lm_table = CrossTable(x=train_data_cont$output_variable, y = output_predict_train, prop.chisq = FALSE)
train_glm_table_accuracy = train_lm_table$prop.tbl[1,1] + train_lm_table$prop.tbl[2,2]
train_glm_table_precision = train_lm_table$prop.tbl[2,2]/(train_lm_table$prop.tbl[2,2]+train_lm_table$prop.tbl[1,2])
train_glm_table_recall = train_lm_table$prop.tbl[2,2]/(train_lm_table$prop.tbl[2,2]+train_lm_table$prop.tbl[2,1])

############################## Confusion Matrix Test Data Logistic Regression #######################################

output_predict_test = as.factor(output_predict_test)
levels(output_predict_test)
test_lm_table = CrossTable(x=test_data_cont$output_variable, y = output_predict_test, prop.chisq = FALSE)
test_glm_table_accuarcy = test_lm_table$prop.tbl[1,1] + test_lm_table$prop.tbl[2,2]
test_glm_table_precision = test_lm_table$prop.tbl[2,2]/(test_lm_table$prop.tbl[2,2]+test_lm_table$prop.tbl[1,2])
test_glm_table_recall = test_lm_table$prop.tbl[2,2]/(test_lm_table$prop.tbl[2,2]+test_lm_table$prop.tbl[2,1])

################################## Subset Selection ###########################################

fit <- regsubsets(output_variable~., data = train_data_cont, method = "exhaustive", nvmax = 13)
summary_subset <- summary(fit)
names(summary_subset)
cp = summary_subset$cp
bic = summary_subset$bic

number_vars_cp = which.min(cp)
number_vars_bic = which.min(bic)

summary_subset$outmat
selected_outputs = summary_subset$outmat
ncolumns = ncol(df_Boston)

train_error_vec = c()
test_error_vec = c()

######### Iterating through variables for logistic regression using the output matrix for subset selection ########

#i = 3
for(i in 1:(ncolumns-1))
{
  variable = which(selected_outputs[i,] == "*")
  
  train_data = train_data_cont[, c(14,variable)]
  test_data = test_data_cont[, c(14, variable)]
  
  logit_fit = glm(output_variable~., data = train_data, family = "binomial")
  
  train_predict_logit = predict(logit_fit, newdata = train_data, type = "response")
  output_train_predict <- round(train_predict_logit)
  test_predict_logit = predict(logit_fit, newdata = test_data, type = "response")
  output_test_predict <- round(test_predict_logit)
  
  test_table = CrossTable(x=test_data$output_variable, y = output_test_predict, 
                          prop.chisq = FALSE)
  test_error = test_table$prop.tbl[1,2] + test_table$prop.tbl[2,1]
  train_table = CrossTable(x=train_data$output_variable, y = output_train_predict, 
                           prop.chisq = FALSE)
  train_error = train_table$prop.tbl[1,2] + train_table$prop.tbl[2,1]
  
  train_error_vec = c(train_error_vec, train_error)
  test_error_vec = c(test_error_vec, test_error)

}

upper = max(train_error_vec, test_error_vec)
lower = min(train_error_vec, test_error_vec)

train_error_vec
test_error_vec

plot(train_error_vec, type = "o", lty = 2, col = "blue", ylim = c(lower -0.01, upper +0.01) ,
     xlab = "k-complexity(#variables)", ylab = "test error vs train error", main = "Model Selection Logistic Regression")
lines(test_error_vec, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

###################################################################################################################
###################################################################################################################

###################################### Linear Discrimant Analysis ##########################################
ggpairs(df_Boston, aes(color=output_variable)) + ggtitle("Boston Data with 2 classes")
lda_fit <- lda(output_variable~., data = train_data_cont)
lda_fit
summary(lda_fit)
plot(lda_fit)
lda_pred_train = predict(lda_fit, newdata = train_data_cont)
lda_train_pred_values = lda_pred_train$class
lda_pred_test = predict(lda_fit, newdata = test_data_cont)
lda_test_pred_values = lda_pred_test$class

############################## Confusion Matrix Train Data Linear Discrimant Analysis #############################

output_predict_train_lda = as.factor(lda_train_pred_values)
levels(output_predict_train_lda)
train_lm_table = CrossTable(x=train_data_cont$output_variable, y = output_predict_train, prop.chisq = FALSE)
train_lda_table_error = train_lm_table$prop.tbl[1,2] + train_lm_table$prop.tbl[2,1]
train_lda_table_accuracy = train_lm_table$prop.tbl[1,1] + train_lm_table$prop.tbl[2,2]
train_lda_table_precision = train_lm_table$prop.tbl[2,2]/(train_lm_table$prop.tbl[2,2]+train_lm_table$prop.tbl[1,2])
train_lda_table_recall = train_lm_table$prop.tbl[2,2]/(train_lm_table$prop.tbl[2,2]+train_lm_table$prop.tbl[2,1])

############################## Confusion Matrix Test Data Linear Discrimant Analysis  ###########################

output_predict_test = as.factor(lda_test_pred_values)
levels(output_predict_test)
test_lm_table = CrossTable(x=test_data_cont$output_variable, y = output_predict_test, prop.chisq = FALSE)
test_lda_table_error = test_lm_table$prop.tbl[1,2] + test_lm_table$prop.tbl[2,1]
test_lda_table_accuracy = test_lm_table$prop.tbl[1,1] + test_lm_table$prop.tbl[2,2]
test_lda_table_precision = test_lm_table$prop.tbl[2,2]/(test_lm_table$prop.tbl[2,2]+test_lm_table$prop.tbl[1,2])
test_lda_table_recall = test_lm_table$prop.tbl[2,2]/(test_lm_table$prop.tbl[2,2]+test_lm_table$prop.tbl[2,1])

################################## Subset Selection Linear Discrimant Analysis ######################################

train_error_vec = c()
test_error_vec = c()

#i = 3
for(i in 1:(ncolumns-1))
{
  variable = which(selected_outputs[i,] == "*")
  
  train_data = train_data_cont[, c(14,variable)]
  test_data = test_data_cont[, c(14, variable)]
  
  lda_fit = lda(output_variable~., data = train_data)
  
  train_predict_lda = predict(lda_fit, newdata = train_data)
  output_train_predict <- train_predict_lda$class
  test_predict_lda = predict(lda_fit, newdata = test_data)
  output_test_predict <- test_predict_lda$class
  
  test_table = CrossTable(x=test_data$output_variable, y = output_test_predict, 
                          prop.chisq = FALSE)
  test_error = test_table$prop.tbl[1,2] + test_table$prop.tbl[2,1]
  train_table = CrossTable(x=train_data$output_variable, y = output_train_predict, 
                           prop.chisq = FALSE)
  train_error = train_table$prop.tbl[1,2] + train_table$prop.tbl[2,1]
  
  train_error_vec = c(train_error_vec, train_error)
  test_error_vec = c(test_error_vec, test_error)
  
}

upper = max(train_error_vec, test_error_vec)
lower = min(train_error_vec, test_error_vec)

(test_error_vec - train_error_vec)

plot(train_error_vec, type = "o", lty = 2, col = "blue", ylim = c(lower -0.02, upper +0.02) ,
     xlab = "k-complexity(#variables)", ylab = "test error vs training error", main = "Model Selection Linear Discrimant Analysis")
lines(test_error_vec, type = "o", lty = 1, col = "red")
legend("topright", c("training", "test"), lty = c(2,1), col = c("blue", "red"))

###################################################################################################################
###################################################################################################################

########################################### KNN Classification #################################################

knn_error = data.frame(kvalue= numeric(0), train_er = numeric(0), test_er = numeric(0))

train_error = list()
test_error = list()

for (k in c(1,3,5,7,9,11,13,15,17,19,21,23,25))
{
  train_pred <- knn(train = train_data,test = train_data,cl = train_data$output_variable, k=k)
  knn_pred_test = knn(train = train_data, test = test_data, cl=train_data$output_variable, k = k)
  summary(train_pred)
  cat("for k value = ", k)
  train_table = CrossTable(x=train_data$output_variable, y = knn_pred_train, prop.chisq = FALSE)
  test_table = CrossTable(x=test_data$output_variable, y = knn_pred_test, prop.chisq = FALSE)
  test_error = test_table$prop.tbl[1,2] + test_table$prop.tbl[2,1]
  train_error = train_table$prop.tbl[1,2] + train_table$prop.tbl[2,1]
  knn_error = rbind(knn_error, c(k, train_error, test_error))
}

colnames(knn_error) = c("kvalue","train_error","test_error")

knn_error$error_diff = knn_error$test_error - knn_error$train_error

knn_error_dataset = data.frame(vars= numeric(0), kvalue= numeric(0), train_er = numeric(0), test_er = numeric(0))

for(i in 1:(ncolumns-1))
{
  variable = which(selected_outputs[i,] == "*")
  
  train_data = train_data_cont[, c(14,variable)]
  test_data = test_data_cont[, c(14, variable)]
  
  for(k in c(1,3,5,7,9,11,13,15,17,19,21,23,25,27,29,31,33,35,37,39,41))
  {
    knn_pred_train = knn(train = train_data, test = train_data, cl=train_data$output_variable, k = k)
    knn_pred_test = knn(train = train_data, test = test_data, cl=train_data$output_variable, k = k)
    
    test_table = CrossTable(x=test_data$output_variable, y = knn_pred_test, 
                            prop.chisq = FALSE)
    test_error = test_table$prop.tbl[1,2] + test_table$prop.tbl[2,1]
    train_table = CrossTable(x=train_data$output_variable, y = knn_pred_train, 
                             prop.chisq = FALSE)
    train_error = train_table$prop.tbl[1,2] + train_table$prop.tbl[2,1]
    
    knn_error_dataset = rbind(knn_error_dataset, c(i, k, train_error, test_error))
  }
  
}

colnames(knn_error_dataset) = c("variables","kvalue","train_error","test_error")

knn_error_dataset$error_diff = knn_error_dataset$test_error - knn_error_dataset$train_error

knn_error_filtered = knn_error_dataset[((knn_error_dataset$train_error>0)&(knn_error_dataset$test_error>0)
                   &(knn_error_dataset$error_diff>0)),]

knn_error_filtered[which(knn_error_filtered$error_diff == min(knn_error_filtered$error_diff)),]

knn_error_filtered$variables = as.factor(knn_error_filtered$variables)

####################### Plotting knn ##############################
#quartz()

knn_plot = ggplot(knn_error_filtered, aes(x = knn_error_filtered$train_error, y = knn_error_filtered$test_error,
                               fill=variables, size = kvalue)) + geom_point(shape=21) 
print(knn_plot + labs(title = "KNN performance w.r.t. k values and number of variables",
                               x="training error", y="test error"))

median(knn_error_filtered$variables)
median(knn_error_filtered$train_error)
median(knn_error_filtered$test_error)


plot_ly(x = knn_error_filtered$variables, y = knn_error_filtered$kvalue, 
        z = knn_error_filtered$error_diff, type = "contour") %>% 
  layout(
    title = "KNN error differnce between test and train for various k values and variables",
    scene = list(
      xaxis = list(title = "k-values"),
      yaxis = list(title = "No of variables")
    ))