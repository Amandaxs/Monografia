##############################################################################
######### Ajuste de modelos para prever um bom ou mal devedor ################
##############################################################################
library(tidyverse)
library(caret)
library(randomForest)
library(ROCR)


df <- read.csv("classification.csv")
df$y %>% as.factor %>% summary  ## The classes are balanced

### Drop X column because it's just an id
df <- df %>% subset(select = - c(X))
df$y <- as.factor(if_else(df$y == 1, "Pagou", "NaoPagou"))

### Sep data set in train test

intrain <- createDataPartition(y = df$y,
                               p = 0.7,
                               list = FALSE)
df_train <- df[intrain, ]
df_test <- df[-intrain, ]

## check proportion of each category in train and test datasets
df_train$y %>% summary 
df_test$y %>%  summary 


##### For logistic regression, there's no need of a cross validation

fit <- train(y~.,
             data = df_train,
             method="glm", 
             family="binomial")
### 


pred <- predict(fit, newdata=df_test)
varImp(fit)
confusionMatrix(pred,df_test$y)
#############





########################################################################
########################### Random Florest #############################
########################################################################


rf <- randomForest(y ~.,ntree=100, data = df_train, importance = TRUE)
pred = predict(rf, df_test[-23])
## Matriz de confusão
cm = table(df_test[,23], pred)
cm

## Curva roc

prediction_for_roc_curve <- predict(rf,df_test[,-23],type="prob")
classes <- levels(df_test$y)
pretty_colours <- c("#F8766D","#00BA38")

for (i in 1:2){
  # Define which observations belong to class[i]
  true_values <- ifelse(df_test[,23]==classes[i],1,0)
  # Assess the performance of classifier for class[i]
  pred <- prediction(prediction_for_roc_curve[,i],true_values)
  perf <- performance(pred, "tpr", "fpr")
  if (i==1)
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i]) 
  }
  else
  {
    plot(perf,main="ROC Curve",col=pretty_colours[i],add=TRUE) 
  }
  # Calculate the AUC and print it to screen
  auc.perf <- performance(pred, measure = "auc")
  print(auc.perf@y.values)
}



########################################################################
######################## Análise discriminante #########################
########################################################################

########################## Linear

