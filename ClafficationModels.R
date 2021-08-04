##############################################################################
######### Ajuste de modelos para prever um bom ou mal devedor ################
##############################################################################
library(tidyverse)
library(caret)
library(randomForest)
library(ROCR)
library(e1071)
library(naivebayes)
df <- read.csv("classification.csv")
df$y %>% as.factor %>% summary  ## The classes are balanced




### Drop X column because it's just an id
df <- df %>% subset(select = - c(X))
#df$y1 <- as.factor(df$y)
#df$y <- as.factor(if_else(df$y == 1, "Pagou", "NaoPagou"))




##### Ajustando a variável respósta antes de cada caso antes de modelar
df_0_1 <- df 
df_fat <- df
df_fat$y <- as.factor(if_else(df$y == 1, "Pagou", "NaoPagou"))
df_1_1 <- df
df_1_1$y<-if_else(df$y == 1, 1, -1)

### Sep data set in train test
set.seed(201116)
intrain <- createDataPartition(y = df$y,
                               p = 0.7,
                               list = FALSE)


#### COdificado em 0 e 1
df_0_1_train <- df_0_1[intrain, ]
df_0_1_test <-  df_0_1[-intrain, ]
#### COdificado com nome
df_fat_train <- df_fat[intrain, ]
df_fat_test <-  df_fat[-intrain, ]
#### COdificado em -1 e 1
df_1_1_train <- df_1_1[intrain, ]
df_1_1_test <-  df_1_1[-intrain, ]



#######################################################################
#######################################################################
################# FUnção para salvar métricas


resultados <- data.frame(
      "Modelo" = character(0),
      "Acurácia" = numeric(0),
      "Pos pred" = numeric(0),
      "Pos neg" = numeric(0),
      "AUC" = numeric(0)
)



Salvametricas <- function( modelo, matconfusion , perform, resul = resultados){
  #### CAulculando a AUC
  (auc_ROCR <- performance(perform, measure = "auc"))
  (auc_ROCR <- auc_ROCR@y.values[[1]])
  ### linha ja existentes
  n <- nrow(resul)
  ### ERscrevendono data frame
  resul[n+1,] <- c(
    modelo,
    round(matconfusion$overall['Accuracy'],3),
    round(matconfusion$byClass['Pos Pred Value'],3),
    round(matconfusion$byClass['Neg Pred Value'],3),
    round(auc_ROCR,3)
  )
  return(resul)
}





#######################################################################
#######################################################################


##### For logistic regression, there's no need of a cross validation
mod <- glm(y~., family=binomial(link="logit"), data = df_0_1_train)

#fit <- train(y~.,
#             data = df_train,
  #           method="glm", 
   #          family="binomial")
### #


pred <- predict(mod, newdata=subset (df_0_1_test, select = -c(y)), type="response")
#varImp(fit)
conflog <- confusionMatrix(data = as.factor(as.numeric(pred > 0.5)),as.factor(df_0_1_test$y))
#############
conflog


rocpredlog <- prediction(predictions = pred, labels =df_0_1_test$y )
roc_perflog <- performance(rocpredlog , "tpr" , "fpr")
plot(roc_perflog,
     colorize = TRUE,
     #print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))
#(auc_ROCR <- performance(rocpredlog, measure = "auc"))
#(auc_ROCR <- auc_ROCR@y.values[[1]])
resultados <- Salvametricas("Regessão Logística",
              matconfusion = conflog,
              perform = rocpredlog)

resultados
########################################################################
########################### Random Florest #############################
########################################################################


rf <- randomForest(y ~.,ntree=100, data = df_fat_train, importance = TRUE)

pred = predict(rf, newdata=subset (df_fat_test, select = -c(y)))
## Matriz de confusão
#cm = table(df_test[-c(23,24)], pred)
#cm
confarvore <- confusionMatrix(pred,df_fat_test$y)
confarvore


pd = predict(rf, newdata=subset (df_fat_test, select = -c(y)), type ="prob")[,2]

rocpredarv<- prediction(predictions = pd, labels = df_fat_test$y )
roc_perflog <- performance(rocpredarv, "tpr" , "fpr")
plot(roc_perflog,
     colorize = TRUE,
     #print.cutoffs.at= seq(0,1,0.05),
     text.adj=c(-0.2,1.7))


resultados <- Salvametricas("Random Forest",
                            matconfusion = confarvore,
                            perform = rocpredarv)

resultados
######################################################################
######################### SVM ########################################
######################################################################

########## SVM básico sem nenhum kernel 
SVMbasico <- svm(y ~., data = df_fat_train )
summary(SVMbasico)
#plot(SVMbasico, df_1_1_train)
predsvm1 <- predict(SVMbasico, newdata=subset (df_fat_test, select = -c(y)))
confsvmbasico <- confusionMatrix(predsvm1,df_fat_test$y)
confsvmbasico

#predsvm1 <- predict(SVMbasico, newdata=subset (df_fat_test, select = -c(y)))
#rocpredsvmlin<- prediction(predictions = predsvm1, labels = df_fat_test$y )


######## SVM utilizando um tune

tunesvm<- tune(svm, 
               train.x = subset (df_fat_test, select = -c(y)),
               train.y = df_fat_test$y,
               ranges = list(cost=10^(-1:2), gamma=c(0.5, 1,2))
               )
print(tunesvm)
SVMtune <- svm(y ~., data = df_fat_train,  cost=tunesvm$best.parameters[1], gamma= tunesvm$best.parameters[2])
predsvm2 <- predict(SVMtune, newdata=subset (df_fat_test, select = -c(y)))
confsvmtune1<- confusionMatrix(predsvm2,df_fat_test$y)
confsvmtune1

####### SVM utilizando um kernelpolinomial d egrau 2

SVMpoly2 <- svm(y ~., data = df_fat_train, kernel= "polynomial",degree=2 )
predsvm3 <- predict(SVMpoly2, newdata=subset (df_fat_test, select = -c(y)))
confsvmpoly2<- confusionMatrix(predsvm3,df_fat_test$y)
confsvmpoly2


####### SVM utilizando um kernelpolinomial de grau 3

SVMpoly3 <- svm(y ~., data = df_fat_train, kernel= "polynomial",degree=3 )
predsvm4 <- predict(SVMpoly3, newdata=subset (df_fat_test, select = -c(y)))
confsvmpoly3<- confusionMatrix(predsvm4,df_fat_test$y)
confsvmpoly3



### Vamos entar fixar o custo do svm com kernel de grau 3

tuneply3<- tune(svm, 
               train.x = subset (df_fat_test, select = -c(y)),
               train.y = df_fat_test$y,
               kernel= "polynomial",
               degree=3,
               ranges = list(cost=10^(-1:2))
)
print(tuneply3)

SVMpoly3.1 <- svm(y ~., data = df_fat_train, kernel= "polynomial",degree=3, cost=tuneply3$best.parameters[1] )
predsvm5 <- predict(SVMpoly3.1, newdata=subset (df_fat_test, select = -c(y)))
confsvmpoly3.1<- confusionMatrix(predsvm5,df_fat_test$y)
confsvmpoly3.1


##################### Por ultimo, vamos tentar um SVM com o kernel radial
SVMrad <- svm(y ~., data = df_fat_train, kernel= "radial")
predsvm6 <- predict(SVMrad, newdata=subset)
confsvmrad<- confusionMatrix(predsvm6,df_fat_test$y)
confsvmrad

########################################################################
######################## Análise discriminante #########################
########################################################################











########################## Naive bayes ####################

naiveb <- naive_bayes(y ~., data = df_fat_train)
#3plot(naiveb)


predict(naiveb,  subset(df_fat_test, select = -c(y)), type = 'prob')#,  usekernel =  T
#head(cbind(p, df_test))
prednaive <- predict(naiveb,subset(df_fat_test, select = -c(y)))

confnaive <- confusionMatrix(prednaive,df_fat_test$y)
confnaive






########################################
conflog

confarvore

confnaive


## Acurácia
## PRecisão -- pos Pred Value e Neg Pred Value
## AUC 
## Detection Rate
(auc_ROCR <- auc_ROCR@y.values[[1]])

roc_perflog 
(auc_ROCR <- performance(rocpredlog, measure = "auc"))
(auc_ROCR <- auc_ROCR@y.values[[1]])


auc_ROCR@y.values[[1]]


resultados <- data.frame("Acurácia" = numeric(0),
                         "Pos pred" = numeric(0),
                         "Pos neg" = numeric(0),
                         "AUC" = numeric(0)
                         )







Salvametricas <- function( matconfusion , perform, resul = resultados){
  #### CAulculando a AUC
  roc_perflog <- performance(perform , "tpr" , "fpr")
  (auc_ROCR <- performance(rocpredlog, measure = "auc"))
  (auc_ROCR <- auc_ROCR@y.values[[1]])
  ### linha ja existentes
  n <- nrow(resul)
  ### ERscrevendono data frame
  resul[n+1,] <- c(
    conflog$overall['Accuracy'],
    conflog$byClass['Pos Pred Value'],
    conflog$byClass['Neg Pred Value'],
    auc_ROCR
  )
  return(resul)
}

resultados = Salvametricas("Regessão Logística",matconfusion = conflog, perform = rocpredlog)
resultados




#######################################################

