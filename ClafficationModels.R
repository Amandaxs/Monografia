##############################################################################
######### Ajuste de modelos para prever um bom ou mal devedor ################
##############################################################################
library(tidyverse)
library(caret)

library(yardstick)
library(randomForest)
library(ROCR)
library(e1071)
library(naivebayes)
############
library(lattice)
library(latticeExtra)
library(MASS)
library(ellipse)
library(mvtnorm)



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


#saveRDS(mod, "modeloLogistico.rds")
#mod <- readRDS("modeloLogistico.rds")

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
     colorize = TRUE
     #print.cutoffs.at= seq(0,1,0.05),
    )


resultados <- Salvametricas("Random Forest",
                            matconfusion = confarvore,
                            perform = rocpredarv)

resultados
######################################################################
######################### SVM ########################################
######################################################################

########## SVM básico sem nenhum kernel 
SVMbasico <- svm(y ~., data = df_fat_train , probability = T)
summary(SVMbasico)
#plot(SVMbasico, df_1_1_train)
predsvm1 <- predict(SVMbasico, newdata=subset (df_fat_test, select = -c(y)))
confsvmbasico <- confusionMatrix(predsvm1,df_fat_test$y)
confsvmbasico

#predsvm1 <- predict(SVMbasico, newdata=subset (df_fat_test, select = -c(y)))
#rocpredsvmlin<- prediction(predictions = predsvm1, labels = df_fat_test$y )


x.svm.prob <- predict(SVMbasico,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)
x.svm.prob.rocr <- prediction(attr(x.svm.prob, "probabilities")[,2], df_fat_test$y )
x.svm.perf <- performance(x.svm.prob.rocr, "tpr","fpr")
plot(x.svm.perf, colorize = TRUE)#, add=TRUE


resultados <- Salvametricas("SVM LInear simples",
                            matconfusion = confsvmbasico,
                            perform = x.svm.prob.rocr)
resultados


######## SVM utilizando um tune

tunesvm<- tune(svm, 
               train.x = subset (df_fat_test, select = -c(y)),
               train.y = df_fat_test$y,
               ranges = list(cost=10^(-1:2))
               )
print(tunesvm)
#coste =10
SVMtune <- svm(y ~., data = df_fat_train,  cost=tunesvm$best.parameters[1], probability =  T)
predsvm2 <- predict(SVMtune, newdata=subset (df_fat_test, select = -c(y)))
confsvmtune1<- confusionMatrix(predsvm2,df_fat_test$y)
confsvmtune1

## Curva roc e salvando resultados
x.svm.prob1 <- predict(SVMtune,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)
x.svm.prob.rocr1 <- prediction(attr(x.svm.prob1, "probabilities")[,2], df_fat_test$y )
x.svm.perf <- performance(x.svm.prob.rocr1, "tpr","fpr")
plot(x.svm.perf, colorize = TRUE)#, add=TRUE


resultados <- Salvametricas("SVM com custo = 10",
                            matconfusion = confsvmtune1,
                            perform = x.svm.prob.rocr1)
resultados

####### SVM utilizando um kernelpolinomial d egrau 2

SVMpoly2 <- svm(y ~., data = df_fat_train, kernel= "polynomial",degree=2, probability= T )
predsvm3 <- predict(SVMpoly2, newdata=subset (df_fat_test, select = -c(y)))
confsvmpoly2<- confusionMatrix(predsvm3,df_fat_test$y)
confsvmpoly2

## Curva roc e salvando resultados
x.svm.prob2 <- predict(SVMpoly2,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)
x.svm.prob.rocr2 <- prediction(attr(x.svm.prob2, "probabilities")[,2], df_fat_test$y )
x.svm.perf <- performance(x.svm.prob.rocr2, "tpr","fpr")
plot(x.svm.perf, colorize = TRUE)#, add=TRUE


resultados <- Salvametricas("SVM plinomial grau 2",
                            matconfusion = confsvmpoly2,
                            perform = x.svm.prob.rocr2)
resultados

####### SVM utilizando um kernelpolinomial de grau 3

SVMpoly3 <- svm(y ~., data = df_fat_train, kernel= "polynomial",degree=3, probability= T )
predsvm4 <- predict(SVMpoly3, newdata=subset (df_fat_test, select = -c(y)))
confsvmpoly3<- confusionMatrix(predsvm4,df_fat_test$y)
confsvmpoly3

## Curva roc e salvando resultados
x.svm.prob3 <- predict(SVMpoly3,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)
x.svm.prob.rocr3 <- prediction(attr(x.svm.prob3, "probabilities")[,2], df_fat_test$y )
x.svm.perf <- performance(x.svm.prob.rocr3, "tpr","fpr")
plot(x.svm.perf, colorize = TRUE)#, add=TRUE


resultados <- Salvametricas("SVM plinomial grau 3",
                            matconfusion = confsvmpoly3,
                            perform = x.svm.prob.rocr3)
resultados




### Vamos entar fixar o custo do svm com kernel de grau 3
### Não compensoou por
tuneply3<- tune(svm, 
               train.x = subset (df_fat_test, select = -c(y)),
               train.y = df_fat_test$y,
               kernel= "polynomial",
               degree=3,
               ranges = list(cost=10^(-1:2))
)
print(tuneply3)
## Custo 1
SVMpoly3.1 <- svm(y ~., data = df_fat_train, kernel= "polynomial",degree=3, cost=tuneply3$best.parameters[1], probability = T )
predsvm5 <- predict(SVMpoly3.1, newdata=subset (df_fat_test, select = -c(y)))
confsvmpoly3.1<- confusionMatrix(predsvm5,df_fat_test$y)
confsvmpoly3.1


x.svm.prob3.1 <- predict(SVMpoly3.1,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)
x.svm.prob.rocr3.1 <- prediction(attr(x.svm.prob3.1, "probabilities")[,2], df_fat_test$y )
x.svm.perf <- performance(x.svm.prob.rocr3.1, "tpr","fpr")
plot(x.svm.perf, colorize = TRUE)#, add=TRUE


#resultados <- Salvametricas("SVM plinomial grau 3 e custo 1",
#                            matconfusion = confsvmpoly3.1,
#                            perform = x.svm.prob.rocr3.1)
#resultados


##################### Por ultimo, vamos tentar um SVM com o kernel radial
SVMrad <- svm(y ~., data = df_fat_train, kernel= "radial", probability = T)
predsvm6 <- predict(SVMrad, newdata=subset (df_fat_test, select = -c(y)))
confsvmrad<- confusionMatrix(predsvm6,df_fat_test$y)
confsvmrad


## Curva roc e salvando resultados
x.svm.prob6 <- predict(SVMrad,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)
x.svm.prob.rocr6 <- prediction(attr(x.svm.prob6, "probabilities")[,2], df_fat_test$y )
x.svm.perf <- performance(x.svm.prob.rocr6, "tpr","fpr")
plot(x.svm.perf, colorize = TRUE)#, add=TRUE


resultados <- Salvametricas("SVM radial",
                            matconfusion = confsvmrad,
                            perform = x.svm.prob.rocr6)
resultados


########################################################################
######################## Análise discriminante #########################
########################################################################

############################## Linear  #################################



fit <- lda(y ~., data = df_fat_train)
#fit
preddisc <- predict(fit, newdata=subset (df_fat_test, select = -c(y)), type="response")
confdisc<- confusionMatrix(preddisc$class,df_fat_test$y)
confdisc
## Curva roc e salvando resultados
#x.svm.prob7 <- predict(fit,type = "prob", newdata=subset (df_fat_test, select = -c(y)), probability =  T)

pred <- prediction(preddisc$posterior[,2], df_fat_test$y) 
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

resultados <- Salvametricas("análise discriminante Lienar",
                            matconfusion = confdisc,
                            perform = pred)
resultados


########################## Naive bayes ####################

naiveb <- naive_bayes(y ~., data = df_fat_train)
#3plot(naiveb)


p2<- predict(naiveb,  subset(df_fat_test, select = -c(y)), type = 'prob')#,  usekernel =  T
#head(cbind(p, df_test))
prednaive <- predict(naiveb,subset(df_fat_test, select = -c(y)))

confnaive <- confusionMatrix(prednaive,df_fat_test$y)
confnaive


pred <- prediction(p2[,2], df_fat_test$y) 
perf <- performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)

resultados <- Salvametricas("Naive Bayes",
                            matconfusion = confnaive,
                            perform = pred)
resultados



########################################

plotamatriz <- function(matriz){
mat<- matriz$table %>% 
    as.data.frame() %>%
    ggplot(aes(x = Prediction, y= Reference, fill= Freq))+
      geom_tile()+
      coord_equal() +
      scale_fill_distiller(palette="Blues", direction=1) +
      geom_text(aes(label=Freq), color="black")+
      theme_minimal()
  return(mat)
}
mat<- plotamatriz(conflog)
mat
#data.frame("predito" = prednaive, "real" =df_fat_test$y)
