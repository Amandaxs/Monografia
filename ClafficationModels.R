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



#################################


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




########################################################################
########################### Random Florest #############################
########################################################################


library(randomForest)

rf <- randomForest(y ~.,ntree=100, data = df_fat_train, importance = TRUE)

pred = predict(rf, newdata=subset (df_fat_test, select = -c(y)))
## Matriz de confusão
#cm = table(df_test[-c(23,24)], pred)
#cm
confarvore <- confusionMatrix(pred,df_fat_test$y)
confarvore


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














#######################################################

