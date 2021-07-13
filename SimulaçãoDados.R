#################################################################
#####################  SImulação dos dados  #####################
#################################################################
library(tidyverse)
library(caret)
if(!require("coxed")) {install.packages("coxed"); library("coxed")}
### Criando um data frame para as variáveis de interesse
n <-  10000
proprietario <- sample(c(0,1), n, T)  # 0 = sim , 1 = Não
TempoRelacionamento <- sample(0:60,n,T)
idade <- sample(18:60,n,T)
ScoreSerasa <- sample(0:1000,n,T)
Quitação <- sample(c("integal","ParceladoExercicio", "ParceladoDA"), n,T)
Pagament <- sample(c("parcial", "sem pagamento"), n, T)
NaturezaDívida <- sample(c("GrandeDevedor","Fraude"), n, T)
AtrasoAnterior  <-  sample(c(0,1), n, T)  # 0 = sim , 1 = Não
RespSolidaria  <-  sample(c(0,1), n, T)
negociaçãoanteior <- sample(c(0,1), n, T)
protestos <- rpois(n, 1)
dividasexecutadas <- rpois(n, 1)
dividasDA <- rpois(n, 1)
regularidadeAcessorias <- sample(c(0,1), n, T) 
BencajudREnajud <- sample(c(0,1), n, T) 
prescricao <- sample(c(0,1), n, T) 
precatResInd <- sample(c(0,1), n, T) 
saldodevedor <- runif(n,0,1)
#max = 0.007 * 347092
## Pensar numa distribuição ou em algo pra com esse máximo

#### colocando num dataFrame

dados= data.frame(
  proprietario , #2
  scale(TempoRelacionamento) , #3
  scale(idade) , #2
  scale(ScoreSerasa),#4
  Quitação ,# integra1 = 1, parcelado DA = -1, , parcelamento parcial = 1
  Pagament ,# parcial = 1.5, -0.5
  NaturezaDívida , # fraude = 3, -2
  AtrasoAnterior  ,# -1
  RespSolidaria ,# 0.5
  negociaçãoanteior, # -1
  scale(protestos) , #-1
  scale(dividasexecutadas), #-2
  scale(dividasDA), # -3
  scale(regularidadeAcessorias),#-1
  scale(BencajudREnajud),#-0.5
  prescricao, # -2
  precatResInd, #-1
  saldodevedor #-2
  
)

dummy <- dummyVars(" ~ .", data=dados)
newdata <- data.frame(predict(dummy, newdata = dados)) 
names(newdata)
#betas <-  c(2,3,2,4,1,-1,1,0.2,0.2,0.3, -2,-0.1,-0.2,-0.1,-0.2,-0.2,-0.1,1, -0.5, -0.3 , 0.5)
betas <-  c(2,3,2,4,1,-1,1,1.5,0.5,3,-2,-1,0.5,-1,-1,-2,-3,-1, -0.5, -2 ,-1,-2.5)
eta = rowSums(mapply("*", newdata, betas))
p = 1 / (1 + exp(-eta))
y = rbinom(n = n, size = 1, prob = p)
newdata$y <- y

write.csv(newdata, "classification.csv")
## Mantendo somente os que pagaram 80% da dívida ao final do período
dados = dados %>% filter( y ==1) %>% subset(select = -y)
## Removendo o Y



summary(as.factor(y))
  
  ###################################################################
  ###################################################################
  
  #if(!require("coxed")) {install.packages("coxed"); library("coxed")}
  #library("coxed")
  
  #Simulacao:
  set.seed(1)
  simdata <- sim.survdata( T=180, num.data.frames=1, X = newdata, beta = betas)
  attributes(simdata)
 #head(simdata$data, 10)
#head(simdata$xdata, 10)#head(simdata$baseline, 10)
 #simdata$betas
  
dados2<- simdata$data
summary(dados2$y)
summary(dados2$failed)

