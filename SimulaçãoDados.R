#################################################################
#####################  SImulação dos dados  #####################
#################################################################
library(tidyverse)
library(caret)
if(!require("coxed")) {install.packages("coxed"); library("coxed")}
### Criando um data frame para as variáveis de interesse
n <-  1000
proprietario <- sample(c(0,1), n, T)  # 0 = sim , 1 = Não
TempoRelacionamento <- sample(0:60,n,T)
idade <- sample(18:60,n,T)
ScoreSerasa <- sample(0:1000,n,T)
Quitação <- sample(c("integal","ParceladoExercicio", "ParceladoDA"), n,T)
Pagament <- sample(c("parcial", "sem pagamento"), n, T)
#QuantDIvidas <- rnorm(n)
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
### Para saldo devedor, vamos escolher uma faixa e calcular o máximo com base
#nesse valor
#max = 0.007 * 347092
## Pensar numa distribuição ou em algo pra com esse máximo

#### colocando num dataFrame

dados= data.frame(
  proprietario ,
  scale(TempoRelacionamento) ,
  scale(idade) ,
  scale(ScoreSerasa),
  Quitação ,
  Pagament ,
  NaturezaDívida ,
  AtrasoAnterior  ,
  RespSolidaria ,
  negociaçãoanteior,
  scale(protestos) ,
  scale(dividasexecutadas),
  scale(dividasDA),
  scale(regularidadeAcessorias),
  scale(BencajudREnajud),
  prescricao,
  precatResInd 
  
)


dummy <- dummyVars(" ~ .", data=dados)
newdata <- data.frame(predict(dummy, newdata = dados)) 
names(newdata)
betas <-  c(0.2,0.25,0.2,0.4,1,-1,1,0.2,0.2,0.3, -0.2,-0.1,-0.2,-0.1,-0.2,-0.2,-0.1,1, -0.5, -0.3 , 0.5)

eta = rowSums(mapply("*", newdata, betas))
p = 1 / (1 + exp(-eta))
y = rbinom(n = n, size = 1, prob = p)


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
