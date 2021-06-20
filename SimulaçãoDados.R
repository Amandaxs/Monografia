#################################################################
#####################  SImulação dos dados  #####################
#################################################################

library(caret)

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
  #QuantDIvidas <- rnorm(n)
  NaturezaDívida ,
  AtrasoAnterior  ,
  RespSolidaria ,
  negociaçãoanteior,
  scale(protestos) ,
  scale(dividasexecutadas),
  scale(dividasDA) 
  
  
)


dummy <- dummyVars(" ~ .", data=dados)
newdata <- data.frame(predict(dummy, newdata = dados)) 


#### Tendo criado as variáveis, vamos fazer a simulação apra a regressão
#logística.

eta = (
  2 + 
  proprietario * 1 +
  scale(TempoRelacionamento) * 2 +
  scale(idade) * -1 +
  scale(ScoreSerasa) * 4 +
  #Quitação <- sample(c("integal","Parcelado"), n,T)
  #Pagament <- sample(c("parcial", "sem pagamento"), n, T)
  #QuantDIvidas <- rnorm(n)
  #NaturezaDívida <- sample(c("GrandeDevedor","Fraude"), n, T)
  AtrasoAnterior * 2 + 
  RespSolidaria * -2 +
  negociaçãoanteior * 2 +
  scale(protestos) * 3 +
  scale(dividasexecutadas) * 3 +
  scale(dividasDA) * 3
)
#sim_logistic_data = function(sample_size = 25, beta_0 = 1, beta_1 = 3) {
 # x = rnorm(n = sample_size)
 # eta = beta_0 + beta_1 * x
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = n, size = 1, prob = p)
  #data.frame(y, x)
#}

  summary(as.factor(y))

  
  
  ###################################################################
  ###################################################################
  
  if(!require("coxed")) {install.packages("coxed"); library("coxed")}
  library("coxed")
  #Simulacao:
  set.seed(1)
  simdata <- sim.survdata( T=180, num.data.frames=1, X = newdata)
  attributes(simdata)
  head(simdata$data, 10)
  head(simdata$xdata, 10)
  head(simdata$baseline, 10)
  simdata$betas
  
dados2<- simdata$data
summary(dados2$y)
summary(dados2$failed)
