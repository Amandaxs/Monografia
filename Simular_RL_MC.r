# Simulacao do Modelo de Regressao Logistica:

sim_logistic_data = function(sample_size = 25, beta_0 = -2, beta_1 = 3) {
  x = rnorm(n = sample_size)
  eta = beta_0 + beta_1 * x
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}

set.seed(1)
example_data = sim_logistic_data()
head(example_data)

#Ajuste dos dados simulados:
modelb <- glm(y ~ x, data=example_data, family=binomial)
modelb$coefficients



# Simulacao do Modelo de Cox:
if(!require("coxed")) {install.packages("coxed"); library("coxed")}
library("coxed")
#Simulacao:
set.seed(1)
simdata <- sim.survdata(N=1000, T=100, num.data.frames=1)
attributes(simdata)
head(simdata$data, 10)
head(simdata$xdata, 10)
head(simdata$baseline, 10)
simdata$betas

#Ajuste dos dados simulados:
modelc <- coxph(Surv(y, failed) ~ X1 + X2 + X3, data=simdata$data)
modelc$coefficients
head(cbind(simdata$xb, simdata$exp.xb))
simdata$ind.survive[1,]



