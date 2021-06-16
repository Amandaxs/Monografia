#################################################################
#####################  SImulação dos dados  #####################
#################################################################

### Criando um data frame para as variáveis de interesse
n = 100
proprietario = sample(c(0,1), n, T)  # 0 = sim , 1 = Não
TempoRelacionamento = sample(0:60,n,T)
idade = sample(18:60,n,T)
ScoreSerasa = sample(0:1000,n,T)
Quitação = sample(c("integal","Parcelado"), n,T)
Pagament = sample(c("parcial", "sem pagamento"), n, T)
QuantDIvidas = rnorm(n)




sim_logistic_data = function(sample_size = 25, beta_0 = 1, beta_1 = 3) {
  x = rnorm(n = sample_size)
  eta = beta_0 + beta_1 * x
  p = 1 / (1 + exp(-eta))
  y = rbinom(n = sample_size, size = 1, prob = p)
  data.frame(y, x)
}

