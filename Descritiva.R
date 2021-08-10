############################################################################
########################## Análise descritiva ##############################
############################################################################

require(tidyverse)
require(scales)

df <- read.csv("classification.csv")
#df$y %>% as.factor %>% summary  #
df$y<- as.factor(if_else(df$y == 1, "Pagou", "Não Pagou"))

df %>%
  select(y) %>%
  group_by(y) %>%
  summarise(n =n()) %>%
  mutate(perc = n/sum(n))%>%
ggplot(aes(x=y, y = n,
             label = paste(scales::percent(perc), " (", n,")")))+
  geom_bar(fill= "#254864", stat = "identity") +
  geom_text(position = position_dodge(.9), vjust = -0.3)+
  labs(y= 'Quantidade',
       x = " ",
      title = "Quantidade de inadimplentes por status de pagamento")+
  theme_classic()

