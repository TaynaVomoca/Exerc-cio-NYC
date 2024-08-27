######################
#PACOTES
install.packages("readr") 
library(readr)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("corrplot")
library("corrplot")
########################

#a)
#MATRIZ DE CORRELAÇÃO
#Removendo das duas coluna primeiras colunas
nyc_2 <- nyc[, -c(1,2)]

#matriz de correlações
M <- cor(nyc_2)

#gráfico para a matriz de correlações
corrplot(M, method = 'number')

#b)
#DIAGRAMA DE DISPERSÃO

ggplot(data = nyc_2, aes(x = Food, y = Price)) + geom_point(size = 3) +
  theme_classic() + xlab("Avaliação da comida") + ylab("Preço") + geom_smooth(method = "lm", se = FALSE)

ggplot(data = nyc_2, aes(x = Decor, y = Price)) + geom_point(size = 3) +
  theme_classic() + xlab("Decoração") + ylab("Preço") + geom_smooth(method = "lm", se = FALSE)

ggplot(data = nyc_2, aes(x = Service, y = Price)) + geom_point(size = 3) +
  theme_classic() + xlab("Serviço") + ylab("Preço") + geom_smooth(method = "lm", se = FALSE)

#COEFICIENTE DE CORRELAÇÃO LINEAR

cor(nyc_2$Food, nyc_2$Price)
cor(nyc_2$Decor, nyc_2$Price)
cor(nyc_2$Service, nyc_2$Price)

#c)
#MODELO DE REGRESSÃO LINEAR SIMPLES

modelo_price <- lm(formula = Price ~ Food, data = nyc_2)
summary(modelo_price)

modelo_decor <- lm(formula = Price ~ Decor, data = nyc_2)
summary(modelo_decor)

modelo_service <- lm(formula = Price ~ Service, data = nyc_2)
summary(modelo_service)

#D)

nyc %>% group_by(East) %>% summarise(media = mean(Price))

dados_east <- nyc_2 %>% filter(East == "1")
dados_west <- nyc_2 %>% filter(East == "0")

#east
mean(dados_east$Price, na.rm = TRUE)#Média
sd(dados_east$Price, na.rm = TRUE)#desvio-padrao
median(dados_east$Price, na.rm = TRUE) #mediana


#west
mean(dados_west$Price, na.rm = TRUE)#Média
sd(dados_west$Price, na.rm = TRUE)#desvio-padrao
median(dados_west$Price, na.rm = TRUE) #mediana


boxplot(nyc_2$Price ~ nyc_2$East)#boxplot

#e
#MODELO DE REGRESSÃO
#east
modelo_nyc <- lm(formula = Price ~ Food, data = dados_east)
summary(modelo_nyc)

modelo_nyc <- lm(formula = Price ~ Decor, data = dados_east)
summary(modelo_nyc)

modelo_nyc <- lm(formula = Price ~ Service, data = dados_east)
summary(modelo_nyc)

#west
modelo_nyc <- lm(formula = Price ~ Food, data = dados_west)
summary(modelo_nyc)

modelo_nyc <- lm(formula = Price ~ Decor, data = dados_west)
summary(modelo_nyc)

modelo_nyc <- lm(formula = Price ~ Service, data = dados_west)
summary(modelo_nyc)

#f
modelo_completo <- lm(formula = Price ~ Food + Decor + East , data = nyc_2)
summary(modelo_completo)

plot(modelo_nyc)

#g)ANALIS DOS RESÍDUOS
#os reíduos evem ter media 0
t.test(modelo_completo$residuals, mu = 0)
#se valor-p for maior que 0 nao ha evidencias para rejeitar a hipótese media 0

#os residuos evem ter distribuição normal
#usando qqnorm
qqnorm(modelo_completo$residuals)
qqline(modelo_completo$residuals)
# os residuos parecem seguir a distribuição normal

shapiro.test(modelo_completo$residuals)
# o teste de shpiro reforça que a distribuição parece normal
#Teste de hapiro: Se valor-p for maior que 0 nao ha evidencis para regeitar a hipotese de distribuição normal

#homocedasticidade
plot(modelo_completo$fitted.values, modelo_completo$residuals)
abline(0,0)

#h) 0 que mais influencia e Food, decor e east

