#### Growth model COVID-19 CODE #######

#### Calling COVID-19 external data #######

library(varhandle)

dados_world<-unfactor(read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")) 
dados_world$date<-as.Date(dados_world$date)
diff(as.Date(sort(unique(dados_world$date))))

library(plyr)

dados_world<-plyr::arrange(dados_world,dados_world$date)

dia_de_previsao<-Sys.Date()-2

#### Data set considered in past information (from 25 weeks ago until now) #######

quantidade_de_semanas<-25
dias_de_previsao<-rep(0,quantidade_de_semanas+1)
dias_de_previsao[1]<-dia_de_previsao

d<-1
for (d in 2:length(dias_de_previsao)) {
  ifelse(length(dias_de_previsao)==1,dias_de_previsao<-dia_de_previsao,dias_de_previsao[d]<-dias_de_previsao[d-1]-7)
}

dias_de_previsao<-as.Date(dias_de_previsao, origin = "1970-01-01")

#### Procedure to run all over the weeks all growth models for each country #######

resultados<-NA
resultados_erro<-NA

Tempo_de_analise<-1000

d<-1
for (d in 1:length(dias_de_previsao)) {
  
dados<-subset(dados_world,dados_world$date<=dias_de_previsao[d])

i=1
for (i in 1:nrow(dados)) {
  if(dados$total_cases[i]<0 | is.na(dados$total_cases[i])) {dados$total_cases[i]<-0}
  if(dados$total_deaths[i]<0 | is.na(dados$total_deaths[i])) {dados$total_deaths[i]<-0}
}

dados[which(dados$total_cases<0),]
dados[which(dados$total_deaths<0),]
dados[is.na(dados$total_cases),]
dados[is.na(dados$total_deaths),]

cidades_afetadas <- unique(plyr::arrange(dados,desc(dados$total_cases))$location)[-1]
cidades_mais_afetadas <- sort(c("Brazil","Canada", "United States", "South Korea", "New Zealand", "Italy", "Spain" , "United Kingdom"))  #cidades_afetadas[1:8]

populacao<-rep(0,length(cidades_mais_afetadas))
contaminados_atuais<-rep(0,length(cidades_mais_afetadas))
primeiro_caso<-rep(NA,length(cidades_mais_afetadas))
p<-1
for (p in 1:length(cidades_mais_afetadas)) {
  populacao[p]<- unique(subset(dados_world,dados_world$location==cidades_mais_afetadas[p])$population)
  contaminados_atuais[p]<- max(subset(dados_world,dados_world$location==cidades_mais_afetadas[p])$total_cases,na.rm = TRUE)
  primeiro_caso[p]<- min(subset(dados_world,dados_world$location==cidades_mais_afetadas[p])$date) - 1 +
  which(subset(dados_world,dados_world$location==cidades_mais_afetadas[p])$total_cases>0)[1]
}
primeiro_caso<-as.Date(primeiro_caso, origin = "1970-01-01")


R_quadrado_cidades <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
RMSE_cidades <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
pico_cidades <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
fim_cidades <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
pico_cidades_normalizado <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
fim_cidades_normalizado <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
pop_afetada <- matrix(NA,nrow = 17, ncol = length(cidades_mais_afetadas))
proporcao_fim <- 0.01

cidade <- 1

for (cidade in 1:length(cidades_mais_afetadas)) {
  
dados_rj<-subset(dados,
                 dados$location==cidades_mais_afetadas[cidade]
)

i=1
for (i in 2:nrow(dados_rj)) {
  if(dados_rj$total_cases[i]<dados_rj$total_cases[i-1]) {dados_rj$total_cases[i]<-dados_rj$total_cases[i-1]}
  if(dados_rj$total_deaths[i]<dados_rj$total_deaths[i-1]) {dados_rj$total_deaths[i]<-dados_rj$total_deaths[i-1]}
}


library(xts)

series_rj<-xts(dados_rj[,c("total_cases","total_deaths")], order.by = seq.Date(min(dados_rj$date), as.Date(min(dados_rj$date)+nrow(dados_rj)-1),by ="day"))
colnames(series_rj)<-c("casos", "mortes")


plot.xts(series_rj, grid.col = NA, main = paste0("Evolução da quantidade total de casos e mortes ",unique(dados_rj$location)));xts::addLegend(legend.loc = "topleft", legend.names = colnames(series_rj), lty=1,bg="lightblue")
plot.xts(diff(series_rj), grid.col = NA, main = paste0("Quantidade de casos de casos e mortes ",unique(dados_rj$location)));xts::addLegend(legend.loc = "topleft", legend.names = colnames(series_rj), lty=1,bg="lightblue")


serie_rj<-series_rj$casos


plot(c(as.numeric(serie_rj[1]),as.numeric(na.omit(diff(serie_rj)))),type = "l")
#lines(smoother::smth(c(as.numeric(serie_rj[1]),as.numeric(na.omit(diff(serie_rj)))), window = 7, tails = TRUE),type = "l",col =2)
#plot(stl(ts(smoother::smth(c(as.numeric(serie_rj[1]),as.numeric(na.omit(diff(serie_rj)))), window = 7, tails = TRUE),frequency = 7),s.window = "periodic"))
#plot(stl(ts(c(as.numeric(serie_rj[1]),as.numeric(na.omit(diff(serie_rj)))),frequency = 7),s.window = "periodic"))

library(growthmodels)

x = 1:length(serie_rj)
y = as.numeric(serie_rj)

df <- data.frame(x, y)

min0 = min(y); max0 = max(y)*2; beta0 = 0.1; k0 = 0.01; m0 = 0.9; t0 = 0; gamma0 = 0.1

library(MLmetrics)

{
modelo<-NA
  tryCatch(
    modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::chapmanRichards(t = 1:Tempo_de_analise,alpha,beta,k,m)[x]), data = df, 
                                start = list(alpha = max0,beta = beta0,k = k0,m = m0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                                upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
    , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[1,cidade]<-NA,
       R_quadrado_cidades[1,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[1,cidade]<-NA,
       RMSE_cidades[1,cidade]<- RMSE(diff(growthmodels::chapmanRichards(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[1,cidade]<-NA,
       pico_cidades[1,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::chapmanRichards(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[1,cidade]<-NA,
       pico_cidades_normalizado[1,cidade]<- which.max(diff(growthmodels::chapmanRichards(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[1,cidade]<-NA,
       pop_afetada[1,cidade]<- max(growthmodels::chapmanRichards(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[1,cidade]<-NA,
       fim_cidades[1,cidade]<- min(index(serie_rj))+which(growthmodels::chapmanRichards(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[1,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[1,cidade]<-NA,
       fim_cidades_normalizado[1,cidade]<- which(growthmodels::chapmanRichards(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[1,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::gompertz(t = 1:Tempo_de_analise,alpha,beta,k)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[2,cidade]<-NA,
       R_quadrado_cidades[2,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[2,cidade]<-NA,
       RMSE_cidades[2,cidade]<- RMSE(diff(growthmodels::gompertz(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[2,cidade]<-NA,
       pico_cidades[2,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::gompertz(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pico_cidades[2,cidade]<-NA,
       pico_cidades_normalizado[2,cidade]<- which.max(diff(growthmodels::gompertz(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pop_afetada[2,cidade]<-NA,
       pop_afetada[2,cidade]<- max(growthmodels::gompertz(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))
)
ifelse(is.na(modelo), fim_cidades[2,cidade]<-NA,
       fim_cidades[2,cidade]<- min(index(serie_rj))+which(growthmodels::gompertz(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[2,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[2,cidade]<-NA,
       fim_cidades_normalizado[2,cidade]<- which(growthmodels::gompertz(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[2,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::loglogistic(t = 1:Tempo_de_analise,alpha,beta,k)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[3,cidade]<-NA,
       R_quadrado_cidades[3,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[3,cidade]<-NA,
       RMSE_cidades[3,cidade]<- RMSE(diff(growthmodels::loglogistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[3,cidade]<-NA,
       pico_cidades[3,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::loglogistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pico_cidades[3,cidade]<-NA,
       pico_cidades_normalizado[3,cidade]<- which.max(diff(growthmodels::loglogistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pop_afetada[3,cidade]<-NA,
       pop_afetada[3,cidade]<- max(growthmodels::loglogistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))
)
ifelse(is.na(modelo), fim_cidades[3,cidade]<-NA,
       fim_cidades[3,cidade]<- min(index(serie_rj))+which(growthmodels::loglogistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[3,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[3,cidade]<-NA,
       fim_cidades_normalizado[3,cidade]<- which(growthmodels::loglogistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[3,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::generalisedRichard(t = 1:Tempo_de_analise,A,U,k,m,beta,t0)[x]), data = df, 
                              start = list(A=min0,U = max0,k = k0,m = m0,beta = beta0,t0=t0),algorithm = "LM", trace = T, lower = c(0,0,-Inf,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],populacao[cidade],Inf,Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[4,cidade]<-NA,
       R_quadrado_cidades[4,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[4,cidade]<-NA,
       RMSE_cidades[4,cidade]<- RMSE(diff(growthmodels::generalisedRichard(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4],
                                                                           beta = coef(modelo)[5], t0 = coef(modelo)[6]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[4,cidade]<-NA,
       pico_cidades[4,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::generalisedRichard(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4],
                                                                                                     beta = coef(modelo)[5], t0 = coef(modelo)[6])))
)
ifelse(is.na(modelo), pico_cidades[4,cidade]<-NA,
       pico_cidades_normalizado[4,cidade]<- which.max(diff(growthmodels::generalisedRichard(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4],
                                                                                                     beta = coef(modelo)[5], t0 = coef(modelo)[6])))
)
ifelse(is.na(modelo), pop_afetada[4,cidade]<-NA,
       pop_afetada[4,cidade]<- max(growthmodels::generalisedRichard(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4],
                                                                    beta = coef(modelo)[5], t0 = coef(modelo)[6]))
)
ifelse(is.na(modelo), fim_cidades[4,cidade]<-NA,
       fim_cidades[4,cidade]<- min(index(serie_rj))+which(growthmodels::generalisedRichard(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4],
                                                                      beta = coef(modelo)[5], t0 = coef(modelo)[6])>=(1-proporcao_fim)*pop_afetada[4,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[4,cidade]<-NA,
       fim_cidades_normalizado[4,cidade]<- which(growthmodels::generalisedRichard(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4],
                                                                                           beta = coef(modelo)[5], t0 = coef(modelo)[6])>=(1-proporcao_fim)*pop_afetada[4,cidade])[1]
)


modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::schnute(t = 1:Tempo_de_analise,r0,beta,k,m)[x]), data = df, 
                              start = list(r0=min0,beta = beta0,k = k0,m = m0),algorithm = "LM", trace = T, lower = c(-Inf,-Inf,-Inf,-Inf), 
                              upper = c(Inf,Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[5,cidade]<-NA,
       R_quadrado_cidades[5,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[5,cidade]<-NA,
       RMSE_cidades[5,cidade]<- RMSE(diff(growthmodels::schnute(t = 1:Tempo_de_analise, r0 = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[5,cidade]<-NA,
       pico_cidades[5,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::schnute(t = 1:Tempo_de_analise, r0 = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[5,cidade]<-NA,
       pico_cidades_normalizado[5,cidade]<- which.max(diff(growthmodels::schnute(t = 1:Tempo_de_analise, r0 = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[5,cidade]<-NA,
       pop_afetada[5,cidade]<- max(growthmodels::schnute(t = 1:Tempo_de_analise, r0 = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[5,cidade]<-NA,
       fim_cidades[5,cidade]<- min(index(serie_rj))+which(growthmodels::schnute(t = 1:Tempo_de_analise, r0 = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[5,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[5,cidade]<-NA,
       fim_cidades_normalizado[5,cidade]<- which(growthmodels::schnute(t = 1:Tempo_de_analise, r0 = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[5,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::stannard(t = 1:Tempo_de_analise,alpha,beta,k,m)[x]), data = df, 
                              start = list(alpha = max(y)*2,beta = beta0,k = k0,m = m0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[6,cidade]<-NA,
       R_quadrado_cidades[6,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[6,cidade]<-NA,
       RMSE_cidades[6,cidade]<- RMSE(diff(growthmodels::stannard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[6,cidade]<-NA,
       pico_cidades[6,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::stannard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[6,cidade]<-NA,
       pico_cidades_normalizado[6,cidade]<- which.max(diff(growthmodels::stannard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[6,cidade]<-NA,
       pop_afetada[6,cidade]<- max(growthmodels::stannard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[6,cidade]<-NA,
       fim_cidades[6,cidade]<- min(index(serie_rj))+which(growthmodels::stannard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[6,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[6,cidade]<-NA,
       fim_cidades_normalizado[6,cidade]<- which(growthmodels::stannard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[6,cidade])[1]
)


modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::monomolecular(t = 1:Tempo_de_analise,alpha,beta,k)[x]), data = df, 
                              start = list(alpha =max0,beta = beta0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[7,cidade]<-NA,
       R_quadrado_cidades[7,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[7,cidade]<-NA,
       RMSE_cidades[7,cidade]<- RMSE(diff(growthmodels::monomolecular(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[7,cidade]<-NA,
       pico_cidades[7,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::monomolecular(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pico_cidades[7,cidade]<-NA,
       pico_cidades_normalizado[7,cidade]<- which.max(diff(growthmodels::monomolecular(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pop_afetada[7,cidade]<-NA,
       pop_afetada[7,cidade]<- max(growthmodels::monomolecular(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))
)
ifelse(is.na(modelo), fim_cidades[7,cidade]<-NA,
       fim_cidades[7,cidade]<- min(index(serie_rj))+which(growthmodels::monomolecular(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[7,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[7,cidade]<-NA,
       fim_cidades_normalizado[7,cidade]<- which(growthmodels::monomolecular(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[7,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise,A,U,k,beta,t0)[x]), data = df, 
                              start = list(A=min0,U = max0,k = k0,beta = beta0,t0=t0),algorithm = "LM", trace = T, lower = c(0,0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[8,cidade]<-NA,
       R_quadrado_cidades[8,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[8,cidade]<-NA,
       RMSE_cidades[8,cidade]<- RMSE(diff(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3],
                                                                            beta = coef(modelo)[4], t0 = coef(modelo)[5]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[8,cidade]<-NA,
       pico_cidades[8,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3],
                                                                                                      beta = coef(modelo)[4], t0 = coef(modelo)[5])))
)
ifelse(is.na(modelo), pico_cidades[8,cidade]<-NA,
       pico_cidades_normalizado[8,cidade]<- which.max(diff(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3],
                                                                                                      beta = coef(modelo)[4], t0 = coef(modelo)[5])))
)
ifelse(is.na(modelo), pop_afetada[8,cidade]<-NA,
       pop_afetada[8,cidade]<- max(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3],
                                                                     beta = coef(modelo)[4], t0 = coef(modelo)[5]))
)
ifelse(is.na(modelo), fim_cidades[8,cidade]<-NA,
       fim_cidades[8,cidade]<- min(index(serie_rj))+which(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3],
                                                                       beta = coef(modelo)[4], t0 = coef(modelo)[5])>=(1-proporcao_fim)*pop_afetada[8,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[8,cidade]<-NA,
       fim_cidades_normalizado[8,cidade]<- which(growthmodels::generalisedLogistic(t = 1:Tempo_de_analise, A = coef(modelo)[1], U = coef(modelo)[2], k = coef(modelo)[3],
                                                                                            beta = coef(modelo)[4], t0 = coef(modelo)[5])>=(1-proporcao_fim)*pop_afetada[8,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::mitcherlich(t = 1:Tempo_de_analise,alpha,beta,k)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[9,cidade]<-NA,
       R_quadrado_cidades[9,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[9,cidade]<-NA,
       RMSE_cidades[9,cidade]<- RMSE(diff(growthmodels::mitcherlich(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[9,cidade]<-NA,
       pico_cidades[9,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::mitcherlich(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pico_cidades[9,cidade]<-NA,
       pico_cidades_normalizado[9,cidade]<- which.max(diff(growthmodels::mitcherlich(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pop_afetada[9,cidade]<-NA,
       pop_afetada[9,cidade]<- max(growthmodels::mitcherlich(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))
)
ifelse(is.na(modelo), fim_cidades[9,cidade]<-NA,
       fim_cidades[9,cidade]<- min(index(serie_rj))+which(growthmodels::mitcherlich(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[9,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[9,cidade]<-NA,
       fim_cidades_normalizado[9,cidade]<- which(growthmodels::mitcherlich(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[9,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::brody(t = 1:Tempo_de_analise,alpha,w0,k)[x]), data = df, 
                              start = list(alpha = max0,w0 = min0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[10,cidade]<-NA,
       R_quadrado_cidades[10,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[10,cidade]<-NA,
       RMSE_cidades[10,cidade]<- RMSE(diff(growthmodels::brody(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], k = coef(modelo)[3]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[10,cidade]<-NA,
       pico_cidades[10,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::brody(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pico_cidades[10,cidade]<-NA,
       pico_cidades_normalizado[10,cidade]<- which.max(diff(growthmodels::brody(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pop_afetada[10,cidade]<-NA,
       pop_afetada[10,cidade]<- max(growthmodels::brody(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], k = coef(modelo)[3]))
)
ifelse(is.na(modelo), fim_cidades[10,cidade]<-NA,
       fim_cidades[10,cidade]<- min(index(serie_rj))+which(growthmodels::brody(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[10,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[10,cidade]<-NA,
       fim_cidades_normalizado[10,cidade]<- which(growthmodels::brody(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[10,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::weibull(t = 1:Tempo_de_analise,alpha,beta,k,m)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0,m = m0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[11,cidade]<-NA,
       R_quadrado_cidades[11,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[11,cidade]<-NA,
       RMSE_cidades[11,cidade]<- RMSE(diff(growthmodels::weibull(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[11,cidade]<-NA,
       pico_cidades[11,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::weibull(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[11,cidade]<-NA,
       pico_cidades_normalizado[11,cidade]<- which.max(diff(growthmodels::weibull(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[11,cidade]<-NA,
       pop_afetada[11,cidade]<- max(growthmodels::weibull(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[11,cidade]<-NA,
       fim_cidades[11,cidade]<- min(index(serie_rj))+which(growthmodels::weibull(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[11,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[11,cidade]<-NA,
       fim_cidades_normalizado[11,cidade]<- which(growthmodels::weibull(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[11,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::negativeExponential(t = 1:Tempo_de_analise,alpha,k)[x]), data = df, 
                              start = list(alpha = max0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf), 
                              upper = c(populacao[cidade],Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[12,cidade]<-NA,
       R_quadrado_cidades[12,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[12,cidade]<-NA,
       RMSE_cidades[12,cidade]<- RMSE(diff(growthmodels::negativeExponential(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], k = coef(modelo)[2]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[12,cidade]<-NA,
       pico_cidades[12,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::negativeExponential(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], k = coef(modelo)[2])))
)
ifelse(is.na(modelo), pico_cidades[12,cidade]<-NA,
       pico_cidades_normalizado[12,cidade]<- which.max(diff(growthmodels::negativeExponential(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], k = coef(modelo)[2])))
)
ifelse(is.na(modelo), pop_afetada[12,cidade]<-NA,
       pop_afetada[12,cidade]<- max(growthmodels::negativeExponential(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], k = coef(modelo)[2]))
)
ifelse(is.na(modelo), fim_cidades[12,cidade]<-NA,
       fim_cidades[12,cidade]<- min(index(serie_rj))+which(growthmodels::negativeExponential(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], k = coef(modelo)[2])>=(1-proporcao_fim)*pop_afetada[12,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[12,cidade]<-NA,
       fim_cidades_normalizado[12,cidade]<- which(growthmodels::negativeExponential(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], k = coef(modelo)[2])>=(1-proporcao_fim)*pop_afetada[12,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::logistic(t = 1:Tempo_de_analise,alpha,beta,k)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[13,cidade]<-NA,
       R_quadrado_cidades[13,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[13,cidade]<-NA,
       RMSE_cidades[13,cidade]<- RMSE(diff(growthmodels::logistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[13,cidade]<-NA,
       pico_cidades[13,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::logistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pico_cidades[13,cidade]<-NA,
       pico_cidades_normalizado[13,cidade]<- which.max(diff(growthmodels::logistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])))
)
ifelse(is.na(modelo), pop_afetada[13,cidade]<-NA,
       pop_afetada[13,cidade]<- max(growthmodels::logistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3]))
)
ifelse(is.na(modelo), fim_cidades[13,cidade]<-NA,
       fim_cidades[13,cidade]<- min(index(serie_rj))+which(growthmodels::logistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[13,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[13,cidade]<-NA,
       fim_cidades_normalizado[13,cidade]<- which(growthmodels::logistic(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3])>=(1-proporcao_fim)*pop_afetada[13,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::mmf(t = 1:Tempo_de_analise,alpha,w0,gamma,m)[x]), data = df, 
                              start = list(alpha = max0,w0 = min0,gamma = gamma0,m = m0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[14,cidade]<-NA,
       R_quadrado_cidades[14,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[14,cidade]<-NA,
       RMSE_cidades[14,cidade]<- RMSE(diff(growthmodels::mmf(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], gamma = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[14,cidade]<-NA,
       pico_cidades[14,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::mmf(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], gamma = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[14,cidade]<-NA,
       pico_cidades_normalizado[14,cidade]<- which.max(diff(growthmodels::mmf(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], gamma = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[14,cidade]<-NA,
       pop_afetada[14,cidade]<- max(growthmodels::mmf(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], gamma = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[14,cidade]<-NA,
       fim_cidades[14,cidade]<- min(index(serie_rj))+which(growthmodels::mmf(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], gamma = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[14,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[14,cidade]<-NA,
       fim_cidades_normalizado[14,cidade]<- which(growthmodels::mmf(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], gamma = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[14,cidade])[1]
)


modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::richard(t = 1:Tempo_de_analise,alpha,beta,k,m)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0,m = m0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[15,cidade]<-NA,
       R_quadrado_cidades[15,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[15,cidade]<-NA,
       RMSE_cidades[15,cidade]<- RMSE(diff(growthmodels::richard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[15,cidade]<-NA,
       pico_cidades[15,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::richard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[15,cidade]<-NA,
       pico_cidades_normalizado[15,cidade]<- which.max(diff(growthmodels::richard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[15,cidade]<-NA,
       pop_afetada[15,cidade]<- max(growthmodels::richard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[15,cidade]<-NA,
       fim_cidades[15,cidade]<- min(index(serie_rj))+which(growthmodels::richard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[15,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[15,cidade]<-NA,
       fim_cidades_normalizado[15,cidade]<- which(growthmodels::richard(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[15,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise,alpha,beta,k,m)[x]), data = df, 
                              start = list(alpha = max0,beta = beta0,k = k0,m = m0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[16,cidade]<-NA,
       R_quadrado_cidades[16,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[16,cidade]<-NA,
       RMSE_cidades[16,cidade]<- RMSE(diff(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[16,cidade]<-NA,
       pico_cidades[16,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[16,cidade]<-NA,
       pico_cidades_normalizado[16,cidade]<- which.max(diff(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[16,cidade]<-NA,
       pop_afetada[16,cidade]<- max(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[16,cidade]<-NA,
       fim_cidades[16,cidade]<- min(index(serie_rj))+which(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[16,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[16,cidade]<-NA,
       fim_cidades_normalizado[16,cidade]<- which(growthmodels::vonBertalanffy(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], beta = coef(modelo)[2], k = coef(modelo)[3], m = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[16,cidade])[1]
)

modelo<-NA
tryCatch(
  modelo <- minpack.lm::nlsLM(y ~ I(growthmodels::blumberg(t = 1:Tempo_de_analise,alpha,w0,m,t0)[x]), data = df, 
                              start = list(alpha = max0,w0 = min0,m = m0,t0 = t0),algorithm = "LM", trace = T, lower = c(0,-Inf,-Inf,-Inf), 
                              upper = c(populacao[cidade],Inf,Inf,Inf),control=minpack.lm::nls.lm.control(maxiter = 1024, factor =100, maxfev = integer()))
  , error = function(e) {})

ifelse(is.na(modelo), R_quadrado_cidades[17,cidade]<-NA,
       R_quadrado_cidades[17,cidade]<- 1-(sum(residuals(modelo)^2)/sum((y - mean(y))^2))
)
ifelse(is.na(modelo), RMSE_cidades[17,cidade]<-NA,
       RMSE_cidades[17,cidade]<- RMSE(diff(growthmodels::blumberg(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], m = coef(modelo)[3], t0 = coef(modelo)[4]))[1:(length(y)-1)],diff(y))
)
ifelse(is.na(modelo), pico_cidades[17,cidade]<-NA,
       pico_cidades[17,cidade]<- min(index(serie_rj))+which.max(diff(growthmodels::blumberg(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], m = coef(modelo)[3], t0 = coef(modelo)[4])))
)
ifelse(is.na(modelo), pico_cidades[17,cidade]<-NA,
       pico_cidades_normalizado[17,cidade]<- which.max(diff(growthmodels::blumberg(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], m = coef(modelo)[3], t0 = coef(modelo)[4])))
)
ifelse(is.na(modelo), pop_afetada[17,cidade]<-NA,
       pop_afetada[17,cidade]<- max(growthmodels::blumberg(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], m = coef(modelo)[3], t0 = coef(modelo)[4]))
)
ifelse(is.na(modelo), fim_cidades[17,cidade]<-NA,
       fim_cidades[17,cidade]<- min(index(serie_rj))+which(growthmodels::blumberg(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], m = coef(modelo)[3], t0 = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[17,cidade])[1]
)
ifelse(is.na(modelo), fim_cidades[17,cidade]<-NA,
       fim_cidades_normalizado[17,cidade]<- which(growthmodels::blumberg(t = 1:Tempo_de_analise, alpha = coef(modelo)[1], w0 = coef(modelo)[2], m = coef(modelo)[3], t0 = coef(modelo)[4])>=(1-proporcao_fim)*pop_afetada[17,cidade])[1]
)

}

}

pico_cidades_convertido <- data.frame(pico_cidades)
fim_cidades_convertido <- data.frame(fim_cidades)

nomes_modelos<-c("chapmanRichards","gompertz","loglogistic","generalisedRichard","schnute","stannard","monomolecular",
                 "generalisedLogistic","mitcherlich","brody","weibull","negativeExponential","logistic","mmf","richard","vonBertalanffy","blumberg")

rownames(R_quadrado_cidades)<-nomes_modelos;rownames(RMSE_cidades)<-nomes_modelos;rownames(pico_cidades)<-nomes_modelos
colnames(R_quadrado_cidades)<-cidades_mais_afetadas;colnames(RMSE_cidades)<-cidades_mais_afetadas;colnames(pico_cidades)<-cidades_mais_afetadas




j<-1
for (j in 1:ncol(pico_cidades)) {
  pico_cidades_convertido[,j]<-as.Date(pico_cidades[,j])
  fim_cidades_convertido[,j] <- as.Date(fim_cidades[,j])
}

modelos_selecionados<-rep(NA,length(cidades_mais_afetadas))
pico_modelos_selecionados<-rep(NA,length(cidades_mais_afetadas))
fim_modelos_selecionados<-rep(NA,length(cidades_mais_afetadas))
pico_modelos_selecionados_normalizado<-rep(NA,length(cidades_mais_afetadas))
fim_modelos_selecionados_normalizado<-rep(NA,length(cidades_mais_afetadas))
erro_modelos_selecionados<-rep(NA,length(cidades_mais_afetadas))
pop_afetada_modelos_selecionados<-rep(NA,length(cidades_mais_afetadas))

critério_de_selecao<-RMSE_cidades

library(matrixStats)

j<-1
for (j in 1:ncol(pico_cidades_convertido)) {
  modelos_selecionados[j]<-nomes_modelos[which(critério_de_selecao==matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],arr.ind = TRUE)[1]]
  pico_modelos_selecionados[j]<-pico_cidades_convertido[which(critério_de_selecao==matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],arr.ind = TRUE)[1],j]
  fim_modelos_selecionados[j]<-fim_cidades_convertido[which(critério_de_selecao==matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],arr.ind = TRUE)[1],j]
#  pico_modelos_selecionados_normalizado[j]<-pico_cidades_normalizado[which(critério_de_selecao==matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],arr.ind = TRUE)[1],j]
#  fim_modelos_selecionados_normalizado[j]<-fim_cidades_normalizado[which(critério_de_selecao==matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],arr.ind = TRUE)[1],j]
  pop_afetada_modelos_selecionados[j]<-pop_afetada[which(critério_de_selecao==matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],arr.ind = TRUE)[1],j]
  erro_modelos_selecionados[j]<-round(matrixStats::colMins(critério_de_selecao,na.rm = TRUE)[j],digits = 0)
}

pico_modelos_selecionados<-as.Date(pico_modelos_selecionados)
fim_modelos_selecionados<-as.Date(fim_modelos_selecionados)

pico_modelos_selecionados_normalizado<-as.numeric(pico_modelos_selecionados-primeiro_caso)
fim_modelos_selecionados_normalizado<-as.numeric(fim_modelos_selecionados-primeiro_caso)

populacao_afetada_por_1k_habitantes_modelo<-round(pop_afetada_modelos_selecionados*1000/populacao,digits = 2)
populacao_afetada_por_1k_habitantes_real<-round(contaminados_atuais*1000/populacao,digits = 2)

#resultado<-data.frame(cidades_mais_afetadas,modelos_selecionados,pico_modelos_selecionados,erro_modelos_selecionados)

resultado<-data.frame(cidades_mais_afetadas,primeiro_caso,modelos_selecionados,pico_modelos_selecionados,fim_modelos_selecionados,
                      pico_modelos_selecionados_normalizado,fim_modelos_selecionados_normalizado,pop_afetada_modelos_selecionados,
                      populacao,populacao_afetada_por_1k_habitantes_real,populacao_afetada_por_1k_habitantes_modelo,erro_modelos_selecionados,dias_de_previsao[d])

resultado
# View(resultado)

resultados<-rbind(resultados,resultado)

resultado_erro<-NA
resultado_erro<-data.frame(cidades_mais_afetadas,as.data.frame((round(t(RMSE_cidades),digits = 2)),row.names = F),dias_de_previsao[d])

resultados_erro<-rbind(resultados_erro,resultado_erro)

}


#### Growth model results #######
resultados<-tidyr::drop_na(resultados)
View(resultados)

#### Error results #######

resultados_erro_final<-resultados_erro[-1,]
View(resultados_erro_final)

#### Growth Models Frequency all over the weeks #######

resultado_modelos_consolidados<-plyr::count(resultados$modelos_selecionados)
resultado_modelos_consolidados<-plyr::arrange(resultado_modelos_consolidados,desc(resultado_modelos_consolidados$freq))

barplot(resultado_modelos_consolidados$freq,names.arg = resultado_modelos_consolidados$x,
        horiz = FALSE,angle = 90,cex.names=0.8, main = "Growth models frequency",xlab = "Models",ylab = "Frequency",col = rainbow(nrow(resultado_modelos_consolidados)))

percentual<-resultado_modelos_consolidados$freq/sum(resultado_modelos_consolidados$freq)

resultado_modelos_consolidados_final<-data.frame(resultado_modelos_consolidados,percentual)

resultado_modelos_consolidados_final<-plyr::arrange(resultado_modelos_consolidados_final,desc(percentual))

cumulative_percentual<-cumsum(resultado_modelos_consolidados_final$percentual)

resultado_modelos_final<-data.frame(resultado_modelos_consolidados_final,cumulative_percentual)

resultado_modelos_final

barplot(resultado_modelos_final$percentual,names.arg = resultado_modelos_final$x,
        horiz = FALSE,angle = 90,cex.names=0.8, main = "Growth models frequency",xlab = "Models",ylab = "Frequency",col = rainbow(nrow(resultado_modelos_consolidados)))


