setwd("/home/diogo/MDC/inf-0612/Trabalho")

library(ggplot2)

consecutive <- function(vector, k = 1) {
  n <- length(vector)
  result <- logical(n)
  for (i in (1+k):n){
    if (all(vector[(i-k):(i-1)] == vector[i])){result[i] <- TRUE}
  }
  for (i in 1:(n-k)){
    if (all(vector[(i+1):(i+k)] == vector[i])){result[i] <- TRUE}
  }
  return(result)
}

# Carrega dados do arquivo .csv
columns <- c("id", "horario", "temp", "vento", "umid", "sensa")
cepagri <- read.csv("cepagri.csv", header = TRUE, sep = ",", col.names = columns, stringsAsFactors = FALSE)

# Remove coluna "id" desnecessária
cepagri$id <- NULL

# Transforma tipo da coluna de horário para data
cepagri$horario <- as.Date(as.character(cepagri$horario), format = '%d/%m/%Y-%H:%M')

# Mostra os tipos das colunas
sapply(cepagri, class)

# Seleciona somente os dados no perído de interesse
cepagri <- cepagri[cepagri$horario >= "2015-01-01",]
head(cepagri)
cepagri <- cepagri[cepagri$horario <= "2019-12-31",]
tail(cepagri)

# Criação das colunas de ano e mês
cepagri$ano <- as.numeric(format(cepagri$horario,'%Y'))
cepagri$mes <- as.numeric(format(cepagri$horario,'%m'))
cepagri$mes <- factor(month.abb[cepagri$mes], levels = month.abb, ordered = TRUE)
cepagri$dia <- as.numeric(format(cepagri$horario,'%d'))
cepagri$anomes <- paste(cepagri$ano,cepagri$mes, sep = "/")
cepagri$estacao <- quarters(cepagri$horario)
cepagri$estacao <- factor(cepagri$estacao, levels = c("Q1","Q2","Q3","Q4"), labels = c("Verão","Outono","Inverno","Primavera"),ordered = TRUE)

# Substitui valores na coluna temp iguais a ' [ERRO]' por NA
sum(cepagri$temp == ' [ERRO]')
cepagri$temp <- as.character(cepagri$temp)
cepagri$temp <- as.numeric(cepagri$temp)
sum(is.na(cepagri$temp))

# Remove linhas com valores NA por enquanto
cepagri <- cepagri[rowSums(is.na(cepagri))==0,]

# Remove valores de 99.9 na coluna de sensaçã térmica
cepagri[cepagri$sensa == 99.9, "sensa"] <- NA

# Remove linhas com valores NA por enquanto
cepagri <- cepagri[rowSums(is.na(cepagri))==0,]

# Sumário das colunas de dados
summary(cepagri[,2:5])

# Remoção de dados repetidos por 1h
filtro <- consecutive(cepagri$temp, 6)
cepagri <- cepagri[!filtro,]

# Análise 1 - El Niño e La Niña
# Os anos de 2017, 2018 foram significativamente menos úmidos comparados
# aos anos anteriores de 2015 e 2016. Esta queda na umidade é justificada
# pela ocorrência do evento La Niña, que torna os invernos no Sul e Sudeste
# do Brasil mais áridos e verões com temperaturas mais amenas. Em 2015 e
# 2016 ocorreu o fenômeno El Niño, que aumentas as temperaturas de forma
# geral no sudeste.

verao <- cepagri
verao <- verao[verao$estacao == 'Verão',]
verao$ano <- as.character(verao$ano)

inverno <- cepagri
inverno <- inverno[inverno$estacao == 'Inverno',]
inverno$ano <- as.character(inverno$ano)

# plot da temperatura no verão
ggplot(verao, aes(x = ano,
                  y = temp,
                  group = ano,
                  fill = ano)) +
  geom_violin() + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Temperatura no verão ")

# plot da umidade no inverno
ggplot(inverno, aes(x = ano,
                    y = umid,
                    group = ano,
                    fill = ano)) +
  geom_violin() + 
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Umidade no inverno ")


# tabela de temperatura e umidade média por mês
temp_mes <- NULL
for (ano in unique(cepagri$ano)){
  aux <- cepagri[cepagri$ano==ano,]
  temp_mes <- rbind(temp_mes, tapply(aux$temp, aux$mes, mean))
}
rownames(temp_mes) <- unique(cepagri$ano); temp_mes

umid_mes <- NULL
for (ano in unique(cepagri$ano)){
  aux <- cepagri[cepagri$ano==ano,]
  umid_mes <- rbind(umid_mes, tapply(aux$umid, aux$mes, mean))
}
rownames(umid_mes) <- unique(cepagri$ano); umid_mes

# Análise 2 - Sensação térmica e vento
# Esta análise busca relacionar a sensação térmica com a velocidade do vento.
# É de se esperar que a sensação seja mais baixa quando o vento atinge velocidades
# elevadas. Quando observamos a densidade de sensação para ventos fracos, médios e fortes,
# observamos que as menores sensações são acompanhadas de ventos fortes ou médios.

# criando faixas de velocidade do vento
cepagri$vento_cat <- NULL
cepagri$vento_cat <- ifelse(cepagri$vento <= 25, 'Vento Fraco', 
                     ifelse(cepagri$vento > 25 & cepagri$vento <= 50, 'Vento Médio', 'Vento Forte'))

# plot da umidade no inverno
cepagri_2016 <- cepagri[cepagri$ano==2016,]
ggplot(cepagri_2016, aes(x = sensa,
                         colour = vento_cat,
                         fill = vento_cat)) +
  geom_density(alpha = 0.25) +
  labs(title = "Velocidade do vento e sensação térmica ")

# Análise 3 - Aquecimento Global
# Verificar as temperaturas durante os 5 anos, incluindo a média, a máxima e a mínima.
# Estudo apontam um aumento de 0.0225ºC/ano por ano na região de Campinas na temperatura média mínima,
# entre os anos de 1890 e 2006. Será analisado essa variação média, mínima e máxima, e a distribuição das temperaturas.
#

temp_media <- aggregate(cepagri$temp, list(cepagri$mes,cepagri$ano), mean)
temp_media$max <- tapply(cepagri$temp, cepagri$anomes, max);
temp_media$min <- tapply(cepagri$temp, cepagri$anomes, min);
colnames(temp_media) <- c("Mês", "Ano","Média", "Máx", "Mín")
temp_media$Ano <- temp_media$Ano
temp_media$AnoMês <- paste(temp_media$Ano,temp_media$Mês, sep = "/")
temp_media$AnoMês <- factor(temp_media$AnoMês, levels = temp_media$AnoMês, ordered = TRUE)

ggplot(temp_media, aes(x = temp_media$AnoMês, group = 3)) +
  geom_line(aes(y = Média))+
  geom_line(aes(y = Máx), colour = "red")+
  geom_line(aes(y = Mín), colour = "blue")+
  xlab("Meses")+
  ylab("Temperatura")+
  labs(title = "Temperaturas Média, Máxima, Mínima")+
  theme(axis.text.x = element_text(angle=90))+
  theme(legend.background = element_rect(linetype = "solid"))+
  theme(legend.position = "bottom")+ 
  theme(legend.position = c(0.85, 0.5)); 


#Gráfico da média de temperatura por trimestre dos 5 anos

ggplot(cepagri, aes(x = ano))+
  xlab("Ano")+
  ylab("Temperatura")+
  ggtitle("Temperatura Trimestral em Campinas de 2015 a 2019")+
  theme_gray()+
  geom_point(aes(y = temp, colour = temp),alpha = 0.01)+
  scale_color_continuous(low = "blue", high = "red")+
  scale_x_continuous(breaks = unique(ceiling(cepagri$ano)))+
  facet_wrap(~ estacao);

#Análise 4 - Sensação Térmica, temperatura e Umidade do ar
# O ar úmido aumenta a sensação de calor e frio. Quando a temperatura está alta, se a umidade relativa do ar 
# também estiver alta, a sensação térmica tenderá a ser maior. Quando a temperatura está baixa (faixa de 24ºC), se a umidade relativa do ar
# for muito alta, a sensação térmica será mais alta. Se estiver muito quente e a umidade
# do ar for muito baixa, a sensação térmica será mais baixa.  

ggplot(cepagri_2016, aes(x = temp, y = umid, colour = sensa)) +
  xlab("Temperatura")+
  ylab("Umidade")+
  ggtitle("Sensação térmica em relação a Umidade e Temperatura") +
  scale_colour_gradientn(colours = terrain.colors(10))+
  geom_point(alpha = 0.1)


#Análise 5 - Estudo da probabilidade de instalação de fazendas eólicas
# Atualmente, os governadores mundiais estão optando em investir fontes de energia renováveis (solar, eólica, biomassa..) em detrimento das que utilizam combustíveis fósseis.  
# Portanto estudos são necessários para implementação das mesmas nas mais diversas áreas possíveis. Hoje no Brasil, o foco da instalação de fazendas eólicas 
# se concentra no Nordeste (principalmente RN e BA) e na região Sul (SC). Um dos fatores predominentes é que a velocidade dos ventos deve manter em grande parte
# acima dos 7m/s para que seja viável economicamente a instalação dos aerogeradores na regiã oestudada.

vel <- sapply(cepagri$windSpeed,function(x){x/3.6})
dfs_wind <- data.frame(vel)
k_w <- ((sd(dfs_wind$vel)/mean(dfs_wind$vel)))^(-1.086)
dfs_wind$value <- dweibull(vel, shape = k_w)

k_wString <- as.character(round(k_w))

stringLegend <- paste("k=",k_wString,sep=" ")

plotWeibull <- ggplot(dfs_wind, aes(x = vel))
plotWeibull <- plotWeibull + geom_line(aes(y = value, colour = stringLegend))
plotWeibull <- plotWeibull + scale_x_continuous(name = "Speed (m/s)", limits = c(0, 5))
plotWeibull <- plotWeibull + scale_y_continuous(name = "Prob")
plotWeibull <- plotWeibull + labs (colour = "Legenda: ", title = "Curva de Weibull")
print(plotWeibull)

plotHisto <- ggplot(dfs_wind, aes(x=vel)) + geom_histogram(color="black", fill="white")
plotHisto <- plotHisto + scale_x_continuous(name = "Speed (m/s)")
plotHisto <- plotHisto + scale_y_continuous(name = "Frequency")
plotHisto <- plotHisto + labs (title = "Histograma de ventos 2015-2019")
print(plotHisto)



