###################################################################################################################
#
# ROTINA PARA ARTIGO: "UM ÍNDICE PARA MEDIR A DESIGUALDADE DE APRENDIZADO ENTRE GRUPOS DE ESTUDANTES SEGUNDO A RAÇA"
# Rotina em R
#
# AUTORES: VICTOR MAIA S. DELGADO (UFOP) M.R. FIGUEIREDO TRIPODI (UFOP); PEDRO AUGUSTO S. OLIVEIRA (UFOP);
# e-mail: victor.delgado@ufop.edu.br (Victor)
# DATA da última versão: 12/10/2021
# CPU 1:      Desktop HP; processor i7-3770 @ 3.40GHz, RAM: 16.0 GB
#  OS 1:      Windows 7 Professional
# CPU 2:      ACER Aspire E5-573G; processor i7-5500U @ 2.40GHz, RAM: 8.0 GB
#  OS 2:      Windows 10 Home Single Language
# Versão R: 4.0.5(x64) 
#
# OBS.1: Essa é a rotina para se acompanhar o artigo com tabelas e gráficos. Para maiores detalhes ou dúvidas entre em contato com o autor presente no e-mail acima.
# OBS.2: Essa Rotina foi gerada primordialmente no R 4.0.5 para Windows 7, 64 Bits, para rodá-la em outros sistemas operacionais alguns detalhes devem ser observados.
#
###################################################################################################################

# Para acompanhar o artigo é essencial instalar o Pacote FOREIGN, o RELDIST e o FGARCH, estes pacotes são para im-
# portar dados do SPSS; para obter a distância relativa e seu indicador de entropia a distância KL; e para obter
# uma normal assimétrica para a distribuição extrema.
# informações sobre cada um dos pacotes estão nos endereços eletrônico abaixo:
# https://cran.r-project.org/web/packages/foreign/index.html
# https://cran.r-project.org/web/packages/reldist/index.html
# https://cran.r-project.org/web/packages/fGarch/index.html

library(foreign)
# library(reldist)
# library (fGarch)

# Para facilidade de leitura dos dados use setwd para mudar o Working Directory onde estão salvos os arquivos do
# Desigualdade_dados.zip
# Complete abaixo com o seu diretório, exemplo: setwd("C:/Users/UFOP/Documents/Trabalho/UFOP/Artigos/Em Andamento/Índice de Desempenho Relativo (IDR)/Dados")

setwd("E:/Trabalho/UFOP/Artigos/Desigualdades Educacionais Etnicas/Dados")
# setwd() # Salve a rotina com seu diretório.

# Rodar a rotina toda leva aproximadamente 2557.74 seg ou: 42 min, 37.74 segs.
# Pode levar mais ou menos tempo de acordo com as especificações da máquina.

ptm <- proc.time()

# Leitura dos dados:

load("provabrasil_serie_historica.RData")

# Em Dados, há os percentis de referência utilizados:

Dados <- read.spss("PercentisReferências1.sav", to.data.frame=TRUE)

###################################################################
#
# Obtendo a referência em Leitura 5º Ano:
#
###################################################################

# Percentis:

perc <- Dados$ReferênciaLeitura5

# Ampliar os intercalos para 0 e 100:

perc <- c(perc[1] - (perc[2] - perc[1])/2, perc, perc[99] + (perc[99] - perc[98])/2)

# Para o processo ser repetido da mesma maneira, colocamos uma semente de aleatoriedade:

set.seed(22)

# Amostra de números entre 1 a 99, processo descrito na página 7 do artigo e presente em 
# Angus(1994):

ru <- sample(0:99,10000, replace = TRUE) 		# Uma amostra aleatória uniforme de 0 a 99. 
amostra <- runif(10000, perc[ru+1], perc[ru+2]) # Podemos empregar uma uniforme com intervalos de 1 a 100 com os valores dos percentis gerados.

# A amostra é de fato nossa referência baseada em nossos percentis. Obtida abaixo:

Referência.Leitura.Quinto <- amostra

# As referências seguintes serão menos comentadas mas reproduzem o mesmo processo.

###################################################################
#
# Referência Leitura 9º Ano
#
###################################################################

perc2 <- Dados$ReferênciaLeitura9
perc2 <- c(perc2[1] - (perc2[2] - perc2[1])/2, perc2, perc2[99] + (perc2[99] - perc2[98])/2)
set.seed(13)
ru <- sample(0:99,10000, replace = TRUE) # amostra de números entre 1 a 99
amostra2 <- runif(10000, perc2[ru+1], perc2[ru+2])
Referência.Leitura.Nono  <- amostra2

###################################################################
#
# Referência MATEMÁTICA 5ºANO:
#
###################################################################

perc3 <- Dados$ReferênciaMatemática5
perc3 <- c(perc3[1] - (perc3[2] - perc3[1])/2, perc3, perc3[99] + (perc3[99] - perc3[98])/2)
set.seed(pi)
ru <- sample(0:99,10000, replace = TRUE) # amostra de números entre 1 a 99
amostra3 <- runif(10000, perc3[ru+1], perc3[ru+2])
Referência.Matemática.Quinto  <- amostra3

###################################################################
#
# Referência MATEMÁTICA 9ºANO:
#
###################################################################

perc4 <- Dados$ReferênciaMatemática9
perc4 <- c(perc4[1] - (perc4[2] - perc4[1])/2, perc4, perc4[99] + (perc4[99] - perc4[98])/2)
set.seed(sqrt(2))
ru <- sample(0:99,10000, replace = TRUE) # amostra de números entre 1 a 99
amostra4 <- runif(10000, perc4[ru+1], perc4[ru+2])
Referência.Matemática.Nono  <- amostra4

###################################################################
#
# GRÁFICO 1
#
###################################################################

# Português na primeira Linha:
# Matematica na segunda Linha:

brancos_5s <- matrix(0, nrow = 2, ncol = 8)
for(i in 1:5){
	brancos_5s[1,i] <- mean(provabrasil$Leitura[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5], na.rm = TRUE)
	brancos_5s[2,i] <- mean(provabrasil$Matemática[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5], na.rm = TRUE)
}

# Erros-Padrão 
# 2 primeiras linhas máximo e mínimo em leitura.
# 2 últimas linhas máximo e mínimo em matemática.

ep <- function(x) sd(x, na.rm = TRUE)/sqrt(length(complete.cases(x)) - 1)

sd_b <- matrix(0, nrow = 4, ncol = 8)
for(i in 1:5){
	sd_b[1,i] <- mean(provabrasil$Leitura[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5]) + 
					100*ep(provabrasil$Leitura[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5])
	sd_b[2,i] <- mean(provabrasil$Leitura[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5]) - 
					100*ep(provabrasil$Leitura[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5])
	sd_b[3,i] <- mean(provabrasil$Matemática[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5]) + 
					100*ep(provabrasil$Matemática[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5])
	sd_b[4,i] <- mean(provabrasil$Matemática[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5]) - 
					100*ep(provabrasil$Matemática[provabrasil$NumAno == i & provabrasil$Branco == 1 & provabrasil$AnoEscolar2 == 5])
}

brancos_5s[1,6] <- mean(pb2015$PROFICIENCIA_LP_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"], na.rm = TRUE); brancos_5s[2,6] <- mean(pb2015$PROFICIENCIA_MT_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"])
brancos_5s[1,7] <- mean(pb2017$PROFICIENCIA_LP_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"], na.rm = TRUE); brancos_5s[2,7] <- mean(pb2017$PROFICIENCIA_MT_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"])
brancos_5s[1,8] <- mean(pb2019$PROFICIENCIA_LP_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"], na.rm = TRUE); brancos_5s[2,8] <- mean(pb2019$PROFICIENCIA_MT_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"])

sd_b[1,6] <- mean(pb2015$PROFICIENCIA_LP_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"], na.rm = TRUE) + 100*ep(pb2015$PROFICIENCIA_LP_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"]) 
sd_b[2,6] <- mean(pb2015$PROFICIENCIA_LP_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"], na.rm = TRUE) - 100*ep(pb2015$PROFICIENCIA_LP_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"]) 
sd_b[3,6] <- mean(pb2015$PROFICIENCIA_MT_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"], na.rm = TRUE) + 100*ep(pb2015$PROFICIENCIA_MT_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"])
sd_b[4,6] <- mean(pb2015$PROFICIENCIA_MT_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"], na.rm = TRUE) - 100*ep(pb2015$PROFICIENCIA_MT_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Branca"])

sd_b[1,7] <- mean(pb2017$PROFICIENCIA_LP_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"], na.rm = TRUE) + 100*ep(pb2017$PROFICIENCIA_LP_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"]) 
sd_b[2,7] <- mean(pb2017$PROFICIENCIA_LP_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"], na.rm = TRUE) - 100*ep(pb2017$PROFICIENCIA_LP_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"]) 
sd_b[3,7] <- mean(pb2017$PROFICIENCIA_MT_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"], na.rm = TRUE) + 100*ep(pb2017$PROFICIENCIA_MT_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"])
sd_b[4,7] <- mean(pb2017$PROFICIENCIA_MT_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"], na.rm = TRUE) - 100*ep(pb2017$PROFICIENCIA_MT_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Branca"])

sd_b[1,8] <- mean(pb2019$PROFICIENCIA_LP_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"], na.rm = TRUE) + 100*ep(pb2019$PROFICIENCIA_LP_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"]) 
sd_b[2,8] <- mean(pb2019$PROFICIENCIA_LP_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"], na.rm = TRUE) - 100*ep(pb2019$PROFICIENCIA_LP_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"]) 
sd_b[3,8] <- mean(pb2019$PROFICIENCIA_MT_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"], na.rm = TRUE) + 100*ep(pb2019$PROFICIENCIA_MT_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"])
sd_b[4,8] <- mean(pb2019$PROFICIENCIA_MT_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"], na.rm = TRUE) - 100*ep(pb2019$PROFICIENCIA_MT_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Branca"])

colnames(brancos_5s) <- c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")
rownames(brancos_5s) <- c("Leitura", "Matemática")

pretos_5s <- matrix(0, nrow = 2, ncol = 8)
for(i in 1:5){
	pretos_5s[1,i] <- mean(provabrasil$Leitura[provabrasil$NumAno == i & provabrasil$Preto == 1 & provabrasil$AnoEscolar2 == 5], na.rm = TRUE)
	pretos_5s[2,i] <- mean(provabrasil$Matemática[provabrasil$NumAno == i & provabrasil$Preto == 1 & provabrasil$AnoEscolar2 == 5], na.rm = TRUE)
}

pretos_5s[1,6] <- mean(pb2015$PROFICIENCIA_LP_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Preta"], na.rm = TRUE); pretos_5s[2,6] <- mean(pb2015$PROFICIENCIA_MT_SAEB[pb2015$ID_SERIE == 5 & pb2015$Cor == "Preta"], na.rm = TRUE)
pretos_5s[1,7] <- mean(pb2017$PROFICIENCIA_LP_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Preta"], na.rm = TRUE); pretos_5s[2,7] <- mean(pb2017$PROFICIENCIA_MT_SAEB[pb2017$ID_SERIE == 5 & pb2017$Cor == "Preta"], na.rm = TRUE)
pretos_5s[1,8] <- mean(pb2019$PROFICIENCIA_LP_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Preta"], na.rm = TRUE); pretos_5s[2,8] <- mean(pb2019$PROFICIENCIA_MT_SAEB[pb2019$ID_SERIE == 5 & pb2019$Cor == "Preta"], na.rm = TRUE)

colnames(pretos_5s) <- c("2005", "2007", "2009", "2011", "2013", "2015", "2017", "2019")
rownames(pretos_5s) <- c("Leitura", "Matemática")

# O Gráfico propriamente dito:

p <- par()
#mar = c(4,4,0.7,2)

par(mfrow = c(1,2))

# Leitura:

plot(seq(2005,2019, by = 2), brancos_5s[1,], pch = "", xlim = c(2004,2020), ylim = c(100,250), xaxt = "n", yaxt = "n", xlab = "Anos", ylab = "Proficiência Escala SAEB", main = "Língua Portuguesa")
axis(1, at = seq(2005,2019, by = 2))
axis(2, at = seq(100,250, by = 25))
for(j in seq(2005,2019, by = 2)){
segments(x0 = j, y0 = 0, x1 = j, y1 = 500, lty = 2)
}
for(k in seq(100,250, by = 25)){
segments(x0 = 2000, y0 = k, x1 = 2030, y1 = k, lty = 2)
}
polygon(x = c(seq(2005,2019, by = 2),seq(2019,2005, by = -2)), y = c(sd_b[1,],sd_b[2,8:1]), col = "lightgrey", border = NA)
points(seq(2005,2019, by = 2), brancos_5s[1,], col = "aquamarine4", cex = 1.2, pch = 19)
lines(seq(2005,2019, by = 2), brancos_5s[1,], col = "aquamarine4", lwd = 3)
points(seq(2005,2019, by = 2), pretos_5s[1,], col = "coral3", cex = 1.3, pch = 19)
lines(seq(2005,2019, by = 2), pretos_5s[1,], col = "coral3", lwd = 3)
legend(x = c(2004.2,2010.5), y = c(247,227), legend = c("Alunos Brancos","Alunos Pretos"), pch = c(19,18), pt.cex = c(1.1,1.5), lwd = 3, col = c("aquamarine4", "coral3"), bg = "white")

# Matemática:

plot(seq(2005,2019, by = 2), brancos_5s[2,], pch = "", xlim = c(2004,2020), ylim = c(100,250), xaxt = "n", yaxt = "n", xlab = "Anos", ylab = "Proficiência Escala SAEB", main = "Matemática")
axis(1, at = seq(2005,2019, by = 2))
axis(2, at = seq(100,250, by = 25))
for(j in seq(2005,2019, by = 2)){
segments(x0 = j, y0 = 0, x1 = j, y1 = 500, lty = 2)
}
for(k in seq(100,250, by = 25)){
segments(x0 = 2000, y0 = k, x1 = 2030, y1 = k, lty = 2)
}
points(seq(2005,2019, by = 2), brancos_5s[2,], col = "aquamarine4", cex = 1.2, pch = 19)
lines(seq(2005,2019, by = 2), brancos_5s[2,], col = "aquamarine4", lwd = 3)
points(seq(2005,2019, by = 2), pretos_5s[2,], col = "coral3", cex = 1.2, pch = 19)
lines(seq(2005,2019, by = 2), pretos_5s[2,], col = "coral3", lwd = 3)
legend(x = c(2004.2,2010.5), y = c(247,227), legend = c("Alunos Brancos","Alunos Pretos"), pch = c(19,18), pt.cex = c(1.1,1.5), lwd = 3, col = c("aquamarine4", "coral3"), bg = "white")
