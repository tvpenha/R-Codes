rm(list=ls()) 

# pacotes necess�rios para realizar a correla��o #
install.packages("xlsx")
install.packages("Hmisc")
install.packages("rJava")
install.packages("corrplot")
install.packages("psych")
install.packages("ggplot2")
install.packages("GGally")
install.packages("PerformanceAnalytics")

# Abrir os pacotes
library(xlsx)
library(Hmisc)
library(rJava)
library(corrplot)
library(psych)
library(ggplot2)
library(GGally)
library(PerformanceAnalytics)


# Definir o caminho do Diret�rio da �rea de trabalho
setwd("C:\\Adaptabrasil\\")
# checar se o caminho definido est� correto
getwd()

# Importar a planilha de dados

# abrir planilha CSV
data_desastres <- read.csv("C:/Adaptabrasil/Analise de correla��o Capacidade adaptativa desastres/Indicadores para terceira rodada_normalizados.csv")

# Primeiras colunas
head(data_desastres)

# Resumo das estat�sticas b�sicas
summary(data_desastres)

# converter as colunas de texto para n�mero
# 
#data_desastres_corrigido <- transform(data_desastres, 
#                                      S056163a = as.numeric(S056163a),
#                                      S064244 = as.numeric(S064244),
#                                      ICA20061 = as.numeric(ICA20061),
#                                      ICA08015ag = as.numeric(ICA08015ag),
#                                      ICA17053ag = as.numeric(ICA17053ag),
#                                      ICA17053ag = as.numeric(ICA17053ag),
#                                      ICA17051b = as.numeric(ICA17051b),
#                                     ICA17052n = as.numeric(ICA17052n),
#                                      ICA17054 = as.numeric(ICA17054),
#                                      ICA17035ag = as.numeric(ICA17035ag),
#                                      S053228 = as.numeric(S053228),
#                                      S041368 = as.numeric(S041368),
#                                     ICA17043 = as.numeric(ICA17043),
#                                      ICA17048ag = as.numeric( ICA17048ag),
#                                      ICA17029ag = as.numeric(ICA17029ag),
#                                      ICA07013 = as.numeric(ICA07013),
#                                      ICA17042ag = as.numeric(ICA17042ag),
#                                      ICA17037 = as.numeric(ICA17037),
#                                      ICA07004ag = as.numeric(ICA07004ag),
#                                      S031126 = as.numeric(S031126)
#                                      )

# retira o cabe�alho dos nomes
data_desastres <- data.frame(data_desastres, row.names = 1)
# converte para uma matriz, que � o formato de entrada do pacote estat�stico
data_desastres_matrix <- data.matrix(data_desastres)


#Correla��es

# rodar correla��o de "spearman" 
corr_data_desastres <- rcorr(data_desastres_matrix, type = c("spearman")) # caso deseje, substitua por "pearson"
corr_data_desastres$r  # valores de correla��o
corr_data_desastres$P  # valores de p-valor teste

# Expsotar para CSV os resultados (coluna $r onde est� a correla��o e coluna $P est� o p-valor)
write.csv2(corr_data_desastres$r, "correla��o_spearman_data_desastres.csv")
write.csv2(corr_data_desastres$P, "correla��o_spearman_data_desastres_p-value.csv")

# Visualiza��o dos resultados

#pacotes exigidos
library(corrplot)
library(RColorBrewer)

# Op��o 1 de visualiza��o
corrplot(corr_data_desastres$r, type="upper", order="hclust",
         method = "number",
         addCoef.col = "black",
         col=brewer.pal(n=8, name="RdYlBu"))

# Op��o 2 de visualiza��o
corrplot(corr_data_desastres$r, method="number", type="upper",
         p.mat = p.mat, sig.level = 0.05)

# Op��o 3 de visualiza��o
corrplot(corr_data_desastres$r,
         method = "number",
         type = "lower",
         diag = FALSE,
         tl.col = "black",
         tl.srt = 45,
         p.mat = corr_data_desastres$p,
         sig.level = 0.05)




#Visualiza��o em ranking das correla��es mais relevantes

install.packages("lares")
library(lares)

a = as.data.frame(corr_data_desastres$r) 

corr_cross(a, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
