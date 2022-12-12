rm(list=ls()) 

# pacotes necessários para realizar a correlação #
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


# Definir o caminho do Diretório da área de trabalho
setwd("C:\\Adaptabrasil\\")
# checar se o caminho definido está correto
getwd()

# Importar a planilha de dados

# abrir planilha CSV
data_desastres <- read.csv("C:/Adaptabrasil/Analise de correlação Capacidade adaptativa desastres/Indicadores para terceira rodada_normalizados.csv")

# Primeiras colunas
head(data_desastres)

# Resumo das estatísticas básicas
summary(data_desastres)

# converter as colunas de texto para número
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

# retira o cabeçalho dos nomes
data_desastres <- data.frame(data_desastres, row.names = 1)
# converte para uma matriz, que é o formato de entrada do pacote estatístico
data_desastres_matrix <- data.matrix(data_desastres)


#Correlações

# rodar correlação de "spearman" 
corr_data_desastres <- rcorr(data_desastres_matrix, type = c("spearman")) # caso deseje, substitua por "pearson"
corr_data_desastres$r  # valores de correlação
corr_data_desastres$P  # valores de p-valor teste

# Expsotar para CSV os resultados (coluna $r onde está a correlação e coluna $P está o p-valor)
write.csv2(corr_data_desastres$r, "correlação_spearman_data_desastres.csv")
write.csv2(corr_data_desastres$P, "correlação_spearman_data_desastres_p-value.csv")

# Visualização dos resultados

#pacotes exigidos
library(corrplot)
library(RColorBrewer)

# Opção 1 de visualização
corrplot(corr_data_desastres$r, type="upper", order="hclust",
         method = "number",
         addCoef.col = "black",
         col=brewer.pal(n=8, name="RdYlBu"))

# Opção 2 de visualização
corrplot(corr_data_desastres$r, method="number", type="upper",
         p.mat = p.mat, sig.level = 0.05)

# Opção 3 de visualização
corrplot(corr_data_desastres$r,
         method = "number",
         type = "lower",
         diag = FALSE,
         tl.col = "black",
         tl.srt = 45,
         p.mat = corr_data_desastres$p,
         sig.level = 0.05)




#Visualização em ranking das correlações mais relevantes

install.packages("lares")
library(lares)

a = as.data.frame(corr_data_desastres$r) 

corr_cross(a, # name of dataset
           max_pvalue = 0.05, # display only significant correlations (at 5% level)
           top = 10 # display top 10 couples of variables (by correlation coefficient)
)
