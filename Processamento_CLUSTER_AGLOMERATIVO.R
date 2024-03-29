require(stats)
library(clValid)
library(factoextra)
library(gridExtra)
library(NbClust)
library(tidyverse)

#Carregamento dos dados e tratamento dos dados

#Importando CSV
indicadores = read.csv2("C:/Adaptabrasil/Julia/Tipologias_urbanas/Clusters_vulnerabilidade_com_exposi��o/N_27/Aglomerativo_hierarquico/Ind. Tipologias V6_posAF_27ind_vulnerabilidade_comexpo.csv", header = TRUE, row.names = 1)
# trazido com NA substituido por 0

str(indicadores)

df = data.frame(indicadores)

#detectando NA's
sapply(df, function(x) sum(is.na(x)))

# substuiir NA
#df[is.na(df)] <- ""

# transforma todas as colunas para numeric
df[, c(1:22)] <- sapply(df[, c(1:22)], as.numeric)

str(df)

# Transforma o dataframe em uma matrix num�rica com o valor normalizado pela m�dia/desvio padr�o
df = scale(df) #normaliza��o med/desv.pad


head(df, n=3)


#########################################################################################

# Tutorial: https://rpubs.com/jairotosc/854773

# https://www.infoteca.cnptia.embrapa.br/infoteca/bitstream/doc/1126478/1/5360.pdf

# Para a aplica��o desse algoritmo de agrupamento � exigido o que se escolha um m�todo de clusteriza��o e de dist�ncia
# As t�cnicas de single, complete e average, e os m�todos de dist�ncia euclidean, manhattan e maximum

methodsHc <- c("single", "complete", "average")

methodsDist <- c("euclidean", "maximum", "manhattan")


# fun��o para auxiliar na testagem do m�todo de dist�ncia
res.dist <- function(x){
  dist(df, method = methodsDist[x])
}


# Idenfica��o de qual combina��o representa o melhor agrupamento (utilizaremos o m�todo de correla��o apra verificar)
correlacao <- function(x,y){
  res.dist <- dist(df, method = methodsDist[x])
  res.hc <- hclust(res.dist, method = methodsHc[y])
  res.coph <- cophenetic(res.hc)
  cor(res.dist, res.coph)
}
for(i in 1:3){
  for (j in 1:3){
    a <- paste("D:", methodsDist[i], "Hc:", methodsHc[j], "Cor:", correlacao(i, j))
    print(a)
  }
}

#Average + Maximum apresenta maior valor

# Constru��o do Dendograma

plot1 <- fviz_dend(
  hclust(d = res.dist(1), method = methodsHc[6]), 
  cex = 0.5, main = "D: maximum Hc: average")
plot1


# Corte para N clusters
res.dist <- dist(df, method = methodsDist[2]) #Maximum
res.hc <- hclust(res.dist, method = methodsHc[3]) # Average

grp <- cutree(res.hc, k = 4)

# visualizando o melhor resultado
b <- NbClust(df, distance = methodsDist[2], min.nc = 3, max.nc = 10, method = methodsHc[3], index = "silhouette")

b[1:2]


#Visualizando o resultado

library(RColorBrewer)

mycolors = c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
mycolors2 = c("#2E9FDF", "#00AFBB", "#E7B800")
# DENDOGRAMA
fviz_dend(
  res.hc,
  k = 4, # cortar em k grupos
  k_colors = mycolors,
  color_labels_by_K = TRUE,	 	 # Define cores no label dos grupos
  cex = 0.3,	# Define o tamanho do label
  lwd = 0.2,
  rect = TRUE # Adiciona ret�ngulos no entorno dos grupos
)

clust_hc = b$Best.partition

# CLUSTER SCATTER PLOT
fviz_cluster(clust_hc, data = df,
             palette = mycolors2, 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)


cluster_aglo_k4 = data.frame(b$Best.partition)


cluster_aglo_k4_result = data.frame(df, cluster = b$Best.partition)




write.csv2(cluster_aglo_k4_result, "Aglomerative_cluster_HC_k4_result.csv")


#Resumo
media_cluster_aglo_k3 <- cluster_aglo_k3_result %>%
  group_by(cluster_aglo_k3_result$cluster) %>%
  colMeans(cluster_aglo_k3_result, na.rm=T)












# OP��O SIMPLES

# cria a dist�ncia entre os pares de dados que ser�o objeto de an�lise para a forma��o dos primeiros pares de dados similares
dista=dist(df, method="euclidean")


# Depois de calculada a dist�ncia entre os objetos � poss�vel verificar os �ndices encontrados (matriz de dissimilaridade)
as.matrix(dista)[1:3,1:3]

# cria��o dos clusters com base na similaridade, por meio de itera��es que agrupem as vari�veis em clusters cada vez maiores at� formar uma �rvore hier�rquica.
dista.hc=hclust(d=dista, method="ward.D")


# representa��o gr�fica da Aglomera��o Hier�rquica � efetuada atrav�s da imagem de um dendograma,
library("factoextra")
fviz_dend(dista.hc, cex=0.5)


# Gera��o dos clusters hieraquicos
# https://rpubs.com/jairotosc/854773

grp <- cutree(dista.hc, k = 3)

# visualizando o melhor resultado
b <- NbClust(df, distance = "euclidean", min.nc = 3, max.nc = 10, method = "ward.D", index = "silhouette")

b[1:2]

library(RColorBrewer)

mycolors = c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')


fviz_dend(
  dista.hc,
  k = 3, # cortar em k grupos
  k_colors = mycolors,
  color_labels_by_K = TRUE,	 	 # Define cores no label dos grupos
  cex = 0.3,	# Define o tamanho do label
  lwd = 0.2,
  rect = TRUE # Adiciona ret�ngulos no entorno dos grupos
)


cluster_aglo_k3 = data.frame(b$Best.partition)


cluster_aglo_k3_result = data.frame(df, cluster = b$Best.partition)



write.csv(cluster_aglo_k3_result, "Aglomerative_cluster_result.csv")


#Resumo
media_cluster_aglo_k3 <- cluster_aglo_k3_result %>%
  group_by(cluster_aglo_k3_result$cluster) %>%
  colMeans(cluster_aglo_k3_result, na.rm=T)
  