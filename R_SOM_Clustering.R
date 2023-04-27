##################################################################################
# Pacotes exigidos

install.packages("kohonen")
library(kohonen)
require(kohonen)
install.packages("xlsx")
library(xlsx)
install.packages("RColorBrewer")
library("RColorBrewer")
library(varclust)
library(Hmisc)
install.packages("corrplot")
library(corrplot)
display.brewer.all()

####################################################################################
# TOTORIAIS DE REFERÊNCIA

# https://towardsdatascience.com/understanding-self-organising-map-neural-network-with-python-code-7a77f501e985
# https://rpubs.com/AlgoritmaAcademy/som
# https://www.polarmicrobes.org/tutorial-self-organizing-maps-in-r/
# https://www.cs.hmc.edu/~kpang/nn/som.html
# https://analyticsindiamag.com/beginners-guide-to-self-organizing-maps/

# PAPERS
# T. Kohonen, "The self-organizing map," in Proceedings of the IEEE, vol. 78, no. 9, pp. 1464-1480, Sept. 1990, doi: 10.1109/5.58325.
# J. Tian, M. H. Azarian, and M. Pecht, "Anomaly Detection Using Self-Organizing Maps-Based K-Nearest Neighbor Algorithm", PHME_CONF, vol. 2, no. 1, Jul. 2014.
# R. Wehrens and L.M.C. Buydens, J. Stat. Softw. 21 (5), 2007; R. Wehrens and J. Kruisselbrink, submitted, 2017.

####################################################################################

###Data
getwd()

setwd("C:/Adaptabrasil/Julia/Tipologias_urbanas/Clusters_vulnerabilidade_com_exposição/N_27/SOM/")

#Importando CSV
data <- read.csv2("Ind. Tipologias V6_posAF_27ind_vulnerabilidade_comexpo.csv", header = T, row.names=1, na.strings = "NaN")
# trazido com NA substituido por 0

str(data) # verificar se todas as colunas são numericas

df = data.frame(data)

summary(df)

#detectando NA's
sapply(df, function(x) sum(is.na(x)))


# transforma todas as colunas para numeric se necessário
df[, c(1:22)] <- sapply(df[, c(1:22)], as.numeric)

str(df)

# Transforma o dataframe em uma matrix numérica com o valor normalizado pela média/desvio padrão
# Caso a planilha não esteja normalizada
df_scale = scale(df) #normalização med/desv.pad

# transforma o data frame em matriz (input data for SOM)
df = as.matrix(df)

head(df, n=3)


# Análise de correlação para avaliar tendências dos dados
corr_ind <- cor(na.omit(data), method = c("spearman"))
corrplot(corr_ind, method = "circle")


###############################################################################

#The Self-Organising Map learning algorithm (online learning) can be described in the following 4 steps.

#     1. Initialisation
#     Weights of neurons in the map layer are initialised.

#     2. Competitive process
#     Select one input sample and search the best matching unit among all neurons in n x m grid using distance measures.

#     3. Cooperative process
#     Find the proximity neurons of BMU by neighbourhood function.

#     4. Adaptation process
#     Update the BMU and neighbours' weights by shifting the values towards the input pattern.
#     If the maximum count of training iteration is reached, exit. If not, increment the iteration count by 1 and repeat the process from 2.

###############################################################################

# Hyperparameters (https://towardsdatascience.com/understanding-self-organising-map-neural-network-with-python-code-7a77f501e985)

# Hyperparameters are non-trainable parameters that need to be selected before training algorithms. 
# They are: (1) the number of neurons, (2) the dimension of the SOM grid, (3) the number of training steps, (4) the learning rate and (5) the neighbourhood range from the BMU.

###############################################################################
# SOM PROCESSING
set.seed(7)

#(1) the number of neurons
# Determinando o numero de neurônios (M) 
# Tian et al. (2014) https://papers.phmsociety.org/index.php/phme/article/view/1554
# J. Tian, M. H. Azarian, and M. Pecht, "Anomaly Detection Using Self-Organizing Maps-Based K-Nearest Neighbor Algorithm", PHME_CONF, vol. 2, no. 1, Jul. 2014.

M = 5 *sqrt(5570) 

# M ??? 5 sqrt(N)
# where M is the number of neurons, which is an integer close to the result of the right hand side of the equation, and N is the number of observations.


# (2) the dimension of the SOM grid
# Criando a dimensão da grade SOM

grid.size <- ceiling(M ^ (1/2)) # https://www.polarmicrobes.org/tutorial-self-organizing-maps-in-r/

som.grid <- somgrid(xdim = grid.size, ydim = grid.size, topo = 'hexagonal', toroidal = T)

# Os hiperparâmetros:
# (3) the number of training steps, 
# (4) the learning rate and 
# (5) the neighbourhood range from the BMU são definidos na construção do modelo

som.model <- som(data.matrix(data), grid = som.grid, 
                 rlen = 500, # (3) the number of training steps
                 alpha = c(0.05, 0.01), # (4) the learning rate
                 radius = 2.5, # (5)  the radius of the neighbourhood, either given as a single number or a vector (start, stop)
                 keep.data=TRUE, maxNA.fraction = 0.5)


str(som.model)

# Analisando os resultados SOM

som.model$data # matriz de dados analisado
som.model$unit.classif   # the winning units for all data objects
som.model$distances    # distances of objects to their corresponding winning unit
som.model$codes     #  a list of matrices containing codebook vectors from a kohonen object
som.model$changes # matrix of mean average deviations from code vectors; every map corresponds with one column.


# Exportando modelo- SOM
save(som.model, file="SOM_model")


###Palettes

                  pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')
                  contrast <- c("#FA4925", "#22693E", "#D4D40F", "#2C4382", "#F0F0F0", "#3D3D3D") #my own, contrasting pairs
                  kindofpretty <- c("#B39B66", "#3B3828", "#FAE6B9", "#F2F2F2", "#86BA9F", "#135E1F", "#FFF70A", "#FFB10A", "#0498BD", "#FF780A") #my own
                  kindofpretty2 <- c("#B39B66", "#3B3828", "#FAE6B9", "#F2F2F2", "#F58B00", "#F5D800", "#7185A3", "#786187") #my own
                  coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
                  GreenRed <- function(n, alpha = 1) {rainbow(n, end=3/6, alpha=alpha)[n:1]}
                  GreenRed1 <- function(n, alpha = 1) {topo.colors(n, end=6/8, alpha=alpha)[n:1]}
#or define palettes using RColorBrewer:
                  bg.pallet <- c("red", "blue", "yellow", "purple", "green")
                  color <- brewer.pal(4, "Paired")
                  bgcol <- brewer.pal(8, "Dark2")
                  contrast2 <- c("#FA4925","#3D3D3D")




#### Plotagens mapas de kohonem e afins

plot(som.model, type ='changes')
plot(som.model, type = 'mapping', shape = "straight", labels_munic)  ## distribuição de frequencia das amostras nos neurônios
plot(som.model, type = 'counts', main="Node Counts",shape = "straight", palette = coolBlueHotRed)  ## distribuição de frequencia das amostras nos neurônios
plot(som.model, type = 'dist.neighbours', main = "SOM neighbour distances", shape="straight", palette = coolBlueHotRed)  ##U-Matrix 
plot(som.model, type ='codes', shape = "straight", palette.name=rainbow)  #       codeRendering = "segments"  codeRendering = "stars"
plot(som.model, type="quality", zlim=c(0,12),
     shape = "straight", palette = coolBlueHotRed)  ## Qualidade topológica do mapa


# Mapas de calor de cada variável no modelo
heatmap.som <- function(model){
  for (i in 1:27) {
    plot(model, type = "property", property = getCodes(model)[,i], 
         main = colnames(getCodes(model))[i]) 
  }
}

heatmap.som(som.model)


# Verificar similaridades
similarities <- plot(som.model, type="quality", palette.name = terrain.colors)


### Exportar principais resultados


write.csv2(som.model$unit.classif, "SOM_unit_objetcs_classif.csv")
#write.xlsx(som_data$unit.classif, "SOM_unit_objetcs_classif.xlsx", sheetName="Sheet1", 
           #col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)

write.csv2(som.model$distances, "SOM_euc_dist.csv")
#write.xlsx(som_data2$distances, "som_dist_som_data2_31-10-19.xlsx", sheetName="Sheet1", 
           #col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)

write.csv2(som.model$codes , "SOM_kohonen_codes.csv")
#write.xlsx(som_data2$codes, "som_codes_som_data2_31-10-19.xlsx", sheetName="Sheet1", 
           #col.names=TRUE, row.names=TRUE, append=FALSE, showNA=TRUE, password=NULL)


###########################################################################################

# Clusterizando a partir do SOM model
# https://rpubs.com/AlgoritmaAcademy/som


library(clValid)
library(factoextra)
library(gridExtra)
library(NbClust)
library(tidyverse)

# https://rpubs.com/jairotosc/854773
# Verificando melhor algoritmo de agrupamento

clmethods <- c("hierarchical","kmeans","pam")

# Avaliação por medida internal
# The internal measures include the connectivity, and Silhouette Width, and Dunn Index. 
# The connectivity indicates the degree of connectedness of the clusters, as determined by the k-nearest neighbors. The neighbSize argument specifies the number of neighbors to use. 
# The connectivity has a value between 0 and infinity and should be minimized.

intern <- clValid(som.model$codes[[1]], nClust = 3:8,
                  clMethods = clmethods, validation = "internal")
summary(intern)

# Avaliaçãpo por medida de estabilidade
# The stability measures are a special version of internal measures which evaluate the stability of a clustering result by comparing it with the clusters obtained by removing one column at a time. 
# These measures include the average proportion of non-overlap (APN),
# the average distance (AD), the average distance between means (ADM), and the figure of merit (FOM). 
# The APN, AD, and ADM are all based on the cross-classification table of the original clustering with the clustering based on the removal of one column

stab <- clValid(som.model$codes[[1]], nClust = 3:8, clMethods = clmethods,
                validation = "stability")
optimalScores(stab)


# Outros indices para verificar o k ideal
library(factoextra)


# Elbow method
fviz_nbclust(som.model$codes[[1]], kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(som.model$codes[[1]], kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")



# Charrad, Malika, Nadia Ghazzali, Véronique Boiteau, and Azam Niknafs. 2014. 
# "NbClust: An R Package for Determining the Relevant Number of Clusters in a Data Set." 
# Journal of Statistical Software 61: 1-36. http://www.jstatsoft.org/v61/i06/paper.
install.packages("NbClust") 
library(NbClust)


NbClust(data = som.model$codes[[1]], diss = NULL, distance = "euclidean", 
        min.nc = 2, max.nc = 15, method = "kmeans")


# Clusterização k-means para n clusters
set.seed(100)
clust_som <- kmeans(som.model$codes[[1]], 4)

print(clust_som, diss = NULL, distance = "euclidean", min.nc = 3, max.nc = 15, method = "kmeans")


plot(som.model, type = "codes", bgcol = rainbow(9)[clust_som$cluster], main = "Cluster Map")
add.cluster.boundaries(som.model, clust_som$cluster)


# know cluster each data
ads.cluster <- data.frame(data, cluster = clust_som$cluster[som.model$unit.classif])
tail(ads.cluster, 10)


write.csv2(ads.cluster, "SOM_cluster_kmeans_4_result.csv")



###CLUSTERS HIERARQUICO DENDOGRAMA


### Selecao de variáveis   (package Hmisc)

# clusterização hierarquica
data_dm <- data.matrix(data)
clust <- varclus(data_dm, similarity=c("spearman"), method = "ward.D2")
clust
plot(clust)
boxplot(data_dm, cex.axis=0.8, las=2)


# GERAR CLUSTER HIERARQUICO AGLOMERATIVO
par(mfrow=c(1,1))


# cria a distância entre os pares de dados que serão objeto de análise para a formação dos primeiros pares de dados similares
som.model$codes[[1]]
dist_som_data_codes <- dist(som.model$codes[[1]], method="euclidean")  


# Depois de calculada a distância entre os objetos é possível verificar os índices encontrados (matriz de dissimilaridade)
as.matrix(dist_som_data_codes)[1:3,1:3]

# criação dos clusters com base na similaridade, por meio de iterações que agrupem as variáveis em clusters cada vez maiores até formar uma árvore hierárquica.
dist.hc=hclust(d=dist_som_data_codes, method="ward.D")


# representação gráfica da Aglomeração Hierárquica é efetuada através da imagem de um dendograma,
library("factoextra")
fviz_dend(dist.hc, cex=0.5)


# Geração dos clusters hieraquicos
# https://rpubs.com/jairotosc/854773

grp <- cutree(dist.hc, k = 3)

# visualizando o resultado
b <- NbClust(som.model$codes[[1]], distance = "euclidean", min.nc = 3, max.nc = 10, method = "ward.D", index = "silhouette")

b[1:2]

library(RColorBrewer)

mycolors = c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2')


fviz_dend(
  dist.hc,
  k = 3, # cortar em k grupos
  k_colors = mycolors,
  color_labels_by_K = TRUE,	 	 # Define cores no label dos grupos
  cex = 0.3,	# Define o tamanho do label
  lwd = 0.2,
  rect = TRUE # Adiciona retângulos no entorno dos grupos
)


cluster_aglo_k3 = data.frame(b$Best.partition)


cluster_aglo_k3_result = data.frame(df, cluster = b$Best.partition)



write.csv(cluster_aglo_k3_result, "Aglomerative_cluster_result.csv")


#Resumo
media_cluster_aglo_k3 <- cluster_aglo_k3_result %>%
  group_by(cluster_aglo_k3_result$cluster) %>%
  colMeans(cluster_aglo_k3_result, na.rm=T)

