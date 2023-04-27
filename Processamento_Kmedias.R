require(stats)

#Carregamento dos dados e tratamento dos dados

#Importando CSV
indicadores = read.csv2("C:/Adaptabrasil/Julia/Tipologias_urbanas/Clusters_vulnerabilidade_com_exposição/N_27/K-medias/Teste4/Ind. Tipologias V6_posAF_27ind_vulnerabilidade_comexpo.csv", header = TRUE, sep = ";", row.names = 1)
# trazido com NA substituido por 0

str(indicadores)

df = data.frame(indicadores)


#substituição de "," por "."
cols <- sapply(df, function(x) any(grepl(",", x)))
df[cols] <- lapply(df[cols], function(x) as.numeric(sub(",", ".", x)))


#detectando NA's
sapply(df, function(x) sum(is.na(x)))

# substuiir NA
#df[is.na(df)] <- ""

summary(df)

# Transforma o dataframe em uma matrix numérica com o valor normalizado pela média/desvio padrão
#df = scale(df) #normalização med/desv.pad
  
  
#head(df, n=3)

# rodar k-means genérico
kmeans(x, centers, iter.max=10, nstart=1)


# Número ótimo de clusters
# https://www.datanovia.com/en/lessons/determining-the-optimal-number-of-clusters-3-must-know-methods/
# https://www.analyticsvidhya.com/blog/2021/05/k-mean-getting-the-optimal-number-of-clusters/

library(factoextra)


# Elbow method
fviz_nbclust(df, kmeans, method = "wss")+
  geom_vline(xintercept = 3, linetype = 2)+
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

# Silhouette method
fviz_nbclust(df, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")


# Charrad, Malika, Nadia Ghazzali, Véronique Boiteau, and Azam Niknafs. 2014. 
# "NbClust: An R Package for Determining the Relevant Number of Clusters in a Data Set." 
# Journal of Statistical Software 61: 1-36. http://www.jstatsoft.org/v61/i06/paper.
install.packages("NbClust") 
library(NbClust)

NbClust(data = df, diss = NULL, distance = "euclidean",
        min.nc = 2, max.nc = 15, method = "kmeans")


# Clusterização k-means para n clusters
set.seed(123)
km.res=kmeans(df, 3, nstart=25)
print(km.res)


#Result
# K-means clustering with 3 clusters of sizes 2537, 2295, 738

# agregar as informações dos clusters
tabela_cluster = aggregate(df, by=list(cluster=km.res$cluster), mean)

# agregar clusters aos municipios
indicadores_clusterizados =cbind(df, cluster=km.res$cluster)
head(indicadores_clusterizados)

# Vizualizando os clusters

library(ggplot2)
library(factoextra)

fviz_cluster(km.res, data=indicadores_clusterizados,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#66b032" ),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)







# Clusterização k-means para 4 clusters
set.seed(123)
km.res=kmeans(df, 4, nstart=25)
print(km.res)

#Result
# K-means clustering with 3 clusters of sizes 2537, 2295, 738

# agregar as informações dos clusters
tabela_cluster = aggregate(df, by=list(cluster=km.res$cluster), mean)

# agregar clusters aos municipios
indicadores_clusterizados =cbind(df, cluster=km.res$cluster)
head(indicadores_clusterizados)

# Vizualizando os clusters

library(ggplot2)
library(factoextra)

fviz_cluster(km.res, data=indicadores_clusterizados,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07","#66b032", "#ddc6bb" ),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)



#Exportar tabela de cluster

write.csv2(indicadores_clusterizados, "C:/Adaptabrasil/Julia/Tipologias_urbanas/Clusters_vulnerabilidade_com_exposição/N_27/K-medias/Teste4/Mun_clusters_V6_15vulnerabilidade_k4_kmeans_Julia2.csv")



# Via dendograma

dist_mat <- dist(df, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
plot(hclust_avg)



