##### This script runs cluster analysis on proc_ouvert variable

## Load data
load("totscores_modif.Rda") 


######################## CLUSTER ANALYSES #################################
library(cluster)
library(factoextra)
library(GGally)
library(tidyr)
library(plotly)
library(NbClust)
library(clValid)
library(dendextend)
library(purrr)
library(psych)
library(car)
library(fpc)

# isolate clustering variables (no proc variable)
clust_dat <- totscores_mod[ ,c(25:32)]

# Cluster with only proc variable
clust_dat <- totscores_mod[ ,c(24)]

# Standardize the variables
scaled_dat <- scale(clust_dat)

# Compute the dissimilarity matrix
res.dist <- dist(scaled_dat, method = "euclidean")
dist_matrix <- as.matrix(res.dist)

# comparer les différentes méthodes pour construire le dendogramme
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
    agnes(scaled_dat, method = x)$ac
}

map_dbl(m, ac) ## L'analyse montre que Ward method est la meilleure

# Assessing Clustering Tendency -------------------------------

# Statistical evaluation of clustering tendency
library(clustertend)

set.seed(1234)
hopkins(scaled_dat, n = nrow(scaled_dat) - 1) # Needs to be below the threshold of .5

fviz_dist(res.dist, show_labels = F) +
    labs(title = "yolo")

# Determining the Optimal Number of Clusters -----------------

### Code qui compare plusieurs méthodes simultanément ###
res.nbclust <-
    NbClust(
        scaled_dat,
        distance = "euclidean",
        min.nc = 2,
        max.nc = 9,
        method = "complete",
        index = "all"
    )

factoextra::fviz_nbclust(res.nbclust) + 
    theme_minimal() + 
    ggtitle("NbClust's optimal number of clusters")


###                     ###
### Result == 2 cluster ###
###                     ###

# K-Means -----------------------------------------------------------------
###                                         ### 
### Seems for this algo  - Nb. cluster is 2 ###
###                                         ###

# Computing Kmeans clustering
set.seed(124)
km.res <- kmeans(res.dist, 2, nstart = 1500)
print(km.res)

# Visualizing kmeans clusters
fviz_cluster(
    km.res,
    data = res.dist,
    #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
    ellipse.type = "euclid",
    star.plot = T,
    repel = T,
    ggtheme = theme_minimal()
)

# K-Medoids ---------------------------------------------------------------

# Computing PAM

# PAM where vector of cluster is tested
set.seed(100)
pamk(
    res.dist,
    krange = 1:10,
    criterion = "asw",
    usepam = T,
    diss = F,
    critout = T
)

# PAM where nb. of cluster is specified
set.seed(101)
pam.res <- pam(dist_matrix, 2)
print(pam.res)

# Visualizing PAM clusters
fviz_cluster(
    pam.res,
    #  palette = c("#00AFBB", "#FC4E07"),
    ellipse.type = "t",
    repel = T,
    ggtheme = theme_classic()
)


# Agglomerative Clustering -----------------------------------

res.dist <- dist(scaled_dat, method = "euclidean")

# Create hierarchical tree
res.hc <- hclust(d = res.dist, method = "ward.D2")
print(res.hc)

# Dendrogram
fviz_dend(res.hc, cex = .5)

# Compute cophentic distance
res.coph <- cophenetic(res.hc)

# Correlation between cophenetic distance and the original distance
# The bigger the correlation, the better the method represents
# The original distances
cor(res.dist, res.coph)

fviz_dend(
    res.hc,
    k = 2,
    cex = .5,
    # k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
    color_labels_by_k = T,
    rect = T
)

# Cut the dendrogram into different groups
grp <- cutree(res.hc, k = 2)
table(grp)

fviz_cluster(
    list(data = dist_matrix, cluster = grp),
    #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
    ellipse.type = "convex",
    repel = T,
    show.clust.cent = F,
    ggtheme = theme_minimal()
)

# Agglomerative Nesting (Hierarchical clustering)
res.agnes <- agnes(
    x = dist_matrix,
    stand = T,
    method = "ward"
)

print(res.agnes)

fviz_dend(res.agnes, cez = .6, k = 2)

# Cut the dendrogram into different groups
grp <- cutree(res.agnes, k = 2)

fviz_cluster(
    list(data = dist_matrix, cluster = grp),
    #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
    ellipse.type = "convex",
    repel = T,
    show.clust.cent = F,
    ggtheme = theme_minimal()
)


# Divisive Analysis Clustering

##                                        ##
## Looks like the best algo for this data ##
##                                        ##

res.diana <- diana(x = dist_matrix)

fviz_dend(res.diana, cex = .6, k = 2)

grp_res.diana <- cutree(res.diana, k = 2)

fviz_cluster(
    list(data = dist_matrix, cluster = grp),
    #palette = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07")
    ellipse.type = "convex",
    repel = T,
    show.clust.cent = F,
    ggtheme = theme_minimal()
)


table(grp_res.diana)

#### Create a group variable based on clustering #####

totscores_mod$cluster_res.diana <- grp_res.diana

save(totscores_mod, file = "totscores_w_cluster.Rda")

# Chapter IX: Visualizing dendrograms -------------------------------------

# to draw a horizontal dendrogram
fviz_dend(
    res.diana,
    k = 2,
    cex = 0.9,
    horiz = TRUE,
    k_colors = "jco",
    rect = TRUE,
    rect_border = "jco",
    rect_fill = TRUE,
    main = "Analyse en clusters hiérarchique",
    ggtheme = theme_classic()
)


# Chapter XVI: Hierarchical K-Means Clustering ----------------------------

res.hk <- hkmeans(dist_matrix, 2)
names(res.hk)
res.hk
table(res.hk$cluster)

# Visualize the hkmeans final clusters
fviz_cluster(
    res.hk,
    cex = .6,
    palette = "jco",
    repel = T,
    ggtheme = theme_classic()
)

# Visualize the tree
fviz_dend(res.hk, cex = 0.6, palette = "jco",
          rect = TRUE, rect_border = "jco", rect_fill = TRUE)

grp_res.hk <- res.hk$cluster
table(grp_res.hk)

#### Create a group variable based on clustering #####

totscores_mod$cluster_res.hk <- grp_res.hk

save(totscores_mod, file = "totscores_w_cluster.Rda")
