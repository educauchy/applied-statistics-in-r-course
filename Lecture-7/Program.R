Sys.setlocale(category = "LC_ALL", locale = "UTF-8")
path <- "~/Yandex.Disk.localized/Университет/Магистратура/2semester/R/Lecture-7/Datasets/"

library(factoextra)
library(cluster)
library(NbClust)
library(ggplot2)
library(ModelMetrics)


#Elbow Method for finding the optimal number of clusters
elbow <- function(data) {
  set.seed(123)
  # Compute and plot wss for k = 2 to k = 15.
  k.max <- 15
  wss <- sapply(1:k.max, 
                function(k) {
                  kmeans(data, k, nstart=50, iter.max = 15)$tot.withinss
                })
  wss
  plot(1:k.max, wss,
       type="b", pch = 19, frame = FALSE, 
       xlab="Number of clusters K",
       ylab="Total within-clusters sum of squares")
}


# file 1
f1_init <- read.table(file = paste0(path, "file01.txt"), header = T, dec = ".")
f1 <- as.matrix(f1_init[, c("Node", "Inclination", "Axis")])
rownames(f1) <- f1_init$Name
f1_dist <- dist(f1, method = "euclidean")

# hierarchical
f1_hclust <- hclust(f1_dist)
plot(f1_hclust)
rect.hclust(f1_hclust, k = 5, border = 2:5)

# optimal # of clusters
elbow(f1)

# k-means
# k-means for euclidean distance
f1_kmeans <- kmeans(f1_dist, 3)
fviz_cluster(f1_kmeans, data = f1, ellipse.type = "convex")

# k-means for manhattan distance
f1_cheb_dist <- dist(f1, method = "manhattan")
f1_cheb_kmeans <- kmeans(f1_cheb_dist, 3)
fviz_cluster(f1_cheb_kmeans, data = f1, ellipse.type = "convex")

# k-means for minkowski distance with k = 5
f1_minkow_dist <- dist(f1, method = "minkowski", p = 5)
f1_minkow_kmeans <- kmeans(f1_cheb_dist, 3)
fviz_cluster(f1_minkow_kmeans, data = f1, ellipse.type = "convex")



# file 2
f2_init <- read.table(file = paste0(path, "file02.txt"), header = T, dec = ".")
f2 <- as.matrix(f2_init[, c("Water", "Protein", "Fat", "Lactose")])
rownames(f2) <- f2_init$Name
f2_dist <- dist(f2, method = "euclidean")

# hierarchical
f2_hclust <- hclust(f2_dist)
plot(f2_hclust)
rect.hclust(f2_hclust, k = 4, border = 2:5)

# optimal # of clusters
elbow(f2)

# k-means
# k-means for euclidean distance
f2_kmeans <- kmeans(f2_dist, 4)
fviz_cluster(f2_kmeans, data = f2, ellipse.type = "convex")

# k-means for manhattan distance
f2_cheb_dist <- dist(f2, method = "manhattan")
f2_cheb_kmeans <- kmeans(f2_cheb_dist, 4)
fviz_cluster(f2_cheb_kmeans, data = f2, ellipse.type = "convex")

# k-means for minkowski distance with k = 5
f2_minkow_dist <- dist(f2, method = "minkowski", p = 5)
f2_minkow_kmeans <- kmeans(f2_cheb_dist, 4)
fviz_cluster(f2_minkow_kmeans, data = f2, ellipse.type = "convex")



# file 7
f7_init <- read.table(file = paste0(path, "file07.txt"), header = T, dec = ".")
f7 <- as.matrix(f7_init[, c("M0", "M25", "M50", "M75", "F0", "F25", "F50", "F75")])
rownames(f7) <- f7_init$Country
f7_dist <- dist(f7, method = "euclidean")

# hierarchical
f7_hclust <- hclust(f7_dist)
plot(f7_hclust)
rect.hclust(f7_hclust, k = 5, border = 2:5)

# optimal # of clusters
elbow(f7)

# k-means
# k-means for euclidean distance
f7_kmeans <- kmeans(f7_dist, 5)
fviz_cluster(f7_kmeans, data = f7, ellipse.type = "convex")

# k-means for manhattan distance
f7_cheb_dist <- dist(f7, method = "manhattan")
f7_cheb_kmeans <- kmeans(f7_cheb_dist, 3)
fviz_cluster(f7_cheb_kmeans, data = f7, ellipse.type = "convex")

# k-means for minkowski distance with k = 5
f7_minkow_dist <- dist(f7, method = "minkowski", p = 5)
f7_minkow_kmeans <- kmeans(f7_cheb_dist, 3)
fviz_cluster(f7_minkow_kmeans, data = f7, ellipse.type = "convex")



# Iris
iris_init <- read.table(file = paste0(path, "iris.txt"), header = F, col.names = c("sepal_l", "sepal_w", "petal_l", "petal_w", "type"), dec = ".", sep = ",")
iris <- as.matrix(iris_init[, c("sepal_l", "sepal_w", "petal_l", "petal_w")])
iris <- na.omit(iris)
#rownames(iris) <- iris_init$type
iris_dist <- dist(iris, method = "euclidean")

# hierarchical
iris_hclust <- hclust(iris_dist)
plot(iris_hclust)
rect.hclust(iris_hclust, k = 3, border = 2:5)

# optimal # of clusters
elbow(iris)

# k-means
# k-means for euclidean distance
iris_kmeans <- kmeans(iris_dist, 3)
fviz_cluster(iris_kmeans, data = iris, ellipse.type = "convex")

# k-means for manhattan distance
iris_cheb_dist <- dist(iris, method = "manhattan")
iris_cheb_kmeans <- kmeans(iris_cheb_dist, 3)
fviz_cluster(iris_cheb_kmeans, data = iris, ellipse.type = "convex")

# k-means for minkowski distance with k = 5
iris_minkow_dist <- dist(iris, method = "minkowski", p = 5)
iris_minkow_kmeans <- kmeans(iris_cheb_dist, 3)
fviz_cluster(iris_minkow_kmeans, data = iris, ellipse.type = "convex")

# confusion matrix
iris_init$type_clust <- factor(iris_kmeans$cluster)
iris_init$type_name <- iris_init$type
levels(iris_init$type) <- c(2, 1, 3)

iris_type <- iris_init$type
iris_type_clust <- iris_init$type_clust

confusionMatrix(iris_type, iris_type_clust)
