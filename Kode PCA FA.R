setwd("D:/analisis multivariat_smt4")

dataset <- read.csv("automobile_numeric_data_fix_final.csv")

str(dataset)    
summary(dataset)  
head(dataset)   

original_colnames <- colnames(dataset)

# Menyesuaikan nama kolom
colnames(dataset) <- paste0("X", 1:ncol(dataset))

boxplot(dataset, main = "Boxplot Data", las = 2)


sum(is.na(dataset))
p <- ncol(dataset)

#STATISTIKA DESKRIPTIF

# Mean
means <- apply(dataset, 2, mean, na.rm = TRUE)
print("Mean Tiap Kolom:")
print(means)

# Median
medians <- apply(dataset, 2, median, na.rm = TRUE)
print("Median Tiap Kolom:")
print(medians)

# Standard Deviation
sds <- apply(dataset, 2, sd, na.rm = TRUE)
print("Standard Deviation Tiap Kolom:")
print(sds)

# Range (Rentang)
mins <- apply(dataset, 2, min, na.rm = TRUE)
maxs <- apply(dataset, 2, max, na.rm = TRUE)
ranges <- maxs - mins
print("Range Tiap Kolom:")
print(ranges)

# Skewness
library(e1071)
skewness_vals <- apply(dataset, 2, skewness, na.rm = TRUE)
print("Skewness Tiap Kolom:")
print(skewness_vals)


library(factoextra)

# Check KMO
library(psych)
r <- cor(dataset)  
KMO(r)  

dataset <- dataset[, !(colnames(dataset) %in% c("X7"))]

str(dataset)

# Cek KMO ulang setelah X7 dihapus
KMO(cor(dataset))


#Bartlett Test
cor_matrix <- cor(dataset)
cortest.bartlett(cor_matrix, n = nrow(dataset))



data_scaled <- scale(dataset)  
head(data_scaled)


correlation_matrix <- cor(data_scaled)
heatmap(correlation_matrix)

#----PCA---------------
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

summary(pca_result)
pca_result$rotation
head(pca_result$x)

plot(pca_result$x[,1], pca_result$x[,2],
     xlab = "PC1", ylab = "PC2",
     main = "PCA - PC1 vs PC2")


# Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

library(ggplot2)

pca_df <- as.data.frame(pca_result$x)  


# correlation circle
contrib_circle <- fviz_pca_var(pca_result, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                               repel = TRUE) + 
  ggtitle("Kontribusi Variabel")
plot(contrib_circle)




#----factor analisis---------------
varcov = cov(data_scaled)
pc = eigen(varcov)
eigenvalues = pc$values

cat("Eigenvalues:\n")
print(pc$values)

cat("\nEigenvectors (Principal Components):\n")
print(pc$vectors)
num_factors = sum(eigenvalues > 1)
cat("Jumlah faktor berdasarkan Kaiser's Criterion:", num_factors, "\n")


L = matrix(nrow = ncol(data_scaled), ncol = num_factors)
for (i in 1:num_factors) {
  L[, i] = sqrt(eigenvalues[i]) * pc$vectors[, i]
}
print(L)  

cor_matrix <- cor(data_scaled)
eigen(cor_matrix)$values  
fa_result <- fa(r = data_scaled, 
                nfactors = num_factors, 
                rotate = "varimax", 
                scores = "tenBerge")  


print(fa_result$loadings)

fa.diagram(fa_result$loadings)
