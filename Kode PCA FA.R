setwd("D:/analisis multivariat_smt4")

dataset <- read.csv("automobile_numeric_data_fix_final.csv")

str(dataset)    # Melihat struktur data
summary(dataset)  # Statistik ringkasan
head(dataset)   # Melihat beberapa baris pertama

# Simpan nama asli kolom
original_colnames <- colnames(dataset)

# Ubah nama kolom di dataset
colnames(dataset) <- paste0("X", 1:ncol(dataset))

# Sekarang dataset masih data framenya, tapi nama kolom berubah
boxplot(dataset, main = "Boxplot Data", las = 2)


#Pre-processing
sum(is.na(dataset))
p <- ncol(dataset)

# --- STATISTIKA DESKRIPTIF UTAMA ---

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
r <- cor(dataset)  # Hitung matriks korelasi dari data asli
KMO(r)  # Uji KMO

# Hapus kolom X7 dari dataset
dataset <- dataset[, !(colnames(dataset) %in% c("X7"))]

# Cek hasil struktur dataset setelah penghapusan
str(dataset)

# Cek KMO ulang setelah X7 dihapus
KMO(cor(dataset))


#Bartlett Test
# Buat matriks korelasi
cor_matrix <- cor(dataset)

# Jalankan uji Bartlett's test of sphericity
cortest.bartlett(cor_matrix, n = nrow(dataset))

data_scaled <- scale(dataset)  # Standarisasi (mean=0, std=1)

head(data_scaled)


correlation_matrix <- cor(data_scaled)
heatmap(correlation_matrix)

# PCA dengan scaling (standarisasi) & centering (rata-rata 0)
pca_result <- prcomp(data_scaled, center = TRUE, scale. = TRUE)

# Ringkasan PCA: proporsi varian tiap PC
summary(pca_result)

# Matriks rotasi (loadings), mirip eigenvectors
pca_result$rotation

# Nilai komponen utama (scores), posisi data di ruang PC
head(pca_result$x)

plot(pca_result$x[,1], pca_result$x[,2],
     xlab = "PC1", ylab = "PC2",
     main = "PCA - PC1 vs PC2")


# Scree Plot
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 100))

library(ggplot2)

# Mengonversi hasil PCA ke dataframe
pca_df <- as.data.frame(pca_result$x)  


# correlation circle
contrib_circle <- fviz_pca_var(pca_result, col.var = "contrib",
                               gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
                               repel = TRUE) + 
  ggtitle("Kontribusi Variabel")
plot(contrib_circle)




#----factor analisis---------------
# Hitung eigenvalue dari matriks kovarians
varcov = cov(data_scaled)
pc = eigen(varcov)
eigenvalues = pc$values

# Print eigenvalues
cat("Eigenvalues:\n")
print(pc$values)

# Print eigenvectors
cat("\nEigenvectors (Principal Components):\n")
print(pc$vectors)
# Menentukan jumlah faktor dengan Kaiser's Criterion (eigenvalue > 1)
num_factors = sum(eigenvalues > 1)
cat("Jumlah faktor berdasarkan Kaiser's Criterion:", num_factors, "\n")

# Faktor Loadings
L = matrix(nrow = ncol(data_scaled), ncol = num_factors)
for (i in 1:num_factors) {
  L[, i] = sqrt(eigenvalues[i]) * pc$vectors[, i]
}
print(L)  # Menampilkan factor loadings

cor_matrix <- cor(data_scaled)
eigen(cor_matrix)$values  # lihat eigenvalues

fa_result <- fa(r = data_scaled, 
                nfactors = num_factors, 
                rotate = "varimax", 
                scores = "tenBerge")  # ganti metode scoring di sini


# Menampilkan factor loadings
print(fa_result$loadings)

# Visualisasi Faktor
fa.diagram(fa_result$loadings)


