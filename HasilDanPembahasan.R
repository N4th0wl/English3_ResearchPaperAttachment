library(readxl)

# Insialisasi Data / Pembersihan Data
# 1. Import file dengan baris pertama sebagai header
OlahDataBI <- read_excel(
  "C:/Users/ivand/OneDrive/Documents/KULIAH/Semester 3/UM142 - Indonesian Language/UJIAN DAN BELAJAR/RDump/OlahDataBI.xlsx",
  col_names = TRUE)
# 2. Hapus kolom yang seluruhnya NA (kolom kosong)
OlahDataBI <- OlahDataBI[, colSums(is.na(OlahDataBI)) < nrow(OlahDataBI)]
# 3. Tampilkan nama kolom
names(OlahDataBI)



# Tahap Pertama Uji Reliabilitas Variabel X dan Y
library(psych)
data_X <- OlahDataBI[, paste0("X", 1:7)]
alpha(data_X)

data_Y <- OlahDataBI[, paste0("Y", 1:7)]
alpha(data_Y)

# Tahap Kedua Analisis Statistik Deskriptif

# Variabel X
describe(data_X)
# Variabel Y
describe(data_Y)

# Kurva X

# Kurva Mean Variabel X
data_Y$overall_mean <- rowMeans(data_X[, c("X1","X2","X3","X4","X5","X6","X7")])
mean_overall <- mean(data_X$overall_mean)
sd_overall   <- sd(data_X$overall_mean)

x_vals <- seq(1, 5, length.out = 300)

gauss_curve <- data.frame(
  x = x_vals,
  density = (1 / (sd_overall * sqrt(2*pi))) * 
    exp(-(x_vals - mean_overall)^2 / (2 * sd_overall^2))
)

ggplot(gauss_curve, aes(x = x, y = density)) +
  geom_line(linewidth = 1.3, color = "blue") +
  geom_vline(xintercept = mean_overall, 
             linetype = "dashed", linewidth = 1) +
  labs(
    title = "Kurva Gaussian dari Rata-rata Likert Tiap Responden",
    x = "Overall Mean Likert (1–5)",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(1, 5)) + 
  theme_minimal()

# Kurva Frekuensi Variabel X
library(ggplot2)
library(dplyr)

data_long <- stack(data_X)

# Hitung frekuensi setiap nilai per variabel
freq_tbl <- data_long %>%
  group_by(ind, values) %>%
  summarise(freq = n(), .groups = "drop")

# Smooth kurvanya
ggplot(freq_tbl, aes(x = values, y = freq, color = ind)) +
  geom_point(size = 2) +                        # frekuensi asli
  geom_smooth(method = "loess", se = FALSE) +   # kurva melengkung
  scale_x_continuous(breaks = 2:5) +            # nilai Likert
  labs(
    title = "Kurva Frekuensi Nilai Likert Variabel X",
    x = "Nilai Likert",
    y = "Frekuensi",
    color = "Pertanyaan"
  ) +
  theme_minimal()


# Kurva Y

# Kurva Mean Variabel Y
data_Y$overall_mean <- rowMeans(data_Y[, c("Y1","Y2","Y3","Y4","Y5","Y6","Y7")])
mean_overall <- mean(data_Y$overall_mean)
sd_overall   <- sd(data_Y$overall_mean)

x_vals <- seq(1, 5, length.out = 300)

gauss_curve <- data.frame(
  x = x_vals,
  density = (1 / (sd_overall * sqrt(2*pi))) * 
    exp(-(x_vals - mean_overall)^2 / (2 * sd_overall^2))
)

ggplot(gauss_curve, aes(x = x, y = density)) +
  geom_line(linewidth = 1.3, color = "blue") +
  geom_vline(xintercept = mean_overall, 
             linetype = "dashed", linewidth = 1) +
  labs(
    title = "Kurva Gaussian dari Rata-rata Likert Tiap Responden",
    x = "Overall Mean Likert (1–5)",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(1, 5)) + 
  theme_minimal()

# Kurva Frekuensi Variabel Y
library(ggplot2)
library(dplyr)

data_long <- stack(data_Y)

# Hitung frekuensi setiap nilai per variabel
freq_tbl <- data_long %>%
  group_by(ind, values) %>%
  summarise(freq = n(), .groups = "drop")

# Smooth kurvanya
ggplot(freq_tbl, aes(x = values, y = freq, color = ind)) +
  geom_point(size = 2) +                        # frekuensi asli
  geom_smooth(method = "loess", se = FALSE) +   # kurva melengkung
  scale_x_continuous(breaks = 2:5) +            # nilai Likert
  labs(
    title = "Kurva Frekuensi Nilai Likert Variabel Y",
    x = "Nilai Likert",
    y = "Frekuensi",
    color = "Pertanyaan"
  ) +
  theme_minimal()

# Kurva Mean Gabungan

# ===============================
# KURVA GAUSSIAN XY (Likert 1–5)
# ===============================

library(ggplot2)
library(dplyr)

# --- 1. Gabungkan semua nilai Likert dari X dan Y ---
data_all <- cbind(data_X, data_Y)

# --- 2. Hitung rata-rata Likert tiap responden (gabungan X+Y) ---
data_all$overall_mean <- rowMeans(data_all)

# --- 3. Hitung mean dan standar deviasi keseluruhan ---
mean_overall <- mean(data_all$overall_mean)
sd_overall   <- sd(data_all$overall_mean)

# --- 4. Buat rentang nilai Likert 1–5 ---
x_vals <- seq(1, 5, length.out = 300)

# --- 5. Buat fungsi Gaussian ---
gauss_curve <- data.frame(
  x = x_vals,
  density = (1 / (sd_overall * sqrt(2*pi))) * 
    exp(-(x_vals - mean_overall)^2 / (2 * sd_overall^2))
)

# --- 6. Plot kurva Gaussian ---
ggplot(gauss_curve, aes(x = x, y = density)) +
  geom_line(linewidth = 1.3, color = "blue") +
  geom_vline(xintercept = mean_overall, 
             linetype = "dashed", linewidth = 1) +
  labs(
    title = "Kurva Gaussian dari Rata-rata Likert X dan Y (per Responden)",
    x = "Rata-rata Likert (1–5)",
    y = "Density"
  ) +
  coord_cartesian(xlim = c(1, 5)) +
  theme_minimal()

# Tahap Ketiga Korelasi Pearson
# ===============================
# KORELASI PEARSON MEAN X & MEAN Y
# ===============================

library(dplyr)

# --- 1. Hitung mean X per responden ---
mean_X <- data_X %>% 
  mutate(mean_X = rowMeans(across(everything()))) %>%
  pull(mean_X)

# --- 2. Hitung mean Y per responden ---
mean_Y <- data_Y %>% 
  mutate(mean_Y = rowMeans(across(everything()))) %>%
  pull(mean_Y)

# --- 3. Hitung Korelasi Pearson ---
cor_result <- cor(mean_X, mean_Y, method = "pearson")

# Tampilkan hasil
cor_result



# Kurva Scatter Plot
library(ggplot2)
library(dplyr)

# === 1. Hitung Mean X per responden ===
mean_X <- data_X %>% 
  mutate(mean_X = rowMeans(across(everything()))) %>% 
  pull(mean_X)

# === 2. Hitung Mean Y per responden ===
mean_Y <- data_Y %>% 
  mutate(mean_Y = rowMeans(across(everything()))) %>% 
  pull(mean_Y)

# === 3. Gabungkan ke 1 data frame ===
df_plot <- data.frame(mean_X, mean_Y)

# === 4. Hitung Korelasi Pearson ===
cor_value <- cor(mean_X, mean_Y, method = "pearson")

# === 5. Scatterplot + garis regresi ===
ggplot(df_plot, aes(x = mean_X, y = mean_Y)) +
  geom_point(color = "blue", size = 3, alpha = 0.7) +     # Titik scatter
  geom_smooth(method = "lm", color = "red", linewidth = 1.2, se = FALSE) +  # Garis regresi
  annotate("text",
           x = min(mean_X) + 0.1,
           y = max(mean_Y) - 0.1,
           label = paste("r =", round(cor_value, 3)),
           size = 5,
           hjust = 0,
           color = "black") +
  labs(
    title = "Scatterplot Mean X vs Mean Y",
    x = "Mean X per Responden",
    y = "Mean Y per Responden"
  ) +
  theme_minimal()

head(data_X)
head(data_Y)

# Tahap Keempat Analisis Regresi Linear Sederhana
# =======================================
# REGRESI LINEAR SEDERHANA
# X = Mean Variabel X per responden
# Y = Mean Variabel Y per responden
# =======================================

library(dplyr)
library(ggplot2)

# --- 1. Hitung Mean X per responden ---
mean_X <- data_X %>%
  mutate(mean_X = rowMeans(across(everything()), na.rm = TRUE)) %>%
  pull(mean_X)

# --- 2. Hitung Mean Y per responden ---
mean_Y <- data_Y %>%
  mutate(mean_Y = rowMeans(across(starts_with("Y")), na.rm = TRUE)) %>%
  pull(mean_Y)

# --- 3. Gabungkan data ---
df_reg <- data.frame(mean_X, mean_Y)

# --- 4. Bangun model regresi ---
model <- lm(mean_Y ~ mean_X, data = df_reg)

# --- 5. Tampilkan hasil regresi ---
summary(model)

# --- 6. Scatterplot + garis regresi ---
ggplot(df_reg, aes(x = mean_X, y = mean_Y)) +
  geom_point(size = 3, alpha = 0.6, color = "blue") +
  geom_smooth(method = "lm", se = TRUE, color = "red", linewidth = 1.2) +
  labs(
    title = "Regresi Linear: Mean X → Mean Y",
    x = "Mean X per Responden",
    y = "Mean Y per Responden"
  ) +
  theme_minimal()


# --- 1. MENGHITUNG MEAN PER BARIS (RESPONDEN) ---
# Pastikan data_X dan data_Y hanya berisi angka (numeric)
Mean_X <- rowMeans(data_X, na.rm = TRUE)
Mean_Y <- rowMeans(data_Y, na.rm = TRUE)

# Membuat dataframe baru agar rapi
df_final <- data.frame(Mean_X, Mean_Y)

# Cek 5 data teratas
print("Preview Data Mean:")
head(df_final)

# --- 2. UJI KORELASI PEARSON ---
korelasi <- cor.test(df_final$Mean_X, df_final$Mean_Y, method = "pearson")

print("=== HASIL KORELASI PEARSON ===")
print(korelasi)

# --- 3. REGRESI LINEAR ---
# Rumus: Y (Dependen) ~ X (Independen)
model_regresi <- lm(Mean_Y ~ Mean_X, data = df_final)

print("=== HASIL REGRESI LINEAR ===")
summary(model_regresi)

# --- 4. VISUALISASI DATA & GARIS REGRESI ---
plot(df_final$Mean_X, df_final$Mean_Y,
     main = "Scatterplot & Garis Regresi",
     xlab = "Mean Variabel X",
     ylab = "Mean Variabel Y",
     pch = 19,        # Bentuk titik (bulat penuh)
     col = "blue")    # Warna titik

# Menambahkan garis regresi (warna merah)
abline(model_regresi, col = "red", lwd = 2)
