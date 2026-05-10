# ============================================================
# Analisis Data Reasearch Paper English 3
# Jumlah Responden 105
# Asunsi X adalah Variabel Dependen dan Y Independen
# ============================================================


# 0. INSTALASI & LOADING PACKAGES

# Packages List yang nantinya digunakan
packages_needed <- c("readxl", "psych", "ggplot2", "dplyr", "tidyr",
                     "ggpubr", "scales", "gridExtra")

# Install otomatis jika belum ada
installed <- rownames(installed.packages())
for (pkg in packages_needed) {
  if (!pkg %in% installed) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

# Load semua package
library(readxl)
library(psych)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggpubr)
library(scales)
library(gridExtra)


# 1. IMPORT & PEMBERSIHAN DATA
# Sesuaikan path jika file dipindahkan
file_path <- "Final_CLEANData.xlsx"

raw_data <- read_excel(file_path, col_names = TRUE)

# Hapus kolom yang seluruhnya NA (kolom pemisah kosong di Excel)
raw_data <- raw_data[, colSums(is.na(raw_data)) < nrow(raw_data)]

# Validasi dataset yang masuk dan terbaca oleh file
cat("INFO DATASET\n")
cat("Jumlah responden :", nrow(raw_data), "\n")
cat("Nama kolom       :", paste(names(raw_data), collapse = ", "), "\n\n")

# Pisahkan data per konstruk variabel
data_X <- raw_data[, paste0("X", 1:7)]
data_Y <- raw_data[, paste0("Y", 1:7)]

# Pastikan semua kolom numerik
data_X <- mutate(data_X, across(everything(), as.numeric))
data_Y <- mutate(data_Y, across(everything(), as.numeric))


# 2. UJI RELIABILITAS DENGAN CRONBACH ALPHA
cat("============================================================\n")
cat("UJI RELIABILITAS (CRONBACH ALPHA)\n")
cat("============================================================\n\n")

cat("--- Reliabilitas Variabel X (X1–X7) ---\n")
alpha_X <- psych::alpha(data_X)
print(alpha_X$total)

cat("\n--- Reliabilitas Variabel Y (Y1–Y7) ---\n")
alpha_Y <- psych::alpha(data_Y)
print(alpha_Y$total)


# 3. ANALISIS STATISTIK DESKRIPTIF
cat("============================================================\n")
cat("ANALISIS STATISTIK DESKRIPTIF\n")
cat("============================================================\n\n")

cat("--- Statistik Deskriptif Variabel X ---\n")
print(psych::describe(data_X))

cat("\n--- Statistik Deskriptif Variabel Y ---\n")
print(psych::describe(data_Y))

# ============================================================
# PLOT 1: Kurva Sebaran Frekuensi Nilai Likert Variabel X
# ============================================================

# Ubah ke format long untuk ggplot
data_X_long <- data_X %>%
  pivot_longer(cols = everything(), names_to = "Pertanyaan", values_to = "Nilai") %>%
  group_by(Pertanyaan, Nilai) %>%
  summarise(Frekuensi = n(), .groups = "drop")

plot_freq_X <- ggplot(data_X_long, aes(x = Nilai, y = Frekuensi,
                                        color = Pertanyaan, group = Pertanyaan)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1, span = 1) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title    = "Frequency Distribution Curve of Likert Scale Scores – Variable X",
    subtitle = "Distribution of respondents' answers for each statement item (X1–X7)",
    x        = "Likert's Value (1–5)",
    y        = "Frequency",
    color    = "Point"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

print(plot_freq_X)

# ============================================================
# PLOT 2: Kurva Sebaran Frekuensi Nilai Likert Variabel Y
# ============================================================

data_Y_long <- data_Y %>%
  pivot_longer(cols = everything(), names_to = "Pertanyaan", values_to = "Nilai") %>%
  group_by(Pertanyaan, Nilai) %>%
  summarise(Frekuensi = n(), .groups = "drop")

plot_freq_Y <- ggplot(data_Y_long, aes(x = Nilai, y = Frekuensi,
                                        color = Pertanyaan, group = Pertanyaan)) +
  geom_point(size = 2.5, alpha = 0.8) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 1.1, span = 1) +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title    = "Frequency Distribution Curve of Likert Scale Scores – Variable Y",
    subtitle = "Distribution of respondents' answers for each statement item (Y1–Y7)",
    x        = "Likert Value (1–5)",
    y        = "Frequency",
    color    = "Point"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    legend.position = "bottom"
  )

print(plot_freq_Y)

# =============================================================
# PLOT 3: Kurva Gaussian, Mean Likert Variabel X per Responden
# =============================================================

mean_X_resp <- rowMeans(data_X, na.rm = TRUE)  # Mean X tiap responden
mu_X  <- mean(mean_X_resp)
sig_X <- sd(mean_X_resp)
x_seq <- seq(1, 5, length.out = 500)

gauss_X <- data.frame(
  x       = x_seq,
  density = dnorm(x_seq, mean = mu_X, sd = sig_X)
)

plot_gauss_X <- ggplot() +
  geom_histogram(
    data = data.frame(v = mean_X_resp),
    aes(x = v, y = after_stat(density)),
    bins = 15, fill = "#4E79A7", alpha = 0.4, color = "white"
  ) +
  geom_line(data = gauss_X, aes(x = x, y = density),
            color = "#2171B5", linewidth = 1.5) +
  geom_vline(xintercept = mu_X,
             linetype = "dashed", color = "red", linewidth = 1) +
  annotate("text",
           x     = mu_X + 0.05,
           y     = max(gauss_X$density) * 0.95,
           label = paste0("μ = ", round(mu_X, 3), "\nσ = ", round(sig_X, 3)),
           hjust = 0, size = 4, color = "red") +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title    = "Gaussian Curve – Average Likert Score of Variable X per Respondent",
    subtitle = "Empirical histogram + normal distribution curve",
    x        = "Likert's Mean X (1–5)",
    y        = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(plot_gauss_X)

# ============================================================
# PLOT 4: Kurva Gaussian Mean Likert Variabel Y per Responden
# ============================================================

mean_Y_resp <- rowMeans(data_Y, na.rm = TRUE)
mu_Y  <- mean(mean_Y_resp)
sig_Y <- sd(mean_Y_resp)

gauss_Y <- data.frame(
  x       = x_seq,
  density = dnorm(x_seq, mean = mu_Y, sd = sig_Y)
)

plot_gauss_Y <- ggplot() +
  geom_histogram(
    data = data.frame(v = mean_Y_resp),
    aes(x = v, y = after_stat(density)),
    bins = 15, fill = "#E15759", alpha = 0.4, color = "white"
  ) +
  geom_line(data = gauss_Y, aes(x = x, y = density),
            color = "#C0392B", linewidth = 1.5) +
  geom_vline(xintercept = mu_Y,
             linetype = "dashed", color = "navy", linewidth = 1) +
  annotate("text",
           x     = mu_Y + 0.05,
           y     = max(gauss_Y$density) * 0.95,
           label = paste0("μ = ", round(mu_Y, 3), "\nσ = ", round(sig_Y, 3)),
           hjust = 0, size = 4, color = "navy") +
  scale_x_continuous(breaks = 1:5, limits = c(1, 5)) +
  labs(
    title    = "Gaussian Curve – Average Likert Score of Variable Y per Respondent",
    subtitle = "Empirical histogram + normal distribution curve",
    x        = "Likert's Mean (1–5)",
    y        = "Density"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40")
  )

print(plot_gauss_Y)

# ============================================================
# 4. UJI KORELASI PEARSON
# ============================================================

cat("============================================================\n")
cat("TAHAP 3: UJI KORELASI PEARSON\n")
cat("============================================================\n\n")

# Hitung Mean_X dan Mean_Y per responden
Mean_X <- rowMeans(data_X, na.rm = TRUE)
Mean_Y <- rowMeans(data_Y, na.rm = TRUE)

df_cor <- data.frame(Mean_X, Mean_Y)

# Uji korelasi Pearson (termasuk p-value dan confidence interval)
hasil_korelasi <- cor.test(df_cor$Mean_X, df_cor$Mean_Y, method = "pearson")

cat("--- Hasil Uji Korelasi Pearson ---\n")
print(hasil_korelasi)

cat("\nInterpretasi Nilai r:\n")
r_val <- round(hasil_korelasi$estimate, 4)
r_kat <- dplyr::case_when(
  abs(r_val) >= 0.80 ~ "SANGAT KUAT",
  abs(r_val) >= 0.60 ~ "KUAT",
  abs(r_val) >= 0.40 ~ "SEDANG",
  abs(r_val) >= 0.20 ~ "LEMAH",
  TRUE               ~ "SANGAT LEMAH / TIDAK ADA"
)
cat("  r =", r_val, "\u2192 Korelasi", r_kat, "\n")

p_val <- hasil_korelasi$p.value
kat_p <- ifelse(p_val < 0.05, "SIGNIFIKAN (H0 ditolak)", "TIDAK SIGNIFIKAN")
cat("  p-value =", format(p_val, scientific = TRUE, digits = 4), "→", kat_p, "\n")

# ============================================================
# PLOT 5: Scatter Plot Korelasi Pearson Mean_X vs Mean_Y
# ============================================================

# Hitung batas sumbu dinamis (padding 0.1 di setiap sisi)
pad <- 0.15
xlim_sc <- c(floor(min(Mean_X) * 10) / 10 - pad,
             ceiling(max(Mean_X) * 10) / 10 + pad)
ylim_sc <- c(floor(min(Mean_Y) * 10) / 10 - pad,
             ceiling(max(Mean_Y) * 10) / 10 + pad)

plot_scatter <- ggplot(df_cor, aes(x = Mean_X, y = Mean_Y)) +
  geom_point(color = "#2C3E7A", size = 3, alpha = 0.65, shape = 16) +
  geom_smooth(method = "lm", color = "#E74C3C", linewidth = 1.3,
              fill = "#F1948A", alpha = 0.2) +
  annotate("text",
           x     = xlim_sc[1] + 0.05,
           y     = ylim_sc[2] - 0.05,
           label = paste0("r = ", round(hasil_korelasi$estimate, 3),
                          "\np = ", format(p_val, digits = 3, scientific = TRUE),
                          "\nn = ", nrow(df_cor)),
           hjust = 0, vjust = 1, size = 4.5,
           color = "#1A252F",
           fontface = "italic") +
  coord_cartesian(xlim = xlim_sc, ylim = ylim_sc) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title    = "Scatter Plot: Mean X vs Mean Y",
    subtitle = "Pearson Correlation Test between the Mean of Variable X and Variable Y",
    x        = "Mean of Variable X per Respondent",
    y        = "Mean of Variable Y per Respondent",
    caption  = "Red line = linear regression line; gray area = 95% confidence interval"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(color = "gray50", size = 9)
  )

print(plot_scatter)

# ============================================================
# 5. ANALISIS REGRESI LINEAR SEDERHANA
# ============================================================

cat("============================================================\n")
cat("TAHAP 4: ANALISIS REGRESI LINEAR SEDERHANA\n")
cat("         Y = a + b*X  (Mean_Y ~ Mean_X)\n")
cat("============================================================\n\n")

model_reg <- lm(Mean_Y ~ Mean_X, data = df_cor)
ringkasan <- summary(model_reg)

print(ringkasan)

# Ekstrak koefisien
a_intercept <- coef(model_reg)[1]
b_slope     <- coef(model_reg)[2]
r_squared   <- ringkasan$r.squared
adj_r2      <- ringkasan$adj.r.squared
f_stat      <- ringkasan$fstatistic
p_model     <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

cat("\n--- Ringkasan Regresi ---\n")
cat(sprintf("  Persamaan Regresi  : Ŷ = %.4f + %.4f · X\n", a_intercept, b_slope))
cat(sprintf("  R²                 : %.4f (%.2f%% variansi Y dijelaskan oleh X)\n",
            r_squared, r_squared * 100))
cat(sprintf("  Adjusted R²        : %.4f\n", adj_r2))
cat(sprintf("  F-statistic        : %.4f  (df1=%d, df2=%d)\n",
            f_stat[1], f_stat[2], f_stat[3]))
kat_model <- ifelse(p_model < 0.05, "Model SIGNIFIKAN", "Model TIDAK SIGNIFIKAN")
cat(sprintf("  p-value model      : %s  → %s\n",
            format(p_model, scientific = TRUE, digits = 4),
            kat_model))

# ============================================================
# PLOT 6: Scatter Plot + Garis Regresi + Persamaan
# ============================================================

eq_label <- paste0(
  "Ŷ = ", round(a_intercept, 3), " + ", round(b_slope, 3), "X",
  "\nR² = ", round(r_squared, 4),
  "\np = ", format(p_model, digits = 3, scientific = TRUE)
)

plot_regresi <- ggplot(df_cor, aes(x = Mean_X, y = Mean_Y)) +
  geom_point(color = "#154360", size = 3, alpha = 0.65, shape = 16) +
  geom_smooth(method = "lm", color = "#E74C3C", linewidth = 1.3,
              fill = "#F5B7B1", alpha = 0.25, se = TRUE) +
  annotate("label",
           x        = xlim_sc[1] + 0.05,
           y        = ylim_sc[2] - 0.05,
           label    = eq_label,
           hjust    = 0, vjust = 1,
           size     = 4, color = "#1A252F",
           fill     = "white",
           fontface = "italic") +
  coord_cartesian(xlim = xlim_sc, ylim = ylim_sc) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 6)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) +
  labs(
    title    = "Simple Linear Regression: Mean X vs Mean Y",
    subtitle = "The effect of the average score of Variable X on the average score of Variable Y",
    x        = "Mean of Variable X (Independent)",
    y        = "Mean of Variable Y (Dependent)",
    caption  = "Red line = regression line; gray area = 95% confidence interval"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5, size = 14),
    plot.subtitle = element_text(hjust = 0.5, color = "gray40"),
    plot.caption  = element_text(color = "gray50", size = 9)
  )

print(plot_regresi)

# ============================================================
# 6. ASUMSI REGRESI: PLOT DIAGNOSTIK
# ============================================================

cat("\n--- Plot Diagnostik Model Regresi ---\n")
cat("(Residuals vs Fitted, Q-Q Plot, Scale-Location, Leverage)\n")

par(mfrow = c(2, 2))
plot(model_reg, pch = 16, col = "#2C3E7A")
par(mfrow = c(1, 1))
