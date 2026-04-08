# CASO: HOLLYWOOD RULES
Ivan Mayorga, Cristóbal Herrera
# Analítica de Negocios – Pontificia Universidad Javeriana
# =============================================================
if (!require(readxl))   install.packages("readxl")
if (!require(ggplot2))  install.packages("ggplot2")
if (!require(dplyr))    install.packages("dplyr")
if (!require(car))      install.packages("car")
if (!require(scales))   install.packages("scales")
if (!require(gridExtra))install.packages("gridExtra")
 
library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(scales)
library(gridExtra)
# ── Tema visual cinematográfico ────────────────────────────────
theme_hollywood <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.background    = element_rect(fill = "#1a1a2e", color = NA),
      panel.background   = element_rect(fill = "#16213e", color = NA),
      panel.grid.major   = element_line(color = "#2c2c4e", linewidth = 0.4),
      panel.grid.minor   = element_blank(),
      plot.title         = element_text(color = "#e2b96f", face = "bold", size = 14),
      plot.subtitle      = element_text(color = "#f5e6c8", size = 10),
      axis.title         = element_text(color = "#f5e6c8"),
      axis.text          = element_text(color = "#f5e6c8"),
      legend.background  = element_rect(fill = "#1a1a2e"),
      legend.text        = element_text(color = "#f5e6c8"),
      legend.title       = element_text(color = "#e2b96f"),
      strip.text         = element_text(color = "#e2b96f", face = "bold")
    )
}# ── Cargar datos ───────────────────────────────────────────────
df <- read_excel("Hollywood__1_.xls", sheet = "Exhibit 1")
df <- df %>% mutate(
  Genre  = trimws(Genre),
  Comedy = as.integer(Genre == "Comedy"),
  ROI_US = (`Total U.S. Gross` - Budget) / Budget,
  `Comedy_Critics` = Comedy * `Critics´ Opinion`
)cat("\n========================================================\n")
cat("           HOLLYWOOD RULES – ANÁLISIS COMPLETO\n")
cat("========================================================\n")
 
# =============================================================
# PREGUNTA 1: Estadísticas descriptivas
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 1 – Estadísticas Descriptivas\n")
cat("──────────────────────────────────────────────────────\n")
 
vars_desc <- c("Opening Gross", "Total U.S. Gross", "Total Non-U.S. Gross", "Opening Theatres")
for (v in vars_desc) {
  cat(sprintf("\n%s:\n  Mínimo:   %s\n  Promedio: %s\n  Máximo:   %s\n",
    v,
    format(min(df[[v]]),  big.mark = ",", scientific = FALSE),
    format(round(mean(df[[v]])), big.mark = ",", scientific = FALSE),
    format(max(df[[v]]),  big.mark = ",", scientific = FALSE)
  ))
}
cat(sprintf("\nNúmero de comedias:          %d\n", sum(df$Comedy)))
cat(sprintf("Número de películas R-rated: %d\n", sum(df$MPAA_D)))
 
# Gráfica Q1
p1a <- ggplot(df, aes(x = `Opening Gross` / 1e6)) +
  geom_histogram(fill = "#e2b96f", color = "#1a1a2e", bins = 20) +
  labs(title = "Distribución: Opening Gross", x = "Millones USD", y = "Frecuencia") +
  theme_hollywood()
 
p1b <- ggplot(df, aes(x = `Total U.S. Gross` / 1e6)) +
  geom_histogram(fill = "#2980b9", color = "#1a1a2e", bins = 20) +
  labs(title = "Distribución: Total U.S. Gross", x = "Millones USD", y = "Frecuencia") +
  theme_hollywood()
 
p1c <- ggplot(df, aes(x = reorder(Genre, Genre, function(x) -length(x)))) +
  geom_bar(fill = "#e2b96f", color = "#1a1a2e") +
  labs(title = "Películas por Género", x = "Género", y = "Cantidad") +
  theme_hollywood() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
 
p1d <- ggplot(df, aes(x = MPAA, fill = MPAA)) +
  geom_bar(color = "#1a1a2e") +
  scale_fill_manual(values = c("G"="#27ae60","PG"="#2980b9","PG-13"="#e2b96f","R"="#c0392b","NR"="#8e44ad")) +
  labs(title = "Películas por Rating MPAA", x = "Rating", y = "Cantidad") +
  theme_hollywood() + theme(legend.position = "none")
 
grid.arrange(p1a, p1b, p1c, p1d, ncol = 2,
  top = grid::textGrob("PREGUNTA 1 – Estadísticas Descriptivas",
                        gp = grid::gpar(col = "#e2b96f", fontsize = 15, fontface = "bold")))
 
 
# =============================================================
# PREGUNTA 2: ROI y prueba de hipótesis
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 2 – ROI y Prueba de Hipótesis\n")
cat("──────────────────────────────────────────────────────\n")
 
n        <- nrow(df)
mean_roi <- mean(df$ROI_US)
sd_roi   <- sd(df$ROI_US)
se_roi   <- sd_roi / sqrt(n)
t_crit   <- qt(0.975, df = n - 1)
ci_lo    <- mean_roi - t_crit * se_roi
ci_hi    <- mean_roi + t_crit * se_roi
 
cat(sprintf("2a. ROI promedio U.S.: %.4f  (%.2f%%)\n", mean_roi, mean_roi * 100))
cat(sprintf("2b. IC 95%% ROI: [%.4f, %.4f]  =>  [%.2f%%, %.2f%%]\n",
            ci_lo, ci_hi, ci_lo * 100, ci_hi * 100))
 
# H0: mu = 0.12  vs  H1: mu > 0.12  (one-sided)
t_test2c <- t.test(df$ROI_US, mu = 0.12, alternative = "greater")
cat(sprintf("2c. H0: mu_ROI = 0.12  vs  H1: mu_ROI > 0.12\n"))
cat(sprintf("    t-stat: %.4f   p-valor: %.6f\n", t_test2c$statistic, t_test2c$p.value))
cat(sprintf("    Conclusión: %s al 5%%\n",
    ifelse(t_test2c$p.value < 0.05,
           "SE RECHAZA H0 → ROI significativamente mayor al 12%",
           "No se rechaza H0")))
 
# Gráfica Q2: distribución ROI con IC
p2 <- ggplot(df, aes(x = ROI_US * 100)) +
  geom_histogram(aes(y = after_stat(density)), fill = "#2980b9", color = "#1a1a2e", bins = 20, alpha = 0.8) +
  geom_density(color = "#e2b96f", linewidth = 1) +
  geom_vline(xintercept = mean_roi * 100, color = "#e2b96f", linetype = "dashed", linewidth = 1.2) +
  geom_vline(xintercept = 12, color = "#c0392b", linetype = "dotted", linewidth = 1.2) +
  geom_vline(xintercept = c(ci_lo * 100, ci_hi * 100), color = "#27ae60", linetype = "dashed") +
  annotate("text", x = mean_roi * 100 + 5, y = 0.005, label = sprintf("Media\n%.1f%%", mean_roi*100),
           color = "#e2b96f", size = 3) +
  annotate("text", x = 12 + 5, y = 0.007, label = "12%\n(London)", color = "#c0392b", size = 3) +
  labs(title = "Pregunta 2 – Distribución del ROI (U.S.)",
       subtitle = "Línea dorada = media muestral | Línea roja = 12% de London | Líneas verdes = IC 95%",
       x = "ROI (%)", y = "Densidad") +
  theme_hollywood()
print(p2)
 
 
# =============================================================
# PREGUNTA 3: Comedias vs No-comedias
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 3 – Comedias vs No-comedias\n")
cat("──────────────────────────────────────────────────────\n")
 
t3a <- t.test(`Total U.S. Gross` ~ Comedy, data = df)
cat(sprintf("3a. Media comedias:     $%s\n", format(round(mean(df$`Total U.S. Gross`[df$Comedy==1])), big.mark=",")))
cat(sprintf("    Media no-comedias:  $%s\n", format(round(mean(df$`Total U.S. Gross`[df$Comedy==0])), big.mark=",")))
cat(sprintf("    t-stat: %.4f   p-valor: %.4f\n", t3a$statistic, t3a$p.value))
cat(sprintf("    Conclusión: %s al 5%%\n",
    ifelse(t3a$p.value < 0.05, "Diferencia SIGNIFICATIVA", "SIN diferencia significativa")))
 
t3b <- t.test(ROI_US ~ Comedy, data = df)
cat(sprintf("\n3b. ROI comedias:       %.4f\n", mean(df$ROI_US[df$Comedy==1])))
cat(sprintf("    ROI no-comedias:    %.4f\n", mean(df$ROI_US[df$Comedy==0])))
cat(sprintf("    t-stat: %.4f   p-valor: %.4f\n", t3b$statistic, t3b$p.value))
cat(sprintf("    Conclusión: %s al 5%%\n",
    ifelse(t3b$p.value < 0.05, "Diferencia SIGNIFICATIVA", "SIN diferencia significativa")))
 
# Gráfica Q3
df$GenreGroup <- ifelse(df$Comedy == 1, "Comedia", "No Comedia")
p3a <- ggplot(df, aes(x = GenreGroup, y = `Total U.S. Gross` / 1e6, fill = GenreGroup)) +
  geom_boxplot(color = "#f5e6c8", alpha = 0.8) +
  scale_fill_manual(values = c("Comedia" = "#e2b96f", "No Comedia" = "#2980b9")) +
  labs(title = "3a. Total U.S. Gross: Comedias vs No-comedias",
       x = "", y = "Millones USD") +
  theme_hollywood() + theme(legend.position = "none")
 
p3b <- ggplot(df, aes(x = GenreGroup, y = ROI_US * 100, fill = GenreGroup)) +
  geom_boxplot(color = "#f5e6c8", alpha = 0.8) +
  scale_fill_manual(values = c("Comedia" = "#e2b96f", "No Comedia" = "#2980b9")) +
  labs(title = "3b. ROI: Comedias vs No-comedias",
       x = "", y = "ROI (%)") +
  theme_hollywood() + theme(legend.position = "none")
 
grid.arrange(p3a, p3b, ncol = 2,
  top = grid::textGrob("PREGUNTA 3 – Comparación Comedias vs No-comedias",
                        gp = grid::gpar(col = "#e2b96f", fontsize = 13, fontface = "bold")))
 
 
# =============================================================
# PREGUNTA 4: R-rated vs otras
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 4 – R-rated vs Otras Categorías\n")
cat("──────────────────────────────────────────────────────\n")
 
t4 <- t.test(`Total U.S. Gross` ~ MPAA_D, data = df)
cat(sprintf("Media R-rated:  $%s\n", format(round(mean(df$`Total U.S. Gross`[df$MPAA_D==1])), big.mark=",")))
cat(sprintf("Media otras:    $%s\n", format(round(mean(df$`Total U.S. Gross`[df$MPAA_D==0])), big.mark=",")))
cat(sprintf("t-stat: %.4f   p-valor: %.4f\n", t4$statistic, t4$p.value))
cat(sprintf("Conclusión: %s al 5%%\n",
    ifelse(t4$p.value < 0.05, "Diferencia SIGNIFICATIVA", "SIN diferencia significativa")))
 
df$RatingGroup <- ifelse(df$MPAA_D == 1, "R-rated", "Otras")
p4 <- ggplot(df, aes(x = RatingGroup, y = `Total U.S. Gross` / 1e6, fill = RatingGroup)) +
  geom_boxplot(color = "#f5e6c8", alpha = 0.8) +
  scale_fill_manual(values = c("R-rated" = "#c0392b", "Otras" = "#27ae60")) +
  labs(title = "Pregunta 4 – Total U.S. Gross: R-rated vs Otras",
       x = "", y = "Millones USD") +
  theme_hollywood() + theme(legend.position = "none")
print(p4)
 
 
