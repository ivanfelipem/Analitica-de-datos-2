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
 
# PREGUNTA 5: Regresión preproducción → Total U.S. Gross
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 5 – Regresión Preproducción\n")
cat("──────────────────────────────────────────────────────\n")

m5a <- lm(Total U.S. Gross ~ Budget + Comedy + MPAA_D + Known Story + Sequel, data = df)
cat("5a. Modelo completo:\n")
print(summary(m5a))

# Eliminar variables con p > 0.10
m5b <- lm(Total U.S. Gross ~ Budget + Sequel, data = df)
cat("\n5b. Modelo final (variables significativas al 10%):\n")
print(summary(m5b))
cat(sprintf("\n5c. Coeficiente Sequel: $%s\n", format(round(coef(m5b)["Sequel"]), big.mark=",")))
cat("    → Las SECUELAS generan mayor Total U.S. Gross, ceteris paribus.\n")

# Gráfica Q5: valores reales vs ajustados
df$fitted5 <- fitted(m5b)
p5 <- ggplot(df, aes(x = fitted5 / 1e6, y = Total U.S. Gross / 1e6)) +
  geom_point(color = "#e2b96f", alpha = 0.7, size = 2.5) +
  geom_abline(slope = 1, intercept = 0, color = "#c0392b", linetype = "dashed", linewidth = 1) +
  labs(title = "Pregunta 5 – Valores Ajustados vs Reales",
       subtitle = "Línea roja = ajuste perfecto",
       x = "Predicho (M USD)", y = "Real (M USD)") +
  theme_hollywood()
print(p5)


# =============================================================
# PREGUNTA 6: Regresión Opening Weekend Gross
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 6 – Regresión Opening Weekend\n")
cat("──────────────────────────────────────────────────────\n")

m6a <- lm(Opening Gross ~ Budget + Comedy + MPAA_D + Known Story + Sequel +
            Opening Theatres + Summer + Holiday + Christmas, data = df)
cat("6a. Modelo completo:\n")
print(summary(m6a))

m6b <- lm(Opening Gross ~ Budget + Sequel + Opening Theatres + Summer, data = df)
cat("\n6b. Modelo final:\n")
print(summary(m6b))

cat("\n6c. Interpretación de coeficientes:\n")
coefs6 <- coef(m6b)
cat(sprintf("  Budget:           Por cada $1 adicional en presupuesto, Opening Gross aumenta $%.2f\n", coefs6["Budget"]))
cat(sprintf("  Sequel:           Las secuelas generan $%s más en opening weekend\n", format(round(coefs6["Sequel"]), big.mark=",")))
cat(sprintf("  Opening Theatres: Cada teatro adicional aporta $%s al opening gross\n", format(round(coefs6["Opening Theatres"]), big.mark=",")))
cat(sprintf("  Summer:           Estrenar en verano reduce el opening en $%s\n", format(round(abs(coefs6["Summer"])), big.mark=",")))

# 6d: +100 theatres
se_theatres <- summary(m6b)$coefficients["Opening Theatres", "Std. Error"]
t_crit6     <- qt(0.975, df = m6b$df.residual)
point6d     <- 100 * coefs6["Opening Theatres"]
ci_lo6d     <- 100 * (coefs6["Opening Theatres"] - t_crit6 * se_theatres)
ci_hi6d     <- 100 * (coefs6["Opening Theatres"] + t_crit6 * se_theatres)
cat(sprintf("\n6d. +100 teatros → cambio esperado en Opening Gross:\n"))
cat(sprintf("    Estimación puntual: $%s\n", format(round(point6d), big.mark=",")))
cat(sprintf("    IC 95%%: [$%s, $%s]\n", format(round(ci_lo6d), big.mark=","), format(round(ci_hi6d), big.mark=",")))

# Gráfica Q6
df$fitted6 <- fitted(m6b)
p6 <- ggplot(df, aes(x = Opening Theatres, y = Opening Gross / 1e6, color = factor(Sequel))) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, aes(group = 1), color = "#e2b96f", fill = "#e2b96f44") +
  scale_color_manual(values = c("0" = "#2980b9", "1" = "#c0392b"),
                     labels = c("No secuela", "Secuela"), name = "") +
  labs(title = "Pregunta 6 – Opening Gross vs Número de Teatros",
       x = "Teatros en opening", y = "Opening Gross (M USD)") +
  theme_hollywood()
print(p6)

# =============================================================
# PREGUNTA 7: Total U.S. Gross ~ Opening Gross
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 7 – Total U.S. Gross vs Opening Gross\n")
cat("──────────────────────────────────────────────────────\n")

# 7a: con intercepto
m7a <- lm(Total U.S. Gross ~ Opening Gross, data = df)
cat("7a. Regresión simple (con intercepto):\n")
print(summary(m7a))

slope7a <- coef(m7a)["Opening Gross"]
se7a    <- summary(m7a)$coefficients["Opening Gross", "Std. Error"]
cat(sprintf("\n7b. Si 25%% fuera el opening → slope debería ser 4.0\n"))
cat(sprintf("    Slope estimado: %.4f\n", slope7a))

# 7c: test slope = 4
t_stat7c <- (slope7a - 4) / se7a
p_val7c  <- 2 * pt(abs(t_stat7c), df = m7a$df.residual, lower.tail = FALSE)
cat(sprintf("\n7c. H0: slope = 4  vs  H1: slope ≠ 4\n"))
cat(sprintf("    t-stat: %.4f   p-valor: %.6f\n", t_stat7c, p_val7c))
cat(sprintf("    Conclusión: %s al 5%%\n",
    ifelse(p_val7c < 0.05, "SE RECHAZA H0", "No se rechaza H0")))

cat("\n7d. Crítica: la regresión CON intercepto no es correcta para\n")
cat("    evaluar la hipótesis 'Total = 4 × Opening', ya que ésta\n")
cat("    implica un modelo SIN intercepto. El intercepto absorbe\n")
cat("    variación y sesga el slope estimado.\n")

# 7e: sin intercepto
m7e <- lm(Total U.S. Gross ~ Opening Gross - 1, data = df)
cat("\n7e. Regresión sin intercepto:\n")
print(summary(m7e))

slope7e <- coef(m7e)["Opening Gross"]
se7e    <- summary(m7e)$coefficients["Opening Gross", "Std. Error"]
t_stat7f <- (slope7e - 4) / se7e
p_val7f  <- 2 * pt(abs(t_stat7f), df = m7e$df.residual, lower.tail = FALSE)
cat(sprintf("\n7f. H0: slope = 4  (sin intercepto)\n"))
cat(sprintf("    t-stat: %.4f   p-valor: %.6f\n", t_stat7f, p_val7f))
cat(sprintf("    Conclusión: %s al 5%%\n",
    ifelse(p_val7f < 0.05, "SE RECHAZA H0", "No se rechaza H0")))

cat(sprintf("\n7g. R² (con intercepto): %.4f  → %.1f%% de la variación\n",
            summary(m7a)$r.squared, summary(m7a)$r.squared * 100))

# Gráfica Q7
p7 <- ggplot(df, aes(x = Opening Gross / 1e6, y = Total U.S. Gross / 1e6)) +
  geom_point(color = "#e2b96f", alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", color = "#2980b9", fill = "#2980b944", linewidth = 1.2) +
  geom_abline(slope = 4, intercept = 0, color = "#c0392b", linetype = "dashed", linewidth = 1) +
  annotate("text", x = 50, y = 180, label = "Slope = 4 (hipótesis)", color = "#c0392b", size = 3.5) +
  annotate("text", x = 50, y = 160, label = sprintf("Slope estimado = %.2f", slope7a), color = "#2980b9", size = 3.5) +
  labs(title = "Pregunta 7 – Total U.S. Gross vs Opening Gross",
       subtitle = "Azul = regresión estimada | Rojo = hipótesis (slope = 4)",
       x = "Opening Gross (M USD)", y = "Total U.S. Gross (M USD)") +
  theme_hollywood()
print(p7)

# =============================================================
# PREGUNTA 8: Post-opening → Total U.S. Gross
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 8 – Regresión Post-Opening Weekend\n")
cat("──────────────────────────────────────────────────────\n")

m8a <- lm(Total U.S. Gross ~ Budget + Comedy + MPAA_D + Known Story + Sequel +
            Opening Theatres + Summer + Holiday + Christmas +
            Opening Gross + Critics´ Opinion, data = df)
cat("8a. Modelo completo:\n")
print(summary(m8a))

m8b <- lm(Total U.S. Gross ~ Budget + MPAA_D + Opening Gross + Critics´ Opinion, data = df)
cat("\n8b. Modelo final (variables significativas al 10%):\n")
print(summary(m8b))

# 8c: Flags of Our Fathers
flags <- df[grepl("Flags", df$Movie, ignore.case = TRUE), ]
cat("\nDatos de Flags of Our Fathers:\n")
print(flags[, c("Movie","Budget","MPAA_D","Opening Gross","Critics´ Opinion","Total U.S. Gross")])

pred8c <- predict(m8b, newdata = flags, interval = "prediction", level = 0.95)
cat(sprintf("\n8c. Predicción para Flags of Our Fathers:\n"))
cat(sprintf("    Estimación puntual: $%s\n", format(round(pred8c[,"fit"]), big.mark=",")))
cat(sprintf("    IP 95%%: [$%s, $%s]\n", format(round(pred8c[,"lwr"]), big.mark=","),
                                          format(round(pred8c[,"upr"]), big.mark=",")))
cat(sprintf("    Valor real: $%s\n", format(flags$Total U.S. Gross, big.mark=",")))

coef_critics <- coef(m8b)["Critics´ Opinion"]
gain_10 <- 10 * coef_critics
cat(sprintf("\n8d. Coef Critics' Opinion: $%s por punto\n", format(round(coef_critics), big.mark=",")))
cat(sprintf("    +10 puntos → +$%s en Total U.S. Gross\n", format(round(gain_10), big.mark=",")))
cat(sprintf("    Máximo a invertir para +10 puntos: $%s\n", format(round(gain_10), big.mark=",")))

# Gráfica Q8: Critics vs Total US Gross
p8 <- ggplot(df, aes(x = Critics´ Opinion, y = Total U.S. Gross / 1e6, color = factor(MPAA_D))) +
  geom_point(alpha = 0.7, size = 2.5) +
  geom_smooth(method = "lm", aes(group = 1), color = "#e2b96f", fill = "#e2b96f44") +
  scale_color_manual(values = c("0" = "#2980b9", "1" = "#c0392b"),
                     labels = c("Otras", "R-rated"), name = "Rating") +
  labs(title = "Pregunta 8 – Opinión de Críticos vs Total U.S. Gross",
       x = "Critics' Opinion Score (0-100)", y = "Total U.S. Gross (M USD)") +
  theme_hollywood()
print(p8)


# =============================================================
# PREGUNTA 9: Interacción Comedy × Critics
# =============================================================
cat("\n──────────────────────────────────────────────────────\n")
cat("PREGUNTA 9 – Interacción Comedias × Críticos\n")
cat("──────────────────────────────────────────────────────\n")

m9 <- lm(Total U.S. Gross ~ Budget + MPAA_D + Opening Gross + Critics´ Opinion + Comedy_Critics,
         data = df)
print(summary(m9))

p_interact <- summary(m9)$coefficients["Comedy_Critics", "Pr(>|t|)"]
cat(sprintf("\nCoef interacción Comedy×Critics: $%s\n", format(round(coef(m9)["Comedy_Critics"]), big.mark=",")))
cat(sprintf("p-valor: %.4f\n", p_interact))
cat(sprintf("Conclusión: La teoría de Griffith %s al 10%%\n",
    ifelse(p_interact < 0.10, "SE CONFIRMA", "NO SE PUEDE CONFIRMAR")))

# Gráfica Q9: Critics vs Gross por grupo
p9 <- ggplot(df, aes(x = Critics´ Opinion, y = Total U.S. Gross / 1e6, color = GenreGroup)) +
  geom_point(alpha = 0.6, size = 2.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.15) +
  scale_color_manual(values = c("Comedia" = "#e2b96f", "No Comedia" = "#2980b9")) +
  labs(title = "Pregunta 9 – Critics' Opinion vs Total U.S. Gross por Género",
       subtitle = "¿Afectan menos las críticas a las comedias?",
       x = "Critics' Opinion Score", y = "Total U.S. Gross (M USD)",
       color = "Género") +
  theme_hollywood()
print(p9)

