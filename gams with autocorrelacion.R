library(readxl)
library(dplyr)
library(ggplot2)
library(mgcv)
library(nlme)       # necesario para corAR1
library(patchwork)

# -------------------------------
# Load and prepare data
# -------------------------------
laselva <- read_excel("laselva.xlsx")

laselva_z <- laselva %>%
  rename(ONI_First = `ONI-First`) %>%
  mutate(across(c(`CG-Rich`, `Fi-Rich`, `Pi-Rich`, `Pr-Rich`, `Sc-Rich`, `Sh-Rich`,
                  `Total-Rich`, Shannon_H, `Simpson_1-D`, `FamRichness`,
                  `Tricho`, `Dipt`, `Non-Insects`),
                ~ scale(.)[,1], .names = "{.col}_z"))

names(laselva_z) <- gsub("-", "_", names(laselva_z))

# -------------------------------
# Function to create GAM plot
# -------------------------------
plot_gam <- function(model, data, y_var, y_label, line_color){
  new_data <- data.frame(ONI_First = seq(min(data$ONI_First, na.rm = TRUE),
                                         max(data$ONI_First, na.rm = TRUE),
                                         length.out = 200))
  
  pred <- predict(model$gam, newdata = new_data, se.fit = TRUE)
  new_data$fit <- pred$fit
  new_data$se <- pred$se.fit
  
  ggplot() +
    geom_point(data = data, aes_string(x = "ONI_First", y = y_var),
               color = "grey60", alpha = 0.6) +
    geom_line(data = new_data, aes(x = ONI_First, y = fit),
              color = line_color, linewidth = 1.2) +
    geom_ribbon(data = new_data, aes(x = ONI_First, ymin = fit - 2*se, ymax = fit + 2*se),
                fill = line_color, alpha = 0.2) +
    theme_minimal(base_size = 13) +
    labs(x = "ONI (Oceanic Niño Index)",
         y = y_label) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
}

# -------------------------------
# Fit GAMMs with AR1 autocorrelation
# -------------------------------
mod1 <- gamm(Total_Rich_z ~ s(ONI_First),
             data = laselva_z,
             correlation = corAR1(form = ~1))
mod2 <- gamm(Shannon_H_z ~ s(ONI_First),
             data = laselva_z,
             correlation = corAR1(form = ~1))
mod3 <- gamm(Simpson_1_D_z ~ s(ONI_First),
             data = laselva_z,
             correlation = corAR1(form = ~1))
mod4 <- gamm(FamRichness_z ~ s(ONI_First),
             data = laselva_z,
             correlation = corAR1(form = ~1))

# -------------------------------
# Summaries
# -------------------------------
summary(mod1$gam)
summary(mod2$gam)
summary(mod3$gam)
summary(mod4$gam)

# -------------------------------
# Check autocorrelation of residuals
# -------------------------------
acf(residuals(mod1$lme, type = "normalized"), main = "ACF Residuals - Total_Rich_z")
acf(residuals(mod2$lme, type = "normalized"), main = "ACF Residuals - Shannon_H_z")
acf(residuals(mod3$lme, type = "normalized"), main = "ACF Residuals - Simpson_1_D_z")
acf(residuals(mod4$lme, type = "normalized"), main = "ACF Residuals - FamRichness_z")

# Lag-1 autocorrelation numeric values
acf(residuals(mod1$lme, type = "normalized"), plot = FALSE)$acf[2]
acf(residuals(mod2$lme, type = "normalized"), plot = FALSE)$acf[2]
acf(residuals(mod3$lme, type = "normalized"), plot = FALSE)$acf[2]
acf(residuals(mod4$lme, type = "normalized"), plot = FALSE)$acf[2]

# -------------------------------
# Create plots
# -------------------------------
plot_tr <- plot_gam(mod1, laselva_z, "Total_Rich_z", "Total Richness (z-score)", "blue")
plot_shannon <- plot_gam(mod2, laselva_z, "Shannon_H_z", "Shannon Diversity (z-score)", "darkred")
plot_simpson <- plot_gam(mod3, laselva_z, "Simpson_1_D_z", "Simpson 1-D (z-score)", "darkgreen")
plot_family <- plot_gam(mod4, laselva_z, "FamRichness_z", "Family Richness (z-score)", "purple")

# -------------------------------
# Combine plots 2x2 with patchwork
# -------------------------------
combined_plot <- (plot_tr | plot_shannon) / (plot_simpson | plot_family) +
  plot_annotation(title = "Effects of ONI on variable X (GAMM with AR1)",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16)))

combined_plot
