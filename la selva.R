



laselva <- read_excel("laselva.xlsx")
names(laselva)

laselva_z <- laselva %>%
  rename(ONI_First = `ONI-First`) %>%
  mutate(across(c(`CG-Rich`, `Fi-Rich`, `Pi-Rich`, `Pr-Rich`, `Sc-Rich`, `Sh-Rich`,
                  `Total-Rich`, Shannon_H, `Simpson_1-D`, `FamRichness`,
                  `Tricho`, `Dipt`, `Non-Insects`),
                ~ scale(.)[,1], .names = "{.col}_z"))


mean(laselva_z$`Simpson_1_D_z`)
sd(laselva_z$`Simpson_1_D_z`)


install.packages("writexl")
library(writexl)
write_xlsx(laselva_z, "laselva_z.xlsx")


oni_range <- data.frame(ONI_First = c(-1.1, 1.9))


# Total richnes -----------------------------------------------------------
# 3. Definir función bootstrap
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Total-Rich_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")


# Shannon_H ---------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Shannon_H_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")


# Simpson_1-D_z ---------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Simpson_1-D_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")

# Crear dataframe con todos los replicados
boot_df <- data.frame(delta = boot_res$t)

# Graficar distribución tipo violin/density
ggplot(boot_df, aes(x = 1, y = delta)) +
  geom_violin(fill = "skyblue", alpha = 0.5, color = "blue") +  # forma general
  geom_boxplot(width = 0.1, outlier.shape = NA) +                # boxplot al centro
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +  # referencia cero
  ylab("Cambio predicho (ONI -1.1 → 1.9)") +
  xlab("") +
  theme_minimal() +
  coord_flip()  # girar para que quede acostado


# CG-Rich ---------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`CG-Rich` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")


# Fi-Rich_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Fi-Rich_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")

# Pr-Rich_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Pr-Rich_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")



names(laselva_z)
# Sc-Rich_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Sc-Rich_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")

names(laselva_z)
# FamRichness_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`FamRichness_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")


names(laselva_z)
# Tricho_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Tricho_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")


names(laselva_z)
# Dipt_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Dipt_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")


names(laselva_z)
# Dipt_z -----------------------------------------------------------------
boot_fun <- function(data, indices) {
  d <- data[indices, ]
  model <- lm(`Non-Insects_z` ~ ONI_First, data = d)
  preds <- predict(model, newdata = oni_range)
  return(preds[2] - preds[1])
}

# 4. Ejecutar bootstrap
boot_res <- boot(data = laselva_z, statistic = boot_fun, R = 10000)
boot.ci(boot_res, type = "perc")
