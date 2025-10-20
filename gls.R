library(nlme)
library(ggplot2)

# Modelo cuadrático con autocorrelación temporal AR(1)
gls_quad <- gls(Total_Rich_z ~ ONI_First + I(ONI_First^2),
                data = laselva_z,
                correlation = corAR1(form = ~1))

summary(gls_quad)

# Crear una secuencia de valores de ONI
new_data <- data.frame(ONI_First = seq(min(laselva_z$ONI_First, na.rm = TRUE),
                                       max(laselva_z$ONI_First, na.rm = TRUE),
                                       length.out = 100))

# Predicciones
new_data$fit <- predict(gls_quad, newdata = new_data)

# Graficar
ggplot(laselva_z, aes(x = ONI_First, y = Total_Rich_z)) +
  geom_point(alpha = 0.6) +
  geom_line(data = new_data, aes(x = ONI_First, y = fit),
            color = "blue", size = 1.2) +
  theme_minimal(base_size = 14) +
  labs(x = "ONI (Oceanic Niño Index)",
       y = "Standardized Total Richness (z)",
       title = "Quadratic relationship between ONI and Total Richness (GLS with AR(1))")
