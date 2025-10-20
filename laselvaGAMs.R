



# load data 
laselva <- read_excel("laselva.xlsx")
names(laselva)

laselva_z <- laselva %>%
  rename(ONI_First = `ONI-First`) %>%
  mutate(across(c(`CG-Rich`, `Fi-Rich`, `Pi-Rich`, `Pr-Rich`, `Sc-Rich`, `Sh-Rich`,
                  `Total-Rich`, Shannon_H, `Simpson_1-D`, `FamRichness`,
                  `Tricho`, `Dipt`, `Non-Insects`),
                ~ scale(.)[,1], .names = "{.col}_z"))

names(laselva_z)

# Replace hyphens with underscores in column names
names(laselva_z) <- gsub("-", "_", names(laselva_z))
names(laselva_z)




# Total Richness ----------------------------------------------------------
mod1 <- gam(Total_Rich_z ~ s(ONI_First), data = laselva_z)
summary(mod1)
plot(mod1, pages = 1, shade = TRUE)

# Create predictions on a sequence of ONI_First values
new_data <- data.frame(ONI_First = seq(min(laselva_z$ONI_First, na.rm = TRUE),
                                       max(laselva_z$ONI_First, na.rm = TRUE),
                                       length.out = 200))

# Get predictions and standard error
pred <- predict(mod1, newdata = new_data, se.fit = TRUE)

# Add predictions to the new data.frame
new_data$fit <- pred$fit
new_data$se <- pred$se.fit

# Plotting
tr <- ggplot() +
  geom_point(data = laselva_z, aes(x = ONI_First, y = Total_Rich_z),
             color = "grey60", alpha = 0.6) +
  geom_line(data = new_data, aes(x = ONI_First, y = fit),
            color = "blue", linewidth = 1.2) +
  geom_ribbon(data = new_data, aes(x = ONI_First, ymin = fit - 2*se, ymax = fit + 2*se),
              fill = "blue", alpha = 0.2) +
  theme_minimal(base_size = 13) +
  labs(x = "ONI (Oceanic Niño Index)",
       y = "Total Richness (z-score)",
       title = "") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

tr

# Shannon ------------------------------------------------------------------
mod2 <- gam(Shannon_H_z ~ s(ONI_First), data = laselva_z)
summary(mod2)
plot(mod2, pages = 1, shade = TRUE)


new_data <- data.frame(ONI_First = seq(min(laselva_z$ONI_First, na.rm = TRUE),
                                       max(laselva_z$ONI_First, na.rm = TRUE),
                                       length.out = 200))


pred <- predict(mod2, newdata = new_data, se.fit = TRUE)
new_data$fit <- pred$fit
new_data$se <- pred$se.fit


shannon_plot <- ggplot() +
  geom_point(data = laselva_z, aes(x = ONI_First, y = Shannon_H_z),
             color = "grey60", alpha = 0.6) +
  geom_line(data = new_data, aes(x = ONI_First, y = fit),
            color = "darkred", linewidth = 1.2) +
  geom_ribbon(data = new_data, aes(x = ONI_First, ymin = fit - 2*se, ymax = fit + 2*se),
              fill = "red", alpha = 0.2) +
  theme_minimal(base_size = 13) +
  labs(x = "ONI (Oceanic Niño Index)",
       y = "Shannon Diversity (H')",
       title = "") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

shannon_plot

# Simpson_1_D -------------------------------------------------------------
mod3 <- gam(Simpson_1_D_z ~ s(ONI_First), data = laselva_z)
summary(mod3)
plot(mod3, pages = 1, shade = TRUE)

pred <- predict(mod3, newdata = new_data, se.fit = TRUE)
new_data$fit <- pred$fit
new_data$se <- pred$se.fit


Simpson_plot <- ggplot() +
  geom_point(data = laselva_z, aes(x = ONI_First, y = Simpson_1_D_z),
             color = "grey60", alpha = 0.6) +
  geom_line(data = new_data, aes(x = ONI_First, y = fit),
            color = "darkred", linewidth = 1.2) +
  geom_ribbon(data = new_data, aes(x = ONI_First, ymin = fit - 2*se, ymax = fit + 2*se),
              fill = "red", alpha = 0.2) +
  theme_minimal(base_size = 13) +
  labs(x = "ONI (Oceanic Niño Index)",
       y = "Simpson_1_D_z",
       title = "") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

Simpson_plot

# Family Richness -------------------------------------------------------------
mod4 <- gam(FamRichness_z ~ s(ONI_First), data = laselva_z)
summary(mod4)
plot(mod4, pages = 1, shade = TRUE)


pred <- predict(mod4, newdata = new_data, se.fit = TRUE)
new_data$fit <- pred$fit
new_data$se <- pred$se.fit

family_plot <- ggplot() +
  geom_point(data = laselva_z, aes(x = ONI_First, y = FamRichness_z),
             color = "grey60", alpha = 0.6) +
  geom_line(data = new_data, aes(x = ONI_First, y = fit),
            color = "darkred", linewidth = 1.2) +
  geom_ribbon(data = new_data, aes(x = ONI_First, ymin = fit - 2*se, ymax = fit + 2*se),
              fill = "red", alpha = 0.2) +
  theme_minimal(base_size = 13) +
  labs(x = "ONI (Oceanic Niño Index)",
       y = "Simpson_1_D_z",
       title = "") +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))

family_plot



# Plots -------------------------------------------------------------------
library(patchwork)

combined_plot <- tr + shannon_plot + Simpson_plot + family_plot +
  plot_layout(ncol = 2) +
  plot_annotation(title = "",
                  theme = theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 16)))

combined_plot
