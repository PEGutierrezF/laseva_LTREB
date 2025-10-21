library(dplyr)
guilds <- laselva_z %>%
  select(CG_Rich, Fi_Rich, Pr_Rich, Sc_Rich, Sh_Rich)  # excluimos Piercers

guilds_prop <- guilds / rowSums(guilds)  # normalizar a proporciones que sumen 1


library(compositions)
guilds_clr <- clr(guilds_prop)  # centered log-ratio transformation


library(vegan)

# Distancia Bray-Curtis sobre proporciones
guilds_dist <- vegdist(guilds_prop, method = "bray")

# PERMANOVA
permanova_res <- adonis2(guilds_dist ~ ENSO, data = laselva_z, permutations = 999)
permanova_res





library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)

# Select FFG columns and ENSO + Date
guilds <- laselva_z %>%
  select(ENSO, Date, CG_Rich, Fi_Rich, Pi_Rich, Pr_Rich, Sc_Rich, Sh_Rich)

# Convert to long format
guilds_long <- guilds %>%
  pivot_longer(cols = CG_Rich:Sh_Rich,
               names_to = "Guild",
               values_to = "Count")

# Calculate proportions within each month (sum = 1 per Date)
guilds_long <- guilds_long %>%
  group_by(Date) %>%
  mutate(Proportion = Count / sum(Count)) %>%
  ungroup()

# Plot FFG proportions by ENSO phase
ggplot(guilds_long, aes(x = ENSO, y = Proportion, fill = Guild)) +
  geom_boxplot(alpha = 0.7, outlier.size = 1) +
  facet_wrap(~Guild, scales = "free_y") +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  theme_minimal(base_size = 14) +
  labs(x = "ENSO phase", y = "Proportion of Guild",
       title = "Functional Feeding Groups by ENSO Phase (per month)") +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
