# -------------------------------
# Load libraries
# -------------------------------
# install.packages("DHARMa")
library(readxl)
library(dplyr)
library(lubridate)
library(glmmTMB)
library(ggplot2)
library(fitdistrplus)
library(nlme)
library(DHARMa)

# -------------------------------
# Load data
# -------------------------------
laselva <- read_excel("laselva.xlsx", sheet = "alldata")

# -------------------------------
# Prepare data
# -------------------------------
# Convert Date to proper Date object
laselva$Date <- dmy(paste0("01-", laselva$Date))  # e.g., "Jan-1997" -> "1997-01-01"

# Extract Year and Month index
laselva$Year <- year(laselva$Date)
laselva$Month_idx <- month(laselva$Date)
head(laselva,13)

# -------------------------------
# Subset months (Feb, May, Sep) and remove 2024
# -------------------------------
laselva <- laselva[laselva$Month_idx %in% c(2,5,9) & laselva$Year != 2024, ]
head(laselva, 13)

# Make stream a factor
laselva$stream <- factor(laselva$stream)


# Create factor for AR1 (unique time points)
# Date (class Date) no funciona directamente en el AR1 de glmmTMB
laselva$time_f <- factor(laselva$Date, levels = sort(unique(laselva$Date)))
head(laselva,13)

# Quick summary
summary(laselva$value)
ggplot(laselva, aes(value)) +
  geom_histogram(bins = 50)

fitdistrplus::descdist(laselva$value, discrete = TRUE)
# We checked the distribution of the count variable (value) using fitdistrplus::descdist(). The summary shows:
# Mean = 12.3
# Variance (SD²) ≈ 6.24² = 38.9
# So variance >> mean
# For count data, this indicates overdispersion.
# If the data were well described by a Poisson, we would expect:
# Mean ≈ Variance
# https://cran.r-project.org/web/packages/GlmSimulatoR/vignettes/count_data_and_overdispersion.html
# https://pacificpapermill.com/slideshows/PPM__Poisson_Regression.pdf

# -------------------------------
# Fit negative binomial GLMM with AR1
# -------------------------------
# We use glmmTMB with negative binomial because 'value' is count data with overdispersion 
# (variance >> mean). We include both streams and model AR1 autocorrelation within each stream.
# glmmTMB allows flexible specification of distributions (Poisson, negative binomial, Gaussian, etc.),
# random effects, and correlation structures, making it ideal for overdispersed count data with repeated measures.
# Include stream as random effect. AR1 autocorrelation omitted to avoid convergence issues.
laselva <- laselva %>%
  group_by(stream) %>%
  arrange(Months, .by_group = TRUE) %>%
  mutate(time_factor = factor(row_number())) %>%
  ungroup()

# Ajustar el modelo AR1 usando time_factor
model1 <- glmmTMB(
  value ~ ONI_First + SOI + ENSO + temp_min + rain_51 + stream +
    ar1(time_factor + 0 | stream),  # AR1 por stream
  family = nbinom2,
  data = laselva
)

# This model may be more appropriate because:
# 1) Accounts for repeated measures / grouped data:
#    a) Multiple observations within the same year
#    b) Observations in the same year are not independent
# 2) Captures unmeasured annual variation:
#    a) There may be factors affecting insect counts each year that are not included in the model 
#       (e.g., unusual weather events, habitat changes, sampling differences)
#    b) The random intercept allows each year to shift the baseline abundance up or down
# 3) Partially controls for temporal autocorrelation:
#    a) Observations are trimestral and not evenly spaced, so a classical AR1 correlation is not ideal
#    b) By allowing each year to have its own intercept, observations within the same year are more similar 
#       than those in other years
model2 <- glmmTMB(
  value ~ ONI_First + SOI + ENSO + temp_min + rain_51 + stream +
    (1 | Year),  # random intercept for Year
  family = nbinom2,
  data = laselva
)

summary(model1)                          
summary(model2) 


# -------------------------------
# Extract residuals
# -------------------------------
# Simular residuales DHARMa
res <- simulateResiduals(model2, n = 1000)

# Graficar residuales
plot(res)
testUniformity(res) 
testDispersion(res)

# Calcular autocorrelación por stream, ignorando NAs
unique_streams <- unique(laselva$stream)

for(s in unique_streams){
  cat("Autocorrelation for stream:", s, "\n")
  
  # Subset indices para este stream
  idx <- which(laselva$stream == s)
  
  # Extraer residuales escalados para este stream
  res_num <- residuals(res, type = "scaled")[idx]
  
  # Eliminar NAs
  res_num <- res_num[!is.na(res_num)]
  
  # Calcular autocorrelación usando cor()
  if(length(res_num) > 1){
    acf_val <- cor(res_num[-length(res_num)], res_num[-1])
    cat("Lag-1 autocorrelation:", round(acf_val, 3), "\n\n")
  } else {
    cat("No enough data for autocorrelation\n\n")
  }
}


# Alternatively, Durbin-Watson test for autocorrelation
library(lmtest)
dwtest(model2)

# We assessed autocorrelation. While there is noticeable autocorrelation in Carapa, 
# it is not strictly appropriate to account for temporal autocorrelation using AR1 models 
# because the observations are not equally spaced in time and the series are not fully linear 
# (for example, there are longer intervals between September -> February than 
# between February -> May and May -> September).
