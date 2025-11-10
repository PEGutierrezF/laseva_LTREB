# -------------------------------
# Load libraries
# -------------------------------
install.packages("fitdistrplus")
library(readxl)
library(dplyr)
library(lubridate)
library(glmmTMB)
library(ggplot2)
library(fitdistrplus)

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


# -------------------------------
# Fit negative binomial GLMM with AR1
# -------------------------------
# We use glmmTMB with negative binomial because 'value' is count data with overdispersion 
# (variance >> mean). We include both streams and model AR1 autocorrelation within each stream.
# glmmTMB allows flexible specification of distributions (Poisson, negative binomial, Gaussian, etc.),
# random effects, and correlation structures, making it ideal for overdispersed count data with repeated measures.

model1 <- glmmTMB(
  value ~ `ONI-First` + stream + 
    ar1(time_f | stream),  # AR1 autocorrelation by stream
  family = nbinom2,        # overdispersed counts
  data = laselva
)

summary(model1)

# -------------------------------
# Extract residuals
# -------------------------------
# Pearson residuals
resid_pearson <- residuals(model1, type = "pearson")

# Fitted values
fitted_vals <- fitted(model1)

# -------------------------------
# Residual plots
# -------------------------------

# 1️⃣ Histogram of Pearson residuals
ggplot(data.frame(resid = resid_pearson), aes(x = resid)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  theme_minimal() +
  labs(title = "Histogram of Pearson Residuals")

# 2️⃣ Residuals vs Fitted
ggplot(data.frame(resid = resid_pearson, fitted = fitted_vals), aes(x = fitted, y = resid)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values")

# 3️⃣ Residuals over time by stream
ggplot(data.frame(resid = resid_pearson, time = laselva$Date, stream = laselva$stream),
       aes(x = time, y = resid, color = stream)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Residuals over Time by Stream")

# -------------------------------
# Optional: Check fitted effect of ONI-First
# -------------------------------
ggplot(laselva, aes(x = `ONI-First`, y = value, color = stream)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "glm", method.args = list(family = "poisson"), se = TRUE) +
  theme_minimal() +
  labs(title = "Effect of ONI-First on Total Richness by Stream")
