# -------------------------------
# Load libraries
# -------------------------------
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

# Make stream a factor
laselva$stream <- factor(laselva$stream)

# Create factor for AR1 (unique time points)
laselva$time_f <- factor(laselva$Date, levels = sort(unique(laselva$Date)))

# Quick summary
summary(laselva$value)
fitdistrplus::descdist(laselva$value, discrete = TRUE)

# -------------------------------
# Fit negative binomial GLMM with AR1
# -------------------------------
m_oni_ar1 <- glmmTMB(
  value ~ `ONI-First` + stream + ar1(time_f | stream),  # AR1 autocorrelation by stream
  family = nbinom2,                                     # overdispersed counts
  data = laselva
)


summary(m_oni_ar1)

# -------------------------------
# Extract residuals
# -------------------------------
# Pearson residuals
resid_pearson <- residuals(m_oni_ar1, type = "pearson")

# Fitted values
fitted_vals <- fitted(m_oni_ar1)

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
