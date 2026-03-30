# ==============================================================================
# AS3209 Probabilistic Modelling - R Lab Session 1: EVT                        
# Author:       Emilio Luis Sáenz Guillén                                   
# Professor:    Russell Gerrard                                            
# Institution:  Bayes Business School - City St George's, University of London 
# Date:         5th March 2026                                              
# Description:  This script introduces Extreme Value Theory (EVT) and       
#               demonstrates practical applications using R.                
# Dependencies: dplyr, extRemes, evd, GeDS, ggplot2, ggpubr, goftest, POT,
#                tseries, tidyr                                    
# ==============================================================================

# Clean environment
rm(list = ls())       # Remove all objects
graphics.off()        # Close all graphical devices
# cat("\014")         # Clean console

# Set working directory (Modify accordingly)
setwd("C:\\Users\\addj700\\Dropbox\\Bayes Business School\\Teaching\\Bayes Business School\\2025-26\\AS3209 - Probabilistic Modelling\\EVT")

## Load required packages (Check if installed first)

# i) Inefficient way to install and load R packages
# install.packages("dplyr")
library(dplyr)

# ii) More efficient way
# Package names
packages <- c("dplyr", "evd", "extRemes", "GeDS", "ggplot2", "ggpubr", "goftest", "POT", "tseries", "tidyr")

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))

# iii) Most efficient way
# install.packages("pacman")
pacman::p_load(dplyr, evd, extRemes, GeDS, ggplot2, ggpubr, goftest, POT, tseries, tidyr) 


# ================================================
# 1. Data Management
# ================================================

# File Formats:
# - TSV (Tab-Separated Values): A plain text file where columns are separated by tab characters ("\t").
# - CSV (Comma-Separated Values): A plain text file where columns are separated by commas (",").
# - XLS/XLSX (Excel Spreadsheet): A proprietary Microsoft Excel file format; XLS is older (binary format), while XLSX is the modern XML-based format.

# Cooling Degree Days (CDD) Data Analysis
# CDD is the sum of the number of degrees the daily average temperature is above 22°C each day.

# Load the data from a tab-separated file
cdd = read.table('CDD.tsv', header = TRUE)  

# Display the dataset in a spreadsheet-like viewer (only works in interactive environments)
# View(cdd)
# Preview the first few rows of the dataset
head(cdd)
# Get the names of the columns
names(cdd)
# Check the data type of the object
typeof(cdd)
# Check the structure of the dataset (columns, data types, and a few observations)
str(cdd)
# Get summary statistics (min, max, mean, median, etc.) to understand distribution
summary(cdd)
# Convert to a data frame (ensuring it's in the correct format for analysis)
cdd = as.data.frame(cdd)
# Remove any rows with missing values to avoid issues in analysis
cdd = na.omit(cdd)
# Check there's no year skipped
all(diff(cdd$Year) == 1)

# Check whether the Annual variable corresponds to the sum of the monthly values
if( all(rowSums(cdd[, c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")]) == cdd$Annual) ) {
  print("Yes, Annual corresponds to the sum of all the monthly values!")
}

# Plot the annual CDD over time
plot(cdd$Year, cdd$Annual, type = "l", 
     main = "Cooling Degree Days Over Time", 
     xlab = "Year", ylab = "Annual CDD")

## 1.1. Detecting Outliers
# i) Detect outliers graphically
# Boxplot to visualize the distribution and detect potential outliers
boxplot(cdd$Annual, 
        main = "Boxplot of Annual Cooling Degree Days", 
        ylab = "Cooling Degree Days")

# ii) EXERCISE 1: Detect outliers analytically 
# Compute Q1, Q3, and IQR
Q1 <- quantile(cdd$Annual, 0.25)
Q3 <- quantile(cdd$Annual, 0.75)
IQR_value <- Q3 - Q1

# Define outlier thresholds
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Identify rows with outliers
outliers <- cdd[cdd$Annual < lower_bound | cdd$Annual > upper_bound, ]

# Print outliers if any exist
if (nrow(outliers) > 0) {
  cat("\nOutliers detected:\n")
  print(outliers)
} else {
  cat("\nNo outliers detected.\n")
}

## 1.2. Stationarity
# A time series is stationary if its statistical properties (mean, variance,
# and autocorrelation) remain constant over time. Most time series models (like
# ARIMA) assume stationarity.

# Improved time series plot with points and trend line
plot(cdd$Year, cdd$Annual, type="o", pch=16, 
     main="Annual Cooling Degree Days Over Time", 
     xlab="Year", ylab="Annual CDD")

# Add a linear trend line to observe general direction of change over time
abline(lm(Annual ~ Year, data = cdd), col="red", lwd=2)

## EXERCISE 2: Formally test whether the series is stationary or not
# ADF test
adf_original <- tseries::adf.test(cdd$Annual)

# Function to interpret ADF test results
interpret_adf <- function(adf_result, description) {
  # Decision rule based on p-value
  if (adf_result$p.value < 0.05) {
    cat("\nReject H0: The series is stationary.\n")
  } else {
    cat("\nNo evidence to reject H0. The series is non-stationary.\n")
  }
}

interpret_adf(adf_original)

# We can deal with trend stationarity, e.g., by computing first differences
cdd$diff_Annual <- c(NA, diff(cdd$Annual))
adf_diff <- tseries::adf.test(na.omit(cdd$diff_Annual))
interpret_adf(adf_diff)

# Plot the differenced values (change in CDD from one year to the next)
plot(cdd$Year[-1], cdd$diff_Annual[-1], type="l", 
     main="Differenced Cooling Degree Days", 
     xlab="Year", ylab="Change in Annual CDD")

# ================================================
# 2. BLOCK MAXIMA METHOD (BMM)
# ================================================

# Primary package we use for EVT: 'extRemes'
# Data source: https://archives.mountainscholar.org/digital/collection/p17393coll152/id/8528/
# And updated data can be find in: https://climate.colostate.edu/data_access_new.html
# # M: no or missing observations
# # T: non-measurable trace of precipitation or snow
data = read.table(file = 'MONTH_PCPN.tsv', header = T)
# View(data)
# Check there's no year skipped
diff(data$Year)

data <- data[data$Year >= 1889 & data$Year <= 2015,]
data <- data[rowSums(is.na(data)) < ncol(data), ]

# Reshape data
data <- data %>%
  mutate(across(Jan:Dec, as.character)) %>%  # Ensure all month columns are character
  pivot_longer(cols = Jan:Dec, names_to = "Month", values_to = "pcpn")
# Format pcpn as numeric (and get rid of the Ms and Ts)
data$pcpn <- as.numeric(data$pcpn)

## EXERCISE 3: Ensure all years in the range are present
# If not check if you could complete the missing using "StnData.csv"
full_years <- data.frame(
  Year = rep(1889:2015, each = 12),
  Month = month.abb[rep(1:12, times = length(1889:2015))]
)
data <- full_years %>%
  left_join(data, by = c("Month", "Year"))

# https://climate.colostate.edu/data_access_new.html
# M: no or missing observations
# T: non-measurable trace of precipitation or snow
StnData <- read.csv("StnData.csv", header = TRUE)
names(StnData)[names(StnData) == "FORT.COLLINS"] <- "pcpn"
StnData$pcpn <- as.numeric(StnData$pcpn)
# Create a new column with row names
StnData$Date <- as.Date(paste0(rownames(StnData), "-01"), format = "%Y-%m-%d")
# Extract the year as a separate column
StnData$Year <- as.numeric(format(StnData$Date, "%Y"))

data$pcpn[data$Year == 1999] <- StnData$pcpn[StnData$Year == 1999]

# Check number of NAs in pcpn per year
tapply(is.na(data$pcpn), data$Year, sum)

# Create Date variable
data <- data %>%
  mutate(
    Month = match(Month, month.abb),  # Convert month names to numeric (Jan = 1, Feb = 2, etc.)
    Date = as.Date(paste(Year, Month, "01", sep = "-"), format = "%Y-%m-%d")  # Create Date variable
  ) %>%
  select(Date, Year, Month, pcpn, Annual) %>%  # Reorder columns
  arrange(Date) # Sort by Date

## EXERCISE 4: calculate the annual maxima
data <- data %>%
  # Group by year and calculate the maximum rain
  group_by(Year) %>%
  mutate(annual_max_pcpn = max(pcpn, na.rm = TRUE))

## Annual Block Maxima plot
# i) Using base R
plot(data$Date, data$pcpn, type = "n", 
     main = "Ft. Collins Monthly Precipitation",
     xlab = "",
     xaxt = "n",
     ylab = "Precipitation (in)")
# Add vertical line ranges
segments(x0 = data$Date, y0 = 0, 
         x1 = data$Date, y1 = data$pcpn, 
         col = "lightgray", lwd = 0.5)
points(data$Date, data$pcpn, col = "#0073C2FF", pch = 19)
# Highlight annual maxima in red
points(data$Date[data$pcpn == data$annual_max_pcpn], 
       data$pcpn[data$pcpn == data$annual_max_pcpn], 
       col = "red4", pch = 19)
# Add x-axis
axis(1, at = seq(min(data$Date), max(data$Date), by = "7 years"), 
     labels = format(seq(min(data$Date), max(data$Date), by = "7 years"), "%Y"))

# ii) More visually appealing using ggplot
ggplot(data, aes(Date, pcpn)) +
  geom_linerange(
    aes(x = Date, ymin = 0, ymax = pcpn),
    color = "lightgray", linewidth = 0.5,
    position = position_dodge(0.3)
  ) +
  geom_point(
    aes(color = pcpn != annual_max_pcpn),
    color = "#0073C2FF",
    position = position_dodge(0.3), size = 2
  ) +
  geom_point(
    data = data %>% filter(pcpn == annual_max_pcpn),
    color = "red4",
    position = position_dodge(0.3), size = 2
  ) +
  labs(
    title = "Ft. Collins Monthly Precipitation",
    x = NULL, 
    y = "Precipitation (in)"
  ) + 
  scale_x_date(date_labels = "%Y", date_breaks = "8 year") +
  theme_pubclean()


################################################################################
## Fit a Generalized Extreme Value (GEV) distribution to the block maxima data #
################################################################################
# maximum-likelihood fitting of the GEV distribution
bm_pcpn <- data %>%
  distinct(Year, annual_max_pcpn, .keep_all = TRUE) %>%
  pull(annual_max_pcpn)
fit_mle <- extRemes::fevd(bm_pcpn, method = "MLE", type="GEV")
fit_mle$results$par
# Now interpret the results! Which GEV Type corresponds?
mu = fit_mle$results$par[[1]]
sigma = fit_mle$results$par[[2]]
xi = fit_mle$results$par[[3]]

# Plot results
plot(fit_mle)


## EXERCISE 5: Can you explain in your words what each of the plots above is?

# Q-Q Plot (Quantile-Quantile Plot): Compares sample quantiles to theoretical quantiles;
# focus on deviations in the tails.
plot(fit_mle, type = 'qq')

## EXERCISE 5.1.: build the Q-Q plot manually
n <- length(bm_pcpn)
# Empirical quantiles (sorted data)
x_sorted <- sort(bm_pcpn)
# Theoretical GEV quantiles
xp <- (1:n)/(n+1) # n+1 adjustment to keep probabilities strictly between 0 and 1
q_theoretical <- extRemes::qevd(xp, loc = mu, scale = sigma, shape = xi, type = "GEV")

qq_table <- data.frame(
  i = 1:n,
  empirical_quantile = x_sorted,
  theoretical_quantile = q_theoretical
)

tail(qq_table)

plot(
  qq_table$theoretical_quantile,
  qq_table$empirical_quantile,
  xlab = "Theoretical Quantiles (GEV)",
  ylab = "Empirical Quantiles"
)

abline(0, 1, col = "red", lwd = 2)

# P-P Plot (Probability-Probability Plot): Compares the cumulative probabilities of the 
# sample and theoretical distributions; focus on center of the distribution.
plot(fit_mle, type = 'probprob')

## EXERCISE 5.2.: build the Q-Q plot manually

# Empirical probabilities
F_emp <- (1:n)/(n+1) # n+1 adjustment to keep probabilities strictly between 0 and 1

# Model probabilities
F_model <- extRemes::pevd(
  x_sorted,
  loc = mu,
  scale = sigma,
  shape = xi,
  type = "GEV"
)

pp_table <- data.frame(
  i = 1:n,
  empirical_probability = F_emp,
  model_probability = F_model
)

tail(pp_table)

plot(pp_table$empirical_probability,
     pp_table$model_probability,
     xlab = "Empirical Probabilities",
     ylab = "Model Probabilities")

abline(0, 1, col = "red", lwd = 2)


# Test your fit
# Define fitted cdf for testing
fit_cdf = function(p){
  return(extRemes::pevd(p, loc = mu, scale = sigma, shape = xi, type = 'GEV'))
} 

# Kolmogorov-Smirnov
ks.test(bm_pcpn, fit_cdf)$p.value
ks.test(bm_pcpn, evd::pgev, loc = mu, scale = sigma, shape = xi)$p.value

# Cramer von Mises
goftest::cvm.test(bm_pcpn, null = fit_cdf )$p.value
goftest::cvm.test(bm_pcpn, evd::pgev, loc = mu, scale = sigma, shape = xi)$p.value

# ================================================
# 3. PEAK OVER THRESHOLD (POT)
# ================================================

pcpn <- na.omit(data$pcpn)

## EXERCISE 6: review your lecture notes and build the mean excess plot, without using
# any package!

# define the thresholds over the range
u.range <- c(sort(pcpn)[1], sort(pcpn)[length(pcpn) - 4])
u <- seq(u.range[1], u.range[2], length = length(pcpn))

e_u <- list()
for (j in 2:length(pcpn)){
  m <- list()
  n <- list()
  for(i in 1:length(pcpn)){
    m[i] <- (pcpn[i] - u[j]) * as.numeric(pcpn[i] > u[j])
    n[i] <- as.numeric(pcpn[i] > u[j])
  }
  e_u[j] <- sum(unlist(m))/sum(unlist(n))
}
plot(u[2:length(pcpn)], unlist(e_u), type = "l", col= "red",
     xlab = "u", ylab = "e(u)", main = "Mean Excess Plot", lwd = 3,
     xlim = c(min(u),u[which.min(unlist(e_u))]*1.05))
points(u[2:length(pcpn)], unlist(e_u), col = "blue")

# Alternatively, using the POT package
mrlplot <- POT::mrlplot(pcpn,
                        main = "Mean Excess Plot",
                        ylab="Mean_Excess",col = c('red4', 'steelblue', 'red4'))

mrlplot_data <- data.frame(mrlplot$x, mrlplot$y[,2])%>%
  dplyr::rename(c("threshold" = "mrlplot.x", "mean_excess" = "mrlplot.y...2."))


#####################################
# Find thresholds using Linear GeDS #
#####################################
(Gmod <- GeDS::NGeDS(mean_excess ~ f(threshold), phi = 0.9, data = mrlplot_data))

# Plot linear fit
plot(Gmod, n = 2)
# Extract knots of the linear fit
knots(Gmod, n = 2)

u <- knots(Gmod, n = 2)[3] # take the first internal knots as candidate
pot_mle <- fevd(pcpn, method = "MLE", type="GP", threshold = u)

sigma_pot = pot_mle$results$par[1]
xi_pot = pot_mle$results$par[2]


## EXERCISE 7: produce a histogram of the excess data and assess graphically
# the quality of the fit by plotting the theoretical density over it

# Remember we're modelling the excesses above the threshold u
w <- pcpn[pcpn > u]
w <- w - u

t = seq(min(w), max(w), length.out = 100)
hist(w, freq = F,  col = 'steelblue', xlab = 'precipitation')
lines(t, devd(t, loc = 0, scale = sigma_pot, shape = xi_pot, type = 'GP'))

# Define fitted cdf for testing
fit_cdf_pot = function(p){
  return(pevd(p, loc = 0, scale = sigma_pot, shape = xi_pot, type = 'GP'))
} 

# Kolmogorov-Smirnov
ks.test(w, fit_cdf_pot)
ks.test(w, evd::pgpd, scale = sigma_pot, shape = xi_pot)

# Cramer von Mises
goftest::cvm.test(w, null = fit_cdf_pot)
goftest::cvm.test(w, evd::pgpd, scale = sigma_pot, shape = xi_pot)





