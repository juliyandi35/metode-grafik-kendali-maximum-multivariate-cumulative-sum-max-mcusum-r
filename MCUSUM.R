# Install and load the qcc package
library(qcc)
library(readxl)
# Generate some sample data
set.seed(123)
data <- read_excel('Data_Nisa.xlsx')

#Normalize the data using min-max normalization
data_norm = (data - min(data))/(max(data) - min(data));

# Descriptive Analysis of the Data
summary(data_norm)

# Correlation Test for Each Variables
Varcorr <- cor(data_norm)
Varcorr

# Test for multivariate normality
str(data_norm)
shapiro.test(as.matrix(data_norm))

# Calculate the MCUSUM statistic for each variable
mcusum_stats <- apply(data, 2, cumsum)

# Calculate the Maximum MCUSUM statistic
max_mcusum_stat <- apply(mcusum_stats, 1, max)

# Calculate the MCUSUM statistic for variance
mcusum_stats_var <- apply(data, 2, function(x) cumsum((x - mean(x))^2))

# Calculate the Maximum MCUSUM statistic for variance
max_mcusum_stat_var <- apply(mcusum_stats_var, 1, max)

# Create the control chart for mean
lcl <- -9.46 # set lower control limit
ucl <- 9.46 # set upper control limit
qcc_obj_mean <- qcc(mcusum_stats, type = "xbar.one", title = "MCUSUM Mean Chart", ylim = c(lcl, ucl))
qcc_obj_mean_max <- qcc(max_mcusum_stat, type = "xbar.one", title = "Maximum MCUSUM Mean Chart", ylim = c(lcl, ucl))

# Create the control chart for variance
qcc_obj_var <- qcc(mcusum_stats_var, type = "xbar.one", title = "MCUSUM Variance Chart", ylim = c(lcl, ucl))
qcc_obj_var_max <- qcc(max_mcusum_stat_var, type = "xbar.one", title = "Maximum MCUSUM Variance Chart", ylim = c(lcl, ucl))
