# COVID-19 Global Data Analysis
# Author: Unnat Umarye
# Date: 18-11-2025

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(gridExtra)
library(viridis)
library(RColorBrewer)

# Read the data
covid_data <- read.csv("country_wise_latest.csv", stringsAsFactors = FALSE)

# Data preprocessing
covid_data <- covid_data %>%
  mutate(
    Deaths.100.Cases = as.numeric(Deaths...100.Cases),
    Recovered.100.Cases = as.numeric(Recovered...100.Cases),
    Deaths.100.Recovered = as.numeric(Deaths...100.Recovered),
    Week.Change.Pct = as.numeric(X1.week...increase)
  )

# Create PDF for all visualizations
pdf("COVID19_Analysis_Visualizations.pdf", width = 12, height = 8)
