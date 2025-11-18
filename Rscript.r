# COVID-19 Data Analysis and Visualization

# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(RColorBrewer)

# Read the data
covid_data <- read.csv("country_wise_latest.csv", stringsAsFactors = FALSE)

# Data preprocessing
covid_data <- covid_data %>%
  mutate(
    Mortality_Rate = (Deaths / Confirmed) * 100,
    Recovery_Rate = (Recovered / Confirmed) * 100,
    Active_Rate = (Active / Confirmed) * 100
  )

# Create PDF for all visualizations
pdf("COVID19_Analysis_Visualizations.pdf", width = 12, height = 8)

# ===== GRAPH 1: Top 15 Countries by Confirmed Cases =====
top_15_confirmed <- covid_data %>%
  arrange(desc(Confirmed)) %>%
  head(15)

ggplot(top_15_confirmed, aes(x = reorder(Country.Region, Confirmed), y = Confirmed, fill = Country.Region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Blues"))(15)) +
  labs(title = "Top 15 Countries by Confirmed COVID-19 Cases",
       subtitle = "Total confirmed cases as of latest data",
       x = "Country",
       y = "Confirmed Cases") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# ===== GRAPH 2: Top 15 Countries by Death Count =====
top_15_deaths <- covid_data %>%
  arrange(desc(Deaths)) %>%
  head(15)

ggplot(top_15_deaths, aes(x = reorder(Country.Region, Deaths), y = Deaths, fill = Country.Region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = colorRampPalette(brewer.pal(9, "Reds"))(15)) +
  labs(title = "Top 15 Countries by COVID-19 Deaths",
       subtitle = "Total deaths reported as of latest data",
       x = "Country",
       y = "Total Deaths") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# ===== GRAPH 3: Cases Distribution by WHO Region =====
region_summary <- covid_data %>%
  group_by(WHO.Region) %>%
  summarise(
    Total_Confirmed = sum(Confirmed),
    Total_Deaths = sum(Deaths),
    Total_Recovered = sum(Recovered),
    Total_Active = sum(Active)
  ) %>%
  arrange(desc(Total_Confirmed))

ggplot(region_summary, aes(x = reorder(WHO.Region, Total_Confirmed), y = Total_Confirmed, fill = WHO.Region)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "COVID-19 Cases by WHO Region",
       subtitle = "Total confirmed cases across different WHO regions",
       x = "WHO Region",
       y = "Total Confirmed Cases") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12))

# ===== GRAPH 4: Case Status Breakdown for Top 10 Countries =====
top_10_data <- covid_data %>%
  arrange(desc(Confirmed)) %>%
  head(10) %>%
  select(Country.Region, Confirmed, Deaths, Recovered, Active) %>%
  pivot_longer(cols = c(Deaths, Recovered, Active), 
               names_to = "Status", 
               values_to = "Count")

ggplot(top_10_data, aes(x = reorder(Country.Region, -Confirmed), y = Count, fill = Status)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  scale_fill_manual(values = c("Deaths" = "#d73027", "Recovered" = "#1a9850", "Active" = "#fdae61")) +
  labs(title = "COVID-19 Case Status Breakdown",
       subtitle = "Distribution of deaths, recovered, and active cases for top 10 countries",
       x = "Country",
       y = "Number of Cases",
       fill = "Status") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "bottom")

# ===== GRAPH 5: Weekly Growth Rate Analysis =====
growth_data <- covid_data %>%
  filter(Confirmed > 10000) %>%
  arrange(desc(X1.week...increase)) %>%
  head(15)

ggplot(growth_data, aes(x = reorder(Country.Region, X1.week...increase), 
                        y = X1.week...increase, 
                        fill = X1.week...increase)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "#fee5d9", high = "#a50f15") +
  labs(title = "Weekly Growth Rate of COVID-19 Cases",
       subtitle = "Top 15 countries with highest 1-week percentage increase (min 10,000 cases)",
       x = "Country",
       y = "1 Week % Increase") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# ===== GRAPH 6: Comparison of Cases, Deaths, and Recovered (Line Chart) =====
top_20_countries <- covid_data %>%
  arrange(desc(Confirmed)) %>%
  head(20) %>%
  select(Country.Region, Confirmed, Deaths, Recovered) %>%
  pivot_longer(cols = c(Confirmed, Deaths, Recovered), 
               names_to = "Category", 
               values_to = "Count")

ggplot(top_20_countries, aes(x = Country.Region, y = Count, color = Category, group = Category)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = comma) +
  scale_color_manual(values = c("Confirmed" = "#4575b4", "Deaths" = "#d73027", "Recovered" = "#1a9850")) +
  labs(title = "COVID-19 Cases Trend: Confirmed vs Deaths vs Recovered",
       subtitle = "Top 20 countries by confirmed cases",
       x = "Country",
       y = "Number of Cases",
       color = "Category") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

# ===== GRAPH 7: Distribution of Mortality Rates (Histogram) =====
ggplot(covid_data %>% filter(Confirmed > 1000), 
       aes(x = Mortality_Rate)) +
  geom_histogram(bins = 30, fill = "#d73027", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Mortality_Rate, na.rm = TRUE)), 
             color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Mortality Rates Across Countries",
       subtitle = "Countries with more than 1,000 confirmed cases (Blue line = Global Average)",
       x = "Mortality Rate (%)",
       y = "Number of Countries") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# ===== GRAPH 8: Distribution of Recovery Rates (Histogram) =====
ggplot(covid_data %>% filter(Confirmed > 1000, Recovery_Rate > 0), 
       aes(x = Recovery_Rate)) +
  geom_histogram(bins = 30, fill = "#1a9850", color = "black", alpha = 0.7) +
  geom_vline(aes(xintercept = mean(Recovery_Rate, na.rm = TRUE)), 
             color = "blue", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Recovery Rates Across Countries",
       subtitle = "Countries with more than 1,000 confirmed cases (Blue line = Global Average)",
       x = "Recovery Rate (%)",
       y = "Number of Countries") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

# Close PDF device
dev.off()

# Print summary statistics
cat("\n=== COVID-19 DATA ANALYSIS SUMMARY ===\n\n")
cat("Total Countries:", nrow(covid_data), "\n")
cat("Total Confirmed Cases:", format(sum(covid_data$Confirmed), big.mark = ","), "\n")
cat("Total Deaths:", format(sum(covid_data$Deaths), big.mark = ","), "\n")
cat("Total Recovered:", format(sum(covid_data$Recovered), big.mark = ","), "\n")
cat("Global Mortality Rate:", round(sum(covid_data$Deaths) / sum(covid_data$Confirmed) * 100, 2), "%\n")
cat("Global Recovery Rate:", round(sum(covid_data$Recovered) / sum(covid_data$Confirmed) * 100, 2), "%\n\n")

cat("PDF file 'COVID19_Analysis_Visualizations.pdf' has been created successfully!\n")
