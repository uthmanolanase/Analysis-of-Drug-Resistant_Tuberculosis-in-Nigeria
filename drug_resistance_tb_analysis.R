###########################################################
# Drug Resistance Tuberculosis Analysis
# Author: Uthman Al-Ameen Olalekan
# Date: 2025-06-05
###########################################################

# Load required packages
library(tidyverse)
library(psych)
library(officer)
library(scales)
library(flextable)
library(readr)

###########################################################
# 1. Load Datasets
###########################################################

TB_drug_resistance_Estimate <- read_csv("C:/Users/THIS PC/Downloads/TB drug resistance Estimate.csv")
TB_cases_started_on_MDR_RR_TB_treatment <- read_csv("C:/Users/THIS PC/Downloads/TB cases started on MDR_RR TB treatment.csv")
TB_Drug_resistance_confimed_cases <- read_csv("C:/Users/THIS PC/Downloads/TB Drug resistance confimed cases.csv")

###########################################################
# 2. Confirmed cases of MDR-TB (2015–2023)
###########################################################

confirmed_cases_of_MDR_TB <- TB_Drug_resistance_confimed_cases %>%
  filter(Period >= 2015 & Period <= 2023) %>%
  select(Year = Period, Confirmed_Cases = FactValueNumeric)

print(confirmed_cases_of_MDR_TB)

###########################################################
# 3. Estimated number of MDR-TB cases (2015–2023)
###########################################################

estimated_number_of_MDR_TB_cases <- TB_drug_resistance_Estimate %>%
  filter(
    Period >= 2015 & Period <= 2023,
    IndicatorCode == "TB_e_inc_rr_num",   
    SpatialDimValueCode == "NGA"          
  ) %>%
  select(Year = Period, Estimated_Cases = FactValueNumeric)

print(estimated_number_of_MDR_TB_cases)

###########################################################
# 4. Cases started on MDR-TB treatment (2015–2023)
###########################################################

cases_started_on_MDR_TB_treatment <- TB_cases_started_on_MDR_RR_TB_treatment %>%
  filter(Period %in% 2015:2023) %>%
  select(Year = Period, Started_Treatment = Value)

print(cases_started_on_MDR_TB_treatment)

###########################################################
# 5. Merge all datasets
###########################################################

combined_tb_data <- confirmed_cases_of_MDR_TB %>%
  left_join(estimated_number_of_MDR_TB_cases, by = "Year") %>%
  left_join(cases_started_on_MDR_TB_treatment, by = "Year")

print(combined_tb_data)

###########################################################
# 6. Flextable output
###########################################################

ft <- flextable(combined_tb_data)
save_as_docx("Combined MDR TB Data Table" = ft, path = "combined_mdr_tb_data_table.docx")

summary(combined_tb_data)
describe(combined_tb_data)

###########################################################
# 7. Convert to long format
###########################################################

combined_tb_data_long <- combined_tb_data %>%
  pivot_longer(cols = c(Confirmed_Cases, Estimated_Cases, Started_Treatment),
               names_to = "Variable",
               values_to = "Value")

print(combined_tb_data_long)

flextable(combined_tb_data_long)

###########################################################
# 8. Derived percentages
###########################################################

derived_percentages <- combined_tb_data %>%
  mutate(
    Percentage_Confirmed = round((Confirmed_Cases / Estimated_Cases) * 100, 1),
    Percentage_Treated_of_Confirmed = round((Started_Treatment / Confirmed_Cases) * 100, 1)
  ) %>%
  select(Year, Percentage_Confirmed, Percentage_Treated_of_Confirmed)

print(derived_percentages)
describe(derived_percentages)

ft <- flextable(derived_percentages)
save_as_docx("derived_percentages" = ft, path = "derived_percentages_table.docx")

###########################################################
# 9. Plots
###########################################################

# Line chart for raw counts
data_long_counts <- combined_tb_data %>%
  pivot_longer(cols = c(Estimated_Cases, Confirmed_Cases, Started_Treatment),
               names_to = "Case_Type",
               values_to = "Count")

ggplot(data_long_counts, aes(x = Year, y = Count, color = Case_Type)) +
  geom_line(size = 1.2) +
  labs(
    title = "Trend of MDR/RR-TB Estimated, Confirmed, and Treatment-Started Cases in Nigeria (2015–2023)",
    y = "Number of Cases",
    color = "Case Type"
  ) +
  scale_x_continuous(breaks = 2015:2023) +
  theme_minimal()

# Line chart for percentages
data_long_percent <- derived_percentages %>%
  pivot_longer(cols = c(Percentage_Confirmed, Percentage_Treated_of_Confirmed),
               names_to = "Percentage_Type",
               values_to = "Percentage")

ggplot(data_long_percent, aes(x = Year, y = Percentage, color = Percentage_Type)) +
  geom_line(size = 1.3) +
  labs(
    title = "Trend of Diagnostic and Treatment Coverage of MDR/RR-TB in Nigeria (2015–2023)",
    y = "Percentage (%)",
    color = "Percentage Type"
  ) +
  theme_minimal() +
  scale_x_continuous(breaks = 2015:2023) +
  scale_y_continuous(labels = percent_format(scale = 1))

# Scatter plot
ggplot(combined_tb_data, aes(x = Confirmed_Cases, y = Started_Treatment)) +
  geom_point(color = "blue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "black") +
  labs(
    title = "Relationship Between Confirmed and Started Treatment MDR/RR-TB Cases in Nigeria",
    x = "Confirmed MDR/RR-TB Cases",
    y = "Started MDR/RR-TB Treatment Cases"
  ) +
  theme_minimal()

###########################################################
# 10. Correlation and Regression
###########################################################

combined_tb_data <- combined_tb_data %>%
  mutate(
    Confirmed_Cases = as.numeric(Confirmed_Cases),
    Started_Treatment = as.numeric(Started_Treatment)
  )

# Correlation
correlation_result <- cor.test(combined_tb_data$Confirmed_Cases, combined_tb_data$Started_Treatment)
print(correlation_result)

# Linear Regression
lm_model <- lm(Started_Treatment ~ Confirmed_Cases, data = combined_tb_data)
summary(lm_model)
