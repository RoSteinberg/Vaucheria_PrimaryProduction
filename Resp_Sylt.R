# Load required libraries
library(tidyverse)

# Load the data
file_name <- "C:/Users/rosteinb/Documents/Ronny Steinberg/00_PhD/01_Chap_2_O2_Production/00_Data/Resp_O2_Incu.txt"
data <- read.csv(file = file_name, fileEncoding = "latin1",
                 row.names = NULL, header = TRUE, sep = "\t")
rm(file_name)

# View the data structure
head(data)
# Function to calculate rate (Respiration or Photosynthesis)
calculate_rate <- function(initial_O2, final_O2, time, volume) {
  rate <- (final_O2 - initial_O2) / time * volume
  return(rate)
}

# Add calculated columns for rate analysis
data <- data %>%
  mutate(
    # Calculate the change in O2 (ΔO2)
    Delta_O2 = O2_2 - O2_1,
    # Convert incubation time from minutes to hours
    Incubation_Time_hr = Incubation_Time / 60,
    # Calculate rates (µmol O2/h)
    Rate = Delta_O2 / Incubation_Time_hr * Volume
  ) %>%
  na.omit

# Separate data by treatment
dark_data <- data %>% filter(Treatment == "Dark_1")
light_data <- data %>% filter(Treatment == "Light_1")

# Summarize results
dark_summary <- dark_data %>% group_by(Sample) %>%
  summarise(
    Avg_Respiration_Rate = mean(Rate),
    SD_Respiration_Rate = sd(Rate)
  )

light_summary <- light_data %>% group_by(Sample) %>%
  summarise(
    Avg_Photosynthesis_Rate = mean(Rate),
    SD_Photosynthesis_Rate = sd(Rate)
  )

# Combine light and dark summaries
combined_summary <- bind_rows(
  dark_summary %>% mutate(Treatment = "Dark"),
  light_summary %>% mutate(Treatment = "Light")
)

# Print results
print("Summary of Respiration and Photosynthesis Rates")
print(combined_summary)

# Calculate gross photosynthesis rate for each sample type
gross_photosynthesis <- data %>%
  group_by(Sample) %>%
  summarise(
    Avg_Respiration_Rate = mean(Rate[Treatment == "Dark_1"], na.rm = TRUE),
    Avg_Photosynthesis_Rate = mean(Rate[Treatment == "Light_1"], na.rm = TRUE)
  ) %>%
  mutate(
    P_gross = Avg_Photosynthesis_Rate + abs(Avg_Respiration_Rate)
  )

# Display the results
print("Gross Photosynthesis Rates for Each Sample")
print(gross_photosynthesis)

#############################################################################################
# Separate data by treatment
dark_data <- data %>% filter(Treatment == "Dark_2")
light_data <- data %>% filter(Treatment == "Light_2")

# Summarize results
dark_summary <- dark_data %>% 
  group_by(Sample, Incubation_Time_hr) %>%
  summarise(
    Avg_Photosynthesis_Rate = mean(Rate),
    SD_Photosynthesis_Rate = sd(Rate),
    .groups = "drop"
  )

light_summary <- light_data %>% 
  group_by(Sample, Incubation_Time_hr) %>%
  summarise(
    Avg_Photosynthesis_Rate = mean(Rate),
    SD_Photosynthesis_Rate = sd(Rate),
    .groups = "drop"
  )

# Combine light and dark summaries
combined_summary <- bind_rows(
  dark_summary %>% mutate(Treatment = "Dark"),
  light_summary %>% mutate(Treatment = "Light")
)

# Print results
print("Summary of Respiration and Photosynthesis Rates")
print(combined_summary)

# Calculate gross photosynthesis rate for each sample type
gross_photosynthesis <- data %>%
  group_by(Sample, Incubation_Time_hr) %>%
  summarise(
    Avg_Respiration_Rate = mean(Rate[Treatment == "Dark_2"], na.rm = TRUE),
    Avg_Photosynthesis_Rate = mean(Rate[Treatment == "Light_2"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    P_gross = Avg_Photosynthesis_Rate + abs(Avg_Respiration_Rate)
  )

# Display the results
print("Gross Photosynthesis Rates for Each Sample")
print(gross_photosynthesis)
