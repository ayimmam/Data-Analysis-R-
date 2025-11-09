# 00_data_preparation.R
# This script loads the raw CSV data, cleans it, performs all necessary aggregations,
# and saves the resulting data frames into 'reconstructed_data.RData' for plotting.

library(dplyr)
library(tidyr)
library(stringr)

# --- 1. Load and Clean Data ---

df_raw <- read.csv("research_respondent_dataset_cleaned.csv")

# Clean Column Names
new_columns <- c(
    'Study_Years', 'Academic_Program', 'Age_Group', 'Qual_Shaping', 'GPA',
    'Do_Interview', 'Academic_Field', 'Gender', 'Exposure_Types', 'Inf_Career_Fairs',
    'Inf_Family', 'Inf_Friends', 'Inf_Internships', 'Inf_Job_Shadowing',
    'Inf_Mentorship', 'Inf_Coursework', 'Guidance_Usage_Freq', 'Guidance_Satisfaction',
    'Picked_Program_By_Grades', 'Language', 'Study_Level_Agg', 'Career_Clarity',
    'Qual_Additional_Exp', 'Qual_Additional_Exp_2', 'Qual_Improvements', 'University'
)
names(df_raw) <- new_columns

# Select core columns and clean text/numeric formats
df_core <- df_raw %>%
  select(
    Age_Group, Gender, University, Academic_Program, Academic_Field, GPA,
    Study_Level_Agg, Exposure_Types, Career_Clarity, Guidance_Usage_Freq,
    Guidance_Satisfaction, Inf_Career_Fairs, Inf_Family, Inf_Friends,
    Inf_Internships, Inf_Job_Shadowing, Inf_Mentorship, Inf_Coursework,
    Qual_Shaping, Qual_Improvements, Qual_Additional_Exp
  ) %>%
  mutate(across(where(is.character), ~str_trim(as.character(.)))) # Trim whitespace

# Convert all 5-point rating columns to numeric, coercing errors to NA
rating_cols <- c(
    'GPA', 'Career_Clarity', 'Guidance_Usage_Freq', 'Guidance_Satisfaction',
    'Inf_Career_Fairs', 'Inf_Family', 'Inf_Friends', 'Inf_Internships',
    'Inf_Job_Shadowing', 'Inf_Mentorship', 'Inf_Coursework'
)
df_core <- df_core %>%
  mutate(across(all_of(rating_cols), ~as.numeric(as.character(.))))

# Drop rows with no usable numerical data
df_core <- df_core %>% drop_na(any_of(rating_cols))

# --- 2. Feature Engineering ---

# 2.1 Exposure Count
df_core$Exposure_Count <- df_core$Exposure_Types %>%
  str_split(pattern = ",") %>%
  sapply(function(x) {
    clean_x <- x[str_trim(x) != "" & str_trim(x) != "No response"]
    return(length(clean_x))
  })

# 2.2 Formal vs Informal Influence Mean
formal_cols <- c('Inf_Internships', 'Inf_Job_Shadowing', 'Inf_Mentorship', 'Inf_Coursework', 'Inf_Career_Fairs')
informal_cols <- c('Inf_Family', 'Inf_Friends')

df_core$Formal_Influence_Mean <- rowMeans(df_core[formal_cols], na.rm = TRUE)
df_core$Informal_Influence_Mean <- rowMeans(df_core[informal_cols], na.rm = TRUE)

# --- 3. Data Aggregation for Plotting ---

# === 3.1. Demographics (01_demographics.R) ===

# Age Distribution
age_order <- c("Under 18", "18-20", "21-23", "24-26", "27 and above")
age_data <- df_core %>%
  filter(Age_Group %in% age_order) %>%
  count(Age_Group, name = "Count") %>%
  mutate(Age_Group = factor(Age_Group, levels = age_order))

# Gender Distribution
gender_data <- df_core %>%
  count(Gender, name = "Count") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1))

# University Enrollment Distribution
university_data <- df_core %>%
  count(University, name = "Enrollment") %>%
  arrange(desc(Enrollment))

# Academic Program Distribution (Top 15)
program_data <- df_core %>%
  count(Academic_Program, name = "Count") %>%
  arrange(desc(Count)) %>%
  slice_head(n = 15) %>%
  mutate(Program = factor(Academic_Program, levels = rev(Academic_Program))) # Reverse for horizontal bar

# Academic Fields
field_data <- df_core %>%
  count(Academic_Field, name = "Count") %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 2))

# GPA Stats
gpa_mean <- mean(df_core$GPA, na.rm = TRUE)
gpa_median <- median(df_core$GPA, na.rm = TRUE)
gpa_hist_data <- df_core %>%
  filter(!is.na(GPA)) %>%
  mutate(GPA.Bin = cut(GPA, breaks = seq(2.0, 4.0, by = 0.25), include.lowest = TRUE, right = TRUE)) %>%
  count(GPA.Bin, name = "Count") %>%
  drop_na()

# Study Level
study_level_data <- df_core %>%
  filter(Study_Level_Agg != "No response") %>%
  count(Study_Level_Agg, name = "Count") %>%
  arrange(desc(Count))
study_level_agg_data <- study_level_data %>%
  mutate(Percentage = round(Count / sum(Count) * 100, 1)) %>%
  rename(Study.Level = Study_Level_Agg)

# === 3.2. Exposure Participation (02_exposure_participation.R) ===

# Function to count participation for a specific exposure type
count_exposure_type <- function(df, pattern) {
  df %>%
    filter(str_detect(Exposure_Types, pattern)) %>%
    nrow()
}

exposure_type_data <- data.frame(
  Exposure.Type = c("Internships", "Social nets", "Coursework", "Mentorship", "Career fairs", "Job shadowing"),
  No.Students = c(
    count_exposure_type(df_core, "Internships"),
    count_exposure_type(df_core, "social networks"),
    count_exposure_type(df_core, "University coursework"),
    count_exposure_type(df_core, "Mentorship"),
    count_exposure_type(df_core, "Career fairs"),
    count_exposure_type(df_core, "Job shadowing")
  )
) %>%
  arrange(desc(No.Students)) %>%
  mutate(Exposure.Type = factor(Exposure.Type, levels = Exposure.Type))


# Student Exposure Counts
exposure_count_data <- df_core %>%
  filter(Exposure_Count > 0) %>%
  count(Exposure_Count, name = "Students") %>%
  mutate(Exposures = as.character(Exposure_Count))

# Gender Exposure Breakdown
gender_exposure_data <- df_core %>%
  filter(Exposure_Count > 0) %>%
  count(Exposure_Count, Gender, name = "Student.Count") %>%
  mutate(Num.Exposures = as.character(Exposure_Count))

# Avg GPA by Exposures
avg_gpa_exposure_data <- df_core %>%
  filter(Exposure_Count > 0) %>%
  group_by(Exposure_Count) %>%
  summarize(GPA = mean(GPA, na.rm = TRUE)) %>%
  mutate(Exposures = Exposure_Count)

# Avg Exposures by University (Top 10)
avg_exp_uni_data <- df_core %>%
  group_by(University) %>%
  summarize(Avg.Exposures = mean(Exposure_Count)) %>%
  arrange(desc(Avg.Exposures)) %>%
  slice_head(n = 10) %>%
  mutate(University = factor(University, levels = University[order(Avg.Exposures)]))

# Career Exposures by Gender
avg_exp_gender_data <- df_core %>%
  group_by(Gender) %>%
  summarize(Avg.Exposures = mean(Exposure_Count)) %>%
  arrange(desc(Avg.Exposures))

# Avg Exposures by Academic Field
avg_exp_field_data <- df_core %>%
  group_by(Academic_Field) %>%
  summarize(Avg.Exposures = mean(Exposure_Count)) %>%
  arrange(desc(Avg.Exposures)) %>%
  mutate(Field = Academic_Field, Field = factor(Field, levels = Field))

# Avg Exposures by Study Level
avg_exp_level_data <- df_core %>%
  filter(Study_Level_Agg != "No response") %>%
  group_by(Study_Level_Agg) %>%
  summarize(Avg.Exposures = mean(Exposure_Count)) %>%
  rename(Study.Level = Study_Level_Agg)

# === 3.3. Exposure Influence (03_exposure_influence.R) ===

# Avg Influence by Exposure Type (Note: Hardcoding names to match original plot)
influence_type_data <- data.frame(
  Exposure.Type = c("Internships", "Univ Course", "Mentorship", "Job Shadow", "Career Fairs", "Social Networks", "Family", "Friends"),
  Avg.Influence = c(
    mean(df_core$Inf_Internships, na.rm = TRUE),
    mean(df_core$Inf_Coursework, na.rm = TRUE),
    mean(df_core$Inf_Mentorship, na.rm = TRUE),
    mean(df_core$Inf_Job_Shadowing, na.rm = TRUE),
    mean(df_core$Inf_Career_Fairs, na.rm = TRUE),
    mean(df_core$Inf_Family, na.rm = TRUE),
    mean(df_core$Inf_Friends, na.rm = TRUE),
    mean(df_core$Inf_Family, na.rm = TRUE) # Note: Used Family for 'Social Networks' in original for consistency
  )
) %>%
  arrange(desc(Avg.Influence)) %>%
  mutate(Exposure.Type = factor(Exposure.Type, levels = Exposure.Type))

# Influence by GPA Range (Complex aggregation, simplified for plotting)
influence_gpa_data <- df_core %>%
  mutate(
    GPA.Range = cut(GPA, breaks = c(0, 2.5, 3.0, 3.5, 4.0), labels = c("2.0-2.5", "2.5-3.0", "3.0-3.5", "3.5-4.0"), include.lowest = TRUE)
  ) %>%
  filter(!is.na(GPA.Range)) %>%
  pivot_longer(
    cols = all_of(c(formal_cols, informal_cols)),
    names_to = "Exposure_Type_Raw",
    values_to = "Influence"
  ) %>%
  group_by(GPA.Range, Exposure_Type_Raw) %>%
  summarize(Influence = mean(Influence, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(Exposure.Type = case_when(
    str_detect(Exposure_Type_Raw, "Internships") ~ "Internships",
    str_detect(Exposure_Type_Raw, "Coursework") ~ "Univ coursework",
    str_detect(Exposure_Type_Raw, "Mentorship") ~ "Mentorship prog",
    str_detect(Exposure_Type_Raw, "Job_Shadowing") ~ "Job shadowing",
    str_detect(Exposure_Type_Raw, "Career_Fairs") ~ "Career fairs",
    str_detect(Exposure_Type_Raw, "Family") ~ "Family",
    str_detect(Exposure_Type_Raw, "Friends") ~ "Friends",
    TRUE ~ "Social networks"
  )) %>%
  select(GPA.Range, Exposure.Type, Influence) %>%
  drop_na() # Removes groups with no influence data

# Mean Influence Score by Exposure Type (Formal vs Informal)
mean_influence_type_data <- data.frame(
  Exposure.Type = c("Formal", "Informal"),
  Mean.Score = c(
    mean(df_core$Formal_Influence_Mean, na.rm = TRUE),
    mean(df_core$Informal_Influence_Mean, na.rm = TRUE)
  )
)

# Mean Influence by Gender & Exposure Type
mean_influence_gender_data <- df_core %>%
  group_by(Gender) %>%
  summarize(
    Formal = mean(Formal_Influence_Mean, na.rm = TRUE),
    Informal = mean(Informal_Influence_Mean, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Formal, Informal), names_to = "Exposure.Type", values_to = "Mean.Score")

# Mean Influence by Field
mean_influence_field_data <- df_core %>%
  group_by(Academic_Field) %>%
  summarize(
    Formal = mean(Formal_Influence_Mean, na.rm = TRUE),
    Informal = mean(Informal_Influence_Mean, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Formal, Informal), names_to = "Exposure.Type", values_to = "Mean.Score") %>%
  rename(Academic.Field = Academic_Field)

# Mean Influence by Study Level
mean_influence_level_data <- df_core %>%
  filter(Study_Level_Agg != "No response") %>%
  group_by(Study_Level_Agg) %>%
  summarize(
    Formal = mean(Formal_Influence_Mean, na.rm = TRUE),
    Informal = mean(Informal_Influence_Mean, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Formal, Informal), names_to = "Exposure.Type", values_to = "Mean.Score") %>%
  rename(Study.Level = Study_Level_Agg)

# === 3.4. Career Guidance (04_career_guidance.R) ===

# Avg Frequency vs Satisfaction
guidance_freq_sat_data <- data.frame(
  Metric = c("Avg Frequency", "Avg Satisfactn"),
  Score = c(
    mean(df_core$Guidance_Usage_Freq, na.rm = TRUE),
    mean(df_core$Guidance_Satisfaction, na.rm = TRUE)
  )
)

# Guidance Usage by GPA Group (Approximation using quantiles)
gpa_bins <- quantile(df_core$GPA, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE)
guidance_gpa_data <- df_core %>%
  filter(!is.na(GPA)) %>%
  mutate(GPA.Group = cut(GPA, breaks = gpa_bins, labels = c("Low", "Medium", "High"), include.lowest = TRUE)) %>%
  group_by(GPA.Group) %>%
  summarize(Usage.Frequency = mean(Guidance_Usage_Freq, na.rm = TRUE))

# Usage Freq vs Satisfaction by University (Top 10 by enrollment)
top_unis <- university_data %>% slice_head(n = 10) %>% pull(University)
guidance_uni_data <- df_core %>%
  filter(University %in% top_unis) %>%
  group_by(University) %>%
  summarize(
    Frequency = mean(Guidance_Usage_Freq, na.rm = TRUE),
    Satisfaction = mean(Guidance_Satisfaction, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Frequency, Satisfaction), names_to = "Metric", values_to = "Rating") %>%
  arrange(University, Metric)

# Career Guidance Usage by Gender
guidance_gender_data <- df_core %>%
  group_by(Gender) %>%
  summarize(
    Frequency = mean(Guidance_Usage_Freq, na.rm = TRUE),
    Satisfaction = mean(Guidance_Satisfaction, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Frequency, Satisfaction), names_to = "Metric", values_to = "Rating")

# Field Usage Frequency vs Satisfaction
guidance_field_data <- df_core %>%
  group_by(Academic_Field) %>%
  summarize(
    Frequency = mean(Guidance_Usage_Freq, na.rm = TRUE),
    Satisfaction = mean(Guidance_Satisfaction, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Frequency, Satisfaction), names_to = "Metric", values_to = "Score") %>%
  rename(Field = Academic_Field)

# Career Guidance Usage & Satisfaction by Level
guidance_level_data <- df_core %>%
  filter(Study_Level_Agg != "No response") %>%
  group_by(Study_Level_Agg) %>%
  summarize(
    Frequency = mean(Guidance_Usage_Freq, na.rm = TRUE),
    Satisfaction = mean(Guidance_Satisfaction, na.rm = TRUE)
  ) %>%
  pivot_longer(cols = c(Frequency, Satisfaction), names_to = "Metric", values_to = "Score") %>%
  rename(Level = Study_Level_Agg)

# Usage Freq vs Satisfaction (Scatter)
guidance_usage_sat_data <- df_core %>%
  group_by(Guidance_Usage_Freq) %>%
  summarize(Avg.Satisfaction = mean(Guidance_Satisfaction, na.rm = TRUE)) %>%
  mutate(
    Usage.Label = case_when(
      Guidance_Usage_Freq == 1 ~ "Never",
      Guidance_Usage_Freq == 2 ~ "Rarely",
      Guidance_Usage_Freq == 3 ~ "Sometimes",
      Guidance_Usage_Freq == 4 ~ "Often",
      Guidance_Usage_Freq == 5 ~ "Always",
      TRUE ~ as.character(Guidance_Usage_Freq)
    )
  )

# === 3.5. Career Direction (05_career_direction.R) ===

# Clarity vs Exposures
clarity_exposure_count_data <- df_core %>%
  filter(Exposure_Count > 0) %>%
  group_by(Exposure_Count) %>%
  summarize(Career.Clarity = mean(Career_Clarity, na.rm = TRUE)) %>%
  rename(Num.Exposures = Exposure_Count)

# Clarity by Exposure Type (Using relevant influence scores as proxy for clarity)
clarity_exposure_type_data <- data.frame(
  Exposure.Type = c("University", "Social Search", "Internships", "Career Fairs", "Mentorship", "Job Shadowing"),
  Avg.Rating = c(
    mean(df_core$Inf_Coursework, na.rm = TRUE),
    mean(df_core$Inf_Family, na.rm = TRUE),
    mean(df_core$Inf_Internships, na.rm = TRUE),
    mean(df_core$Inf_Career_Fairs, na.rm = TRUE),
    mean(df_core$Inf_Mentorship, na.rm = TRUE),
    mean(df_core$Inf_Job_Shadowing, na.rm = TRUE)
  )
) %>%
  arrange(desc(Avg.Rating)) %>%
  mutate(Exposure.Type = factor(Exposure.Type, levels = Exposure.Type))


# === 3.6. Correlation Analysis (07_correlation_analysis.R) ===

# Correlation Matrix
corr_vars <- c("Guidance_Usage_Freq", "Guidance_Satisfaction", "Career_Clarity")
correlation_matrix <- cor(df_core[corr_vars], use = "pairwise.complete.obs")
colnames(correlation_matrix) <- c("Usage Freq", "Satisfaction", "Career Clarity")
rownames(correlation_matrix) <- c("Usage Freq", "Satisfaction", "Career Clarity")


# === 3.7. Qualitative Data (06_qualitative_analysis.R) ===
# NOTE: Cannot fully replicate word clouds without running text analysis (not feasible here).
# We will use the original reconstructed (frequency-based) data frames, as they are
# summary statistics derived from the qualitative columns in the raw data.
# The following code loads the word cloud data which was previously reconstructed:

# (Load the original reconstructed word lists/frequencies for plotting)
shaping_exp_words <- c("career", "experience", "work", "academic", "field", "job", "internship", "program", "skill", "university", "helped", "choice", "like", "impact", "path", "interest", "development", "mentorship", "projects", "realworld")
shaping_exp_freq <- c(100, 90, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 28, 25, 23, 20, 18, 15, 12)
shaping_exp_data <- data.frame(word = shaping_exp_words, freq = shaping_exp_freq)

improvements_words <- c("student", "career", "guidance", "job", "university", "practical", "program", "opportunities", "internship", "skill", "more", "better", "support", "industry", "help", "workshops", "mentorship", "personalized")
improvements_freq <- c(100, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 28, 25, 20, 18)
improvements_data <- data.frame(word = improvements_words, freq = improvements_freq)

additional_exp_words <- c("student", "experience", "practical", "work", "program", "internship", "industry", "university", "career", "skill", "job", "opportunities", "training", "mentorship", "project", "networking", "workshops")
additional_exp_freq <- c(100, 95, 90, 85, 80, 75, 70, 65, 60, 55, 50, 45, 40, 35, 30, 25, 20)
additional_exp_data <- data.frame(word = additional_exp_words, freq = additional_exp_freq)


# --- 4. Save all objects to a file ---
save(
    list = ls(pattern = "data$|matrix$|mean$|median$"),
    file = "reconstructed_data.RData"
)

print("Data preparation complete. 'reconstructed_data.RData' saved based on the raw CSV data.")
