# Plots for the Demographic Profile Analysis section

library(ggplot2)
library(dplyr)

# Load the reconstructed data
load("reconstructed_data.RData")

# --- Plot 1: Age Distribution (Bar Chart) ---
plot_age <- ggplot(age_data, aes(x = Age.Group, y = Count)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Age Distribution", x = "Age Group", y = "Count") +
  theme_minimal(base_size = 14)

print(plot_age)

# --- Plot 2: Gender Distribution (Pie Chart) ---
plot_gender_pie <- ggplot(gender_data, aes(x = "", y = Percentage, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Gender Distribution") +
  theme_void() +
  scale_fill_manual(values = c("Female" = "#E84A5F", "Male" = "#00A0B0"))

print(plot_gender_pie)

# --- Plot 3: University Enrollment Distribution (Bar Chart) ---
plot_university <- ggplot(university_data, aes(x = University, y = Enrollment)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "University Enrollment Distribution", x = "University", y = "Enrollment") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(plot_university)

# --- Plot 4: Top Academic Programs (Horizontal Bar Chart) ---
plot_program <- ggplot(program_data, aes(x = Program, y = Count, fill = Program)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Count), hjust = -0.2, size = 3.5) +
  labs(title = "Top Academic Programs by Number of Respondents", x = "Academic Program", y = "Number of Respondents") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

print(plot_program)

# --- Plot 5: Academic Fields (Pie Chart) ---
plot_field_pie <- ggplot(field_data, aes(x = "", y = Percentage, fill = Field)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5), size = 3) +
  labs(title = "Academic Fields") +
  theme_void()

print(plot_field_pie)

# --- Plot 6: Cumulative GPA Distribution (Histogram Approximation) ---
plot_gpa_hist <- ggplot(gpa_hist_data, aes(x = GPA.Bin, y = Count)) +
  geom_bar(stat = "identity", fill = "#B0E0E6", color = "grey50") +
  geom_vline(aes(xintercept = gpa_mean), color = "red", linetype = "dashed", size = 1.5) +
  geom_text(aes(x = gpa_mean + 0.2, y = 60, label = paste("Mean:", gpa_mean)), color = "red") +
  geom_vline(aes(xintercept = gpa_median), color = "green", size = 1.5) +
  geom_text(aes(x = gpa_median + 0.2, y = 55, label = paste("Median:", gpa_median)), color = "green") +
  labs(title = "Cumulative GPA Distribution of Respondents", x = "Cumulative GPA (Binned)", y = "Number of Students") +
  theme_minimal(base_size = 14)

print(plot_gpa_hist)

# --- Plot 7: Study Level Count (Bar Chart) ---
plot_study_level <- ggplot(study_level_data, aes(x = Study.Level, y = Count)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3.5) +
  labs(title = "Study Level Count", x = "Study Level", y = "Count") +
  theme_minimal(base_size = 14)

print(plot_study_level)

# --- Plot 8: Level of Study Distribution (Bar + Pie) ---

plot_study_agg_bar <- ggplot(study_level_agg_data, aes(x = Study.Level, y = Count, fill = Study.Level)) +
  geom_bar(stat = "identity", color = "grey50") +
  geom_text(aes(label = Count), vjust = -0.5, size = 4) +
  labs(title = "Level of Study Distribution", x = "Study Level", y = "Number of Respondents") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Lower" = "#ADD8E6", "Upper" = "#90EE90", "Graduated" = "#FFD700"))

print(plot_study_agg_bar)

plot_study_agg_pie <- ggplot(study_level_agg_data, aes(x = "", y = Percentage, fill = Study.Level)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar("y", start = 0) +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  labs(title = "Level of Study Distribution (Percentage)") +
  theme_void() +
  scale_fill_manual(values = c("Lower" = "#ADD8E6", "Upper" = "#90EE90", "Graduated" = "#FFD700"))

print(plot_study_agg_pie)
