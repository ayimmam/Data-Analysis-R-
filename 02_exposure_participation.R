# 02_exposure_participation.R

library(ggplot2)
library(dplyr)

# Load the reconstructed data
load("reconstructed_data.RData")

# --- Plot 1: Students by Exposure Type (Bar Chart) ---
plot_exp_type <- ggplot(exposure_type_data, aes(x = Exposure.Type, y = No.Students)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  geom_text(aes(label = No.Students), vjust = -0.5) +
  labs(title = "Students by Exposure Type", x = "Exposure Type", y = "No. Students") +
  theme_minimal(base_size = 14)

print(plot_exp_type)

# --- Plot 2: Student Exposure Counts (Bar Chart) ---
plot_exp_count <- ggplot(exposure_count_data, aes(x = Exposures, y = Students)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Student Exposure Counts", x = "Exposures", y = "Students") +
  theme_minimal(base_size = 14)

print(plot_exp_count)

# --- Plot 3: Gender Exposure Breakdown (Grouped Bar Chart) ---
plot_gender_exp <- ggplot(gender_exposure_data, aes(x = Num.Exposures, y = Student.Count, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Gender Exposure Breakdown", x = "Num Exposures", y = "Student Count") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Female" = "#E84A5F", "Male" = "#00A0B0"))

print(plot_gender_exp)

# --- Plot 4: Avg GPA by Exposures (Line Chart) ---
plot_gpa_exp <- ggplot(avg_gpa_exposure_data, aes(x = Exposures, y = GPA)) +
  geom_line(color = "#00A0B0", size = 1.5) +
  geom_point(color = "#00A0B0", size = 3) +
  geom_hline(aes(yintercept = gpa_mean), linetype = "dashed", color = "red") +
  geom_hline(aes(yintercept = gpa_median), linetype = "dotted", color = "green") +
  labs(title = "Avg GPA by Exposures", x = "Exposures", y = "GPA") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = unique(avg_gpa_exposure_data$Exposures))

print(plot_gpa_exp)

# --- Plot 5: Top 10 Unis by Avg Exposures (Horizontal Bar Chart) ---
plot_uni_exp <- ggplot(avg_exp_uni_data, aes(x = University, y = Avg.Exposures)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  geom_text(aes(label = round(Avg.Exposures, 2)), hjust = -0.2) +
  labs(title = "Top 10 Unis by Avg Exposures", x = "University", y = "Avg Exposures") +
  coord_flip() +
  theme_minimal(base_size = 14)

print(plot_uni_exp)

# --- Plot 6: Career Exposures by Gender (Bar Chart) ---
plot_gender_avg_exp <- ggplot(avg_exp_gender_data, aes(x = Gender, y = Avg.Exposures)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Career Exposures by Gender", x = "Gender", y = "Avg Exposures") +
  theme_minimal(base_size = 14)

print(plot_gender_avg_exp)

# --- Plot 7: Average Exposures by Academic Field (Bar Chart) ---
plot_field_avg_exp <- ggplot(avg_exp_field_data, aes(x = Field, y = Avg.Exposures)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Average Exposures by Academic Field", x = "Academic Field", y = "Avg Exposures") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_field_avg_exp)

# --- Plot 8: Avg Exposures by Study Level (Bar Chart) ---
plot_level_avg_exp <- ggplot(avg_exp_level_data, aes(x = Study.Level, y = Avg.Exposures)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Avg Exposures by Study Level", x = "Study Level", y = "Avg Exposures") +
  theme_minimal(base_size = 14)

print(plot_level_avg_exp)
