library(ggplot2)
library(dplyr)
library(RColorBrewer)

# Load the reconstructed data
load("reconstructed_data.RData")

# --- Plot 1: Avg Influence by Exposure Type (Bar Chart) ---
plot_inf_type <- ggplot(influence_type_data, aes(x = Exposure.Type, y = Avg.Influence, fill = Exposure.Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Avg Influence by Exposure Type", x = "Exposure Type", y = "Avg Influence") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(plot_inf_type)

# --- Plot 2: Influence by GPA Range (Grouped Bar Chart) ---
plot_inf_gpa <- ggplot(influence_gpa_data, aes(x = Exposure.Type, y = Influence, fill = Exposure.Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ GPA.Range, ncol = 4) +
  labs(title = "Influence by GPA Range", x = "Exposure Type", y = "Influence") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  scale_fill_manual(values = c(
    "Internships" = "blue", "Univ coursework" = "green", "Mentorship prog" = "orange",
    "Job shadowing" = "purple", "Career fairs" = "red", "Social networks" = "magenta",
    "Family" = "darkred", "Friends" = "darkgreen"
  ))

print(plot_inf_gpa)

# --- Plot 3: Mean Influence Score by Exposure Type (Bar Chart) ---
plot_inf_formal <- ggplot(mean_influence_type_data, aes(x = Exposure.Type, y = Mean.Score, fill = Exposure.Type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.2f", Mean.Score)), vjust = -0.5) +
  labs(title = "Mean Influence Score by Exposure Type", x = "Exposure Type", y = "Mean Score") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Formal" = "#00A0B0", "Informal" = "#E84A5F"))

print(plot_inf_formal)

# --- Plot 4: Mean Influence by Gender & Exposure Type (Grouped Bar Chart) ---
plot_inf_gender <- ggplot(mean_influence_gender_data, aes(x = Gender, y = Mean.Score, fill = Exposure.Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Influence by Gender & Exposure Type", x = "Gender", y = "Mean Score") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Formal" = "#00A0B0", "Informal" = "#E84A5F"))

print(plot_inf_gender)

# --- Plot 5: Formal vs Informal Mean Scores by Field (Grouped Bar Chart) ---
plot_inf_field <- ggplot(mean_influence_field_data, aes(x = Academic.Field, y = Mean.Score, fill = Exposure.Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Formal vs Informal Mean Scores by Field", x = "Academic Field", y = "Mean Score") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Formal" = "#00A0B0", "Informal" = "#E84A5F"))

print(plot_inf_field)

# --- Plot 6: Mean Influence Score by Study Level (Grouped Bar Chart) ---
plot_inf_level <- ggplot(mean_influence_level_data, aes(x = Study.Level, y = Mean.Score, fill = Exposure.Type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Mean Influence Score by Study Level", x = "Study Level", y = "Mean Score") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Formal" = "#00A0B0", "Informal" = "#E84A5F"))

print(plot_inf_level)
