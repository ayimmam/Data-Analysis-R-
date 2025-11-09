# 04_career_guidance.R

library(ggplot2)
library(dplyr)
library(ggrepel) # For non-overlapping labels

# Load the reconstructed data
load("reconstructed_data.RData")

# --- Plot 1: Avg Frequency vs Satisfaction (Bar Chart) ---
plot_guide_avg <- ggplot(guidance_freq_sat_data, aes(x = Metric, y = Score)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  geom_text(aes(label = round(Score, 2)), vjust = -0.5) +
  labs(title = "Avg Frequency vs Satisfaction", x = "Metrics", y = "Score (1-5)") +
  theme_minimal(base_size = 14) +
  ylim(0, 5)

print(plot_guide_avg)

# --- Plot 2: Career Guidance Usage by GPA Group (Bar Chart) ---
plot_guide_gpa <- ggplot(guidance_gpa_data, aes(x = GPA.Group, y = Usage.Frequency)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Career Guidance Usage by GPA Group", x = "GPA Group", y = "Usage Frequency") +
  theme_minimal(base_size = 14)

print(plot_guide_gpa)

# --- Plot 3: Usage Freq vs Satisfaction by University (Grouped Horizontal Bar) ---
plot_guide_uni <- ggplot(guidance_uni_data, aes(x = reorder(University, -Rating), y = Rating, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Usage Freq vs Satisfaction (Top 10 Universities)", x = "University", y = "Rating (1-5)") +
  coord_flip() +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Frequency" = "#00A0B0", "Satisfaction" = "#E84A5F"))

print(plot_guide_uni)

# --- Plot 4: Career Guidance Usage by Gender (Grouped Bar Chart) ---
plot_guide_gender <- ggplot(guidance_gender_data, aes(x = Gender, y = Rating, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Career Guidance Usage by Gender", x = "Gender", y = "Rating (1-5)") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Frequency" = "#00A0B0", "Satisfaction" = "#E84A5F")) +
  ylim(0, 5)

print(plot_guide_gender)

# --- Plot 5: Field Usage Frequency vs Satisfaction (Grouped Bar Chart) ---
plot_guide_field <- ggplot(guidance_field_data, aes(x = Field, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Field Usage Frequency vs Satisfaction", x = "Field", y = "Score (1-5)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Frequency" = "#00A0B0", "Satisfaction" = "#E84A5F"))

print(plot_guide_field)

# --- Plot 6: Career Guidance Usage & Satisfaction by Level (Grouped Bar Chart) ---
plot_guide_level <- ggplot(guidance_level_data, aes(x = Level, y = Score, fill = Metric)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  labs(title = "Career Guidance Usage & Satisfaction", x = "Level", y = "Score") +
  theme_minimal(base_size = 14) +
  scale_fill_manual(values = c("Frequency" = "#00A0B0", "Satisfaction" = "#E84A5F")) +
  ylim(0, 5)

print(plot_guide_level)

# --- Plot 7: Usage Freq vs Satisfaction (Line/Scatter Plot) ---
plot_guide_usage_sat <- ggplot(guidance_usage_sat_data, aes(x = Guidance_Usage_Freq, y = Avg.Satisfaction)) +
  geom_line(color = "red", size = 1) +
  geom_point(color = "#00A0B0", size = 3) +
  geom_text_repel(aes(label = Usage.Label), box.padding = 0.5) +
  labs(title = "Usage Freq vs Satisfaction", x = "Usage Freq", y = "Avg Satisfaction") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = 1:5)

print(plot_guide_usage_sat)
