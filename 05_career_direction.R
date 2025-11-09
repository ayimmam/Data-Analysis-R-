# 05_career_direction.R

library(ggplot2)
library(dplyr)

# Load the reconstructed data
load("reconstructed_data.RData")

# --- Plot 1: Career Clarity vs Exposures (Line Chart) ---
plot_clarity_count <- ggplot(clarity_exposure_count_data, aes(x = Num.Exposures, y = Career.Clarity)) +
  geom_line(color = "#00A0B0", size = 1.5) +
  geom_point(color = "#00A0B0", size = 3) +
  labs(title = "Career Clarity vs Exposures", x = "Num Exposures", y = "Career Clarity") +
  theme_minimal(base_size = 14) +
  scale_x_continuous(breaks = unique(clarity_exposure_count_data$Num.Exposures)) +
  ylim(1, 5) # Set Y-axis scale from 1 to 5

print(plot_clarity_count)

# --- Plot 2: Career Direction Clarity by Exposure Type (Bar Chart) ---
plot_clarity_type <- ggplot(clarity_exposure_type_data, aes(x = Exposure.Type, y = Avg.Rating)) +
  geom_bar(stat = "identity", fill = "#00A0B0") +
  labs(title = "Career Direction Clarity by Exposure Type", x = "Exposure Type", y = "Avg Rating") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(plot_clarity_type)
