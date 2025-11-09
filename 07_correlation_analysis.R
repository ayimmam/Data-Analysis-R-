# 07_correlation_analysis.R

library(ggplot2)
library(reshape2) # For melt()

# Load the reconstructed data
load("reconstructed_data.RData")

# Melt the correlation matrix for ggplot
melted_corr <- melt(correlation_matrix)
names(melted_corr) <- c("Var1", "Var2", "value")

# --- Plot 1: Correlation Matrix (Heatmap) ---
plot_corr_heatmap <- ggplot(melted_corr, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Correlation"
  ) +
  geom_text(aes(label = sprintf("%.2f", value)), color = "black", size = 4) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    axis.title = element_blank()
  ) +
  labs(title = "Correlation Matrix") +
  coord_fixed() # Ensures squares are square

print(plot_corr_heatmap)
