library(wordcloud)
library(RColorBrewer)

# Load the reconstructed data
load("reconstructed_data.RData")

# Set a consistent color palette
palette <- brewer.pal(8, "Dark2")

# --- Plot 1: Shaping Experiences Word Cloud ---
set.seed(123) # for reproducibility
wordcloud(
  words = shaping_exp_data$word,
  freq = shaping_exp_data$freq,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.3,
  colors = palette
)
title("Word Cloud: Shaping Experiences")


# --- Plot 2: Improvements Word Cloud ---
set.seed(123)
wordcloud(
  words = improvements_data$word,
  freq = improvements_data$freq,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.3,
  colors = palette
)
title("Word Cloud: Improvements")

# --- Plot 3: Additional Exposures Word Cloud ---
set.seed(123)
wordcloud(
  words = additional_exp_data$word,
  freq = additional_exp_data$freq,
  min.freq = 1,
  max.words = 200,
  random.order = FALSE,
  rot.per = 0.3,
  colors = palette
)
title("Word Cloud: Additional Exposures")
