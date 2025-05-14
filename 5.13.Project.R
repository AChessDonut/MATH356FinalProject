# Load required libraries
library(readr)
library(dplyr)
library(tidyr)
install.packages('RDRToolbox') # For LLE implementation

# Clean and prepare numeric data for LLE
qbr_numeric <- qbr_data %>%
  select(where(is.numeric)) %>%
  select(-Rk) %>% # Remove rank column
  na.omit() # Remove rows with missing values

# Scale the data (important for LLE)
qbr_scaled <- scale(qbr_numeric)

# Perform LLE (this may take some time depending on dataset size)
set.seed(42) # For reproducibility
lle_result <- LLE(qbr_scaled, dim = 2, k = 10) # k is the number of neighbors

# Combine results with player names for interpretation
lle_df <- data.frame(
  Player = qbr_data$Player[complete.cases(qbr_numeric)],
  LLE1 = lle_result[,1],
  LLE2 = lle_result[,2]
)

# View the first few results
head(lle_df)

library(ggplot2)

ggplot(lle_df, aes(x = LLE1, y = LLE2)) +
  geom_point(alpha = 0.6) +
  ggtitle("LLE Projection of QBR Data (2D)") +
  theme_minimal()