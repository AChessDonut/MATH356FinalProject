library(readr)
library(dplyr)

qbr_data <- qbr_data %>%  select(-27) 
qbr_data <- rename(qbr_data, Record = PassYds)

numeric_cols <- c("Rk", "Age", "G", "GS", "Cmp", "Att", "Record", "TD", "Int", "Sk", "Rate", "QBR")  # Add others as needed

qbr_numeric <- qbr_data %>% 
  select(-c(Player, Team, Pos, Record, Awards, `Player-additional`)) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  scale()

# Compute pairwise Euclidean distances
dist_matrix <- as.matrix(dist(qbr_numeric, method = "euclidean"))

k <- 10  # Number of neighbors (adjust as needed)
n <- nrow(qbr_numeric)

# Initialize neighbor indices matrix
neighbors <- matrix(0, nrow = n, ncol = k)

for (i in 1:n) {
  neighbors[i, ] <- order(dist_matrix[i, ])[2:(k + 1)]  # Exclude self
}

# Initialize weight matrix
W <- matrix(0, nrow = n, ncol = n)

for (i in 1:n) {
  # Get neighbors of point i
  Xi <- qbr_numeric[i, ]
  Ni <- qbr_numeric[neighbors[i, ], ]
  
  # Center the neighborhood
  Xi_centered <- Xi - colMeans(Ni)
  Ni_centered <- sweep(Ni, 2, colMeans(Ni))
  
  # Local covariance matrix
  C <- Ni_centered %*% t(Ni_centered)
  
  # Solve for weights (add small regularization for stability)
  lambda <- 0.01
  w <- solve(C + lambda * diag(k)) %*% rep(1, k)
  w <- w / sum(w)  # Normalize
  
  # Store weights in W
  W[i, neighbors[i, ]] <- w
}

# Construct the cost matrix M
I <- diag(n)
# After computing M, add checks:
if (any(is.na(M))) {
  M[is.na(M)] <- 0 # Force NA to 0 (or use better imputation)
}

# Ensure M is symmetric (required for eigen)
M <- (M + t(M)) / 2

# Now compute eigenvalues
eig <- eigen(M)
embedding <- eig$vectors[, (n - 1):(n - 2)]  # Last 2 non-constant eigenvectors

# Convert to data frame
lle_df <- data.frame(
  Player = qbr_data$Player,
  Dim1 = embedding[, 1],
  Dim2 = embedding[, 2]
)

library(ggplot2)
ggplot(lle_df, aes(Dim1, Dim2, label = Player)) +
  geom_point(aes(color = qbr_data$Pos)) +
  geom_text(size = 3, vjust = -0.5) +
  labs(title = "LLE (Manual Implementation)")
