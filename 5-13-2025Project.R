library(readr)
library(dplyr)
# Create the composite scores FIRST
qb_rankings <- qbr_data %>%
  select(Player, Team, Yds, TD, `Cmp%`, QBR, Rate) %>%
  mutate(
    across(-c(Player, Team), ~ scale(.)),
    Composite = (Yds * 0.2) + (TD * 0.3) + (`Cmp%` * 0.1) + (QBR * 0.3) + (Rate * 0.1)
  ) %>%
  mutate(PlayerID = paste(Player, Team, sep = "_"))

# Your LLE processing
numeric_cols <- c("Rk", "Age", "G", "GS", "Cmp", "Att", "TD", "Int", "Sk", "Rate", "QBR")
qbr_numeric <- qbr_data %>% 
  select(all_of(numeric_cols)) %>%
  mutate(across(everything(), as.numeric)) %>%
  mutate(across(everything(), ~ ifelse(is.na(.) | is.infinite(.), mean(., na.rm = TRUE), .)) %>%
           mutate(across(where(~ var(., na.rm = TRUE) > 0), scale)) %>%
           as.matrix())  # Convert to matrix explicitly
# Compute pairwise Euclidean distances
dist_matrix <- as.matrix(dist(qbr_numeric, method = "euclidean"))

k <- min(10, nrow(qbr_numeric) - 1)  # Ensure k < n-1
n <- nrow(qbr_numeric)

# Initialize neighbor indices matrix
neighbors <- matrix(0, nrow = n, ncol = k)

for (i in 1:n) {
  neighbors[i, ] <- order(dist_matrix[i, ])[2:(k + 1)]  # Exclude self
}

# Initialize weight matrix
W <- matrix(0, nrow = n, ncol = n)
if (any(is.na(neighbors))) {
  stop("NA values in neighbor indices")
}
for (i in 1:n) {
  Xi_centered <- Xi - colMeans(Ni)
  Ni_centered <- sweep(Ni, 2, colMeans(Ni))
  
  # Convert to matrix if needed
  if (!is.matrix(Ni_centered)) Ni_centered <- as.matrix(Ni_centered)
  
  # Add NA/Inf check
  if (any(is.na(Ni_centered)) | any(is.infinite(Ni_centered))) {
    Ni_centered[is.infinite(Ni_centered)] <- 0
    Ni_centered[is.na(Ni_centered)] <- 0
  }
  
  # Local covariance matrix with tryCatch
  C <- tryCatch(
    Ni_centered %*% t(Ni_centered),
    error = function(e) {
      message("Error at i=", i, ": ", e$message)
      diag(k) * lambda  # Return diagonal matrix as fallback
    }
  )
  
  # Solve for weights
  lambda <- 0.1
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

# Ensure no NA/Inf in M before eigen()
M[is.na(M) | is.infinite(M)] <- 0  
M <- (M + t(M)) / 2  # Force symmetry
# Ensure M is symmetric (required for eigen)
if (any(eig$values < 1e-6)) {
  warning("Small eigenvalues detected. Try increasing lambda or k.")
}
# Now compute eigenvalues
eig <- eigen(M)
embedding <- eig$vectors[, (n - 1):(n - 2)]  # Last 2 non-constant eigenvectors

# Create lle_df with PlayerID
lle_df <- data.frame(
  Player = qbr_data$Player,
  Team = qbr_data$Team,  # Need Team for joining
  Dim1 = embedding[, 1],
  Dim2 = embedding[, 2]
) %>%
  mutate(PlayerID = paste(Player, Team, sep = "_"))

# Join with composite scores
lle_df <- lle_df %>% 
  left_join(qb_rankings %>% select(PlayerID, Composite), by = "PlayerID")

# Verify the join worked
if (!"Composite" %in% names(lle_df)) {
  stop("Composite column not found after join. Check PlayerID matching.")
}

# Now plot
ggplot(lle_df, aes(Dim1, Dim2)) +
  geom_point(aes(color = Composite), size = 3) +
  scale_color_gradient(low = "red", high = "blue") +
  geom_text(aes(label = Player), size = 3, vjust = -1) +
  labs(title = "QB Similarity (LLE) vs. Effectiveness (Composite Score)")



#######################################################
# qb_rankings <- qbr_data %>%
#  select(Player, Yds, TD, `Cmp%`, QBR, Rate) %>%
#  mutate(
#    # Standardize each metric (mean=0, sd=1)
#    across(-Player, ~ scale(.)),
#    # Create composite score (adjust weights as needed)
#    Composite = (Yds * 0.2) + (TD * 0.3) + (`Cmp%` * 0.1) + (QBR * 0.3) + (Rate * 0.1) 
#  ) %>%
#  arrange(desc(Composite))  # Rank from best to worst

# qb_rankings %>%
#  top_n(15, Composite) %>%
#  ggplot(aes(x = reorder(Player, Composite), y = Composite)) +
# geom_col(aes(fill = Composite)) +
# coord_flip() +
#  labs(title = "Top 15 QBs by Composite Score")
