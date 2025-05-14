install.packages("readr")
install.packages('ggplot2')
library(readr)
library(ggplot2)
library(tidyr)
qbr_data <- read_csv("QBR-2.csv", col_names = col_names, skip = 1)

# Check the structure
str(qbr_data)
head(qbr_data)

# Split the single column into multiple columns
qbr_data <- qbr_data %>%
  separate(col = Rk, 
           into = c("Rk", "Player", "Age", "Team", "Pos", "G", "GS", "QBrec", 
                   "Cmp", "Att", "Cmp%", "Yds", "TD", "TD%", "Int", "Int%", 
                   "1D", "Succ%", "Lng", "Y/A", "AY/A", "Y/C", "Y/G", "Rate", 
                   "QBR", "Sk", "SkYds", "Sk%", "NY/A", "ANY/A", "4QC", "GWD", 
                   "Awards", "Player-additional"),
           sep = ",",
           extra = "drop",  # In case there are extra columns
           fill = "right")  # Fill missing values with NA

# Convert numeric columns
numeric_cols <- c("Rk", "Age", "G", "GS", "Cmp", "Att", "Yds", "TD", "Int", 
                 "1D", "Lng", "Sk", "SkYds")
qbr_data <- qbr_data %>%
  mutate(across(all_of(numeric_cols), as.numeric)) %>%
  mutate(across(c("Cmp%", "TD%", "Int%", "Succ%", "Sk%"), 
                ~ as.numeric(gsub("%", "", .))/100))