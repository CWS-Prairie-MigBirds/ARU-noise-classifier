#covariate exploration and graphs

# load libraries

library(tidyverse)


# read in all scored data with metrics

file_list <- list.files(path = "data/", pattern = "\\.csv$", full.names = TRUE)

# 2. Read and combine them all at once
# The 'id' argument adds a column showing which file the data came from

df <- read_csv(file_list, id = "source_file")


