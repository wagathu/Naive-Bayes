

# Import the libraries ----------------------------------------------------

library(pacman)
p_load(e1071, readxl, dplyr, ggplot2, ModelMetrics)


# Importing the Data ------------------------------------------------------

data <- read.csv(file.choose(), as.is = TRUE)
testdata <- read.csv(file.choose(), as.is = TRUE)

colSums(is.na(data))

