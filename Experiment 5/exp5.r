# Load libraries
library(ggplot2)
library(dplyr)
library(lintr)
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(plotly)
library(tm)
library(quanteda)
library(scatterplot3d)

# Load dataset
housing <- read.csv("Datasets\\Housing.csv")

# Convert the 'area' column to a character vector for the word cloud
housing$area <- as.character(housing$area)

# Convert the 'area' column back to numeric for regression analysis
housing$area_numeric <- as.numeric(housing$area)

# Open a PDF device to save all plots
pdf("housing_plots.pdf", onefile = TRUE)

# Create a corpus from the 'area' column
area_corpus <- corpus(housing$area)
dfm_area <- dfm(tokens(area_corpus))
word_freqs_area <- topfeatures(dfm_area, n = nrow(dfm_area))

# Create the word cloud
wordcloud(names(word_freqs_area), freq = word_freqs_area, min.freq = 1, colors = brewer.pal(8, "Dark2"))

# Box and Whisker Plot
p1 <- ggplot(housing, aes(x = as.factor(stories), y = price)) +
  geom_boxplot() +
  theme_minimal() +
  xlab("Number of Stories") +
  ylab("Price") +
  ggtitle("Box and Whisker Plot of Price by Stories")

print(p1)

# Violin Plot
p2 <- ggplot(housing, aes(x = as.factor(bedrooms), y = price)) +
  geom_violin(trim = FALSE) +
  theme_minimal() +
  xlab("Number of Bedrooms") +
  ylab("Price") +
  ggtitle("Violin Plot of Price by Bedrooms")

print(p2)

# Linear Regression Model
linear_model <- lm(price ~ area_numeric + bedrooms + bathrooms + stories + parking, data = housing)
summary(linear_model)

# Plot the linear regression
p3 <- ggplot(housing, aes(x = area_numeric, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", col = "blue") +
  labs(
    title = "Linear Regression of Price on Area",
    x = "Area",
    y = "Price"
  ) +
  theme_light()

print(p3)

# Polynomial Regression Model
poly_model <- lm(price ~ poly(area_numeric, 2) + bedrooms + bathrooms + stories + parking, data = housing)
summary(poly_model)

# Plot the polynomial regression
p4 <- ggplot(housing, aes(x = area_numeric, y = price)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), col = "red") +
  labs(
    title = "Polynomial Regression of Price on Area",
    x = "Area",
    y = "Price"
  ) +
  theme_light()

print(p4)

# Jitter Plot
p5 <- ggplot(housing, aes(x = as.factor(parking), y = price)) +
  geom_jitter(width = 0.1) +
  theme_minimal() +
  xlab("Parking Spaces") +
  ylab("Price") +
  ggtitle("Jitter Plot of Price by Parking Spaces")

print(p5)

# Create the 3D scatter plot with price, area, and bedrooms
scatter_3d <- scatterplot3d(housing$area_numeric, housing$bedrooms, housing$price,
    pch = 16, # Use solid circles for points
    color = "blue", # Set point color to blue
    xlab = "Area", # Label for x-axis
    ylab = "Bedrooms", # Label for y-axis
    zlab = "Price", # Label for z-axis
    main = "3D Scatter Plot of Price vs Area and Bedrooms"
)

# Close the PDF device
dev.off()
