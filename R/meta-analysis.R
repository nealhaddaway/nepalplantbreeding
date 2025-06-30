#' Meta-analysis for Nepal plant breeding meta-analysis

# import data
library(tidyverse)
library(readxl)
library(metafor)

data <- readRDS(here::here("data/Full_data_extraction_sheet.RDS"))
# view the data to see that it imported okay

#data |> head()
#data |> view()


# Subset data for studies comparing hybridization vs introduction
hyb_int_data <- subset(data, `hyb-int` == 1)

# Initialize result dataframe
ES_data <- data.frame(
  ID = character(),
  short_citation = character(),
  crop = character(),
  ES = numeric(),
  V = numeric(),
  CA_judgement = character(),
  stringsAsFactors = FALSE
)

# Loop through each study ID
for (i in unique(hyb_int_data$ID)) {
  subset_data <- subset(hyb_int_data, ID == i)
  
  # Extract metadata
  short_cit <- subset_data$short_citation[1]
  crop <- subset_data$crop[1]
  CA_judgement <- subset_data$CA_judgement[1]
  
  # Separate hybridization and introduction interventions
  hybrid <- subset(subset_data, `intervention main category` == "hybridization")
  introd  <- subset(subset_data, `intervention main category` == "introduction")
  
  # Skip if either group is missing
  if (nrow(hybrid) == 0 | nrow(introd) == 0) next
  
  # Calculate effect size (mean difference)
  ES <- hybrid$`mean (kg/ha)` - introd$`mean (kg/ha)`
  
  # Calculate correct sampling variance of the mean difference
  V <- (hybrid$SD^2 / hybrid$n) + (introd$SD^2 / introd$n)
  
  # Store in results dataframe
  ES_data <- rbind(ES_data, data.frame(
    ID = i,
    short_citation = short_cit,
    crop = crop,
    ES = ES,
    V = V,
    CA_judgement = CA_judgement
  ))
}

# Optional: Add a label
ES_data$comparison <- "Hybridization - Introduction"

# Order for aesthetics (optional)
ES_data <- ES_data[order(ES_data$crop, rev(ES_data$short_citation)), ]

# Run multilevel meta-analysis
model1 <- rma.mv(
  yi = ES,
  V = V,
  data = ES_data,
  mods = ~ factor(crop),
  method = "ML",
  random = ~ 1 | ID
)

# Show results
summary(model1)

# Function to label Q, I², τ²
mlabfun <- function(text, x) {
  list(bquote(paste(.(text),
                    " (Q = ", .(fmtx(x$QE, digits = 2)),
                    ", df = ", .(x$k - x$p), ", ",
                    .(fmtp(x$QEp, digits = 3, pname = "p", add0 = TRUE, sep = TRUE, equal = TRUE)), "; ",
                    I^2, " = ", .(fmtx(x$I2, digits = 1)), "%, ",
                    tau^2, " = ", .(fmtx(x$tau2, digits = 2)), ")")))
}

# Set crops
crops <- unique(ES_data$crop)
crops <- crops[order(crops)]  # optional: sort alphabetically

# Fit subgroup models (only where possible)
subgroup_models <- list()
group_sizes <- sapply(crops, function(crp) sum(ES_data$crop == crp))

for (crop in crops) {
  dat_crop <- subset(ES_data, crop == crop)
  if (nrow(dat_crop) > 1) {
    subgroup_models[[crop]] <- rma.mv(yi = ES, V = V, data = dat_crop, method = "ML", random = ~1 | ID)
  } else {
    subgroup_models[[crop]] <- NULL
  }
}

# Dynamically compute row positions
row_pos <- list()
current_row <- nrow(ES_data) + length(crops)  # leave space for subgroup summaries
rows <- c()
for (crop in crops) {
  n <- sum(ES_data$crop == crop)
  if (n == 0) next
  row_pos[[crop]] <- current_row:(current_row - n + 1)
  rows <- c(rows, row_pos[[crop]])
  current_row <- min(row_pos[[crop]]) - 2  # leave space between groups
}

# Reorder ES_data to match plotting
ES_data$plot_order <- NA_integer_

for (crop in names(row_pos)) {
  crop_rows <- which(ES_data$crop == crop)
  ES_data$plot_order[crop_rows] <- rev(row_pos[[crop]])  # match correct order
}
ES_data <- ES_data[order(-ES_data$plot_order), ]


# Define CA judgement colors
CA_col <- recode(ES_data$CA_judgement,
                 "HIGH" = "darkred",
                 "MEDIUM" = "darkorange",
                 "LOW" = "darkgreen",
                 "UNCLEAR" = "darkgrey")

# Set up base forest plot
forest(model1,
       addfit = FALSE,
       cex = 0.45,
       xlab = "Effect size (kg/ha)",
       cex.lab = 0.6,
       header = "First author (Year)",
       slab = ES_data$short_citation,
       ilab = ES_data$crop,
       ilab.xpos = -10500,
       ylim = c(0, max(rows) + 5),
       xlim = c(-16000, 8000),
       rows = ES_data$plot_order)

# Add coloured CA_judgement labels
text(-8500, rows, ES_data$CA_judgement, col = CA_col, cex = 0.45, font = 2)
text(-10500, max(rows) + 2, 'Crop', cex = 0.45, font = 2)
text(-8500, max(rows) + 2, 'CA Judgement', cex = 0.45, font = 2)

# Add subgroup labels and summary polygons BELOW their studies
for (crop in crops) {
  if (!is.null(row_pos[[crop]])) {
    poly_row <- min(row_pos[[crop]]) - 0.5   # summary polygon just below
    label_row <- poly_row - 1                # label below the polygon
    
    # Add crop label
    text(-16000, label_row, pos = 4, crop, cex = 0.5, font = 2)
    
    # Add subgroup summary polygon
    if (!is.null(subgroup_models[[crop]])) {
      addpoly(subgroup_models[[crop]],
              row = poly_row,
              mlab = mlabfun("RE Model for Subgroup", subgroup_models[[crop]]),
              cex = 0.45,
              col = "darkgrey",
              border = "darkgrey")
    }
  }
}

## publication bias
#Nakagawa, S., Lagisz, M., Jennions, M. D., Koricheva, J., Noble, D. W. A., Parker, T. H., Sánchez-Tójar, A., Yang, Y., & O'Dea, R. E. (2022). Methods for testing publication bias in ecological and evolutionary meta-analyses. Methods in Ecology and Evolution, 13, 4–21. https://doi.org/10.1111/2041-210X.13724

#funnel(model1)
#regtest(rma(yi=ES,vi=PSD,data=ES_data,method="ML"))

#> publication bias can not be assessed using funnel plots for multi-level models so we need to look at the residuals against the standard error

# Extract residuals and standard errors
ES_data$resid <- resid(model1, type = "response")
ES_data$sei <- sqrt(ES_data$V)  # V is the sampling variance per effect size

# 3. Regress residuals on standard errors (Nakagawa-style bias test)
bias_test <- lm(resid ~ sei, data = ES_data)


# 4. Output summary
summary(bias_test)
## if sei is significant then there is small study bias

# Plot it
plot(ES_data$sei, ES_data$resid,
     xlab = "Standard Error", ylab = "Residuals",
     main = "Small-Study Effects Test (Nakagawa)")
abline(bias_test, col = "red", lwd = 2)
