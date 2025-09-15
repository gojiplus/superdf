# SuperDF: Data Frames with Persistent Metadata

SuperDF extends R's `data.frame` to include persistent metadata (version, author, notes) that survives data operations and I/O. Perfect for data lineage, documentation, and reproducible research.

## Installation

```r
# Install from GitHub
remotes::install_github("soodoku/superdf")

# Load the package
library(superdf)
```

## Quick Start

```r
# Create a SuperDataFrame with metadata
df <- SuperDataFrame(
  data.frame(
    name = c("Alice", "Bob", "Charlie"),
    age = c(25, 30, 35),
    city = c("New York", "London", "Tokyo")
  ),
  version = "1.0",
  author = "Data Team",
  notes = "Customer demographics dataset"
)

# View the data (shows metadata too)
df
#   name age     city
# 1 Alice  25 New York  
# 2 Bob    30   London
# 3 Charlie 35    Tokyo
# 
# Metadata:
#   Version: 1.0
#   Author:  Data Team  
#   Notes:   Customer demographics dataset

# Access metadata
version(df)  # "1.0"
author(df)   # "Data Team" 
notes(df)    # "Customer demographics dataset"
```

## Key Features

### ✅ **Fully Compatible with data.frame**
SuperDataFrames work exactly like regular data.frames:

```r
# All standard operations work
subset_df <- df[df$age > 27, ]
combined_df <- rbind(df, more_data)
summary(df)
model <- lm(age ~ city, data = df)
```

### ✅ **Metadata Persists Through Operations**
Your metadata follows your data:

```r
# Subsetting preserves metadata
older_customers <- df[df$age > 30, ]
version(older_customers)  # Still "1.0"

# Combining preserves metadata from first SuperDataFrame
df2 <- SuperDataFrame(more_data, version = "2.0")
combined <- rbind(df, df2)  
version(combined)  # "1.0" (from first dataframe)
```

### ✅ **Easy Metadata Management**

```r
# Update metadata (mutable)
version(df) <- "1.1"
author(df) <- "Updated Author"

# Or create new version (immutable) 
df_v2 <- update_metadata(df, 
                        version = "2.0",
                        notes = "Added validation checks")
```

### ✅ **Convert Existing data.frames**

```r
# Convert existing data.frame
existing_df <- data.frame(x = 1:10, y = rnorm(10))
super_df <- as_superdf(existing_df, 
                      version = "1.0", 
                      author = "Me",
                      notes = "Converted from legacy data")
```

## I/O Operations

### RDS (Recommended)
Metadata is automatically preserved with RDS:

```r
# Save and load (metadata preserved automatically)
saveRDS(df, "data.rds")
df_restored <- readRDS("data.rds")
version(df_restored)  # "1.0"
```

### CSV with Metadata
Special functions for CSV with metadata comments:

```r
# Write CSV with metadata in header comments
write_superdf_csv(df, "data.csv")

# Read back with metadata
df_restored <- read_superdf_csv("data.csv")
author(df_restored)  # "Data Team"
```

## Real-World Examples

### Data Pipeline with Lineage Tracking

```r
# Start with raw data
raw_data <- SuperDataFrame(
  read.csv("raw_sales.csv"),
  version = "0.1",
  author = "ETL Pipeline", 
  notes = "Raw sales data from API"
)

# Clean and transform
clean_data <- raw_data[!is.na(raw_data$amount), ]
version(clean_data) <- "0.2"
notes(clean_data) <- "Removed NA values"

# Final analysis dataset
analysis_data <- clean_data[clean_data$amount > 100, ]
analysis_data <- update_metadata(analysis_data,
  version = "1.0",
  notes = "Final analysis dataset: removed low-value transactions"
)

# The metadata tells the story of your data transformations
```

### Sharing Data with Context

```r
# Create well-documented dataset
survey_data <- SuperDataFrame(
  responses_df,
  version = "2.1",
  author = "Research Team <research@company.com>",
  notes = "Q4 2024 customer satisfaction survey (n=1,847). 
          Cleaned data with outliers removed. 
          See methodology.pdf for details."
)

# Share as RDS - metadata preserved
saveRDS(survey_data, "survey_q4_2024.rds")

# Recipient can immediately understand the data context
received_data <- readRDS("survey_q4_2024.rds")
summary(received_data)  # Shows data summary AND metadata
```

## Ecosystem Compatibility

SuperDataFrame works seamlessly with popular R packages:

### Base R
```r
# Statistical modeling
model <- lm(y ~ x, data = df)
aov_result <- aov(response ~ treatment, data = df)

# All base R functions work normally
apply(df[, numeric_cols], 2, mean)
aggregate(value ~ group, data = df, FUN = sum)
```

### Tidyverse (if installed)
```r
library(dplyr)

result <- df %>%
  filter(age > 25) %>%
  mutate(age_group = cut(age, breaks = 3)) %>%
  group_by(city) %>%
  summarise(avg_age = mean(age))

# Metadata is preserved through the pipeline
```

### ggplot2
```r
library(ggplot2)

# Works directly with ggplot2
ggplot(df, aes(x = city, y = age)) +
  geom_boxplot() +
  labs(caption = paste("Data version:", version(df)))
```

## Why SuperDataFrame?

### The Problem
In data analysis, we often lose track of:
- Where data came from
- Who created or modified it  
- What version we're working with
- What transformations were applied

### The Solution
SuperDataFrame keeps this crucial context attached to your data:

```r
# Six months later, you can still understand your data
mystery_data <- readRDS("old_analysis.rds")
summary(mystery_data)

# SuperDataFrame Summary
# =====================
# 
# Metadata:
#   Version: 2.1
#   Author:  Jane Smith <jane@company.com>
#   Notes:   Final cleaned dataset for Q3 analysis. 
#            Outliers removed using IQR method.
#            Original data: sales_raw_2024_q3.csv
# 
# Data Summary:
# [... regular data.frame summary ...]
```

## Best Practices

1. **Use meaningful versions**: `"1.0"`, `"2024-03-15"`, `"final_v2"`
2. **Include contact info in author**: `"Data Team <data@company.com>"`  
3. **Document transformations in notes**: `"Removed outliers using IQR method"`
4. **Update metadata when data changes**: Don't let it get stale
5. **Use RDS for sharing**: Preserves metadata automatically

## Contributing

Found a bug or have a feature request? Please [open an issue](https://github.com/soodoku/superdf/issues).

## License

MIT License - see [LICENSE](LICENSE) file for details.