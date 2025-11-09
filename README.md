# NHS-waiting-times-2022
## Project Overview

This report analyses NHS England Referral-to-Treatment (RTT) waiting times
(August 2022, provider level) and links them with the Adult Inpatient Survey 
2022 (trust-level satisfaction scores).

The aims for Version 1 are to:

build a clean dataset suitable for further modelling in later versions;

produce descriptive statistics and exploratory graphics for RTT; and

integrate patient satisfaction (overall mean score) to run a simple 
correlation/regression that motivates subsequent analyses.

This dataset was chosen because it connects my clinical pharmacy background 
with my current studies in Operational Research and Data Science, focusing on 
patient flow, waiting-time targets, and perceived care quality — topics directly
relevant to future roles in healthcare data analytics and operational 
optimisation.

## Data Sources and Preparation

The analysis draws on two publicly available datasets from NHS England:

RTT Waiting Times (August 2022) — Incomplete Pathways: Provider Level data.

Adult Inpatient Survey 2022 — Trust-level scores (sheet: Trust_Scores),
including the overall satisfaction measure meanS09.

To ensure consistency and reproducibility, both datasets were processed in R
(tidyverse framework) prior to integration.
The RTT dataset was filtered to include only total treatment pathways 
(“Total / All specialties”) for each NHS Trust. Key variables — provider code,
provider name, median waiting time (weeks), and percentage within 
18 weeks — were retained and converted to numeric form.

The Adult Inpatient Survey data were extracted from the Trust_Scores worksheet, 
keeping the provider code, trust name, and overall satisfaction score (meanS09).

Both datasets were then merged by provider code to produce a unified dataset 
named merged_NHS_RTT_Satisfaction_2022.csv, which forms the analytical basis 
for this version of the report. The cleaned RTT dataset (waiting_clean_2022.csv)
and the merged dataset are included in the submission and made publicly 
accessible via GitHub for reproducibility.

---
title: "Math6006-Coursework"
output: pdf_document
date: "2025-11-09"
---

# 1. Install & Load Packages 
### install.packages(c("tidyverse", "readxl", "readODS"))

```{r}
library(tidyverse)
library(readxl)
library(readODS)
library(ggplot2)
library(dplyr)
```


# 2. Load and Clean RTT Waiting Time Data (August 2022) 

```{r}
waiting_df <- read_excel(
  "D:/UOS-study&Career/UOS-Math6006/Incomplete-Provider-Aug22.xls",
  sheet = "Provider",
  skip = 13
)

glimpse(waiting_df)
```


### Select provider-level data ("Total" or "All specialties")

```{r}
waiting_clean <- waiting_df %>%
  filter(if_any(contains("Treatment"), ~ .x == "Total")) %>%
  select(
    Provider_Code = matches("Provider Code"),
    Provider_Name = matches("Provider Name"),
    Median_Wait_Weeks = matches("Median"),
    Percent_Within_18_Weeks = matches("% within")
  ) %>%
  mutate(
    Year = 2022,
    Median_Wait_Weeks = as.numeric(Median_Wait_Weeks),
    Percent_Within_18_Weeks = as.numeric(Percent_Within_18_Weeks) * 100
  )

write_csv(waiting_clean, "D:/UOS-study&Career/UOS-Math6006/waiting_clean_2022.csv")
glimpse(waiting_clean)
```

# 3. Descriptive Statistics & Visualisation for RTT Data 

### Summary statistics
```{r}
desc_stats <- waiting_clean %>%
  summarise(
    Mean_Wait = mean(Median_Wait_Weeks, na.rm = TRUE),
    Median_Wait = median(Median_Wait_Weeks, na.rm = TRUE),
    SD_Wait = sd(Median_Wait_Weeks, na.rm = TRUE),
    Min_Wait = min(Median_Wait_Weeks, na.rm = TRUE),
    Max_Wait = max(Median_Wait_Weeks, na.rm = TRUE),
    Mean_Percent_18w = mean(Percent_Within_18_Weeks, na.rm = TRUE),
    SD_Percent_18w = sd(Percent_Within_18_Weeks, na.rm = TRUE)
  )

print(desc_stats)
```


### Histogram

```{r}
ggplot(waiting_clean, aes(x = Median_Wait_Weeks)) +
  geom_histogram(bins = 25, fill = "steelblue", color = "white", alpha = 0.8) +
  geom_vline(aes(xintercept = mean(Median_Wait_Weeks, na.rm = TRUE)),
             color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Distribution of Median Waiting Times (NHS RTT Aug 2022)",
       subtitle = "Dashed line = national mean",
       x = "Median Waiting Time (weeks)", y = "Number of NHS Trusts") +
  theme_minimal(base_size = 13)
```

### QQ plot

```{r}
qqnorm(waiting_clean$Median_Wait_Weeks, main="QQ Plot of Median Waiting Time")
qqline(waiting_clean$Median_Wait_Weeks, col="red")

```

### Boxplot

```{r}
ggplot(waiting_clean, aes(y = Median_Wait_Weeks)) +
  geom_boxplot(fill = "#56B4E9", alpha = 0.7) +
  labs(title = "Boxplot of Median Waiting Times", y = "Median Waiting Time (weeks)") +
  theme_minimal()
```

### Scatterplot: Wait vs % within 18 weeks

```{r}
ggplot(waiting_clean, aes(x = Median_Wait_Weeks, y = Percent_Within_18_Weeks)) +
  geom_point(color = "#E69F00", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Relationship between Waiting Time and 18-Week Compliance",
       x = "Median Waiting Time (weeks)", y = "% Within 18 Weeks") +
  theme_minimal()
```

### Density plot

```{r}
ggplot(waiting_clean, aes(x = Median_Wait_Weeks)) +
  geom_density(fill = "#009E73", alpha = 0.6) +
  labs(title = "Density of NHS Waiting Times (2022)",
       x = "Median Waiting Time (weeks)", y = "Density") +
  theme_minimal()
```

### Top 10 longest wait
```{r}
top10 <- waiting_clean %>% arrange(desc(Median_Wait_Weeks)) %>% head(10)
ggplot(top10, aes(x = reorder(Provider_Name, Median_Wait_Weeks), y = Median_Wait_Weeks)) +
  geom_col(fill = "#D55E00") + coord_flip() +
  labs(title = "Top 10 NHS Trusts by Longest Median Waiting Time (2022)",
       x = "Provider Name", y = "Median Waiting Time (weeks)") +
  theme_minimal()
```

### Top 10 shortest wait
```{r}
bottom10 <- waiting_clean %>% arrange(Median_Wait_Weeks) %>% head(10)

ggplot(bottom10, aes(x = reorder(Provider_Name, -Median_Wait_Weeks), y = Median_Wait_Weeks)) +
  geom_col(fill = "#0072B2") + coord_flip() +
  labs(title = "Top 10 NHS Trusts by Shortest Median Waiting Time (2022)",
       x = "Provider Name", y = "Median Waiting Time (weeks)") +
  theme_minimal()

```

# 4. Load and Merge Satisfaction Data (Adult Inpatient 2022)
```{r}
satisfaction_df <- read_ods(
  "D:/UOS-study&Career/UOS-Math6006/Adult-Inpatient-Survey-2022.ods",
  sheet = "Trust_Scores"
)
```

### Select provider code + trust name + overall satisfaction (meanS09)

```{r}
satisfaction_clean <- satisfaction_df %>%
  select(
    Provider_Code = TrustCode,
    Provider_Name = trustname,
    Satisfaction_Score = meanS09
  )
```

### Merge with RTT data
```{r}
merged_df <- waiting_clean %>%
  left_join(satisfaction_clean, by = "Provider_Code")
```

### Save merged dataset
```{r}
write_csv(merged_df, "D:/UOS-study&Career/UOS-Math6006/merged_NHS_RTT_Satisfaction_2022.csv")
```

# 5. Correlation and Regression Analysis 

### Correlation
```{r}
cor.test(merged_df$Median_Wait_Weeks, merged_df$Satisfaction_Score, use = "complete.obs")
```

### Scatter + regression line
```{r}
ggplot(merged_df, aes(x = Median_Wait_Weeks, y = Satisfaction_Score)) +
  geom_point(color = "steelblue", alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  labs(title = "Relationship between Waiting Time and Patient Satisfaction (2022)",
       x = "Median Waiting Time (weeks)", y = "Satisfaction Score (0–10)") +
  theme_minimal()
```

### Linear model

```{r}
model <- lm(Satisfaction_Score ~ Median_Wait_Weeks, data = merged_df)
summary(model)
```

# 6. Optional: Validation 

### Check merged data
```{r}
glimpse(merged_df)
sum(is.na(merged_df$Satisfaction_Score))
```

### Reload verification

```{r}
check_df <- read_csv("D:/UOS-study&Career/UOS-Math6006/merged_NHS_RTT_Satisfaction_2022.csv")
glimpse(check_df)
```
### End of Script
