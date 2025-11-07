#### ==============================
## 1) Setup and Data Import
## ==============================

# This section of code spans lines 30-275
# The file path modification is on line 34
# Users should replace: df <- read.csv("C:\\Users\\cz074\\Desktop\\bus_original.csv")
# with your own corresponding file path

## ==============================
## 2) Data_Description  
## ==============================
# This section of code spans lines 282-714
# The file path modification is on line 293  
# Users should replace: df <- read.csv("C:\\Users\\cz074\\Desktop\\bus_original.csv")
# with your own corresponding file path

## ==============================
## 3) Model_Analysis
## ==============================
# This section of code spans lines 723-1294
# The file path modification is on line 736,833,894,1063,1088,1221#  
# Users should replace: df <- read.csv("C:\\Users\\cz074\\Desktop\\bus_new.xlsx")
# with your own corresponding file path


## ==============================
## 1) Setup and Data Import
## ==============================
library(dplyr)
library(stringr)

# Read raw bus_original dataset
df <- read.csv("C:\\Users\\cz074\\Desktop\\bus_original.csv")


## ==============================
## 1) Drop unnecessary columns & handle missing values
## ==============================
# Remove columns not needed for cleaning
df <- df %>%
  select(-Created_On, -Incident_Number, -Number_Of_Students_On_The_Bus, -Last_Updated_On)

# Convert empty strings "" to NA and remove rows with NA (first pass)
df[df == ""] <- NA
df <- na.omit(df)

# Repeat to ensure consistency (second pass)
df[df == ""] <- NA
df_clean <- na.omit(df)

## ==============================
## 2) Preprocess How_Long_Delayed
## ==============================
# Standardize: convert all values to lowercase
df_clean$How_Long_Delayed <- tolower(df_clean$How_Long_Delayed)

# Keep only rows containing time-related markers (h, m, -, /)
df_clean <- df_clean[grepl("h|m|-|/", df_clean$How_Long_Delayed), ]

# Initialize numeric output column for parsed delays (in minutes)
df_clean$Delay_clean <- NA_real_

## 2.1) Minutes only (contains "m", but not h, -, /, or \)
mask_m_only <- grepl("m", df_clean$How_Long_Delayed) &
  !grepl("h|-|/|\\\\", df_clean$How_Long_Delayed)

df_clean$Delay_clean[mask_m_only] <- as.numeric(
  str_extract(df_clean$How_Long_Delayed[mask_m_only], "\\d+")
)

## 2.2) Minute ranges with "-" (e.g., "10-15m"; no :, /, h)
mask_m_dash <- grepl("m", df_clean$How_Long_Delayed) &
  grepl("-", df_clean$How_Long_Delayed) &
  !grepl(":|/|h", df_clean$How_Long_Delayed)

nums_m_dash <- regmatches(df_clean$How_Long_Delayed[mask_m_dash],
                          gregexpr("\\d+", df_clean$How_Long_Delayed[mask_m_dash]))

num1_m_dash <- as.numeric(sapply(nums_m_dash, `[`, 1))
num2_m_dash <- as.numeric(sapply(nums_m_dash, `[`, 2))

# Fix invalid ranges (if first >= second, strip the first digit of num1)
fix_idx_m_dash <- which(num1_m_dash >= num2_m_dash & !is.na(num1_m_dash) & !is.na(num2_m_dash))
if (length(fix_idx_m_dash) > 0) {
  num1_m_dash[fix_idx_m_dash] <- as.numeric(sub("^\\d", "", sapply(nums_m_dash[fix_idx_m_dash], `[`, 1)))
}

avg_m_dash <- rowMeans(cbind(num1_m_dash, num2_m_dash), na.rm = TRUE)
df_clean$Delay_clean[mask_m_dash] <- avg_m_dash

## 2.3) Minute “fractions” with "/" (e.g., "10/20m")
mask_m_slash <- grepl("m", df_clean$How_Long_Delayed) &
  grepl("/", df_clean$How_Long_Delayed)

nums_m_slash <- regmatches(df_clean$How_Long_Delayed[mask_m_slash],
                           gregexpr("\\d+", df_clean$How_Long_Delayed[mask_m_slash]))

num1_m_slash <- as.numeric(sapply(nums_m_slash, `[`, 1))
num2_m_slash <- as.numeric(sapply(nums_m_slash, `[`, 2))

# Fix invalid pairs (first >= second → drop the first digit of the first)
fix_idx_m_slash <- which(num1_m_slash >= num2_m_slash & !is.na(num1_m_slash) & !is.na(num2_m_slash))
if (length(fix_idx_m_slash) > 0) {
  num1_fixed <- sub("^\\d", "", sapply(nums_m_slash[fix_idx_m_slash], `[`, 1))
  num1_m_slash[fix_idx_m_slash] <- as.numeric(num1_fixed)
}

avg_m_slash <- rowMeans(cbind(num1_m_slash, num2_m_slash), na.rm = TRUE)
df_clean$Delay_clean[mask_m_slash] <- avg_m_slash

## 2.4) Pure numeric ranges (e.g., "10-15")
mask_num_dash_only <- grepl("^[0-9]+-[0-9]+$", df_clean$How_Long_Delayed)

nums_num_dash <- regmatches(df_clean$How_Long_Delayed[mask_num_dash_only],
                            gregexpr("\\d+", df_clean$How_Long_Delayed[mask_num_dash_only]))

num1_num_dash <- as.numeric(sapply(nums_num_dash, `[`, 1))
num2_num_dash <- as.numeric(sapply(nums_num_dash, `[`, 2))

avg_num_dash <- rowMeans(cbind(num1_num_dash, num2_num_dash), na.rm = TRUE)
df_clean$Delay_clean[mask_num_dash_only] <- avg_num_dash

## 2.5) Hours only (e.g., "2h"; contains digits + h, no -, /, :, m)
mask_h_only <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  !grepl("-|/|:|m", df_clean$How_Long_Delayed)

num_hours_only <- as.numeric(regmatches(df_clean$How_Long_Delayed[mask_h_only],
                                        regexpr("\\d+", df_clean$How_Long_Delayed[mask_h_only])))

df_clean$Delay_clean[mask_h_only] <- num_hours_only * 60

## 2.6) Fractional hours with "/" (e.g., "1/2h"; no -, :, m)
mask_h_slash <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  grepl("/", df_clean$How_Long_Delayed) &
  !grepl("-|:|m", df_clean$How_Long_Delayed)

nums_h_slash <- regmatches(df_clean$How_Long_Delayed[mask_h_slash],
                           gregexpr("\\d+", df_clean$How_Long_Delayed[mask_h_slash]))

num1_h_slash <- as.numeric(sapply(nums_h_slash, `[`, 1))
num2_h_slash <- as.numeric(sapply(nums_h_slash, `[`, 2))

avg_hours_h_slash <- rowMeans(cbind(num1_h_slash, num2_h_slash), na.rm = TRUE)
df_clean$Delay_clean[mask_h_slash] <- avg_hours_h_slash * 60

## 2.7) Colon time format (e.g., "1:30h"; hh:mmh; no -, /, m)
mask_h_colon <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  grepl(":", df_clean$How_Long_Delayed) &
  !grepl("-|/|m", df_clean$How_Long_Delayed)

matches_h_colon <- regmatches(df_clean$How_Long_Delayed[mask_h_colon],
                              gregexpr("\\d+", df_clean$How_Long_Delayed[mask_h_colon]))

hours_h_colon <- as.numeric(sapply(matches_h_colon, `[`, 1))
mins_h_colon  <- as.numeric(sapply(matches_h_colon, `[`, 2))
mins_h_colon[is.na(mins_h_colon)] <- 0

df_clean$Delay_clean[mask_h_colon] <- hours_h_colon * 60 + mins_h_colon

## 2.8) Combined hours + minutes (e.g., "1h30m"; no -, /, :)
mask_hm_combo <- grepl("\\d", df_clean$How_Long_Delayed) &
  grepl("h", df_clean$How_Long_Delayed) &
  grepl("m", df_clean$How_Long_Delayed) &
  !grepl("-|/|:|：", df_clean$How_Long_Delayed)

x_hm <- df_clean$How_Long_Delayed[mask_hm_combo]
hours_hm <- as.numeric(sub(".*?(\\d+)\\s*h.*", "\\1", x_hm))
mins_hm  <- as.numeric(sub(".*?(\\d+)\\s*m.*", "\\1", x_hm))
hours_hm[is.na(hours_hm)] <- 0
mins_hm[is.na(mins_hm)]   <- 0

df_clean$Delay_clean[mask_hm_combo] <- hours_hm * 60 + mins_hm

## ==============================
## 3) Export intermediate cleaned data
## ==============================
# Rows where Delay_clean is still NA (for later review)
df_na <- df_clean[is.na(df_clean$Delay_clean), ]

# Export cleaned dataset after delay parsing (to be used by later steps)
write.csv(df_clean, "BusDelay_clean12.csv", row.names = FALSE)


## ==============================
## 4) Reload intermediate file for further cleaning steps
## ==============================
df_clean <- read.csv("BusDelay_clean12.csv", stringsAsFactors = FALSE)

# Remove remaining rows with NA
df_clean <- na.omit(df_clean)

# Keep a copy of rows where Delay_clean is NA (inspection/debugging)
df_na <- df_clean[is.na(df_clean$Delay_clean), ]

## ==============================
## 5) Column adjustments (structure-level cleaning)
## ==============================
# Drop How_Long_Delayed (now redundant after parsing)
df_clean <- df_clean %>%
  select(-How_Long_Delayed)

# Normalize School_Year to the first 4 digits (e.g., "2015-2016" → "2015")
df_clean$School_Year <- sub("^(\\d{4}).*", "\\1", df_clean$School_Year)

## ==============================
## 6) Clean Bus_No (keep pure integers only)
## ==============================
df_clean <- df_clean[grepl("^\\d+$", df_clean$Bus_No), ]

## ==============================
## 7) Clean Route_Number (valid alphanumerics only, not "0")
## ==============================
# Exclude Route_Number == "0"
df_clean <- df_clean[df_clean$Route_Number != "0", ]

# Exclude non-alphanumeric Route_Number values
df_clean <- df_clean[grepl("^[A-Za-z0-9]+$", df_clean$Route_Number), ]

## ==============================
## 8) Normalize Bus Company Names (standardization for downstream grouping)
## ==============================
# Copy original to a working column
df_clean$Bus_company_name2 <- df_clean$Bus_Company_Name

# Uppercase normalization
df_clean$Bus_company_name2 <- toupper(df_clean$Bus_company_name2)

# Use first 5 characters as a prefix key and map to the first seen canonical name
prefix <- substr(df_clean$Bus_company_name2, 1, 5)
mapping <- tapply(df_clean$Bus_company_name2, prefix, function(x) x[1])
df_clean$Bus_company_name2 <- mapping[prefix]

## ==============================
## 9) Export fully cleaned dataset (hand-off to next stages)
## ==============================
write.csv(df_clean, "BusDelay_clean19.csv", row.names = FALSE)


# Visualisation
library(ggplot2)
library(data.table)
# Setting colour scheme
# Colours for unordered categorical variables
cust_colours <- c("#069CC6", "#07AEDA", "#07BCED", "#12C6F8", "#25CBF8", "#39CFF9", "#4DD4F9", "#61D9FA", "#74DEFB")

# Colours for 3 to 4 ordered categorical variables
cust_colours2 <- c("#74DEFB", "#4DD4F9", "#25CBF8", "#07BCED")

# Colours for 2 ordered categorical variables
cust_colours3 <- c("#61D9FA", "#12C6F8")

df2 <- fread("BusDelay_clean19.csv")
df3 <- df2[Delay_clean < 180]

# Graphs
# 2a Delays ADD MEAN, OUTLIERS, ETC.
ggplot(data=df3, aes(x = "", y = Delay_clean)) +
  geom_boxplot(size = 0.8, width = 0.9, fill=cust_colours[9]) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
      axis.text.x = element_text(
      hjust = 0.5,
      size =30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "School Bus Delays")



## ==============================
## 2) Data_Discription
## ==============================
library(dplyr)   
library(lubridate)  
library(ggplot2)
library(tidyr)
library(tidyverse)


#———————————————————————————Data Processing——————————————————————————————————---

install.packages("readxl")
library(readxl)
df <- read.csv("C:\\Users\\cz074\\Desktop\\bus_new.xlsx")

df <- bus_new
df <- df %>%
  mutate(
    Occurred_On = ymd_hms(Occurred_On),  
    Informed_On = ymd_hms(Informed_On)  
  )

# calculate time gap (minutes)
df <- df %>%
  mutate(
    Time_Gap = as.numeric(difftime(Informed_On, Occurred_On, units = "mins"))
  )

# count how many Time_Gap values are negative
num_less_than_zero <- sum(df$Time_Gap < 0, na.rm = TRUE)

# print the count
print(num_less_than_zero)

# remove rows with negative Time_Gap
df <- df[!(df$Time_Gap < 0), ]

# inspect missing values per column
print(colSums(is.na(df)))


#————————————————————————————————Data Summary————————————————————————————————---

# descriptive statistics for numeric variables
df %>%
  select(Delay_clean, Time_Gap) %>%  # select numeric columns
  summary()

## Delay_clean visualization
q <- quantile(df$Delay_clean,na.rm = TRUE)
df_filtered <- df %>% filter(Delay_clean <= q)

# violin plot for Delay_clean (fix x-axis mapping)
ggplot(df_filtered, aes(x = factor(1), y = Delay_clean)) +  # add x=factor(1) as placeholder to avoid error
  geom_violin(fill = "#4285F4", alpha = 0.7, linewidth = 1) +
  geom_boxplot(width = 0.1, fill = "white", linewidth = 0.8) +
  labs(
    title = "Distribution of Delay_clean",
    y = "Delay_clean Value",
    x = ""  # hide x-axis label (placeholder)
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_blank(),  # hide x-axis tick text
    axis.ticks.x = element_blank()  # hide x-axis tick marks
  )

## Time_Gap visualization
# compute 99th percentile of Delay_clean to filter extremes
q99 <- quantile(df$Time_Gap, 0.99, na.rm = TRUE)
df_filtered <- df %>% filter(Time_Gap <= q99)

# violin plot for Time_Gap (same x-axis placeholder)
ggplot(df_filtered, aes(x = factor(1), y = Time_Gap)) +  # add x-axis placeholder
  geom_violin(fill = "#34A853", alpha = 0.7, linewidth = 1) +
  geom_boxplot(width = 0.1, fill = "white", linewidth = 0.8) +
  labs(
    title = "Distribution of Time_Gap",
    y = "Time_Gap Value",
    x = ""  # hide x-axis label
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_blank(),  # hide x-axis tick text
    axis.ticks.x = element_blank(),  # hide x-axis tick marks
    axis.text.y = element_text(size = 12),
    panel.grid.major.y = element_line(color = "gray90", size = 0.5),
    panel.grid.minor.y = element_blank()
  ) +
  scale_y_continuous(
    limits = c(0, max(df_filtered$Time_Gap, na.rm = TRUE)),  # set y-axis range
    breaks = seq(0, max(df_filtered$Time_Gap, na.rm = TRUE), by = 2)  # custom y-axis breaks
  )


#—————————————————————————————frequency table for categorical variables—————————————————————————
df %>%
  select(Run_Type, Reason, Boro, Bus_company_name) %>%
  lapply(table)  # compute frequency for each categorical column

# define a function to plot frequency heatmap for a single categorical variable
plot_single_heatmap <- function(data, var_name) {
  # compute frequency
  freq_df <- data %>%
    select(all_of(var_name)) %>%
    count(!!sym(var_name)) %>%  # count frequency per category
    rename(Category = !!sym(var_name), Frequency = n) %>%
    # add dummy column for x-axis (2D heatmap requirement)
    mutate(Dummy = "Frequency")
  
  # plot heatmap
  ggplot(freq_df, aes(x = Dummy, y = Category, fill = Frequency)) +
    geom_tile(color = "white", linewidth = 0.5) +  # tiles
    geom_text(aes(label = Frequency), size = 4, color = "black") +  # show counts
    scale_fill_gradient(low = "#f1eef6", high = "#980043") +  # color gradient
    labs(
      title = paste("Frequency Heatmap of", var_name),
      x = "", y = var_name, fill = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = element_text(size = 11),
      axis.text.x = element_blank(),  # hide dummy x ticks
      axis.ticks.x = element_blank()
    )
}

# plot single-variable heatmaps
vars <- c("Reason", "Boro", "Bus_company_name")
lapply(vars, function(var) {
  print(plot_single_heatmap(df, var))  # display heatmaps one by one
})

# define function to plot cross-frequency heatmap for two categorical variables
plot_pair_heatmap <- function(data, var1, var2) {
  # compute cross frequency
  cross_freq <- data %>%
    select(all_of(c(var1, var2))) %>%
    count(!!sym(var1), !!sym(var2)) %>%  # cross counts
    rename(Var1 = !!sym(var1), Var2 = !!sym(var2), Frequency = n)
  
  # plot heatmap
  ggplot(cross_freq, aes(x = Var2, y = Var1, fill = Frequency)) +
    geom_tile(color = "white", linewidth = 0.5) +
    geom_text(aes(label = Frequency), size = 2, color = "black") +
    scale_fill_gradientn(colors = c("#ffffcc", "#c2e699", "#78c679", "#238443")) +
    labs(
      title = paste("Heatmap:", var1, "vs", var2),
      x = var2, y = var1, fill = "Frequency"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
      axis.text.y = element_text(size = 10)
    )
}

# define variables for pairwise combinations (all combinations here)
vars <- c("Run_Type", "Reason", "Boro","Bus_company_name")
pairs <- expand.grid(var1 = vars, var2 = vars, stringsAsFactors = FALSE) %>%
  filter(var1 < var2)  # avoid duplicates (e.g. A-B and B-A)

# plot for each pair
lapply(1:nrow(pairs), function(i) {
  print(plot_pair_heatmap(df, pairs$var1[i], pairs$var2[i]))
})


#————————————————————————————Delay_clean_median View————————————————————————————

# group by Boro and Bus_company_name, compute statistics
all_boro_delay_stats <- df %>%
  # exclude records with NA in Boro or company name (optional)
  filter(!is.na(Boro), !is.na(Bus_company_name)) %>%
  
  #------------Calculate the Operational Error Proportion and Visualization----------

# assume the dataset is df, containing columns Bus_company_name and Reason
result <- df %>%
  # group by bus company
  group_by(Bus_company_name) %>%
  # calculate total records per company (all reasons)
  mutate(total_reasons = n()) %>%
  # regroup to ensure total is valid within group
  group_by(Bus_company_name, total_reasons) %>%
  # sum counts of specific reasons
  summarise(
    target_reasons_count = sum(
      Reason %in% c(
        "Flat Tire", 
        "Late return from Field Trip", 
        "Mechanical Problem", 
        "Problem Run",  # note: original typo kept; edit here if needed
        "Won't Start"
      ),
      na.rm = TRUE  # ignore NA
    ),
    .groups = "drop"  # drop grouping
  ) %>%
  # calculate proportion (2 decimals)
  mutate(
    proportion = round(target_reasons_count / total_reasons, 2),
    proportion_percent = paste0(proportion * 100, "%")  # convert to percent
  ) %>%
  # reorder columns (optional)
  select(Bus_company_name, total_reasons, target_reasons_count, proportion, proportion_percent)

# view result
print(result)

# export to CSV (default to working directory)
write.csv(
  result, 
  file = "bus_company_reason_proportion.csv",  # filename (customizable)
  row.names = FALSE  # exclude row names
)

#————————————————————————————Visualization——————————————————————————————————————
# proportion visualization
result %>%
  arrange(proportion) %>%
  mutate(Bus_company_name = fct_inorder(Bus_company_name)) %>%
  ggplot(aes(x = proportion, y = Bus_company_name)) +
  geom_col(aes(fill = proportion)) +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            hjust = -0.2, size = 3.5, color = "black") +
  scale_fill_gradient(low = "green", high = "red", name = "Proportion") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(title = "Bus Companies Ranked by Operation Error Index",
       x = "Proportion", y = NULL) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),  # center and bold
    panel.grid.major.y = element_blank()
  )


# bubble chart
ggplot(result,
       aes(x = total_reasons,
           y = proportion,
           size = target_reasons_count,
           colour = proportion)) +
  geom_point(alpha = .7) +
  scale_size_area(max_size = 15) +
  scale_colour_gradient(low = "orange", high = "darkred") +
  labs(title = "Total vs Operation Error Index vs Count of Target Reasons",
       x = "Total",
       y = "Operation Error Index",
       size = "Target Count",
       colour = "Operation Error Index") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)   # center title
  )


ggplot(top10,
       aes(x = reorder(Bus_company_name, proportion))) +
  # bars: morandi blue gradient
  geom_col(aes(y = target_reasons_count, fill = target_reasons_count)) +
  scale_fill_gradient("Count", low = "#a8dadc", high = "#1d3557") +
  
  # line / points: coral orange
  geom_line(data = line_dat,
            aes(y = proportion * 2000, group = 1, colour = "Operation Error Index"),
            linewidth = 1.2) +
  geom_point(data = line_dat,
             aes(y = proportion * 2000, colour = "Operation Error Index"), size = 3) +
  scale_colour_manual(values = c("Operation Error Index" = "#e63946")) +
  
  scale_y_continuous(name = "Target Reasons Count",
                     sec.axis = sec_axis(~ . / 2000, name = "Operation Error Index")) +
  labs(title = "Top 10 Companies: Count vs Operation Error Index",
       x = NULL, colour = NULL) +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.text = element_text(size = 9))


#——————————————————————————Route_Number—————————————————————————————————————————

route_counts <- df_filtered %>%
  count(Route_Number, sort = TRUE)   # descending order

route_counts %>%
  slice_head(n = 10) %>%                # top 10
  ggplot(aes(x = reorder(Route_Number, n), y = n)) +
  geom_col(aes(fill = n), width = 0.7) +
  scale_fill_gradient("Count", low = "#a8dadc", high = "#1d3557") +
  labs(title = "Top 10 Route Frequency",
       x = NULL, y = "Frequency") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1))

## Visualisation

# Visualisation
library(ggplot2)
library(data.table)
# Setting colour scheme
cust_colours <- c("#069CC6", "#07AEDA", "#07BCED", "#12C6F8", "#25CBF8", "#39CFF9", "#4DD4F9", "#61D9FA", "#74DEFB", "#88E2FB", "#B0ECFC", "#C4F1FD", "#EBFAFE")

# Graphs
# 1a School Bus Ridership
non_dis <- 150000 - 66000
dis <- 66000

rs_df <- data.frame(
  Category = factor(c("Dis", "Non-Dis"), levels = c("Dis", "Non-Dis")),
  Count = c(dis, non_dis)
)

# Convert to percentages
rs_df$Percent <- rs_df$Count / sum(rs_df$Count) * 100
rs_df


ggplot(rs_df, aes(x = 1, y = Count, fill = Category)) +
  geom_col(width = 0.3, position = "stack") +
  coord_flip() +  # make it horizontal
  geom_text(aes(label = paste0(rs_df$Percent, "%")),
            position = position_stack(vjust = 0.5),
            color = "black",
            size = 15,
            fontface = "bold") +
  scale_fill_manual(values = c(cust_colours[4], cust_colours[11])) +
  labs(
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  )

# 2a Delays ADD MEAN, OUTLIERS, ETC.
ggplot(data=df_filtered, aes(x = "", y = Delay_clean)) +
  geom_boxplot(size = 0.8, width = 0.9, fill=cust_colours[10]) +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none",
    axis.text.x = element_text(
      hjust = 0.5,
      size =30,
      face = "bold")
  )+
  scale_y_continuous(labels = NULL) +
  labs(x = NULL, y = NULL, title = "School Bus Delays")

min(df_filtered$Delay_clean)
quantile(df_filtered$Delay_clean, probs = 0.25)
mean(df_filtered$Delay_clean)
median(df_filtered$Delay_clean)
quantile(df_filtered$Delay_clean, probs = 0.75)
max(df_filtered$Delay_clean)




# 2b Delay Causes
cause_table <- df_filtered %>%
  group_by(Reason) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Count) %>%
  mutate(Reason = factor(Reason, levels = Reason))

ggplot(cause_table, aes(x = Count, y = forcats::fct_reorder(Reason, Count), fill = Count)) +
  geom_col(color = "White", size = 1) +
  scale_fill_gradient(low = cust_colours[11], high = cust_colours[1]) +
  geom_text(aes(label = Reason),
            hjust = 1,
            x = -6000,
            size = 10,
            fontface = "bold") +
  geom_text(aes(label = Count, x = Count),
            hjust = -0.1,
            size = 8) +
  scale_x_continuous(expand = expansion(add = c(90000, 30000))) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Frequency of Delay Causes")


# 2c Operation Error Index
result %>%
  filter(proportion > 0) %>%
  arrange(proportion) %>%
  mutate(Bus_company_name = fct_inorder(Bus_company_name)) %>%
  ggplot(aes(x = proportion, y = Bus_company_name)) +
  geom_col(aes(fill = proportion)) +
  geom_text(aes(label = Bus_company_name, x=-0.01),
            hjust = 1, size = 4.5, color = "black") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 0.1)),
            hjust = -0.2, size = 4, color = "black") +
  scale_fill_gradient(low = cust_colours[12], high = cust_colours[1])  +
  scale_x_continuous(expand = expansion(mult = c(0.5, 0.1))) +
  labs(title = "Bus Companies Ranked by Operation Error Index",
       x = "Proportion", y = NULL) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(x = NULL, y = NULL, title = "Operators by Operation Error Index")


# 2d Other factors



## ==============================
## 3) Model_Analysis
## ==============================
#1.1
#------------CART--------------#
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
library(reticulate)

required_py_packages <- c("pandas", "openpyxl", "scikit-learn", "numpy")
for (pkg in required_py_packages) if (!py_module_available(pkg)) py_install(pkg)

invisible(py_run_string(r"(
import pandas as pd
from sklearn.tree import DecisionTreeRegressor
from sklearn.preprocessing import LabelEncoder
import numpy as np
from sklearn.metrics import mean_absolute_error

file_path = r'C:\\Users\\cz074\\Desktop\\bus_new.xlsx'
df = pd.read_excel(file_path, sheet_name='Bus_new', engine='openpyxl')
required_columns = ['Route_Number', 'Day', 'Hours', 'Boro', 'Bus_company_name', 'Delay_clean', 'Reason']
missing_cols = [col for col in required_columns if col not in df.columns]
if missing_cols: exit()
natural_factors = ['Weather Conditions', 'Heavy Traffic', 'Natural Disaster']
df['Natural_Factor'] = df['Reason'].apply(lambda x: 1 if str(x) in natural_factors else 0)
for col in ['Route_Number', 'Day', 'Boro', 'Bus_company_name']:
    le = LabelEncoder()
    df[col] = le.fit_transform(df[col].astype(str))
features = ['Route_Number', 'Day', 'Hours', 'Boro', 'Bus_company_name', 'Natural_Factor']
X = df[features]
y = df['Delay_clean']
model = DecisionTreeRegressor(max_depth=6, min_samples_split=12, random_state=42)
model.fit(X, y)
importance = model.feature_importances_ * 100
result = pd.DataFrame({'Feature': features, 'Importance': np.round(importance, 2)})
result = result.sort_values('Importance', ascending=False)
result['Significance'] = ''
result.iloc[0, 2] = '**'
result.iloc[1, 2] = '*'
y_pred = model.predict(X)
mae = mean_absolute_error(y, y_pred)
natural_cases = df['Natural_Factor'].sum()
total_cases = len(df)
percentage = round(df['Natural_Factor'].mean() * 100, 1)
reason_counts = df['Reason'].value_counts()
bus_results = {
    'feature_importance': result,
    'mae': mae,
    'natural_distribution': {'cases': natural_cases, 'total': total_cases, 'percentage': percentage},
    'reason_counts': reason_counts
}
)"))

results <- py$bus_results

output_lines <- c(
  "Feature Importance Ranking (Scale 0-100):",
  "=================================================="
)

max_feature_length <- max(nchar(ifelse(results$feature_importance$Feature == "Natural_Factor", 
                                       "Reason (Natural factors)", 
                                       as.character(results$feature_importance$Feature))))

for(i in 1:nrow(results$feature_importance)) {
  feature <- as.character(results$feature_importance$Feature[i])
  display_name <- ifelse(feature == "Natural_Factor", "Reason (Natural factors)", feature)
  spaces_needed <- max_feature_length - nchar(display_name) + 2
  output_lines <- c(output_lines,
                    sprintf("%s%s%.2f%s", 
                            display_name,
                            paste(rep(" ", spaces_needed), collapse = ""),
                            results$feature_importance$Importance[i],
                            results$feature_importance$Significance[i]))
}

output_lines <- c(output_lines,
                  "==================================================",
                  "",
                  sprintf("Model Mean Absolute Error (MAE): %.2f minutes", results$mae),
                  "",
                  "Natural factors distribution:",
                  sprintf("Natural factor cases: %d/%d (%.1f%%)", 
                          results$natural_distribution$cases,
                          results$natural_distribution$total,
                          results$natural_distribution$percentage),
                  "",
                  "Original reason categories:",
                  "Reason"
)

reason_df <- as.data.frame(results$reason_counts)
max_reason_length <- max(nchar(rownames(reason_df)))
reason_lines <- sapply(rownames(reason_df), function(reason) {
  spaces_needed <- max_reason_length - nchar(reason) + 2
  sprintf("%s%s%d", reason, paste(rep(" ", spaces_needed), collapse = ""), reason_df[reason, 1])
})

writeLines(c(output_lines, reason_lines))

#2.1
#------------RandomForestRegressor--------------#
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
library(reticulate)

required_py_packages <- c("pandas", "openpyxl", "scikit-learn")
for (pkg in required_py_packages) if (!py_module_available(pkg)) py_install(pkg)

invisible(py_run_string(r"(
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import LabelEncoder
import numpy as np

file_path = r'C:\\Users\\cz074\\Desktop\\bus_new.xlsx'
df = pd.read_excel(file_path, sheet_name='Bus_new', engine='openpyxl')
df = df[['Route_Number', 'Boro', 'Delay_clean', 'Bus_company_name', 'Day', 'Hours']]
df['Day'] = df['Day'].map({
    'Monday':0, 'Tuesday':1, 'Wednesday':2, 'Thursday':3,
    'Friday':4, 'Saturday':5, 'Sunday':6
}).fillna(-1)
for col in ['Route_Number', 'Boro', 'Bus_company_name']:
    df[col] = LabelEncoder().fit_transform(df[col].astype(str))
df['Is_weekend'] = df['Day'].isin([5,6]).astype(int)
df = df.dropna()
X = df[['Route_Number', 'Boro', 'Bus_company_name', 'Day', 'Hours', 'Is_weekend']]
y = df['Delay_clean']
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)
model = RandomForestRegressor(n_estimators=200, max_depth=12, min_samples_leaf=5, random_state=42, n_jobs=-1).fit(X_train, y_train)
feature_importance = pd.DataFrame({
    'Feature': X.columns,
    'Importance': model.feature_importances_
}).sort_values('Importance', ascending=False)
feature_importance['Significance'] = ''
feature_importance.iloc[0, 2] = '**'
feature_importance.iloc[1, 2] = '*'
bus_data_results = {
    'split_records': {'train':len(X_train), 'test':len(X_test)},
    'feature_importance': feature_importance,
    'model_performance': {
        'train_r2': model.score(X_train, y_train),
        'test_r2': model.score(X_test, y_test)
    }
}
)"))

results <- py$bus_data_results

writeLines(c(
  "Data split results:",
  sprintf("Training set: %d records", results$split_records$train),
  sprintf("Test set: %d records", results$split_records$test),
  "",
  "=== Feature Importance with Time Features ===",
  paste(rep("=", 50), collapse = ""),
  sprintf("%-20s %.4f%s", results$feature_importance$Feature, results$feature_importance$Importance, results$feature_importance$Significance),
  paste(rep("=", 50), collapse = ""),
  "",
  "=== Model Performance ===",
  sprintf("Training R²: %.4f", results$model_performance$train_r2),
  sprintf("Test R²: %.4f", results$model_performance$test_r2)
))

#------------Companies with weak operational management--------------#
if (!requireNamespace("reticulate", quietly = TRUE)) install.packages("reticulate")
library(reticulate)

required_py_packages <- c("pandas", "openpyxl")
for (pkg in required_py_packages) {
  if (!py_module_available(pkg)) py_install(pkg)
}

invisible(py_run_string('
import pandas as pd

file_path = r"C:\\Users\\cz074\\Desktop\\bus_new.xlsx"

df = pd.read_excel(file_path, engine="openpyxl")

operational_issues = [
    "Flat Tire",
    "Late return from Field Trip",
    "Mechanical Problem",
    "Problem Run",
    "Won`t Start"
]

operational_df = df[df["Reason"].isin(operational_issues)]

issue_counts = operational_df.groupby(["Bus_company_name", "Reason"]).size().unstack().fillna(0)

issue_counts["Total_Operational_Issues"] = issue_counts.sum(axis=1)

total_delays = df["Bus_company_name"].value_counts()

issue_counts["Operational_Issue_Percentage"] = (issue_counts["Total_Operational_Issues"] / total_delays) * 100

issue_counts = issue_counts[issue_counts["Total_Operational_Issues"] >= 100]

top_companies = issue_counts.sort_values("Operational_Issue_Percentage", ascending=False).head(6)

def get_top_issues(row):
    issues = row[operational_issues]
    top2 = issues.nlargest(2)
    return pd.Series({
        "Top_Issue_1": top2.index[0] if len(top2) > 0 else "",
        "Top_Issue_1_Count": top2.values[0] if len(top2) > 0 else 0,
        "Top_Issue_2": top2.index[1] if len(top2) > 1 else "",
        "Top_Issue_2_Count": top2.values[1] if len(top2) > 1 else 0
    })

top_issues = top_companies.apply(get_top_issues, axis=1)

result = pd.concat([
    top_issues,
    top_companies[["Total_Operational_Issues", "Operational_Issue_Percentage"]]
], axis=1)

result = result.reset_index().rename(columns={"Bus_company_name": "Company_Name"})

result = result[[
    "Company_Name",
    "Top_Issue_1", "Top_Issue_1_Count",
    "Top_Issue_2", "Top_Issue_2_Count",
    "Total_Operational_Issues",
    "Operational_Issue_Percentage"
]]

result.columns = [
    "Bus Company",
    "Primary Issue", "Primary Count",
    "Secondary Issue", "Secondary Count",
    "Total Operational Issues",
    "Operational Issue %"
]

result["Operational Issue %"] = result["Operational Issue %"].map("{:.2f}%".format)

bus_operational_results = {
    "title": "Top Bus Companies by Operational Issues (Minimum 100 Issues)",
    "divider": "=" * 160,
    "result": result.to_string(index=False),
    "note": "Note: Only companies with at least 100 operational issues are included."
}
'))

output_lines <- c(
  "",
  py$bus_operational_results$title,
  py$bus_operational_results$divider,
  py$bus_operational_results$result,
  py$bus_operational_results$divider,
  "",
  py$bus_operational_results$note,
  ""
)

writeLines(output_lines)

#3.1
#------------Bootstrap--------------#
library(reticulate)
library(magrittr)

pd <- import("pandas")
np <- import("numpy")

py_run_string("
import numpy as np
import pandas as pd
def process_data(file_path):
    df = pd.read_excel(file_path, sheet_name='Bootstrap')
    required_cols = ['Bus_company_name', 'Route_Number', 'Reason', 'Delay_clean']
    df = df[required_cols].dropna()
    df['Bus_company_name'] = df['Bus_company_name'].str.strip()
    df['Route_Number'] = df['Route_Number'].astype(str).str.strip()
    df['Reason'] = df['Reason'].str.strip()
    df['Delay_clean'] = pd.to_numeric(df['Delay_clean'], errors='coerce')
    return df.dropna(subset=['Delay_clean'])

def bootstrap_analysis_py(data, n_iter=1000):
    results = {}
    top_companies = data['Bus_company_name'].value_counts().head(6).index
    
    for company in top_companies:
        company_data = data[data['Bus_company_name'] == company]
        delay_stats = [np.mean(company_data.sample(frac=1, replace=True)['Delay_clean']) for _ in range(n_iter)]
        delay_ci = np.percentile(delay_stats, [2.5, 97.5])
        
        failure_types = {}
        top_reasons = company_data['Reason'].value_counts().index[:3]
        for reason in top_reasons:
            stats = [np.mean(company_data.sample(frac=1, replace=True)['Reason'] == reason) for _ in range(n_iter)]
            ci = np.percentile(stats, [2.5, 97.5])
            failure_types[reason] = f\"{ci[0]*100:.1f}% ~ {ci[1]*100:.1f}%\"
        
        route_delays = []
        top_routes = company_data['Route_Number'].value_counts().index[:20]
        for route in top_routes:
            route_data = company_data[company_data['Route_Number'] == route]['Delay_clean']
            if len(route_data) < 5: continue
            stats = [np.mean(np.random.choice(route_data, size=len(route_data), replace=True)) for _ in range(n_iter)]
            ci = np.percentile(stats, [2.5, 97.5])
            route_delays.append((route, ci[0], ci[1], delay_ci[0]))
        
        top_routes_sorted = sorted(route_delays, key=lambda x: -x[1])[:3]
        
        results[company] = {
            'Failure Types': failure_types,
            'Critical Routes': [(route, f\"{lower:.1f} ~ {upper:.1f}\", f\"{company_ref:.1f}\") for route, lower, upper, company_ref in top_routes_sorted],
            'Company Mean Delay': f\"{delay_ci[0]:.1f} ~ {delay_ci[1]:.1f}\"
        }
    return results
")

load_data <- function(file_path) {
  py$process_data(file_path) %>% py_to_r()
}

bootstrap_analysis <- function(data, n_iter=1000L) {
  if (!inherits(data, "python.builtin.object")) {
    data <- r_to_py(data)
  }
  py$bootstrap_analysis_py(data, n_iter = as.integer(n_iter)) %>% py_to_r()
}

print_results <- function(results) {
  for (company in names(results)) {
    res <- results[[company]]
    writeLines(sprintf("\n\033[1m%s\033[0m", toupper(company)))
    writeLines(strrep("=", 60))
    writeLines(sprintf("\nCompany Mean Delay: %s mins (95%% CI)", res[['Company Mean Delay']]))
    writeLines("\n[Top Failure Types]")
    for (reason in names(res[['Failure Types']])) {
      writeLines(sprintf("- %-25s %s", reason, res[['Failure Types']][[reason]]))
    }
    writeLines("\n[Critical Problem Routes]")
    writeLines(sprintf("%-15s%-25s%-20s", "Route", "Route Delay (Mean)", "Company Baseline"))
    writeLines(strrep("-", 60))
    for (route in res[['Critical Routes']]) {
      writeLines(sprintf("%-15s%-25s%-20s", route[[1]], route[[2]], route[[3]]))
    }
  }
}

if (interactive()) {
  df <- load_data("C:\\Users\\cz074\\Desktop\\bus_new.xlsx")
  writeLines(sprintf("Data Overview:\n- Total Records: %d\n- Companies Count: %d", nrow(df), length(unique(df$Bus_company_name))))
  results <- bootstrap_analysis(df)
  writeLines(paste0("\n", strrep("=", 60)))
  writeLines(paste0(strrep(" ", 20), "Bus Breakdown Analysis", strrep(" ", 20)))
  writeLines(strrep("=", 60))
  print_results(results)
}

#4.1
# ============== OPI Ranking ==============
library(tidyverse)
library(knitr)
library(broom)
library(readxl)  

# 1. Define OPI Weights
WEIGHTS <- list(
  Eff = 0.40,      # Efficiency (Delay_clean, negative coefficient/lower is better)
  Resp = 0.20,     # Responsiveness (Time_Gap, negative coefficient/lower is better)
  Rel = 0.30,      # Reliability (Severe_Delay OR, OR < 1/lower is better)
  Comp = 0.10      # Compliance (Notification_Success OR, OR > 1/higher is better)
)

# 2. Read Data and Dependent Variable Construction 
df <- read_excel("C:\\Users\\cz074\\Desktop\\bus_new.xlsx", sheet = "Bootstrap_with_month")

df <- df %>%
  mutate(
    Severe_Delay = ifelse(Delay_clean > 30, 1, 0),
    Notification_Success = ifelse(Has_Contractor_Notified_Schools == "Yes" & 
                                    Has_Contractor_Notified_Parents == "Yes", 1, 0),
    across(c(Bus_company_name, Boro, Reason, Run_Type), as.factor)
  ) %>%
  na.omit()

# 3. Define Global Model Formula
formula_reg <- ~ Bus_company_name + Boro + Reason + Run_Type

# 4. Run the Four Global Regression Models
model_A <- lm(Delay_clean ~ Bus_company_name + Boro + Reason + Run_Type, data = df)
model_B <- lm(Time_Gap ~ Bus_company_name + Boro + Reason + Run_Type, data = df)
model_C <- glm(Severe_Delay ~ Bus_company_name + Boro + Reason + Run_Type, 
               data = df, family = binomial(link = "logit"))
model_D <- glm(Notification_Success ~ Bus_company_name + Boro + Reason + Run_Type, 
               data = df, family = binomial(link = "logit"))

# 5. Extract Adjusted Metrics
baseline_company <- sort(unique(df$Bus_company_name))[1] 

extract_adjusted_metrics <- function(model, metric_type) {
  results <- tidy(model, exponentiate = (metric_type %in% c("Rel", "Comp"))) %>%
    filter(str_detect(term, "^Bus_company_name"))
  
  baseline_row <- tibble(
    term = paste0("Bus_company_name", baseline_company),
    estimate = ifelse(metric_type %in% c("Rel", "Comp"), 1, 0) 
  )
  
  bind_rows(results, baseline_row) %>%
    mutate(
      Bus_company_name = gsub("Bus_company_name", "", term),
      Bus_company_name = gsub("Bus_company_name[T.]", "", Bus_company_name, fixed = TRUE),
      Bus_company_name = gsub("`", "", Bus_company_name, fixed = TRUE)
    ) %>%
    select(Bus_company_name, estimate) %>%
    rename(!!paste0("Adj_", metric_type) := estimate)
}

metrics_Eff <- extract_adjusted_metrics(model_A, "Eff")
metrics_Resp <- extract_adjusted_metrics(model_B, "Resp")
metrics_Rel <- extract_adjusted_metrics(model_C, "Rel")
metrics_Comp <- extract_adjusted_metrics(model_D, "Comp")

adjusted_df <- metrics_Eff %>%
  full_join(metrics_Resp, by = "Bus_company_name") %>%
  full_join(metrics_Rel, by = "Bus_company_name") %>%
  full_join(metrics_Comp, by = "Bus_company_name")

# 6. Standardization
z_score <- function(x) {
  as.numeric(scale(x, center = TRUE, scale = TRUE))
}

performance_scores <- adjusted_df %>%
  mutate(
    S_Eff = z_score(Adj_Eff) * -1,
    S_Resp = z_score(Adj_Resp) * -1,
    S_Rel = z_score(Adj_Rel) * -1,
    S_Comp = z_score(Adj_Comp) * 1
  )

# 7. Calculate OPI
final_ranking <- performance_scores %>%
  mutate(
    OPI = (S_Eff * WEIGHTS$Eff) +
      (S_Resp * WEIGHTS$Resp) +
      (S_Rel * WEIGHTS$Rel) +
      (S_Comp * WEIGHTS$Comp),
    OPI_Score_100 = 70 + (OPI * 10),
    OPI_Score_100 = pmax(0, pmin(100, OPI_Score_100))
  ) %>%
  arrange(desc(OPI_Score_100)) %>%
  mutate(Rank = as.integer(row_number())) %>%
  select(Rank, 
         `Company_Name` = Bus_company_name, 
         `OPI_Score_100`, 
         OPI,
         S_Eff, S_Resp, S_Rel, S_Comp,
         `Adj_Coeff_Delay (min)` = Adj_Eff,
         `Adj_Coeff_Notif (min)` = Adj_Resp,
         `Adj_OR_Severe_Delay` = Adj_Rel,
         `Adj_OR_Notif_Success` = Adj_Comp
  ) %>%
  # Ensure all numeric columns are numeric
  mutate(across(where(is.numeric), as.numeric))

# 8. Output Results
writeLines(c(
  "================================================================================================================================",
  "--- Overall Operational Performance Ranking (OPI) Based on Regression Conclusions ---",
  "================================================================================================================================",
  "",
  " Rank |         Company_Name         | OPI_Score_100 |  OPI  | S_Eff | S_Resp | S_Rel | S_Comp | Adj_Delay(min) | Adj_Notif(min) | Adj_OR_Delay | Adj_OR_Notif",
  "-----:|:----------------------------:|-------------:|:-----:|:-----:|:------:|:-----:|:------:|:--------------:|:--------------:|:------------:|:------------:",
  sapply(1:nrow(final_ranking), function(i) {
    row <- final_ranking[i, ]
    
    
    sprintf("%4d | %-27s | %12.2f | %5.2f | %5.2f | %5.2f | %5.2f | %5.2f | %13.2f | %13.2f | %11.2f | %11.2f",
            row$Rank,
            substr(row$Company_Name, 1, 27),  
            row$OPI_Score_100,
            row$OPI,
            row$S_Eff,
            row$S_Resp,
            row$S_Rel,
            row$S_Comp,
            row$`Adj_Coeff_Delay (min)`,
            row$`Adj_Coeff_Notif (min)`,
            row$Adj_OR_Severe_Delay,
            row$Adj_OR_Notif_Success)
  }),
  "",
  "Explanation:",
  "1. The ranking is based on metrics adjusted by the regression models, controlling for factors like Boro, Reason, and Run_Type.",
  sprintf("2. The baseline company is the first alphabetically (%s), whose adjusted coefficients are 0 and ORs are 1.", baseline_company),
  "3. OPI Weights: Efficiency 40%, Reliability 30%, Responsiveness 20%, Compliance 10%.",
  "4. OPI_Score_100 is a score scaled from 0-100 using Adjusted Z-Score method, where 70 represents average performance."
))

#4.2
# ============== Analysis of Congestion Routes for Each Company ==============
library(tidyverse)
library(knitr)
library(broom)
library(readxl)  

df <- read_excel("C:\\Users\\cz074\\Desktop\\bus_new.xlsx", sheet = "Bootstrap_with_month")

df_clean <- df %>%
  mutate(
    across(c(Bus_company_name, Route_Number, Month, Hours, Day), as.factor)
  ) %>%
  select(Delay_clean, Bus_company_name, Route_Number, Month, Hours, Day) %>%
  na.omit()

company_names <- unique(df_clean$Bus_company_name)

analyze_congested_routes_regression <- function(company_name, data) {
  df_comp <- data %>% filter(Bus_company_name == company_name)
  model <- lm(Delay_clean ~ Route_Number + Month + Hours + Day, data = df_comp)
  
  tidy(model) %>%
    filter(str_detect(term, "^Route_Number"), estimate > 0, p.value < 0.05) %>%
    mutate(
      `Route Number` = gsub("Route_Number", "", term),
      `Route Number` = gsub("`", "", `Route Number`, fixed = TRUE)
    ) %>%
    select(`Route Number`, `Materiality(coef_Route_Number)` = estimate, `P-value` = p.value) %>%
    arrange(desc(`Materiality(coef_Route_Number)`)) %>%
    head(5)
}

all_results <- list()
for (name in company_names) {
  result <- analyze_congested_routes_regression(name, df_clean)
  if (nrow(result) > 0) {
    result$`Company Name` <- name
    all_results[[name]] <- result
  }
}

final_congested_routes_df <- bind_rows(all_results)

# Prepare the output with writeLines
output_lines <- c(
  "======================================================================================",
  "--- Top 5 Most Congested Routes (Regression Adjusted, with Day Control) per Company ---",
  "======================================================================================",
  "",
  "Interpretation: 'Materiality(coef_Route_Number)' is the increase in average delay (in minutes)",
  "caused by this route relative to the baseline route, adjusted for Month, Hour, and Day effects.",
  "",
  "----------------------------------------------------------------------------------------",
  " Company Name            | Route Number | Delay Increase (min) | P-value ",
  "------------------------|--------------|----------------------|---------"
)

if (nrow(final_congested_routes_df) > 0) {
  # Format each row of the results
  result_lines <- sapply(1:nrow(final_congested_routes_df), function(i) {
    row <- final_congested_routes_df[i, ]
    sprintf(" %-22s | %-12s | %19.2f | %7.4f",
            substr(row$`Company Name`, 1, 22),
            row$`Route Number`,
            row$`Materiality(coef_Route_Number)`,
            row$`P-value`)
  })
  
  output_lines <- c(output_lines, result_lines, 
                    "----------------------------------------------------------------------------------------",
                    "",
                    "Note: Only shows routes with statistically significant delays (p < 0.05)")
} else {
  output_lines <- c(output_lines, 
                    "No statistically significant congested routes found.",
                    "")
}

# Write all lines
writeLines(output_lines)


## ==============================
## 4) slide 11 graph
## ==============================
# Create data frame
op_issue <- data.frame(
  Company = c(
    "LEESEL TRANSP CORP.",
    "SNT BUS INC.",
    "BORO TRANSIT, INC.",
    "QUALITY TRANSPORTATION CO.",
    "FIRST STEPS TRANSP INC.",
    "JOFAZ TRANSPORTATION INC."
  ),
  Value = c(26.53, 30.77, 25.14, 24.91, 22.89, 36.78)
)

# Arrange data and create factor levels
op_issue <- op_issue %>%
  arrange(Value) %>%
  mutate(Company = factor(Company, levels = Company))

# Create plot with blue color gradient
ggplot(op_issue, aes(x = Value, y = Company, fill = Value)) +
  geom_col(color = "white",  linewidth = 1) +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  geom_text(
    aes(label = Company),
    hjust = 1,
    x = -1,
    size = 10,
    fontface = "bold"
  ) +
  geom_text(
    aes(label = paste0(Value, "%"), x = Value),
    hjust = -0.1,
    size = 8
  ) +
  scale_x_continuous(expand = expansion(add = c(40, 8))) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(size = 48, face = "bold", hjust = 0.5),
    legend.position = "none"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Percentage of Operational Issues by Operator"
  )
