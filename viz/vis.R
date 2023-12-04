library(tidyverse)
library(readr)
library(tidyr)
library(broom)
library(jsonlite)
library(usmap)
library(dplyr)
library(tigris)
library(tidycensus)
library(patchwork)
library(sf)
library(sp)
library(car)
library(viridis)
library(RColorBrewer)
library(ggpubr)
library(rstatix)
library(flextable)
library(officer)


options(tigris_use_cache = FALSE)

folder = dirname(rstudioapi::getSourceEditorContext()$path)
data_directory = file.path(folder, '..', 'data')
setwd(data_directory)

# congressional data
df <- read.csv("congress_final.csv")

# Get the congressional districts
congressional_districts <-
  tigris::congressional_districts(cb = TRUE,
                                  year = 2021,
                                  resolution = "20m")

shape_data <- tigris::shift_geometry(congressional_districts)
# st_write(shape_data, file.path(data_directory, "shapefiles", "congressional_districts_2021.shp"))

# exclude non contigous US States
non_cont_fips_codes <-
  c('02', '15', '72', '66', '60', '69', '78', '78')
shp_filtered <- shape_data %>%
  filter(!STATEFP %in% non_cont_fips_codes)

shp_filtered <- shp_filtered %>%
  mutate(STATEFP = as.integer(STATEFP))

shp_filtered <- shp_filtered %>%
  mutate(CD116FP = as.integer(CD116FP))

y = c("ideology")

# demographic data
population_cols <-
  c(
    "ACS_Pop_Black",
    "ACS_Pop_White",
    "ACS_POP_Others",
    "ACS_Pop_Latino",
    "ACS_MedianIncome",
    "ACS_EdAttainment_BachelorsPlus"
  )

# member personal factors
personal_factors <-
  c("Gender", "LGBTQ", "RaceEthnicity", "Age", "Religion", "IsVet")

df$Party <- factor(
  df$Party,
  levels = c("Republican",
             "Democratic"),
  labels = c("R",
             "D")
)

# make cat vars as factors
df$LGBTQ <- as.factor(df$LGBTQ)
df$Gender <- as.factor(df$Gender)
df$RaceEthnicity <- as.factor(df$RaceEthnicity)
df$Religion <- as.factor(df$Religion)
df$IsVet <- as.factor(df$IsVet)


# create ideology cluster group
df$ideology_group <- cut(
  df$ideology,
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
  labels = c(
    "Left (Far)",
    "Left (Center)",
    "Centrist",
    "Right (Center)",
    "Right (Far)"
  ),
  include.lowest = TRUE
)

# Reorder ideology_cluster from left -> right
df$ideology_group <- factor(
  df$ideology_group,
  
  levels =  c(
    "Left (Far)",
    "Left (Center)",
    "Centrist",
    "Right (Center)",
    "Right (Far)"
  )
)

## Visualizations ##

# Color scale map
color_palette <- viridis_pal()(5)

scale_map <- data.frame(
  name = c(
    "Left (Far)",
    "Left (Center)",
    "Centrist",
    "Right (Center)",
    "Right (Far)"
  )
  ,
  color = color_palette,
  shape = c(15, 16, 17, 18, 19)
)

# Plot 1: Ideology vs. blick Population (%)
create_plot <- function(data, x_col, y_col, color_col, shape_col, title, x_label, y_label) {
  ggplot(data, aes(
    x = !!sym(x_col),
    y = !!sym(y_col),
    color = !!sym(color_col),
    shape = !!sym(shape_col)
  )) +
    geom_point(size = 2) +
    labs(title = title, x = x_label, y = y_label) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_color_manual(values = scale_map$color,
                       labels = scale_map$name,
                       name = "Ideology Category") +
    scale_shape_manual(values = scale_map$shape,
                       labels = scale_map$name,
                       name = "Ideology Category") +
    theme(
      plot.title = element_text(size = 8),
      axis.text.x = element_text(size = 6),
      axis.title = element_text(size = 7),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    ) +
    theme(text = element_text(family = "Times New Roman"))
}

plot1 <- create_plot(
  data = df, 
  x_col = "ACS_Pop_Black", 
  y_col = "ideology", 
  color_col = "ideology_group", 
  shape_col = "ideology_group",
  title = "Ideology vs. Black Population", 
  x_label = "Black Population (%)", 
  y_label = "Ideology Score"
)

print(plot1)

plot2 <- create_plot(
  data = df, 
  x_col = "ACS_Pop_Latino", 
  y_col = "ideology", 
  color_col = "ideology_group", 
  shape_col = "ideology_group",
  title = "Ideology vs. Latino Population", 
  x_label = "Latino Population (%)", 
  y_label = "Ideology Score"
)

print(plot2)

plot3 <- create_plot(
  data = df, 
  x_col = "ACS_Pop_White", 
  y_col = "ideology", 
  color_col = "ideology_group", 
  shape_col = "ideology_group",
  title = "Ideology vs. White Population", 
  x_label = "White Population (%)", 
  y_label = "Ideology Score"
)

print(plot3)

plot4 <- create_plot(
  data = df, 
  x_col = "ACS_MedianIncome", 
  y_col = "ideology", 
  color_col = "ideology_group", 
  shape_col = "ideology_group",
  title = "Ideology vs. Median Income", 
  x_label = "Median Income ($)", 
  y_label = "Ideology Score"
)

print(plot4)

plot5 <- create_plot(
  data = df, 
  x_col = "ACS_EdAttainment_BachelorsPlus", 
  y_col = "ideology", 
  color_col = "ideology_group", 
  shape_col = "ideology_group",
  title = "Ideology vs. Education Attainment Degree+", 
  x_label = "Education (%)", 
  y_label = "Ideology Score"
)

print(plot5)


plot6 <- create_plot(
  data = df, 
  x_col = "Age", 
  y_col = "ideology", 
  color_col = "ideology_group", 
  shape_col = "ideology_group",
  title = "Ideology vs. Age of a Member", 
  x_label = "Age (Years)", 
  y_label = "Ideology Score"
)

print(plot6)

arranged_plots_ideology <-
  ggarrange(
    plot1,
    plot2,
    plot3,
    plot4,
    plot5,
    plot6,
    ncol = 2,
    nrow = 3,
    common.legend = TRUE,
    legend = "bottom"
  )

print(arranged_plots_ideology)

path_out = file.path(folder, 'figures', 'ideology_vs_features.png')

ggsave(path_out,
       arranged_plots_ideology,
       width = 8.27,
       height = 11.69)


## Barplots of the othe categorical variables ##

create_bar_plot <- function(raw_data, x_col, y_col, fill_col, title, x_label, y_label) {
  # Summarize data inside the function
  group_summary <- raw_data %>%
    group_by(!!sym(x_col), !!sym(fill_col)) %>%
    summarise(count = n(), .groups = 'drop')

  # Proceed with plotting
  ggplot(group_summary, aes_string(
    x = x_col,
    y = y_col,
    fill = fill_col
  )) +
    geom_bar(stat = "identity", position = position_dodge()) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#f5f5f2", color = NA),
      plot.title = element_text(size = 8),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 6),
      axis.title.x = element_text(size = 7),
      axis.title.y = element_text(size = 7),
      legend.text = element_text(size = 7),
      legend.title = element_text(size = 7)
    ) +
    scale_fill_viridis_d() +
    scale_y_continuous(expand = c(0, 0)) +
    theme(text = element_text(family = "Times New Roman"))
}

# For LGBTQ
plot7 <- create_bar_plot(df, "ideology_group", "count", "LGBTQ", 
                         "(A) Count of LGBTQ Status by Ideology Group", 
                         "Ideology Group", "Count")

# For Gender
plot8 <- create_bar_plot(df, "ideology_group", "count", "Gender", 
                         "(B) Count by Gender across Ideology Groups", 
                         "Ideology Group", "Count")

# For RaceEthnicity
plot9 <- create_bar_plot(df, "ideology_group", "count", "RaceEthnicity", 
                         "(C) Count by Ethnicity across Ideology Groups", 
                         "Ideology Group", "Count")

# For Religion
plot10 <- create_bar_plot(df, "ideology_group", "count", "Religion", 
                          "(D) Count by Religion across Ideology Groups", 
                          "Ideology Group", "Count")

# For IsVet (Assuming this is Veteran status)
plot11 <- create_bar_plot(df, "ideology_group", "count", "IsVet", 
                          "(E) Count of Veterans across Ideology Groups", 
                          "Ideology Group", "Count")

# For Party
plot12 <- create_bar_plot(df, "ideology_group", "count", "Party", 
                          "(F) Count by Political Party across Ideology Groups", 
                          "Ideology Group", "Count")

print(plot12
      )
arranged_bar_plots <-
  ggarrange(
    plot7,
    plot8,
    plot9,
    plot10,
    plot11,
    plot12,
    ncol = 2,
    nrow = 3
  )

print(arranged_bar_plots)

path_out = file.path(folder, 'figures', 'bar_plots.png')

ggsave(path_out,
       arranged_bar_plots,
       width = 8.27,
       height = 11.69)


### Perform simple linear regressions of each variable
outcome <- "ideology"
predictors <- c("ACS_Pop_Black", "ACS_Pop_White", "ACS_POP_Others", "ACS_Pop_Latino", "ACS_MedianIncome", "ACS_EdAttainment_BachelorsPlus", "Gender", "LGBTQ", "RaceEthnicity", "Age", "Religion", "IsVet")

# Store results
results <- list()

for (pred in predictors) {
  formula_str <- paste(outcome, "~", pred)
  model <- lm(formula_str, data = df)
  results[[pred]] <- summary(model)
}

for (pred in predictors) {
  cat("\nRegression Results for Predictor:", pred, "\n")
  print(results[[pred]])
}

### Anova Models

anova_result <- aov(ideology ~ Gender, data = df)
welch_test <- oneway.test(ideology ~ LGBTQ, data = df, var.equal = FALSE)

# levene test

leveneTest(anova_result)

# Print the summary
summary(anova_result)

# If significant, follow up with post-hoc tests like Tukey's HSD
TukeyHSD(anova_result)

### Multiple regression Variables ###
### 1. Test for collinearity ###

qqnorm(df$ideology)

ideology_ethnicity <- games_howell_test(df, ideology ~ RaceEthnicity, conf.level = 0.95, detailed = FALSE)
ideology_lgbtq <- games_howell_test(df, ideology ~ LGBTQ, conf.level = 0.95, detailed = FALSE)
ideology_vet <- games_howell_test(df, ideology ~ IsVet, conf.level = 0.95, detailed = FALSE)
ideology_gender <- games_howell_test(df, ideology ~ Gender, conf.level = 0.95, detailed = FALSE)

ideology_ethnicity$category <- "Ethnicity"
ideology_lgbtq$category <- "LGBTQ Status"
ideology_vet$category <- "Veteran Status"
ideology_gender$category <- "Gender"

combined_data <- rbind(ideology_ethnicity, ideology_lgbtq, ideology_vet, ideology_gender)
combined_data <- combined_data[, c(1, which(names(combined_data) == "category"), 
                                   setdiff(2:ncol(combined_data), which(names(combined_data) == "category")))]

combined_data <- combined_data[, !duplicated(colnames(combined_data))]

combined_data$estimate <- round(combined_data$estimate, 3)
combined_data$conf.low <- round(combined_data$conf.low, 3)
combined_data$conf.high <- round(combined_data$conf.high, 3)
combined_data$p.adj <- round(combined_data$p.adj, 3)

ft <- flextable(combined_data)
ft <- ft %>%
  merge_v(j = ~.y.) %>% 
  merge_v(j = ~category)
ft <- width(ft, j = 3:7, width = 1.0)
# ft <- theme_booktabs(ft)
ft <- align(ft, align = "center", part = "all")
ft <- bold(ft, part = "header")
ft <- set_header_labels(ft, .y. = "Variable", category = "Category",
                        group1 = "Group 1", group2 = "Group 2",
                        estimate = "Estimate", conf.low = "Conf. Interval Low",
                        conf.high = "Conf. Interval High", p.adj = "Adjusted P-Value",
                        p.adj.signif = "Significance")

ft <- autofit(ft)

std_border <- fp_border(color = "gray")
ft <- border_inner_h(ft, border = std_border)
# 
# # Display the flextable
# ft <- hline(ft, part = "body", border = std_border)
ft

