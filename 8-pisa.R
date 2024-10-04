library(tidyverse)
library(patchwork)
library(readxl)
library(glue)
library(broom)
library(latex2exp)
theme_set(theme_bw()+
    theme(text = element_text(size = 18, color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill="transparent", color=NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          strip.background = element_rect(fill = "transparent", color = NA),
          strip.text = element_text(face = "bold"),
          strip.text.y = element_text(angle=0)))

# Load the data in the file "Data/PISA2015_TauxReussite_SESCgp.xlsx"
pisa <- ___

# Make the table tidy with `pivot_longer()` : there should be 4 columns: "items", "location", "group", "success_rate"
pisa_tidy <- pisa |> 
    ___

# Make a density plot of the success rates, with a different color for each group, and a different facet for each location
pisa_tidy |> 
    ggplot(___)

# Make a boxplot of the success rates, with a different color for each group, and a different facet for each location
pisa_tidy |> 
    ggplot(___)

# Make a boxplot of the success rates, with a different color for each location, and a different facet for each group
pisa_tidy |> 
    ggplot(___)

# Make a table with the results of a Shapiro-Wilk test for each group and location, and a column indicating if the success rates are Gaussian
pisa_tidy |> 
    nest(data = -c(group, location)) |> 
    mutate(shapiro = map(data, ~shapiro.test(___)),
           tidied = map(shapiro, tidy)) |> 
    unnest(tidied) |> 
    mutate(is_gaussian = ___)

# Make a table with the results of a t-test for each group comparing the difference between locations, and a column indicating if the success rates are statistically different
pisa_tidy |> 
    nest(data = ___) |> 
    ___

