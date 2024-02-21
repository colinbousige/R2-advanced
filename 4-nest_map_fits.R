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

# Find list of all files in the Data directory with the pattern "sample" in their name
flist <- list.files(___
                    full.names = TRUE)
# Read the files in the list and store them in a list of dataframes
d <- read_csv(___, id='file') |>
    nest(___)  |> # nest the data in a list column called "data"
    mutate(___) |>  # remove the path from the file name
    separate(___) |>  # retrieve the information from the file name and store it into new columns called "sample", "temperature", and "time"
    mutate(___, # remove the string "sample" and convert to factor
           ___, # remove the string "K" and convert to numeric
           ___, # remove all digits from the string "time" and store the result in a column "time_unit"
           ___, # remove all letters from the string "time" and convert to numeric
           ___) |> # convert the time to seconds if the time unit is "min"
    select(___)  # remove the column "time_unit"

# Write a function to fit a linear model to the data given a dataframe "df"
# with columns "x" and "y"
myfit <- function(df){
    ___
}

# Fit the linear model to each dataframe in the list column "data"
# using the functions "myfit" and "map()"
d_fitted <- d |>
    mutate(fit = map(___), # do the fit on all elements (tibbles) of the column "data"
           tidied = map(___), # tidy the results of the fit
           augmented = map(___) # augment the results of the fit
           )

# Plot the evolution of the parameters of the fits
d_fitted |>
    unnest(tidied) |>
    ___

# Plot the data and the fits
d_fitted |>
    unnest(augmented) |>
    ___