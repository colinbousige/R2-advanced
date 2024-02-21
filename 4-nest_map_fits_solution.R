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
flist <- list.files(path = "Data",
                    pattern = "sample",
                    full.names = TRUE)
# Read the files in the list and store them in a list of dataframes
d <- read_csv(flist, id='file') |>
    nest(data = -file)  |> # nest the data in a list column called "data"
    mutate(file = basename(file)) |>  # remove the path from the file name
    separate(file, c("sample", "temperature", "time", NA)) |>  # retrieve the information from the file name and store it into new columns
    mutate(sample = sample |> str_remove("sample") |> as.factor(), # remove the string "sample" and convert to factor
           temperature = temperature |> str_remove("K") |> as.numeric(), # remove the string "K" and convert to numeric
           time_unit = time |> str_remove_all("[:digit:]"), # remove all digits from the string "time" and store the result in the column "time_unit"
           time = time |> str_remove_all("[:alpha:]") |> as.numeric(), # remove all letters from the string "time" and convert to numeric
           time = ifelse(time_unit == "min", time*60, time)) |> # convert the time to seconds if the time unit is "min"
    select(-time_unit)  # remove the column "time_unit"

# Write a function to fit a linear model to the data given a dataframe "df"
# with columns "x" and "y"
myfit <- function(df){
    lm(y ~ x, data = df)
}

# Fit the linear model to each dataframe in the list column "data"
# using the functions "myfit" and "map()"
d_fitted <- d |>
    mutate(fit = map(data, myfit), # do the fit on all elements (tibbles) of the column "data"
           tidied = map(fit, tidy), # tidy the results of the fit
           augmented = map(fit, augment) # augment the results of the fit
           )

# This is the same as the previous code, but defining the function "myfit"
#  directly in the "map()" function
d_fitted <- d |>
    mutate(fit = map(data, ~lm(y~x, data = .)),
           tidied = map(fit, tidy),
           augmented = map(fit, augment)
           )

# Plot the evolution of the parameters of the fits
d_fitted |>
    unnest(tidied) |>
    mutate(term = ifelse(term == "(Intercept)", "Intercept", "Slope")) |> # rename the term "(Intercept)" to "intercept" and the term "x" to "slope"
    ggplot(aes(x=temperature, 
               y=estimate, 
               color=factor(time))) +
    geom_point()+
    facet_grid(sample~term) +
    labs(color = "Time [s]",
         x = "Temperature [K]",
         y = "Estimate")

# Plot the data and the fits
d_fitted |>
    unnest(augmented) |>
    ggplot(aes(x=x, y=y, color = factor(temperature))) +
    facet_grid(glue("Sample {sample}")~glue("{time} sec")) +
    geom_point(alpha = .2, size=5)+
    geom_line(aes(y=.fitted), linewidth=2)+
    labs(color = "Temperature [K]",
         x = "y",
         y = "x")



