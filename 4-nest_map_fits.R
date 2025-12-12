library(tidyverse)
library(patchwork)
library(readxl)
library(glue)
library(broom)
library(latex2exp)
theme_set(
    theme_bw() +
        theme(
            text = element_text(size = 18, color = "black"),
            panel.border = element_rect(
                color = "black",
                fill = NA,
                linewidth = 1
            ),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.background = element_rect(fill = "transparent", color = NA),
            legend.background = element_rect(fill = "transparent", color = NA),
            strip.background = element_rect(fill = "transparent", color = NA),
            strip.text = element_text(face = "bold"),
            strip.text.y = element_text(angle = 0)
        )
)


# # # # # # # # # # # # # # # # # # #
# Linear fitting
# # # # # # # # # # # # # # # # # # #

# Read the data in `Data/fit1.csv` into a data.frame called `d1`.
# Plot the data, and make the appropriate fit using `lm()`.
# Overlay the result of the fit as a red line
# Hint: look at `predict()`

d1 <- ___
P1 <- d1 |>
    ggplot(aes(___)) +
    geom____()
P1
fit <- lm(___)
P1 + geom_line(aes(y = ___), ___)

# Read the data in `Data/fit2.csv` into a data.frame called `d2`.
# Plot the data, and make the appropriate fit using `lm()`.
# Overlay the result of the fit as a red line
# Hint: look at `poly()`. Polynomial fits are actually linear fits!

d2 <- ___


# # # # # # # # # # # # # # # # # # #
# Nonlinear fitting
# # # # # # # # # # # # # # # # # # #

# Read `Data/rubis_01.txt` into a data.frame called `d` with the column names `w` and `int`.
# Hint: look at the options of `read.table()`
d <- ___

# Plot the intensity as a function of wavenumber using `ggplot()`.
P <- ggplot(___) + 
    ___
P

# The two peaks are Lorentzian peaks, of the form:
# a/(1 + ((w-w0)/b)^2),
# where a, b, and x0 are the parameters to be fitted.
# Create a function `lor()` that defines a Lorentzian peak.
lor <- function(w, a, w0, b) {
    ___
}

# To find the starting values for the parameters, we will add as a line to the plot the sum of two Lorentzian peaks with guessed parameters so that the starting point is not too far from the data to fit.
initial_guess <- tibble(
    w = ___,
    peak1 = ___,
    peak2 = ___,
    int = peak1 + peak2
)
P +
    geom_line(___)

# Perform the fit using `nls()`, then add the resulting fit to the plot as a red line.
# Hint: look at `predict()`
fit <- nls(
    ___, # what is the data.frame with our data
    ___, # what is the formula to fit? `~` means `as a function of`
    start = list(___) # initial values for the parameters to fit
) 
P +
    geom_line(___) + # initial guess
    geom_line(___) + # nls fit
    labs(___) # add labels to the axes


# # # # # # # # # # # # # # # # # #
# Fitting multiple datasets at once
# # # # # # # # # # # # # # # # # #

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