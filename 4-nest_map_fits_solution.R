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

d1 <- read_csv("Data/fit1.csv")
P1 <- d1 |>
    ggplot(aes(x = x, y = y)) +
    geom_point()
P1
fit <- lm(y ~ x, data = d1)
P1 + geom_line(aes(y = predict(fit)), color = 'red', linewidth = 1)

# Read the data in `Data/fit2.csv` into a data.frame called `d2`.
# Plot the data, and make the appropriate fit using `lm()`.
# Overlay the result of the fit as a red line
# Hint: look at `poly()`. Polynomial fits are actually linear fits!

d2 <- read_csv("Data/fit2.csv")
P2 <- d2 |>
    ggplot(aes(x = x, y = y)) +
    geom_point()
P2
fit <- lm(y ~ poly(x, 2), data = d2)
P2 + geom_line(aes(y = predict(fit)), color = 'red', linewidth = 1)


# # # # # # # # # # # # # # # # # # #
# Nonlinear fitting
# # # # # # # # # # # # # # # # # # #

# Read `Data/rubis_01.txt` into a data.frame called `d` with the column names `w` and `int`.
# Hint: look at the options of `read.table()`
d <- read_table("Data/rubis_01.txt", col_names = c('w', 'int'))

# Plot the intensity as a function of wavenumber using `ggplot()`.
P <- ggplot(d, aes(x = w, y = int)) +
    geom_point()
P

# The two peaks are Lorentzian peaks, of the form:
# a/(1 + ((w-w0)/b)^2),
# where a, b, and x0 are the parameters to be fitted.
# Create a function `lor()` that defines a Lorentzian peak.
lor <- function(w, a, w0, b) {
    a / (1 + ((w - w0) / b)^2)
}

# To find the starting values for the parameters, we will add as a line to the plot the sum of two Lorentzian peaks with guessed parameters so that the starting point is not too far from the data to fit.
initial_guess <- tibble(
    w = d$w,
    peak1 = lor(d$w, 700, 3170, 10),
    peak2 = lor(d$w, 500, 3210, 10),
    int = peak1 + peak2
)
P +
    geom_line(data = initial_guess, color = 'red', linewidth = 1)

# Perform the fit using `nls()`, then add the resulting fit to the plot as a red line.
# Hint: look at `predict()`
fit <- nls(
    data = d, # what is the data.frame with our data
    int ~ lor(w, a1, w01, b1) + lor(w, a2, w02, b2), # what is the formula to fit? `~` means `as a function of`
    start = list(a1 = 700, w01 = 3170, b1 = 10, a2 = 500, w02 = 3210, b2 = 10)
) # initial values for the parameters to fit
P +
    geom_line(
        data = initial_guess,
        aes(color = 'Initial guess'),
        linewidth = 1
    ) +
    geom_line(aes(y = predict(fit), color = 'NLS fit'), linewidth = 1) +
    scale_color_manual(
        name = NULL,
        values = c('Initial guess' = 'red', 'NLS fit' = 'royalblue')
    ) +
    labs(
        title = "Nonlinear fit of two Lorentzian peaks",
        y = "Intensity [a.u.]",
        x = "Wavenumber [1/cm]"
    )


# # # # # # # # # # # # # # # # # #
# Fitting multiple datasets at once
# # # # # # # # # # # # # # # # # #

# Find list of all files in the Data directory with the pattern "sample" in their name
flist <- list.files(path = "Data", pattern = "sample", full.names = TRUE)
# Read the files in the list and store them in a list of dataframes
d <- read_csv(flist, id = 'file') |>
    nest(data = -file) |> # nest the data in a list column called "data"
    mutate(file = basename(file)) |> # remove the path from the file name
    separate(file, c("sample", "temperature", "time", NA)) |> # retrieve the information from the file name and store it into new columns
    mutate(
        sample = sample |> str_remove("sample") |> as.factor(), # remove the string "sample" and convert to factor
        temperature = temperature |> str_remove("K") |> as.numeric(), # remove the string "K" and convert to numeric
        time_unit = time |> str_remove_all("[:digit:]"), # remove all digits from the string "time" and store the result in the column "time_unit"
        time = time |> str_remove_all("[:alpha:]") |> as.numeric(), # remove all letters from the string "time" and convert to numeric
        time = ifelse(time_unit == "min", time * 60, time)
    ) |> # convert the time to seconds if the time unit is "min"
    select(-time_unit) # remove the column "time_unit"

# Write a function to fit a linear model to the data given a dataframe "df"
# with columns "x" and "y"
myfit <- function(df) {
    lm(y ~ x, data = df)
}

# Fit the linear model to each dataframe in the list column "data"
# using the functions "myfit" and "map()"
d_fitted <- d |>
    mutate(
        fit = map(data, myfit), # do the fit on all elements (tibbles) of the column "data"
        tidied = map(fit, tidy), # tidy the results of the fit
        augmented = map(fit, augment) # augment the results of the fit
    )

# This is the same as the previous code, but defining the function "myfit"
#  directly in the "map()" function
d_fitted <- d |>
    mutate(
        fit = map(data, ~ lm(y ~ x, data = .)),
        tidied = map(fit, tidy),
        augmented = map(fit, augment)
    )

# Plot the evolution of the parameters of the fits
d_fitted |>
    unnest(tidied) |>
    mutate(term = ifelse(term == "(Intercept)", "Intercept", "Slope")) |> # rename the term "(Intercept)" to "intercept" and the term "x" to "slope"
    ggplot(aes(x = temperature, y = estimate, color = factor(time))) +
    geom_point() +
    facet_grid(sample ~ term) +
    labs(color = "Time [s]", x = "Temperature [K]", y = "Estimate")

# Plot the data and the fits
d_fitted |>
    unnest(augmented) |>
    ggplot(aes(x = x, y = y, color = factor(temperature))) +
    facet_grid(glue("Sample {sample}") ~ glue("{time} sec")) +
    geom_point(alpha = .2, size = 5) +
    geom_line(aes(y = .fitted), linewidth = 2) +
    labs(color = "Temperature [K]", x = "y", y = "x")
