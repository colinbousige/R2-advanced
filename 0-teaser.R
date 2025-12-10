# See the video here:
# https://www.ted.com/talks/hans_rosling_religions_and_babies?language=en

# # # # # # # # # # # # # # # # #
# Load libraries and set theme
# # # # # # # # # # # # # # # # #
library(tidyverse)
library(glue)
library(gganimate)
library(plotly)
library(ggthemes)
library(scales)
theme_set(
    theme_bw() +
        theme(
            text = element_text(size = 18, color = "black"),
            panel.border = element_rect(
                color = "black",
                fill = NA,
                linewidth = 1
            ),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background = element_rect(fill = "white", color = NA),
            legend.background = element_rect(fill = "white", color = NA),
            strip.background = element_rect(fill = "white", color = NA),
            strip.text = element_text(face = "bold"),
            strip.text.y = element_text(angle = 0)
        )
)

# # # # # # # # # # # # # # # # #
# Read data
# # # # # # # # # # # # # # # # #

children_raw <- read_csv("Data/children_per_woman_total_fertility.csv")
income_raw <- read_csv("Data/income_per_person.csv")
pop_raw <- read_csv("Data/population_total.csv")
religion_raw <- read_csv("Data/religion.csv")

# # # # # # # # # # # # # # # # #
# Prepare data in the right format
# # # # # # # # # # # # # # # # #

# `religion` dataset is in a different format, so we prepare it separately
religion <- religion_raw |>
    # Keep only most recent year
    filter(Year == 2020) |>
    # Select only relevant columns
    select(Country, Buddhists:Unaffiliated) |>
    # Pivot to long (tidy) format
    pivot_longer(
        cols = -Country,
        # New name for the names column
        names_to = "Religion",
        # New name for the values column
        values_to = "Proportion"
    ) |>
    # Keep only the religion with the highest proportion per country
    filter(.by = Country, Proportion == max(Proportion)) |>
    # Remove Proportion column, we don't need it anymore
    select(-Proportion)

# Function to prepare the other datasets that have similar structure
prepare_data <- function(df, value_name) {
    df |>
        # Keep only relevant columns
        select(Country, '1900':'2018') |>
        # Pivot to long (tidy) format
        pivot_longer(
            col = -Country,
            # New name for the names column
            names_to = "Year",
            # New name for the values column
            values_to = value_name,
            # Convert year names to numeric
            names_transform = list(Year = as.numeric)
        )
}

# Prepare each dataset using the function
children <- children_raw |> prepare_data("Fertility")
income <- income_raw |> prepare_data("Income")
pop <- pop_raw |> prepare_data("Population")

# # # # # # # # # # # # # # # # #
# Join all data together
# # # # # # # # # # # # # # # # #

data <- inner_join(children, income) |>
    inner_join(pop) |>
    inner_join(religion)

# # # # # # # # # # # # # # # # #
# Create the plot that you want, without animations
# # # # # # # # # # # # # # # # #

# Logarithmic scale functions snippets for nice breaks and labels
# just store it somewhere to reuse later
breakslog10 <- function(x) {
    low <- floor(log10(min(x)))
    high <- ceiling(log10(max(x)))
    return(10^(seq(low, high)))
}
minorbreakslog10 <- function(x) {
    low <- floor(log10(min(x)))
    high <- ceiling(log10(max(x)))
    return(rep(1:9, length(low:high)) * (10^rep(low:high, each = 9)))
}

P <- data |>
    # Keep only years from 1960 onwards to do like in the video
    filter(Year >= 1960) |>
    # Create the ggplot
    ggplot(
        aes(
            # The aes() tells ggplot what columns to use for what
            x = Income,
            y = Fertility,
            color = Religion,
            size = Population,
            frame = Year, # just for plotly
            id = Country # just for plotly
        )
    ) +
    # Add transparent points
    geom_point(alpha = 0.8) +
    # Customize point size scale: no legend, size range from 3 to 16
    scale_size(guide = "none", range = c(3, 16)) +
    # Customize x axis to be logarithmic with nice breaks, labels and log ticks.
    scale_x_continuous(
        transform = "log10",
        breaks = breakslog10,
        minor_breaks = minorbreakslog10,
        labels = scales::trans_format("log10", scales::math_format(10^.x)),
        guide = guide_axis_logticks(long = 2, mid = 1, short = 0.5)
    ) +
    # Customize x and y axis limits
    coord_cartesian(xlim = c(100, 2e5), ylim = c(0, 10)) +
    # Use colorblind-friendly color palette
    scale_colour_colorblind() +
    # Labels
    labs(x = 'Income in comparable $', y = 'Babies per woman') +
    # Legend with points all the same size (because the size varies otherwise)
    guides(colour = guide_legend(override.aes = list(size = 3)))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Using plotly to create an animated plot
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Create the plotly plot with logarithmic x axis
# we do `+ scale_x_continuous()` because ggplotly ignores the scale_x_continuous
# inside ggplot when converting to plotly, so we need to add it again
#
# plotly will use the `frame` aesthetic to create the animation with a slider
# and the `id` aesthetic to display the country names when hovering over points
PP <- ggplotly(P + scale_x_continuous(), dynamicTicks = TRUE) |>
    # add the logarithmic scale to x axis again
    layout(
        yaxis = list(autorange = FALSE),
        xaxis = list(autorange = FALSE, type = 'log', range = list(2, 5.5))
    )
# Save the plotly plot as an HTML file
htmlwidgets::saveWidget(PP, "Plots/rel_babies.html")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Using gganimate to create an animated plot in a gif/mp4
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# Create the gganimate animation
anim <- P +
    # Set up the animation with year as the transition variable
    # The frame_time variable will be created automatically, and equals Year here
    transition_time(Year) +
    # Add the year label that updates with the animation
    labs(title = 'Year: {round(frame_time,0)}') +
    # Use linear easing for smooth transition
    ease_aes('linear')

# Render and save the animation as gif image
animate(anim, width = 1800, height = 1200, res = 300)
anim_save('Plots/rel_babies_gganimate.gif', animation = last_animation())

# Render and save the animation as mp4 movie
animate(
    anim,
    renderer = ffmpeg_renderer(),
    width = 1800,
    height = 1200,
    res = 300
)
anim_save('Plots/rel_babies_gganimate.mp4', animation = last_animation())
