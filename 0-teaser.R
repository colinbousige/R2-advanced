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

religion <- religion_raw |>
    filter(Year == 2020) |>
    select(Country, Buddhists:Unaffiliated) |>
    pivot_longer(
        cols = -Country,
        names_to = "Religion",
        values_to = "Proportion"
    ) |>
    filter(.by = Country, Proportion == max(Proportion)) |>
    select(-Proportion)

prepare_data <- function(df, value_name) {
    df |>
        select(Country, '1900':'2018') |>
        pivot_longer(
            col = -Country,
            names_to = "Year",
            values_to = value_name,
            names_transform = list(Year = as.numeric)
        )
}

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
    filter(Year >= 1960) |>
    ggplot(aes(
        x = Income,
        y = Fertility,
        color = Religion,
        size = Population,
        frame = Year, # frame and id: just for plotly
        id = Country
    )) +
    geom_point(alpha = 0.8) +
    scale_size(guide = "none", range = c(3, 16)) +
    scale_x_continuous(
        transform = "log10",
        breaks = breakslog10,
        minor_breaks = minorbreakslog10,
        labels = scales::trans_format("log10", scales::math_format(10^.x)),
        guide = guide_axis_logticks(long = 2, mid = 1, short = 0.5)
    ) +
    coord_cartesian(xlim = c(100, 2e5), ylim = c(0, 10)) +
    scale_colour_colorblind() +
    labs(x = 'Income in comparable $', y = 'Babies per woman') +
    guides(colour = guide_legend(override.aes = list(size = 3)))

# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Using plotly to create an animated plot
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

PP <- ggplotly(P + scale_x_continuous(), dynamicTicks = TRUE) |>
    layout(
        yaxis = list(autorange = FALSE),
        xaxis = list(autorange = FALSE, type = 'log', range = list(2, 5.5))
    )
htmlwidgets::saveWidget(PP, "Plots/rel_babies.html")


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Using gganimate to create an animated plot
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

anim <- P +
    labs(title = 'Year: {round(frame_time,0)}') +
    transition_time(Year) +
    ease_aes('linear')
animate(anim, width = 1800, height = 1200, res = 300)
anim_save('Plots/rel_babies_gganimate.gif', animation = last_animation())

animate(
    anim,
    renderer = ffmpeg_renderer(),
    width = 1800,
    height = 1200,
    res = 300
)
anim_save('Plots/rel_babies_gganimate.mp4', animation = last_animation())
