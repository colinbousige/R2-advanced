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

children <- read_csv("Data/children_per_woman_total_fertility.csv")
income <- read_csv("Data/income_per_person.csv")
pop <- read_csv("Data/population_total.csv")
religion <- read_csv("Data/religion.csv")

# # # # # # # # # # # # # # # # #
# Prepare data in the right format
# # # # # # # # # # # # # # # # #

religion <- religion |>
    filter(Year == 2020) |>
    select(Country, Buddhists:Unaffiliated) |>
    pivot_longer(
        cols = -Country,
        names_to = "Religion",
        values_to = "Proportion"
    ) |>
    filter(.by = Country, Proportion == max(Proportion)) |>
    select(-Proportion)

children <- children |>
    select(Country, '1900':'2018') |>
    pivot_longer(
        col = -Country,
        names_to = "Year",
        values_to = "Fertility",
        names_transform = list(Year = as.numeric)
    )

income <- income |>
    select(Country, '1900':'2018') |>
    pivot_longer(
        col = -Country,
        names_to = "Year",
        values_to = "Income",
        names_transform = list(Year = as.numeric)
    )

pop <- pop |>
    select(Country, '1900':'2018') |>
    pivot_longer(
        col = -Country,
        names_to = "Year",
        values_to = "Population",
        names_transform = list(Year = as.numeric)
    )

# # # # # # # # # # # # # # # # #
# Join all data together
# # # # # # # # # # # # # # # # #

data <- inner_join(children, income) |>
    inner_join(pop) |>
    inner_join(religion)

# # # # # # # # # # # # # # # # #
# Create the plot that you want, without animations
# # # # # # # # # # # # # # # # #

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
    scale_x_log10(
        breaks = 10^(-seq(-10, 10, by = 1)),
        minor_breaks = rep(1:9, 2 * 10 + 1) * (10^rep(-10:10, each = 9)),
        labels = trans_format("log10", math_format(10^.x))
    ) +
    coord_cartesian(xlim = c(100, 2e5), ylim = c(0, 10)) +
    scale_colour_colorblind() +
    labs(x = 'Income in comparable $', y = 'Babies per woman') +
    guides(colour = guide_legend(override.aes = list(size = 5)))

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
