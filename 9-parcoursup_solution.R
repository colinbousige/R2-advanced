library(tidyverse)
library(patchwork)
library(glue)
library(broom)
library(latex2exp)
library(stringi)
theme_set(theme_bw()+
    theme(text = element_text(size = 18, color = "black"),
          panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
          panel.background = element_rect(fill = "transparent", color = NA),
          plot.background = element_rect(fill="transparent", color=NA),
          legend.background = element_rect(fill = "transparent", color = NA),
          strip.background = element_rect(fill = "transparent", color = NA),
          strip.text = element_text(face = "bold"),
          strip.text.y = element_text(angle=0)))


# Load the data for the map of France
# - using `stringi::stri_trans_general()` transform the region names to lower case and remove accents and spaces.
# - remove the single quotes from the region names using `stringr::str_remove_all()`
# - replace the spaces with a dash using `stringr::str_replace_all()`
# - remove the `subregion` column using `select()`

cartefrance <- map_data('france') |> 
    as_tibble() |> 
    mutate(region = region |>
           stri_trans_general("lower;Latin-ASCII") |> 
           str_remove_all("'") |>
           str_replace_all(" ","-")) |> 
    select(-subregion)

# Take a look at the data in `cartefrance` using `glimpse()`
glimpse(cartefrance)

# Load the data in the file "Data/fr-esr-parcoursup.xlsx", and
# - rename the "Département de l’établissement" to "region"
# - transform the region names to lower case and remove accents and spaces.
# - remove the single quotes from the region names
# - replace the spaces with a dash
psup <- read_csv2("Data/fr-esr-parcoursup.csv") |> 
    rename(region = `Département de l’établissement`) |> 
    mutate(region = region |>
            stri_trans_general("lower;Latin-ASCII") |> 
            str_remove_all("'") |>
            str_replace_all(" ","-")
          )

# Take a look at the data with `glimpse()`
glimpse(psup)

# We want to compare, as a function of the years:
# - the rate of baccalaureate holders with honours. 
# - the rate of girls and boys accepted onto a course.
# - girls with honours in their baccalauréat accepted into a selective course.

# # # # # # # # # # # # # # # # # # 

# Let's start with the rate of baccalaureate holders with honours.
# We need the following columns: 
# "Session", "region", "Effectif des admis néo bacheliers",
# "Dont effectif des admis néo bacheliers avec mention Très Bien avec félicitations au bac", 
# "Dont effectif des admis néo bacheliers avec mention Très Bien au bac"
# 
# We will rename the columns to:
# "year", "region", "admis", "dont_tb", "dont_tbf"
# then we will:
# - calculate the proportion of baccalaureate holders with honours
# - and summarize the results by region and year

bacHonours <- 
    psup |> 
    select(Session, region,
           `Effectif des admis néo bacheliers`,
           `Dont effectif des admis néo bacheliers avec mention Très Bien avec félicitations au bac`,
           `Dont effectif des admis néo bacheliers avec mention Très Bien au bac`) |> 
    rename(year = Session,
           admis = `Effectif des admis néo bacheliers`,
           dont_tb = `Dont effectif des admis néo bacheliers avec mention Très Bien avec félicitations au bac`,
           dont_tbf = `Dont effectif des admis néo bacheliers avec mention Très Bien au bac`) |> 
    mutate(dont_tb = as.numeric(dont_tb)) |> 
    summarise(.by = c(region,year), 
              admis = sum(admis),
              dont_tb = sum(dont_tb),
              dont_tbf = sum(dont_tbf)) |>
    mutate(prop = (dont_tb+dont_tbf)/admis*100)

# Now we want to plot the results on a map of France.
# Combine the data from `cartefrance` and `bacHonours` using `inner_join()`

carte_bacHonours <- bacHonours |> inner_join(cartefrance)

# Now we can plot the results on a map of France:
carte_bacHonours |> 
    ggplot(aes(long,lat, group=group, fill=prop))+
        geom_polygon()+
        coord_map() +
        scale_fill_distiller(palette=16, direction=1, limits=c(0,NA))+
        facet_wrap(~year)+
        theme_void()+
        labs(fill="% des admis\navec mention TB")+
        theme(strip.text = element_text(face = "bold", size=20))

# # # # # # # # # # # # # # # # # # 

# Now let's look at the rate of girls and boys accepted onto a course.
# We need the following columns:
# "Session", "region", `Effectif total des candidats ayant accepté la proposition de l’établissement (admis)` , `Dont effectif des candidates admises`
# We will rename the columns to:
# "year", "region", "admis", "girls"
# then we will:
# - calculate the proportion of girls accepted onto a course
# - and summarize the results by region and year

girlsBoys <- 
    psup |> 
    select(Session, region,
           `Effectif total des candidats ayant accepté la proposition de l’établissement (admis)`,
           `Dont effectif des candidates admises`) |> 
    rename(year = Session,
           admis = `Effectif total des candidats ayant accepté la proposition de l’établissement (admis)`,
           girls = `Dont effectif des candidates admises`
           ) |> 
    summarise(.by = c(region,year), 
              admis = sum(admis),
              girls = sum(girls)) |>
    mutate(prop = girls/admis * 100)

# Now we want to plot the results on a map of France.
# Combine the data from `cartefrance` and `girlsBoys` using `inner_join()`

carte_girlsBoys <- girlsBoys |> inner_join(cartefrance)

# Now we can plot the results on a map of France:
carte_girlsBoys |> 
    ggplot(aes(long,lat, group=group, fill=prop))+
        geom_polygon()+
        coord_map() +
        scale_fill_distiller(palette=13, direction=1)+
        facet_wrap(~year)+
        theme_void()+
        labs(fill="% de filles\nadmises")+
        theme(strip.text = element_text(face = "bold", size=20))

