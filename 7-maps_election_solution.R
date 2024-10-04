# Load packages
library(tidyverse)
library(readxl)
library(janitor)
library(stringi)
library(patchwork)
theme_set(theme_void())

# Let's work on the data from the 2nd round of the 2017 French presidential election located in `Data/Presidentielle_2017_Resultats_Communes_Tour_2.xls`
# The data can be found here: https://www.data.gouv.fr/fr/datasets/election-presidentielle-des-23-avril-et-7-mai-2017-resultats-du-2eme-tour-2/

# First, take a look at the data, then load them in a tibble called `elections`
# Then:
# - rename the column `Libellé du département` to `region` using `rename()`,
# - using `stringi::stri_trans_general()` transform the region names to lower case and remove accents and spaces.
# - remove the single quotes from the region names using `stringr::str_remove_all()`
# - replace the spaces with a dash using `stringr::str_replace_all()`

elections <- read_excel("Data/Presidentielle_2017_Resultats_Communes_Tour_2.xls",
                        skip=3) |> 
    rename(region=`Libellé du département`) |> 
    mutate(region = region |>
                stri_trans_general("lower;Latin-ASCII") |> 
                str_remove_all("'") |>
                str_replace_all(" ","-"))


# Let's summarize the results for the whole country:
total_results <- elections |> 
    summarise(tot_votant               = sum(Votants), 
              tot_blanc                = sum(Blancs),
              tot_exprimes             = sum(`Exprimés`), 
              tot_inscrits             = sum(Inscrits),
              tot_nul                  = sum(Nuls),
              tot_abs                  = sum(Abstentions), 
              tot_macron               = sum(Voix...22),
              tot_lepen                = sum(Voix...28),
              pourcent_macron_exprimes = tot_macron / tot_exprimes * 100, 
              pourcent_lepen_exprimes  = tot_lepen / tot_exprimes * 100,
              pourcent_macron          = tot_macron / tot_inscrits * 100,
              pourcent_lepen           = tot_lepen / tot_inscrits * 100,
              pourcent_blanc           = (tot_blanc+tot_nul) / tot_inscrits * 100, 
              pourcent_abstention      = tot_abs / tot_inscrits* 100
              ) |> 
    mutate(gagnant = ifelse(pourcent_macron > 50, 'Macron', 'Le Pen'))

# Now we can make a pie chart of the results, after making the previous table tidy:
# Make sure the data for Macron are labelled "E. Macron" and for Le Pen "M. Le Pen"
# We will make two pie charts, one with the numbers with respect to the total number of valid votes, and one with respect to the total number of registered voters.

# numbers with respect to the total number of valid votes
results_exprimes <- total_results |> 
    select(pourcent_macron_exprimes,pourcent_lepen_exprimes) |> 
    pivot_longer(cols=everything(), 
                 names_to='candidat', 
                 values_to='pourcent',
                 names_prefix = "pourcent_") |>
    mutate(candidat = candidat |> str_replace_all("macron_exprimes", 'E. Macron')) |>
    mutate(candidat = candidat |> str_replace_all("lepen_exprimes", 'M. Le Pen'))
# numbers with respect to the total number of registered voters
results_absolu <- total_results |> 
    select(starts_with('pourcent')) |> 
    pivot_longer(cols=everything(), 
                 names_to='candidat', 
                 values_to='pourcent',
                 names_prefix = "pourcent_") |>
    filter(!candidat |> str_detect('exprimes')) |>
    mutate(candidat = candidat |> str_to_title()) |>
    mutate(candidat = candidat |> str_replace_all("Macron", 'E. Macron')) |>
    mutate(candidat = candidat |> str_replace_all("Lepen", 'M. Le Pen'))

# Now we can make a pie chart of the results
absolu <- results_absolu |>
    ggplot(aes(x = "", y = pourcent, fill = candidat))+
        geom_bar(stat="identity", width=1)+
        coord_polar("y")+
        theme(legend.position="bottom")+
        scale_fill_manual(values=c('Abstention'='lightgrey', 
                                   'Blanc'='darkgrey', 
                                   'M. Le Pen'='royalblue', 
                                   'E. Macron'='orange'))+
        labs(subtitle="Pourcentages par rapport aux inscrits sur liste électorale", 
             fill=NULL, 
             x="", y="")+
        theme(plot.title = element_text(hjust = 0.5, size=25),
              legend.position = 'none')+
        geom_text(aes(label = glue::glue("{candidat}\n{round(pourcent,1)} %")),
            position = position_stack(vjust = 0.5), col='white', size=8)
exprimes <- results_exprimes |>
    ggplot(aes(x = "", y = pourcent, fill = candidat))+
        geom_bar(stat="identity", width=1)+
        coord_polar("y")+
        theme(legend.position="bottom")+
        scale_fill_manual(values=c('M. Le Pen'='royalblue', 
                                   'E. Macron'='orange'))+
        labs(subtitle="Pourcentages par rapport votes exprimés", 
             fill=NULL, 
             x="", y="")+
        theme(plot.title = element_text(hjust = 0.5, size=25),
              legend.position = 'none')+
        geom_text(aes(label = glue::glue("{candidat}\n{round(pourcent,1)} %")),
            position = position_stack(vjust = 0.5), col='white', size=8)
exprimes + absolu +
    plot_annotation(title="Résultats du 2nd tour de l'élection présidentielle 2017")


library(waffle)
total_results |> 
    select(starts_with('tot')) |> 
    pivot_longer(cols=everything(), 
                 names_to='candidat', 
                 values_to='total',
                 names_prefix = "tot_") |>
    mutate(candidat = candidat |> str_to_title()) |> 
    mutate(total = total/100000) |>
    filter(candidat%in%c('Macron','Lepen','Nul','Blanc')) |>
    ggplot(aes(fill = candidat, values = total)) +
    geom_waffle(n_rows = 20,
                size = 0.33, 
                colour = "white",
                flip = TRUE) +
    coord_equal() +
    theme_enhance_waffle()



# Now we want to plot the results on a map of France.
# We will use the `ggplot2` package and the `maps` package, both loaded with the `tidyverse` package.
# Transform the data from `cartefrance` to a tibble, then:
# - remove the `subregion` column using `select()`
# - perform the same string modifications as before on the region column

cartefrance <- map_data('france') |> 
    as_tibble() |> 
    mutate(region = region |>
           stri_trans_general("lower;Latin-ASCII") |> 
           str_remove_all("'") |>
           str_replace_all(" ","-")) |> 
    select(-subregion)

# Take a look at the data in `cartefrance` and `elections` using `glimpse()`
elections |> glimpse()
cartefrance |> glimpse()

# Now we want a usable summary of the results.
# We will use the `summarise()` and `mutate()` functions to calculate:
# - the total number of voters, 
# - the total number of blank votes, 
# - the percentage of blank votes, 
# - the total number of abstentions, 
# - the percentage of abstentions, 
# - the total number of votes for Macron,
# - the total number of votes for Le Pen, 
# - the percentage of votes for Macron, 
# - the percentage of votes for Le Pen, 
# - and the winner of the election.
results <- elections |> 
    summarise(.by               = region,
              tot_inscrits      = sum(Inscrits),
              tot_votants       = sum(Votants), 
              tot_exprimes      = sum(`Exprimés`), 
              tot_blanc         = sum(Blancs),
              tot_nul           = sum(Nuls),
              tot_abs           = sum(Abstentions), 
              tot_macron        = sum(Voix...22),
              tot_lepen         = sum(Voix...28),
              pourcent_blancnul = (tot_blanc+tot_nul) / tot_votants * 100, 
              pourcent_abs      = tot_abs / tot_inscrits * 100,
              pourcent_macron   = tot_macron / tot_exprimes * 100, 
              pourcent_lepen    = tot_lepen / tot_exprimes * 100) |> 
    mutate(gagnant = ifelse(pourcent_macron > 50, 'Macron', 'Le Pen'))

# Now let's join the results with the map data.
carte_results <- results |> inner_join(cartefrance)

# Now we can plot the results on a map of France, you can play with what you want to show.

carte_results |> 
    ggplot(aes(long,lat, group=group, fill=gagnant))+
        geom_polygon()+
        coord_map() +
        scale_fill_manual(values=c('royalblue', 'orange'))

macron <-carte_results |> 
    ggplot(aes(long,lat, group=group, fill=pourcent_macron))+
        geom_polygon()+
        coord_map() +
        scale_fill_distiller(palette=13, direction=1)+
        labs(title="E. Macron", fill="% des votes\nexprimés")
lepen <- carte_results |> 
    ggplot(aes(long,lat, group=group, fill=pourcent_lepen))+
        geom_polygon()+
        coord_map() +
        scale_fill_distiller(palette=1, direction=1)+
        labs(title="M. Le Pen", fill="% des votes\nexprimés")
abstention <- carte_results |> 
    ggplot(aes(long,lat, group=group, fill=pourcent_abs))+
        geom_polygon()+
        coord_map() +
        scale_fill_distiller(palette=2, direction=1)+
        labs(title="Abstention", fill="% des\ninscrits")
macron+lepen+abstention & theme(plot.title = element_text(hjust = 0.5, size=25))
