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

elections <- ___

# Let's summarize the results for the whole country:
total_results <- elections %>% 
    summarise(tot_votant               = ___,
              tot_blanc                = ___,
              tot_exprimes             = ___,
              tot_inscrits             = ___,
              tot_nul                  = ___,
              tot_abs                  = ___,
              tot_macron               = ___,
              tot_lepen                = ___,
              pourcent_macron_exprimes = ___,
              pourcent_lepen_exprimes  = ___,
              pourcent_macron          = ___,
              pourcent_lepen           = ___,
              pourcent_blanc           = ___,
              pourcent_abstention      = ___
              ) %>% 
    mutate(gagnant = ifelse(pourcent_macron > 50, 'Macron', 'Le Pen'))

# Now we can make a pie chart of the results, after making the previous table tidy:
# Make sure the data for Macron are labelled "E. Macron" and for Le Pen "M. Le Pen"
# We will make two pie charts, one with the numbers with respect to the total number of valid votes, and one with respect to the total number of registered voters.

# numbers with respect to the total number of valid votes
results_exprimes <- total_results %>% 
    select(___) %>% 
    pivot_longer(___) %>%
    mutate(candidat = ___) # We will use this later to label the pie chart

# numbers with respect to the total number of registered voters
results_absolu <- total_results %>% 
    select(___) %>% 
    pivot_longer(___) %>%
    mutate(candidat = ___)

# Let's do the pie charts, just run the code:
absolu <- results_absolu %>%
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

exprimes <- results_exprimes %>% 
    select(pourcent_macron_exprimes,pourcent_lepen_exprimes) %>% 
    pivot_longer(cols=everything(), 
                 names_to='candidat', 
                 values_to='pourcent',
                 names_prefix = "pourcent_") %>%
    mutate(candidat = candidat %>% str_replace_all("macron_exprimes", 'E. Macron')) %>%
    mutate(candidat = candidat %>% str_replace_all("lepen_exprimes", 'M. Le Pen')) %>%
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



# Now we want to plot the results on a map of France.
# We will use the `ggplot2` package and the `maps` package, both loaded with the `tidyverse` package.
# Transform the data from `cartefrance` to a tibble, then:
# - remove the `subregion` column using `select()`
# - perform the same string modifications as before on the region column

cartefrance <- map_data('france') %>% 
    ___

# Take a look at the data in `cartefrance` and `elections` using `glimpse()`
___

# Now we want a usable summary of the results.
# We will use the `summarise()` and `mutate()` functions to calculate the same things as above, but for each region.
results <- elections %>% 
    summarise(___) %>%
    mutate(gagnant = ___)

# Now let's join the results with the map data.
carte_results <- ___

# Now we can plot the results on a map of France, you can play with what you want to show.

carte_results %>% 
    ggplot(aes(long,lat, group=group, fill=gagnant))+
        geom_polygon()+
        coord_map() +
        scale_fill_manual(values=c('royalblue', 'orange'))

# Other interesting maps...
___


