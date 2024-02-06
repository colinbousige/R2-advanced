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
# We will use the `summarise()` and `mutate()` functions to calculate:
# - the total number of voters, 
# - the total number of blank and null votes, 
# - the total number of abstentions, 
# - the total number of votes for Macron,
# - the total number of votes for Le Pen, 
# - the percentage of abstentions, 
# - the percentage of blank votes, 
# - the percentage of votes for Macron, 
# - the percentage of votes for Le Pen, 
# - and the winner of the election.
results <- elections %>% 
    summarise(.by               = ___,
              tot_inscrits      = ___,
              tot_votants       = ___,
              tot_exprimes      = ___,
              tot_blanc         = ___,
              tot_nul           = ___,
              tot_abs           = ___,
              tot_macron        = ___,
              tot_lepen         = ___,
              pourcent_blancnul = ___,
              pourcent_abs      = ___,
              pourcent_macron   = ___,
              pourcent_lepen    = ___) %>%
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


# Let's summarize the results for the whole country:
total_results <- elections %>% 
    ____

# Now we can make a pie chart of the results, after making the previous table tidy:

total_results %>% 
    select(___) %>% 
    pivot_longer(___) %>%
    ___
    ggplot(aes(x="", y = pourcent, fill = candidat))+
        geom_bar(stat="identity", width=1)+
        coord_polar("y")