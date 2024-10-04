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

# Load the data in the file "Data/PISA2015_TauxReussite_SESCgp.xlsx"
# And also clean the names using `janitor::clean_names()`
pisa <- read_excel("Data/PISA2015_TauxReussite_SESCgp.xlsx")

# Make the table tidy with `pivot_longer()` : there should be 4 columns: "items", "location", "group", "success_rate"
pisa_tidy <- pisa |> 
    pivot_longer(cols = -ITEMS, 
                 names_to = c("location", "group"), 
                 names_sep = "_SESCgp",
                 values_to = "success_rate")

# Make a density plot of the success rates, with a different color for each group, and a different facet for each location
pisa_tidy |> 
    ggplot(aes(x = success_rate, group = group, fill=group)) +
        geom_density(alpha=.3, bw=.05) +
        facet_wrap(~location, nrow=2)+
        scale_fill_brewer(palette = "Spectral")

# Make a boxplot of the success rates, with a different color for each group, and a different facet for each location
pisa_tidy |> 
    ggplot(aes(x=group, y = success_rate, fill=group, color=group)) +
        geom_boxplot(alpha=.5, notch = TRUE, outlier.size = 5) +
        geom_jitter(width = .1) +
        coord_cartesian(ylim = c(0, 1)) +
        facet_wrap(~location, nrow=1)+
        scale_fill_brewer(palette = "Spectral")+
        scale_color_brewer(palette = "Spectral") +
        theme(legend.position = "none")

# Make a boxplot of the success rates, with a different color for each location, and a different facet for each group
pisa_tidy |> 
    ggplot(aes(x=location, y = success_rate, fill=location, color=location)) +
        geom_boxplot(alpha=.5, notch = TRUE) +
        geom_jitter(width = .2) +
        coord_cartesian(ylim = c(0, 1)) +
        facet_wrap(~glue("Group {group}"))+
        scale_fill_manual(values = c('royalblue', 'orange'))+
        scale_color_manual(values = c('royalblue', 'orange'))+
        theme(legend.position = "none")

# Make a table with the results of a Shapiro-Wilk test for each group and location, and a column indicating if the success rates are Gaussian
pisa_tidy |> 
    nest(data = -c(group, location)) |> 
    mutate(shapiro = map(data, ~shapiro.test(.$success_rate)),
           tidied = map(shapiro, tidy)) |> 
    unnest(tidied) |> 
    mutate(gaussian = p.value > .05)

# Make a table with the results of a t-test for each group comparing the difference between locations, and a column indicating if the success rates are statistically different
pisa_tidy |> 
    nest(data = -group) |> 
    mutate(ttest = map(data, ~t.test(success_rate ~ location, data = .)),
           tidied = map(ttest, tidy)) |> 
    unnest(tidied) |> 
    mutate(statistically_different = p.value < .05)


for(loc in c("fra","ocde")){
for(g1 in 1:4){
for(g2 in 1:4){
    if(g1==g2) next
    d <- pisa_tidy |> 
        filter(location == loc, group %in% c(g1,g2)) |>
        t.test(success_rate ~ group, data = .) |> 
        tidy() |>
        mutate(statistically_different = p.value < .05)
    print(glue("{loc}: Group {g1} has different mean than Group {g2} : {d$statistically_different}"))
}}}


