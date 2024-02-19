library(tidyverse)
library(broom)

distribs <- list(A = runif(10, -10, 10), 
                 B = runif(100, -10, 10), 
                 C = runif(1000, -10, 10),
                 D = runif(10000, -10, 10))

# How would you compute the mean of each distribution?
# - Using a for loop:
means <- c()
for (d in distribs) {
    means <- c(means, mean(d))
}
names(means) <- names(distribs)
means

# - Using sapply()
sapply(distribs, mean)

# - Using map() or map_dbl()
map(distribs, mean)
map_dbl(distribs, mean)

# This can be done in the tidyverse using mutate()
distribs %>% as_tibble_col() %>% 
    mutate(name = names(distribs),
           mean = map(value, mean)) %>% 
    unnest(mean)
distribs %>% as_tibble_col() %>% 
    mutate(name = names(distribs),
           mean = map_dbl(value, mean))

# what if:
distribs$A <- c(distribs$A, NA)
# ?
# Does the above code still work? How do we make it work like we want?
distribs %>% as_tibble_col() %>% 
    mutate(name = names(distribs),
           mean = map_dbl(value, mean, na.rm = TRUE))
distribs %>% as_tibble_col() %>% 
    mutate(name = names(distribs),
           mean = map_dbl(value, ~mean(na.rm = TRUE, x=.)))

# map() and map_xxx() are equivalent to lapply() and sapply(), but they are more flexible:
# - they can be used with functions that return lists or vectors
# - they can be used with functions that take several arguments
# - they can be used with functions that take arguments in a different order
# - they can be used with functions that take arguments as named arguments


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Let's load the "Data/measurements.csv" file into a tibble called `measurements`.
measurements <- read_csv("Data/measurements.csv")
# Nest the data by `sample` and `operator` using `nest()`.
measurements %>% 
    nest(data = -c(sample, operator))
# Compute the correlation between `data$x` and `data$y` for each group using `mutate()` and `map()`.
measurements %>% 
    nest(data = -c(sample, operator)) %>% 
    mutate(cor = map_dbl(data, ~cor(.$x, .$y)))

# We could do the same using `summarise()`
measurements %>% 
    summarise(cor = cor(x, y), .by=c(sample, operator))

# This becomes useful when we want to perform more complicated stuff. 
# For example, let's fit a linear model to each group and extract the coefficients and the fitted parameters using `broom::tidy()` and `broom::augment()`:
library(broom)
measurements %>% 
    nest(data = -c(sample, operator)) %>% 
    mutate(fit = map(data, ~lm(data=., x~ y)),
           tidied = map(fit, tidy),
           augmented = map(fit, augment)
           )




# # # # # # # # # # # # # # # # # # # 
# Full example
# # # # # # # # # # # # # # # # # # # 

# Let's read all the files in the `Data/fits` folder. They contain distributions of observed carbon nanotube sizes as a function of temperature and time.

# In a single pipe workflow:
# - read all the files in the `Data/fits` folder. 
# - nest the tibble by filename
# - The files contain distributions of observed carbon nanotube sizes as a function of temperature and time: retrieve these informations from the file names. 
# - Get the average size and standard deviation for each file
# - nest the data by temperature
# - then perform the linear fit of the average size as a function of time for each temperature. 

flist <- list.files(path="Data/fits", 
                    pattern = ".csv", 
                    full.names = TRUE)

data <- read_csv(flist, id='file', col_name='L') %>% 
    mutate(file=basename(file)) %>% 
    nest(data = L) %>% 
    separate(file, c('temperature', 'time', NA)) %>% 
    mutate(temperature = temperature %>% str_remove('K') %>% as.numeric(),
           time = time %>% str_remove('sec') %>% as.numeric()) %>% 
    mutate(Lm = map_dbl(data, ~mean(.$L)),
           dL = map_dbl(data, ~sd(.$L))) %>% 
    select(-data) %>%
    nest(data = -temperature) %>% 
    mutate(fit = map(data, ~lm(data=., Lm~ time, weights = 1/dL^2)),
           tidied = map(fit, tidy),
           augmented = map(fit, augment))

data %>% unnest(tidied)


# Plot the result of the fits on the same graph as the data.



data %>% unnest(augmented) %>% 
    ggplot(aes(x = time, y = Lm, color=factor(temperature), group=temperature)) +
        geom_point(alpha=0.5, size=5) +
        geom_errorbar(aes(ymin = Lm - sqrt(1/`(weights)`), ymax = Lm + sqrt(1/`(weights)`))) +
        geom_line(aes(y = .fitted), alpha=0.5) +
        theme_bw()

# Now plot the slope as a function of temperature. It follows an Arrhenius law (a*exp(-Ea/T)). Fit this law to the data and extract the activation energy. Plot the fit on the same graph as the data.

fitlinear <- data %>% 
    unnest(tidied) %>% 
    filter(term=='time') %>% 
    mutate(invt = 1/temperature) %>% 
    lm(data=., log(estimate) ~ invt)

curvefit <- tibble(temperature = seq(800,1000,1),
                   estimate = exp(predict(fitlinear, tibble(invt = 1/temperature))))
data %>% 
    unnest(tidied) %>% 
    filter(term=='time') %>% 
    ggplot(aes(x = temperature, y = estimate)) +
        geom_point(alpha=0.5, size=5)+
        geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width=0) +
        geom_line(data=curvefit, col='red') +
        theme_bw()
