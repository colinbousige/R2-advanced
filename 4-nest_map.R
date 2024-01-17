# Just for Colin, do not run:
# server = livecode::serve_file()


library(tidyverse)

distribs <- list(A = runif(10, -10, 10), 
                 B = runif(100, -10, 10), 
                 C = runif(1000, -10, 10),
                 D = runif(10000, -10, 10))

# How would you compute the mean of each distribution?
# - Using a for loop


# - Using sapply()


# - Using map() or map_dbl()


# This can be done in the tidyverse using mutate()
distribs %>% as_tibble_col() %>% 
    mutate(name = names(distribs),
           mean = ___)

# Now, what if:
distribs$A <- c(distribs$A, NA)
# ?
# Does the above code still work? How do we make it work like we want?


# map() and map_xxx() are equivalent to lapply() and sapply(), but they are more flexible:
# - they can be used with functions that return lists or vectors
# - they can be used with functions that take several arguments
# - they can be used with functions that take arguments in a different order
# - they can be used with functions that take arguments as named arguments


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Let's load the "Data/measurements.csv" file into a tibble called `measurements`.


# Nest the data by `sample` and `operator` using `nest()`.


# Compute the correlation between `data$x` and `data$y` for each group using `mutate()` and `map()`.


# We could do the same using `summarise()`



# This becomes useful when we want to perform more complicated stuff. 
# For example, let's fit a linear model to each group and extract the coefficients and the fitted parameters using `broom::tidy()` and `broom::augment()`:
library(broom)
measurements %>% 
    ___





# # # # # # # # # # # # # # # # # # # 
# Full example
# # # # # # # # # # # # # # # # # # # 

# Let's read all the files in the `Data/fits` folder. They contain distributions of observed carbon nanotube sizes as a function of temperature and time.

flist <- list.files(___)

# In a single pipe workflow:
# - read all the files in the `Data/fits` folder. 
# - nest the tibble by filename
# - The files contain distributions of observed carbon nanotube sizes as a function of temperature and time: retrieve these informations from the file names. 
# - Get the average size and standard deviation for each file
# - nest the data by temperature
# - then perform the linear fit of the average size as a function of time for each temperature. 


data <- read_csv(___, id=___, col_name='L') %>% 
    ___


# Plot the result of the fits on the same graph as the data.



# Now plot the slope as a function of temperature. It follows an Arrhenius law (a*exp(-Ea/T)). Fit this law to the data and extract the activation energy. Plot the fit on the same graph as the data.
