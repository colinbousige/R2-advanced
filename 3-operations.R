# Just for Colin, do not run:
# server = livecode::serve_file()

# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 1
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load the `"Data/population.csv"` file into a `tibble` called `popul`.

library(___)
popul <- read____("Data/population.csv")

# What are the names of the columns? What's the dimension of the table ?

popul

# Are the data tidy? make the table tidy if needed

popul
popul.tidy <- popul %>% 
    pivot_longer(
        cols     = ___, # what are the columns we want to keep? -> -these
        names_to = ___, # name of the column gathering the original column names
        values_to= ___  # name of the column gathering the original column values
        )

# Create a subset containing the data for Montpellier using a [filtering](https://dplyr.tidyverse.org/reference/filter.html) function from the `tidyverse`.

mtp <- popul.tidy %>% ___

# What is the max and min of population in this city?


# The average population over time?


# What is the total population over all cities in 2012?

popul.tidy %>% 
    ___ %>%         # You need to filter the data for the year 2012
    ___ %>%         # Then select the right column
    ___             # And perform the sum of its data

# What is the total population per year?

popul.tidy %>% 
    ___ %>%    # You need to group data per year
    ___        # Then summarize the data of each year as 
               # the total population of each group

# What is the average population per city over the years?

popul.tidy %>%
    ___ %>%  # You need to group data per...?
    ___      # Then...?


# # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 2
# # # # # # # # # # # # # # # # # # # # # # # # # # 

# First, load the `tidyverse` and `lubridate` packages
___
___

# Load `"Data/people1.csv"` and `"Data/people2.csv"` into `pp1` and `pp2`, and take a look at them.

pp1 <- read____(___)
pp2 <- read____(___)

# Create a new tibble `pp` by using the pipe operator (`%>%`) and successively:
# - joining the two tibbles into one using `inner_join()`
# - adding a column `age` containing the age in years (use `lubridate::time_length(x, 'years')` with x a time difference in days) by using `mutate()`

pp <- pp1 %>%
    ___ %>%       # you need to join with pp2
    mutate(___)   # then add a column `age` computing the right thing
pp

# Display a summary of the table using `glimpse()`


# Using `group_by()` and `summarize()`:
# - Show the number of males and females in the table (use the counter `n()`)
pp %>%
    ___ %>%
    ___
# - Show the average age per gender
pp %>%
    ___ %>%
    ___
# - Show the average size per gender and institution
pp %>%
    ___ %>%
    ___
# - Show the number of people from each country, sorted by descending population
pp %>%
    ___ %>%
    ___ %>%
    ___


# Using `select()`, display:
# - only the name and age columns
pp ___
# - all but the name column
pp ___


# Using `filter()`, show data only for:
# - Chinese people
pp ___
# - From institution ECL and UCBL
pp ___
# - People older than 22
pp ___
# - People with a `e` in their name
pp ___


# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 3
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Here we will see how to load many files at once with the `tidyverse` and how to perform some data wrangling.

### Loading the data

# We will work with the files whose paths are stored in the vector `flist`. These files are all `.csv` files containing two columns and a header. 

# !!!! We can use the fact that `read_csv()` accepts vectors as argument and read them all at once. 

# - Using list.files(), find all the files in the "Data" folder that have the "sampleX_tempK_time-UNIT.csv" pattern.
# - Read them all in a tidy tibble called `tib`.
# - Make sure to add a column named `"file"` containing the list of filenames: look at the `id` parameter 
# - Modify this `"file"` column so that it contains just the file name and not the full path – look at the `basename()` function.

flist <- list.files(____)

tib <- read_csv(___,           # what do we want to read? give the vector of file names
                id = ___) %>%  # what is the name of the column containing the file names ?
        mutate(___)            # modify this column so that it contains just the file
                               # name and not the full path


### Operations on strings

# We also want to get information from our file names, such as the sample number, the temperature, the time, and the time unit. 
# Use the function [`separate()`](https://tidyr.tidyverse.org/reference/separate.html) to split the `file` column into `sample`, `T`, `time` and `time_unit`. 
# If applicable, make sure that the resulting columns are numeric by getting rid of the annoying characters.
# Look into the `stringr` cheat sheet for help: https://github.com/rstudio/cheatsheets/blob/main/strings.pdf

tib <- tib %>% 
    separate(col = ___, # what is the column containing these informations
             into = ___, # vector of strings containing new column names (NA to drop a column)
             convert = ___) %>% # do we convert strings to numbers if applicable?
    mutate(sample = ___,
           T = ___,
           time_unit = ___,
           time = ___
           )
tib

# Now we want all times to be in the same unit. Using `mutate()` and `ifelse()`, convert the minutes in seconds, then get rid of the `time_unit` column.

tib <- tib %>% 
    mutate(time = ifelse(test, yes, no)) %>% # convert minutes to seconds
    select(___) # get rid of the `time_unit` column
tib

