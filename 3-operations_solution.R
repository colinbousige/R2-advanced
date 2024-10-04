# # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 1
# # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# Load the `"Data/population.csv"` file into a `tibble` called `popul`.

library(tidyverse)
popul <- read_csv("Data/population.csv")

# What are the names of the columns? What's the dimension of the table ?

popul |> names()
popul |> dim()
popul |> glimpse()

# Are the data tidy? make the table tidy if needed

popul
popul.tidy <- popul |> 
    pivot_longer(
        cols     = -year, # what are the columns we want to keep? -> -these
        names_to = 'city', # name of the column gathering the original column names
        values_to= 'pop'  # name of the column gathering the original column values
        )

# Create a subset containing the data for Montpellier using a [filtering](https://dplyr.tidyverse.org/reference/filter.html) function from the `tidyverse`.

mtp <- popul.tidy |> filter(city == "Montpellier")

# What is the max and min of population in this city?
range(mtp$pop)

# The average population over time?
mean(mtp$pop)

# What is the total population over all cities in 2012?

popul.tidy |> 
    filter(year==2012) |>         # You need to filter the data for the year 2012
    select(pop) |>         # Then select the right column
    sum()

popul.tidy |> 
    filter(year==2012) |>         # You need to filter the data for the year 2012
    summarise(total = sum(pop))

# What is the total population per year?

popul.tidy |> 
    group_by(year) |>    # You need to group data per year
    summarise(total = sum(pop))        # Then summarize the data of each year as 
               # the total population of each group
popul.tidy |> 
    summarise(.by = year,
              total = sum(pop))

# What is the average population per city over the years?

popul.tidy |>
    group_by(city) |>  # You need to group data per...?
    summarise(average = mean(pop))      # Then...?


# # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 2
# # # # # # # # # # # # # # # # # # # # # # # # # # 

# First, load the `tidyverse` and `lubridate` packages
library(tidyverse)
library(lubridate)

# Load `"Data/people1.csv"` and `"Data/people2.csv"` into `pp1` and `pp2`, and take a look at them.

pp1 <- read_csv("Data/people1.csv")
pp2 <- read_csv("Data/people2.csv")

# Create a new tibble `pp` by using the pipe operator (`|>`) and successively:
# - joining the two tibbles into one using `inner_join()`
# - adding a column `age` containing the age in years (use `lubridate::time_length(x, 'years')` with x a time difference in days) by using `mutate()`

pp <- pp1 |>
    inner_join(pp2) |>       # you need to join with pp2
    mutate(age = time_length(today()-dateofbirth, 'years'))   # then add a column `age` computing the right thing
pp

# Display a summary of the table using `glimpse()`
glimpse(pp)

# Using `group_by()` and `summarize()`:
# - Show the number of males and females in the table (use the counter `n()`)
pp |>
    group_by(gender) |>
    summarise(n = n())
# - Show the average age per gender
pp |>
    group_by(gender) |>
    summarise(mean_age = mean(age))
# - Show the average size per gender and institution
pp |>
    group_by(gender, institution) |>
    summarise(mean_size = mean(size))
# See the difference with this? 
pp |>
    summarise(.by = c(gender, institution),
              mean_size = mean(size))
# the grouping has disappeared in the output tibble

# - Show the number of people from each country, sorted by descending population
pp |>
    group_by(origin) |>
    summarise(n = n()) |>
    arrange(desc(n))

# Using `select()`, display:
# - only the name and age columns
pp |> select(name, age)
# - all but the name column
pp |> select(-name)


# Using `filter()`, show data only for:
# - Chinese people
pp |> filter(origin == "China")
# - From institution ECL and UCBL
pp |> filter(institution %in% c("ECL", "UCBL"))
# - People older than 22
pp |> filter(age > 22)
# - People with a `e` in their name
pp |> filter(str_detect(name, "e"))


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

flist <- list.files(pattern = 'sample', path = "Data", full.names = TRUE)

tib <- read_csv(flist,           # what do we want to read? give the vector of file names
                id = 'file') |>  # what is the name of the column containing the file names ?
        mutate(file=basename(file))            # modify this column so that it contains just the file
                               # name and not the full path


### Operations on strings

# We also want to get information from our file names, such as the sample number, the temperature, the time, and the time unit. 
# Use the function [`separate()`](https://tidyr.tidyverse.org/reference/separate.html) to split the `file` column into `sample`, `T`, `time` and `time_unit`. 
# If applicable, make sure that the resulting columns are numeric by getting rid of the annoying characters.
# Look into the `stringr` cheat sheet for help: https://github.com/rstudio/cheatsheets/blob/main/strings.pdf

tib <- tib |> 
    separate(col = file, 
             into = c('sample','T','time',NA)) |> 
    mutate(sample = sample |> str_remove('sample') |> as.numeric(),
           T = T |> str_remove('K') |> as.numeric(),
           time_unit = time |> str_remove_all('[:digit:]'),
           time = time |> str_remove_all("[:alpha:]") |> as.numeric()
           )
tib

# Now we want all times to be in the same unit. Using `mutate()` and `ifelse()`, convert the minutes in seconds, then get rid of the `time_unit` column.

tib <- tib |> 
    mutate(time = ifelse(time_unit=='min', time*60, time)) |> # convert minutes to seconds
    select(-time_unit) # get rid of the `time_unit` column
tib







