# Just for Colin, do not run:
# server = livecode::serve_file()

library(tidyverse)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## Exercise 1
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# We will work with 3 different files:
#     - "Data/rubis_01.txt"
#     - "Data/population.csv"
#     - "Data/FTIR_rocks.xlsx"

# Load them into separate `tibbles` using the `tidyverse` functions equivalent to `read.table()` and `read.csv()` -- and `readxl::read_excel()
# Make sure that the `rubis_01` tibble has `w` and `intensity` as column names.

rubis_01   <- ___("Data/rubis_01.txt")
population <- ___("Data/population.csv")
FTIR_rocks <- ___("Data/FTIR_rocks.xlsx")


# Print their dimensions and column names.

# Dimensions
rubis_01
population
FTIR_rocks
# Names
rubis_01
population
FTIR_rocks


# # # # # # # # # # # # # # # # # # # # # # # # # #
## Exercise 2
# # # # # # # # # # # # # # # # # # # # # # # # # #

# We will use the TGA data file `"Data/ATG.txt"`

# Load it into a `tibble`. Look into the options of [`read_table()`](https://www.rdocumentation.org/packages/readr/versions/1.3.1/topics/read_table) to get the proper data fields.
# Hints:
# - check how many lines you have to skip before reading
# - check how many lines you have to skip at the end of the file
# - you need to get the column names
# - you need to skip the line with the unit

d <- read_table("Data/ATG.txt",
                ___
                )
d


# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## Exercise 3
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# - Using list.files(), find all the files in the "Data" folder that have the "sampleX_tempK_time-UNIT.csv" pattern.
# - Read them all in a tidy tibble called `tib`.
# - Make sure to add a column named `"file"` containing the list of filenames: look at the `id` parameter

flist <- list.files(____)
tib <- read_csv(___,           # what do we want to read? give the vector of file names
                id = ___)



# # # # # # # # # # # # # # # # # # # # # # # # # # # # #
## Exercise 4
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

# We want to do a similar exercise with the files in the "Data" folder that have the "rubis_XX.txt" pattern. These files have two columns, `w` and `intensity`, and we want to read them in a single tidy tibble called `tib`.
# - What function do we need to use to read such file?
# - What parameters do we need to use to read such file to get the proper column names?
# - For starters, read them all in a tidy tibble called `tib` using a for loop
# - Then try to do the same using `sapply()` and `read_table()`
# - Then try to do the same using the tidyverse-friendly `purrr::map()`

flist <- list.files(____)
