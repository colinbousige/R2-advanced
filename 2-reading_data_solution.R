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

rubis_01   <- read_table("Data/rubis_01.txt", col_names = c("w", "intensity"))
population <- read_csv("Data/population.csv")
FTIR_rocks <- readxl::read_excel("Data/FTIR_rocks.xlsx")


# Print their dimensions and column names. 

# Dimensions
rubis_01 %>% dim()
population %>% dim()
FTIR_rocks %>% dim()
# Names
rubis_01 %>% names()
population %>% names()
FTIR_rocks %>% names()

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
                skip=12,
                col_names=c("Index","t","Tset","Tread","Mass"), 
                n_max=4088)
d


# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 3
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# - Using list.files(), find all the files in the "Data" folder that have the "sampleX_tempK_time-UNIT.csv" pattern.
# - Read them all in a tidy tibble called `tib`.
# - Make sure to add a column named `"file"` containing the list of filenames: look at the `id` parameter 

flist <- list.files(path='Data', pattern='sample', full.names = TRUE)
tib <- read_csv(flist,           # what do we want to read? give the vector of file names
                id = 'file')
tib

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 4
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # 

# We want to do a similar exercise with the files in the "Data" folder that have the "rubis_XX.txt" pattern. These files have two columns, `w` and `intensity`, and we want to read them in a single tidy tibble called `tib`.
# - What function do we need to use to read such file?
# - What parameters do we need to use to read such file to get the proper column names?
# - For starters, read them all in a tidy tibble called `tib` using a for loop
# - Then try to do the same using `lapply()` and `read_table()`
# - Then try to do the same using the tidyverse-friendly `purrr::map()` and `purrr::map_df()`

flist <- list.files(path='Data', pattern='rubis', full.names = TRUE)
# with for loop
tib <- tibble()
for (f in flist) {
    temp <- read_table(f, col_names = c("w", "intensity"))
    tib <- bind_rows(tib, temp)
}
tib

# with lapply()
tib <- lapply(flist, read_table, col_names = c("w", "intensity"))

# with purrr::map()
tib <- map(flist, read_table)

read_function <- function(filename){
    read_table(filename, col_names = c("w", "intensity")) %>% 
        mutate(file = basename(filename))
}
tib <- map_df(flist, read_function)

tib <- tibble(file = flist) %>% 
    mutate(data = map(file, read_table, col_names = c("w", "intensity")),
           file = basename(file)) %>% 
    unnest(data)
