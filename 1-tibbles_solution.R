library(tidyverse)

# Create a 3 column `tibble` called `df` containing three columns `x`, `y` and `z` with:

# - `x` a vector from -pi to pi of length 10
# - `y` the sinus of `x`
# - and `z` the sum of the two first columns.

df <- tibble(
        x = seq(-pi,pi, length=10),# you define the column x
        y = sin(x),# you define the column y by referring tho the column x previously defined
        z = x+y)   # you define the column z by referring tho the columns x and y previously defined
df

# /!\ 
# With a tibble you could do all this in a single call, but NOT with a data.frame
data.frame(x = seq(-10,10), # you define the column x
           y = cos(x), # here to define the column y you refer to 
                       # the vector x that was created outside the call to data.frame
           z = x*y) # here to define the column z you refer to the vectors 
                    # x and y that were created outside the call to data.frame)
tibble(x = seq(-10,10),# you define the column x
       y = cos(x),# you define the column y by referring tho the column x previously defined
       z = x*y)   # you define the column z by referring tho the columns x and y previously defined

# Print the 4 first lines of the table df.
# Hint: Take a look at the head() function
df |> head(4)

# Print the second (*i.e.* `y`) column with 7 different methods.
# Hint: Take a look at the `$` operator, the `[[` operator, the `select()` function, and the `pull()` function.
df$y
df[,2]
df[,'y']
df[['y']]
df[[2]]
df |> select(y)
df |> pull(y)

# Modify the column `z` so that it contains its value minus its minimum using 2 different methods.
# Hint: Take a look at the `$` operator and the `mutate()` function.
df$z <- df$z - min(df$z)
df <- df |> mutate(z = z - min(z))

# Print the average of the `z` column.
mean(df$z)
df |> summarize(mean(z))

# Using `plot(x,y)` where `x` and `y` are vectors, plot the 2nd column as a function of the first.
plot(df$x, df$y)
# Do the same with ggplot2
df |> 
    ggplot(aes(x,y)) + 
    geom_point()

# Look into the function `write_csv()` to write a text file containing this `tibble`. Compare it to the `write.csv()` function.

write_csv(df, "df.csv")


