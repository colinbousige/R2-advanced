library(tidyverse)
library(patchwork)
theme_set(theme_bw())

# # # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 1
# # # # # # # # # # # # # # # # # # # # # # # # # # 

# We will work with the well known table `mtcars` included in R:

?mtcars


### Basic stuff 1

# Modify the following code to add a color depending on the `gear` column:
mtcars |> # we work on the mtcars dataset, send it to ggplot
    # define the x and y variables of the plot, and also the color:
    ggplot(aes(x = wt, y = mpg, col=gear))+ 
        geom_point() # plot with points

# Now change the color and shape of the points, and add transparency
mtcars |> 
    ggplot(aes(x = wt, y = mpg, col=gear))+ 
        geom_point(shape=17, alpha=0.5, size=5)+
        scale_color_distiller(palette="Set1")

### Basic stuff 2

# What happens if you use `factor(gear)` instead?

P <- mtcars |> # we work on the mtcars dataset, send it to ggplot
    # define the x and y variables of the plot, and also the color:
    ggplot(aes(x = wt, y = mpg, color = factor(gear)))+ 
        geom_point(size=5) # plot with points

### Tuning the plot
# Using the previously defined plot P:
# Add nice labels : 
# - wt = Weight (1000 lbs)
# - mpg = Miles/(US) gallon
# - gear = Number of forward gears
# - title : add a title
# Set colors to :
# - ones you define yourself
# - ones you define from the palette 'Set1' from scale_color_brewer()
# Force the x and y ranges to reach 0
# Reorganise the gears in descending order in the legend

P + 
    labs(x="Weight (1000 lbs)", 
         y="Miles/(US) gallon", 
         color="Number of forward gears", 
         title="My plot") +
    scale_color_manual(values=c("black",'red','orange')) +
    coord_cartesian(xlim=c(0,NA), ylim=c(0,NA)) +
    guides(color=guide_legend(reverse=TRUE))

P + 
    labs(x="Weight (1000 lbs)", 
         y="Miles/(US) gallon", 
         color="Number of forward gears", 
         title="My plot") +
    scale_color_brewer(palette="Set1") +
    coord_cartesian(xlim=c(0,NA), ylim=c(0,NA)) +
    guides(color=guide_legend(reverse=TRUE))

### Faceting 1

# Modify the following code to place each `carb` in a different facet. Also add a color, but remove the legend.
mtcars |> # we work on the mtcars dataset, send it to ggplot
    ggplot(aes(x = wt, y = mpg, color=factor(carb)))+ # define the x and y variables of the plot, and also the color
        geom_point(size=5) +   # plot with points
        facet_wrap(~carb) + # add a faceting
        theme(legend.position = 'none')       # remove the legend


### Faceting 2

# Modify the following code to arrange `mpg` vs `wt` plots on a grid showing `gear` vs `carb`. Also add a color depending on `cyl`. Also, try adding a free `x` scale range, or a free `y` scale range, or free `x` and `y` scale ranges.
mtcars |> # we work on the mtcars dataset, send it to ggplot
    ggplot(aes(x = wt, y = mpg, color=factor(cyl)))+ # define the x and y variables of the plot, and also the color
        geom_point(size=5) +   # plot with points
        facet_grid(gear ~ carb, scales = 'free') # add a faceting


# # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 2
# # # # # # # # # # # # # # # # # # # # # # # # # 

# We will look at data loaded into `df`. 
df <- read_table("Data/exo_fit.txt")
df

# Using `ggplot`, plot `y` as a function of `x` with points and save it into `Py`:

Py <- df |> 
    ggplot(aes(x = x, y = y))+ 
        geom_point()
Py

# Add a straight line in `Py` resulting from a linear fit:

Py <- Py +
    geom_smooth(method="lm")
Py

# Using `ggplot`, plot `z` as a function of `x` with a red line and save it into `Pz`:

Pz <- df |> 
    ggplot(aes(x = x, y = z))+ 
        geom_line(color="red")
Pz

# Using `ggplot`, plot a histogram of `w` with transparent blue bars surrounded by a red line, and save it into `Pw`. You can play with the number of bins too.

Pw <- df |> 
    ggplot(aes(x = w))+ 
        geom_histogram(color="red", fill="blue", alpha=0.5, bins=20)
Pw

# Using `ggplot`, plot a density of `u` with a transparent blue area surrounded by a red line, and save it into `Pu`. Play with the `bw` parameter so that you see many peaks.

Pu <- df |> 
    ggplot(aes(x = u))+ 
        geom_density(color="red", fill="blue", alpha=0.5, bw=0.1)
Pu

# Using `patchwork`, gather the previous plots on a 2x2 grid.
library(patchwork)
Py+Pz+Pw+Pu

# Using `patchwork`, gather the previous plots on a grid with 3 plots in the 1st row, and one large plot in the 2nd row. Using `plot_annotation()`, add tags such as (a), (b)...

(Py+Pz+Pw)/Pu + plot_annotation(tag_levels = 'a', tag_suffix = ')')


# # # # # # # # # # # # # # # # # # # # # # # # # 
## Exercise 3
# # # # # # # # # # # # # # # # # # # # # # # # # 

# Let's work on `faithfuld` and plot as a 3D color plot the density, as a function of eruptions vs waiting
# Plot with geom_contour_filled() or geom_raster() and see the difference
# Add big red diamond points in (4.4, 80) and (1.94, 53) using either geom_point() or annotate("point", ...)

faithfuld |> 
    ggplot(aes(x=waiting, y=eruptions, z=density))+
        geom_contour_filled()

faithfuld |> 
    ggplot(aes(x=waiting, y=eruptions, fill=density))+
        geom_raster(interpolate = TRUE)+
        scale_fill_viridis_c(option='B')



