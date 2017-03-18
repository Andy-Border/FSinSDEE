# Created by Andy Zhao 
# Website of the guide of dplyr
# https://www.google.nl/url?sa=t&rct=j&q=&esrc=s&source=web&cd=1&ved=0ahUKEwjymYza5IXSAhXFiRoKHXZHDcAQFggaMAA&url=https%3A%2F%2Fcran.rstudio.com%2Fweb%2Fpackages%2Fdplyr%2Fvignettes%2Fintroduction.html&usg=AFQjCNH7Ylg3tLyDnOEXLKyYXT0tBfmUSQ&sig2=JYBs8lS-baTgrEuiq7cS-A&cad=rja
# 2013/2/12


rm(list = ls())
library(nycflights13)
dim(flights)
head(flights)
library("dplyr")


# filter() allows you to select a subset of rows in a data frame. The first argument is the name of the data
# frame. The second and subsequent arguments are the expressions that filter the data frame:
#   For example, we can select all flights on January 1st with:
filter(flights, month == 1, day == 1)

# To select rows by position, use slice():
first10Rows <- slice(flights, 1:10)
first10Rows

# Arrange rows with arrange()
# arrange() works similarly to filter() except that instead of 
# filtering or selecting rows, it reorders them. It takes a data frame, and a 
# set of column names (or more complicated expressions) to order by. If you 
# provide more than one column name, each additional column will be used to 
# break ties in the values of preceding columns: 

# Order By Time
arrange(flights, year, month, day)
# Use desc() to order a column in descending order:
arrange(flights, desc(dep_delay))

# Select columns with select()
# Often you work with large datasets with many columns but only a few are 
# actually of interest to you. select() allows you to rapidly zoom in on 
# a useful subset using operations that usually only work on numeric variable
# positions:

# Select columns by name
select(flights, year, month, day)
# Select all columns between year and day (inclusive)
select(flights, year:day)
# Select all columns except those from year to day (inclusive)
select(flights, -(year:day))
# You can rename variables with select() by using named arguments:
select(flights, tail_num = tailnum)
# But because select() drops all the variables not explicitly mentioned,
# it¡¯s not that useful. Instead, use rename():
flights2 <- rename(flights, tail_num = tailnum)
flights2
flights


# Extract distinct (unique) rows
# Use distinct()to find unique values in a table:
distinct(flights, tailnum)
distinct(flights, origin, dest)

# Add new columns with mutate()
# Besides selecting sets of existing columns, 
# it¡¯s often useful to add new columns that are functions of existing
# columns. This is the job of mutate():
flight3 <- mutate(flights,
       gain = arr_delay - dep_delay,
       speed = distance / air_time * 60)
# dplyr::mutate() works the same way as plyr::mutate() and similarly to
# base::transform(). The key difference between mutate() and transform()
# is that mutate allows you to refer to columns that you¡¯ve just
# created:
mutate(flights,
       gain = arr_delay - dep_delay,
       gain_per_hour = gain / (air_time / 60)
)
#If you only want to keep the new variables, use transmute():
transmute(flights,
          gain = arr_delay - dep_delay,
          gain_per_hour = gain / (air_time / 60)
)

# Summarise values with summarise()
# The last verb is summarise(). It collapses a data frame to a single row 
# (this is exactly equivalent to plyr::summarise()):
summarise(flights,delay = mean(dep_delay, na.rm = TRUE))


# Randomly sample rows with sample_n() and sample_frac()
# You can use sample_n() and sample_frac() to take a random sample of rows:
# use sample_n() for a fixed number and sample_frac() for a fixed fraction

sample_n(flights, 10)
sample_frac(flights, 0.01)

# Commonalities
# You may have noticed that the syntax and function of all these verbs are very similar:
#   The first argument is a data frame.
# The subsequent arguments describe what to do with the data frame. Notice that you can refer to columns
# in the data frame directly without using $.
# The result is a new data frame
# Together these properties make it easy to chain together multiple simple steps to achieve a complex result.
# These five functions provide the basis of a language of data manipulation. At the most basic level, you can only
# alter a tidy data frame in five useful ways: you can reorder the rows (arrange()), pick observations and
# variables of interest (filter() and select()), add new variables that are functions of existing variables
# (mutate()), or collapse many values to a summary (summarise()). The remainder of the language comes from
# applying the five functions to different types of data. For example, I¡¯ll discuss how these functions work with
# grouped data.

# Grouped operationsopen 

# These verbs are useful on their own, but they become really powerful when you apply them to groups of
# observations within a dataset. In dplyr, you do this by with the group_by() function. It breaks down a dataset
# into specified groups of rows. When you then apply the verbs above on the resulting object they¡¯ll be
# automatically applied ¡°by group¡±. Most importantly, all this is achieved by using the same exact syntax you¡¯d
# use with an ungrouped object.
# Grouping affects the verbs as follows:
#   grouped select() is the same as ungrouped select(), except that grouping variables are always
# retained.
# grouped arrange() orders first by the grouping variables
# mutate() and filter() are most useful in conjunction with window functions (like rank(), or min(x) ==
#                                                                               x). They are described in detail in vignette("window-functions").
# sample_n() and sample_frac() sample the specified number/fraction of rows in each group.
# slice() extracts rows within each group.
# summarise() is powerful and easy to understand, as described in more detail below.
# In the following example, we split the complete dataset into individual planes and then summarise each plane by
# counting the number of flights (count = n()) and computing the average distance (dist = mean(Distance,
#                                                                                        na.rm = TRUE)) and arrival delay (delay = mean(ArrDelay, na.rm = TRUE)). We then use ggplot2 to display the output


library("ggplot2")
by_tailnum <- group_by(flights, tailnum)
delay <- summarise(by_tailnum,count = n(),dist = mean(distance, na.rm = TRUE),delay = mean(arr_delay, na.rm = TRUE))
delay <- filter(delay, count > 20, dist < 2000)
# Interestingly, the average delay is only slightly related to the
# average distance flown by a plane.
ggplot(delay, aes(dist, delay)) +
  geom_point(aes(size = count), alpha = 1/2) +
  geom_smooth() +
  scale_size_area()

# #You use summarise() with aggregate functions, which take a vector of values and return a single number.
# There are many useful examples of such functions in base R like min(), max(), mean(), sum(), sd(), median(),
# and IQR(). dplyr provides a handful of others:
#   n(): the number of observations in the current group
# n_distinct(x):the number of unique values in x
# first(x), last(x) and nth(x, n) - these work similarly to x[1], x[length(x)], and x[n] but give you
# more control over the result if the value is missing.
# For example, we could use these to find the number of planes and the number of flights that go to each
# possible destination:



destinations <- group_by(flights, dest)
dest1 <- summarise(destinations,
          planes = n_distinct(tailnum),
          flights = n()
)

