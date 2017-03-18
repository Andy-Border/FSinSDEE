# The first parameter is the filename of the .csv file
# The second parameter "header=TRUE" means that the first line of the .csv file is actually the variable names (i.e. the names of the columns),
# instead of a normal row with more values of a single case
# The third parameter means that if in a cell of the CSV file there is a string "NA",
# that is actually a missing data, not a string!
ISBSG <- read.csv("ISBSG DATA Release 12.csv", header = TRUE, na.strings = "NA")

# To select a specific column (i.e Functional.Size) you can do this
ISBSG$Functional.Size
# or this
ISBSG[["Functional.Size"]]
# or this
ISBSG[,"Functional.Size"]

# If you want to know the mean of that column you can do this
mean( ISBSG[["Functional.Size"]] )
# but that column has missing data (i.e. NA values) so the result is also NA
# Normally if you want a command to not consider the missing data you have to add
# this parameter na.rm = TRUE
mean( ISBSG[["Functional.Size"]], na.rm = TRUE)

mean.value <- mean( ISBSG[["Functional.Size"]], na.rm = TRUE)
# We check all the missing data of that column
is.na( ISBSG[["Functional.Size"]] )
# The result of that command is an array with a FALSE wherever there is good data
# and a TRUE whereever there is missing data
cells.with.missing.data <- is.na( ISBSG[["Functional.Size"]] )

# Now we are going to select ONLY the values or the cells that have a missing data
ISBSG[["Functional.Size"]] [cells.with.missing.data]
# Finally, we assign to them the mean we calculated before
ISBSG[["Functional.Size"]] [cells.with.missing.data] <- mean.value
# If you clic on the ISBSG variable in the environment tab you will see that
# that column has no missing data now

# Let's put all this in a function

mean.imputation.function <- function(column.name){
  cells.with.missing.data <- is.na( ISBSG[[column.name]] )
  mean.value <- mean( ISBSG[[ column.name ]], na.rm = TRUE)
  ISBSG[[ column.name ]] [cells.with.missing.data] <- mean.value
}

# Now to impute with mean imputation NUMERICAL columns we only have to do
mean.imputation.function("Functional.Size")
# or
mean.imputation.function("Adjusted.Function.Points")

# Mode imputation
calculate.mode <- function(data){
  return(names(sort(table( data ), decreasing = TRUE))[1])
}
# Try to follow these steps to understand it
# Values
ISBSG$Development.Type
# Calculate frequencies of the values
table(ISBSG$Development.Type)
# Sort from most frequent to less
sort(table(ISBSG$Development.Type), decreasing = TRUE)
# Get the first one
names(sort(table(ISBSG$Development.Type), decreasing = TRUE))[1]

# So now you can calculate the mode of a NOMINAL variable like this
calculate.mode( ISBSG[["Primary.Programming.Language"]] )
calculate.mode( ISBSG[["Language.Type"]] )


# kNN imputation
install.packages("VIM")
library(VIM)

# In its simplest form
# Imputes the whole dataset and uses k = 3 to do the imputation
# By default uses mean() for the k values, and something similar to a mode for categorical
kNN( ISBSG, k=3 )
# Another example

kNN( ISBSG, variable=c("Normalised.Work.Effort"), numFun=median, k=5 )

