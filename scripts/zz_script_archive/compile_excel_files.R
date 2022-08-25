# Biogeochem
# Contact: klingesd@si.edu
# 2018.10.25


## Prep workspace #################
library(tidyverse)
library(xlsx)

## Assign variables #################

# For each of these four slots, assign the file path of your first four files
#   where I have put the #C:/Users/input and remove the #
# If you have more than four files, this can be repeated...but for far more
# than a few files, this process should be looped

input_file_path <- "S:/Photobiology/Photomat/Tidal Marsh Project/NASA C Cycle Project/Data/EX02 GCREW/2015/input_compiled/"
output_file_path <- "S:/Photobiology/Photomat/Tidal Marsh Project/NASA C Cycle Project/Data/EX02 GCREW/2015/output_compiled/"

# add all data files in the folder to a list
input_list <- list.files(input_file_path, pattern="*.xlsx", full.names=TRUE)
  

#output list is an empty list to store output
output_list <- list()

# We wrote a function
# We want to APPLY that function to every item in a list
# start with first item in list. Do function. Go to next item in list. Do function.

for (i in 1:length(input_list)) {
  
filename <- read_excel(input_list[[i]]) # note: we put 4 as 2nd input


# Start of data was at different rows, so we need to slice at different points
if (filename[24, 1] == "1") {
  filename <- filename %>%
    slice(-1:-24)
}

if (filename[23, 1] == "1") {
  filename <- filename %>%
    slice(-1:-23)
}

# Change the first row of data to the column headers
colnames(filename) <- filename[1, ] # the first row will be the header
filename <- filename[-1, ]

# We changed the dates and times in excel afterwards, so don't need these steps
#filename[,1] <- mdy(filename[,1])
#filename[,2] <- hms(filename[,2])

# Add the dataset to an output list of datasets
output_list[[i]] <- filename
  
}

# now, compile all the datasets in output_list into one dataset
for (i in 1:length(output_list)) {
  # if we are on the first file....
  # then create a new dataset
  if (i == 1) {
    new_data <- output_list[[i]]
  } else { # else, add the file to the new dataset
    
    new_data <- bind_rows(new_data, output_list[[i]])
  }
}
  

## Read out data #############

# Now R has compiled all of our data into a single object. However,
#   this object does not correspond to a file; it only exists in R.
# So, we have to read this object out as a file

# Read out data as a csv file
write.csv(new_data, paste0(output_file_path, "compiledfile.csv"))

# Read out data as a xlsx file
write.xlsx(new_data, paste0(output_file_path, "compiled_file.xlsx"))
