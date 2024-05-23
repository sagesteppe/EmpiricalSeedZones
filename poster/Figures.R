# tell this R script where it should start to look for files 
setwd('~/Documents/EmpiricalSeedZones/poster') 
getwd() # verify that the script is working from where we intend it to be working
# from 

# load the relevant file 
filenames <- read.csv('../data/scoring/Filename_schema.csv')

# attach a set of functions which we can use to process tabular data, and 
# visualize them using many common types of plots. 
library(tidyverse)


fname_binomial <- filenames %>% # pipe operator = pass this version of the data 
  # onto the next function which we are 'chaining' on after the pipe. 
  filter(Trait == 'Filename-Binomial')
  # filter the data set passed by the pipe operator based on a Column (Variable)
  # we are filtering for an exact match '==' of the character string 'Filename-Binomial' 
