# tell this R script where it should start to look for files 
setwd('~/Documents/EmpiricalSeedZones/poster') 
getwd() # verify that the script is working from where we intend it to be working
# from 

# load the relevant file 
filenames <- read.csv('../data/scoring/Filename_schema.csv')

# attach a set of functions which we can use to process tabular data, and 
# visualize them using many common types of plots. 
library(tidyverse)

filenames %>% # pipe operator = pass this version of the data 
  # onto the next function which we are 'chaining' on after the pipe. 
  filter(Trait == 'Filename-Binomial')
  # filter the data set passed by the pipe operator based on a Column (Variable)
  # we are filtering for an exact match '==' of the character string 'Filename-Binomial' 


# We are going to create a barplot, which we want to use to showcase the current
# disorganization of the existing filenames. 

binom_plot <- filenames %>% 
  # when we save a plot to an object, by default Rstudio will not render it into 
  # the viewing panel (lower right, 'Plots' panel)
  filter(Trait == 'Filename-Binomial') %>% 
  
  # ggplot is low-order plotting device. it will require the following arguments: 
  # 1 data = the dataset which will be plotted, in this instance we denote that the 
  # dataset is being 'piped in' from above, and we can explicitly denote that with
  # the value '.' which means LOOK HERE in programming, and can also be seen when
  # using relative paths to load data. 
  # ggplot then uses the 'aes' or aesthetics arguments to determine HOW it will plot
  # data from the data argument. These Variable (column) names should NEVER be
  # quoted, and must be spelled exactly. 
  # within aes 'y' determines which variable will be plotted along the Y axis
  # within aes 'x' determines which variables will be plotted along the X axis 
  ggplot(., aes(y = Count, x = Characteristic, fill = Characteristic)) + 
  
  # ggplot works on the notions of 'GEOMS', basically what a geom is a different 
  # type of plot. Geoms exist for nearly all common forms of data visualization 
  # if you want to learn more about the arguments  to geom_bar we can use the 
  # '?' function  to get some data on it... '?geom_bar()'
  geom_bar(position = "dodge", stat = "identity") + 
  
  # labs this is a function we can use to specify labels for the plot parameters
  labs(
    x = 'Character State',
    y = 'Count', 
    title = 'Breakdown of how filenames record species names', 
    fill = 'Character State'
      ) + 
  
  # ggplot has a few main default themes for styling the plot in GENERAL, e.g. 
  # whether there are hash lines on the background or not.. 
  theme_minimal()



binom_plot # here we can quickly view the plot in the viewer panel, and by saving
# the plot as an object we can combine it with other plots later in a more complex
# layout. 


seedzone_plot <- filenames %>% 
  # when we save a plot to an object, by default Rstudio will not render it into 
  # the viewing panel (lower right, 'Plots' panel)
  filter(Trait == 'Filename-seedzone') %>% 

  ggplot(., aes(y = Count, x = Characteristic, fill = Characteristic)) + 
  
  geom_bar(position = "dodge", stat = "identity") + 
  
  labs(
    x = 'Character State',
    y = 'Count', 
    title = 'Breakdown of how filenames record species names', 
    fill = 'Character State'
  ) + 
  
  # ggplot has a few main default themes for styling the plot in GENERAL, e.g. 
  # whether there are hash lines on the background or not.. 
  theme_minimal()


