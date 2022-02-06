library(tidyverse)
library(tidyr)
library(ggthemes)
library(lubridate)
library(ggpubr)

# Now that we have learned how to munge (manipulate) data
# and plot it, we will work on using these skills in new ways


####-----Reading in Data and Stacking it ----- ####
#Reading in files
#files <- list.files('data',full.names=T)
#files

# Write a function that opens all files in the data_folder provided:

opn_concat_co <- function(data_folder) {
  path_list <- paste(getwd(),list.files(data_folder, full.names = T), sep='/')
  data <- lapply(path_list, function(x) {
    dat <- read.csv(x)%>% 
    rename(burned=2,unburned=3) %>%
    mutate(data = strsplit(strsplit(strsplit(x, "/")[[1]][9], '_')[[1]][2], '\\.')[[1]][1])
    return(dat)
  })
  combined.data <- do.call(rbind, data)
  return(combined.data)
}

# Open as a long dataset
full_long <- opn_concat_co('data') %>%
  gather(key='site',value='value',-DateTime,-data) %>%
  filter(!is.na(value))

##### Question 1 #####
#1 What is the correlation between NDVI and NDMI? - here I want you to
# convert the full_long dataset in to a wide dataset using the 
# function "spread" and then make a plot that shows the correlation as a
# function of if the site was burned or not

full_wide <- spread(data=full_long, key='data',value='value') %>%
  filter_if(is.numeric,all_vars(!is.na(.))) %>%
  mutate(month = month(DateTime),
         year = year(DateTime))

site_names <- c('burned' = "Burned", 
                'unburned' = "Unburned")

ggplot(full_wide,aes(x=ndmi,y=ndvi)) + 
  geom_point() + 
  geom_smooth(method='lm', formula= y~x) +
  facet_wrap(~site, labeller = as_labeller(site_names)) +
  stat_cor(aes(label = ..rr.label..), color = "red", geom = "label") +
  theme_few() + 
  xlab("NDMI") +
  ylab("NDVI") +
  scale_color_few(palette = 'Dark') + 
  theme(legend.position=c(0.8,0.8))


## End Code for Question 1 -----------


#### Question 2 ####
#2) What is the correlation between average NDSI (normalized 
# snow index) for January - April and average NDVI for June-August?
#In other words, does the previous year's snow cover influence vegetation
# growth for the following summer? 

# Using the wide dataframe to keep month column, 
# select needed columns, 
# make dataframe long for easy filtering and plotting 
# filter the dataframe with an 'or', keeping everthing with NDSI month %in% c(1,2,3,4)
# NDVI month %in% c(6,7,8)

long_plot <- full_wide %>% 
  select(c(month, ndvi, ndsi)) %>%
  pivot_longer(c(ndvi,ndsi)) %>%
  filter(name== 'ndvi' & month %in% c(6,7,8) | name== 'ndsi' & month %in% c(1,2,3,4))


#create line plot for each column in data frame
ggplot(df, aes(index, value)) +
  geom_line(aes(colour = series))
## End code for question 2 -----------------


###### Question 3 ####
#How is the snow effect from question 2 different between pre- and post-burn
# and burned and unburned? 

## Your code here

## End code for question 3

###### Question 4 #####
#What month is the greenest month on average? Does this change in the burned
# plots after the fire? 

##### Question 5 ####
#What month is the snowiest on average?
