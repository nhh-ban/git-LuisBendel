

library(tidyverse)

###########################################
######   Problem 2    #####################
###########################################

# number of variables in the .txt file
n <- 11

# get descriptions from text file
v_descr <- read_delim("suites_dw_Table1.txt",
                      delim = "-",
                      skip_empty_rows = T,
                      col_names = F,
                      trim_ws = T) %>% 
  rename(variable = X1,
         descr = X2)

v_descr <- v_descr[1:n,] %>% 
  mutate(variable = trimws(variable),
         descr = trimws(descr))

# load data from text file
galaxies <- read_delim("suites_dw_Table1.txt",
                       delim = "|",
                       skip = 12,
                       trim_ws = T)[-1,]

# add the decriptions to the description attribute of the data frame
for (i in 1:nrow(v_descr)) {
  variable <- v_descr$variable[i]
  descr <- v_descr$descr[i]
  
  # in case there is no description for one or more variables
  if (variable %in% colnames(galaxies)) {
    attr(galaxies[[variable]], "description") <- descr
  }
}

# extract the description of a variable from the attributes of the dataframe
attr(galaxies$name, "description")



###########################################
######   Problem 3    #####################
###########################################

# the most logical thing would be to show the distribution of diameter
# however, this does not really reveal what we are looking for
galaxies %>% 
  ggplot(aes(x = a_26)) +
  geom_histogram()

# maybe we could take the log to adjust for a few "very large" galaxies?
# this shows, that indeed there are not many very small galaxies, but many small ones
galaxies %>% 
  ggplot(aes(x = log(a_26))) +
  geom_histogram()

# one explenation could be that small galaxies do not shine as bright (b-band is a low negative value)
# this plot shows the joint distribution of log(diameter) and absolute magnitude in b-band
# indeed we see that small galaxies have a higher b-band value
# which means they do not shine as bright and might be more difficult to detect
galaxies %>% 
  ggplot(aes(x = log(a_26), y = m_b)) +
  geom_point()
