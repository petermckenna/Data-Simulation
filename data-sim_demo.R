# Data simulation using R
# Date: 10/11/23
# Session script

# start by creating project

# add folder for data in the command line
# dir.create("data")

# Install and Load packages----

# install packages
#install.packages('tidyverse')
#install.packages('corrr')

# load packages
library(tidyverse)
library(corrr)


# Simulate and plot univariate data ----

set.seed(385) # seed a random number for reproducibility of the script
# you only need to do this once at the start

# Say we wanted to simulate response time to a cogntive test
# where the mean = 1000, the SD = 50, from 150 participants
rt_sim <- 
  tibble(                           # create data table object
    control_rt = rnorm(mean = 1000, # state the mean 
                       sd = 500,    # provide the sd
                       n = 150))    # supply the sample size


# generate the histogram with ggplot2
ggplot(data = rt_sim,                   # specify data source
       mapping = aes(x = control_rt)) + # specify x and y mapping
  geom_histogram(bins = 30) +           # add histogram layer and bin width
  labs(x = "\nResponse time (ms)")      # supply axis labels



# Simulating unrelated bivariate data ----

# simulate two separate datasets
data <- 
  tibble(
    control_rt = rnorm(mean = 1000, 
                       sd = 500,
                       n = 150),
    treatment_rt = rnorm(mean = 1200, 
                         sd = 700,
                         n = 150))

# run a correlation
cor(data$control_rt,   # Pearson is the default
    data$treatment_rt)


# Read in the heights and weights data ----

# read in the data
# make sure you include the full file name and type (e.g., .csv) in the quotes
handw <- 
  read_csv("data/heights_and_weights.csv", 
           col_types = "dd")                # specify that both cols are `double` type


# peek into heights and weights dataset
glimpse(handw)


# Plot the heights and weigths data ----

# generate the scatter plot with ggplot2
ggplot(data = handw,                                    # specify data source
       mapping = aes(x = height_in, y = weight_lbs)) +  # specify x and y axis
  geom_point(alpha = .6) +                              # modify transparency of points
  labs(x = "\nHeight (inches)",                         # supply axis label names
       y = "Weight (pounds)\n")


# Log transform and plot the data ----

# add log transformed vectors to dataset
handw_log <-
  handw %>%
  mutate(hlog = log(height_in),  # create new variable `hlog` which is the log of height_in
         wlog = log(weight_lbs)) # create new variable `wlog` which is the log of weight_lbs

# generate a scatter plot of the log data
ggplot(data = handw_log,                      # don't forget to enter the new dataset              
       mapping = aes(x = hlog, y = wlog)) +  
  geom_point(alpha = .6) +                              
  labs(x = "\nLog(Height)",                           
       y = "log(Weight)\n")


# Gather statistics to simulate data based on handw_log ----

# calculate means and sd
handw_log %>%
  summarise(mean_h = mean(hlog),
            sd_h = sd(hlog),
            mean_w = mean(wlog),
            sd_w = sd(wlog)) %>%
  mutate_if(is.numeric, round, digits = 2) # round to 2 decimal places

# calculate correlation
cor(handw_log$hlog, handw_log$wlog)


# Create matrix for MASS:mvrnorm Sigma ----

# define and store covariances
my_cov <- 
  .96 * .26 * .65 # log of correlation times hlog_sd times wlog_sd

# use the matrix function to define our sigma
my_sigma <-
  matrix(c(.26^2, my_cov, 
           my_cov, .65^2), 
         ncol = 2)

my_sigma          # print the matrix


# Simulate 6 synthetic humans ----

# pass the names vector c(height = 4.11, weight = 4.74)
# for mu gives us column names in the output
log_ht_wt <-
  MASS::mvrnorm(n = 6,                 # our 6 synthetic humans 
                mu = c(height = 4.11,  # log mean of height 
                       weight = 4.74), # log mean of weight
                Sigma = my_sigma)      # our positive-definitive matrix

# view the output
log_ht_wt

# convert back into real values
exp(log_ht_wt)


# Simulate 500 synthetic humans ----

# simulate new humans
handw_sim <-
  MASS::mvrnorm(n = 500,
                mu = c(height_in = 4.11,
                       weight_lbs = 4.74),
                Sigma = my_sigma) %>%
  exp() %>%                     # back-transform from log space to real space
  as_tibble() %>%               # convert from matrix into data table
  mutate(type = "simulated")    # vector labelling data as simulated



# Plot Real vs Simulated data

# Combine real and simulated datasets
# Note: `handw_real` is a table containing data from heights_and_weights.csv

# add type column labelling handw as "real"
handw_real <-
  handw %>%
  mutate(type = "real") # add type column to original dataset, with value "real"

# combine handw_real with new_humans
all_data <-
  rbind(handw_real,
        handw_sim)

# plot the data
ggplot(all_data,
       aes(x = height_in, 
           y = weight_lbs)) +
  geom_point(aes(colour = type), 
             alpha = .6)


# Save the data

# write data as a csv into "data" folder
write_csv(all_data, "data/handw_all.csv")  # all of the data
write_csv(handw_sim, "data/handw_sim.csv") # just the simulated data




