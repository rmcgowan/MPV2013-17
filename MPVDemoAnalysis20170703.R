# goal: look at relationship between areas where police killings take place 
#       relative to US at large

library(dplyr) # 0.5.0
library(ggplot2) # 1.0.1
library(tidyr) # 0.3.1

# get demographic data for all zip codes using previously written script
source("MPVZipCodeDemos20170615.R")

###########################################
# dataset 1: police killings from 2013-17 #
###########################################

pk.data <- read.csv("police_killings_2013-17_updated.csv",
                    stringsAsFactors = FALSE) %>%
  dplyr::filter(!grepl("NA", Location.of.death..zip.code.),
                !is.na(Location.of.death..zip.code.)) %>% 
  dplyr::mutate(Location.of.death..zip.code. = as.numeric(Location.of.death..zip.code.)) %>% 
  dplyr::arrange(Location.of.death..zip.code.)

# convert zip code to five digits
pk.data$Location.of.death..zip.code. <- ifelse(nchar(pk.data$Location.of.death..zip.code.) < 5,
                                               formatC(pk.data$Location.of.death..zip.code.,
                                                       width = 5,
                                                       format = "d",
                                                       flag = "0"),
                                               pk.data$Location.of.death..zip.code.)

# plot of median income for zip codes in dataset
ggplot(pk.data,
       aes(x = median_household_income)) +
  geom_histogram()

# questions of how to look at it:
# distinct zip codes?
length(unique(pk.data$Location.of.death..zip.code.)) # 3702
# distinct incidents (date, address, city, state, zip, county, agency)
nrow(unique(pk.data[,c(7,8,9,10,11,12)])) # 5084

########################################
# get killings count for each zip code #
########################################

pk.zip <- pk.data %>%
  dplyr::rename(zip_code = Location.of.death..zip.code.) %>% 
  dplyr::group_by(zip_code) %>% 
  dplyr::summarise(killings = n()) %>% 
  dplyr::ungroup() 

# distribution of killings per zip code
table(pk.zip$killings)

ggplot(pk.zip,
       aes(x = killings)) +
  geom_histogram()

#######################################################
# look at zip.data joined with pk.zip to compare zips #
# with and without police-involved killings           #
#######################################################

# add count columns to be used later for overall stats
# not perfect for poverty since those figures are based 
# off "Population for whom poverty status is determined"
zip.pk.data <- dplyr::left_join(zip.data,
                                pk.zip,
                                by = "zip_code") %>% 
  dplyr::mutate(killings = ifelse(is.na(killings), 0, killings),
                killings_indicator = killings > 0,
                n_white_alone = pop_total * pop_white_alone,
                n_black_aa = pop_total * pop_black_aa,
                n_amer_ind = pop_total * pop_amer_ind,
                n_asian = pop_total * pop_asian,
                n_hi_pac = pop_total * pop_hi_pac,
                n_other = pop_total * pop_other,
                n_multi_other = pop_total * pop_multi_other,
                n_multi_no_other = pop_total * pop_multi_no_other,
                n_pov_0_0.49 = pop_total * pop_pov_0_0.49,
                n_pov_0.5_0.99 = pop_total * pop_pov_0.5_0.99,
                n_pov_1.00_1.24 = pop_total * pop_pov_1.00_1.24,
                n_pov_1.25_1.49 = pop_total * pop_pov_1.25_1.49,
                n_pov_1.50_1.84 = pop_total * pop_pov_1.50_1.84,
                n_pov_1.85_1.99 = pop_total * pop_pov_1.85_1.99,
                n_pov_2.00 = pop_total * pop_pov_2.00)

# calculate summary stats for zips with and without police-involved killings
zip.pk.compare <- zip.pk.data %>% 
  dplyr::group_by(killings_indicator) %>% 
  dplyr::summarise(pop_total = sum(pop_total),
                   n_white_alone = sum(n_white_alone),
                   n_black_aa = sum(n_black_aa),
                   n_amer_ind = sum(n_amer_ind),
                   n_asian = sum(n_asian),
                   n_hi_pac = sum(n_hi_pac),
                   n_other = sum(n_other),
                   n_multi_other = sum(n_multi_other),
                   n_multi_no_other = sum(n_multi_no_other),
                   n_pov_0_0.49 = sum(n_pov_0_0.49),
                   n_pov_0.5_0.99 = sum(n_pov_0.5_0.99),
                   n_pov_1.00_1.24 = sum(n_pov_1.00_1.24),
                   n_pov_1.25_1.49 = sum(n_pov_1.25_1.49),
                   n_pov_1.50_1.84 = sum(n_pov_1.50_1.84),
                   n_pov_1.85_1.99 = sum(n_pov_1.85_1.99),
                   n_pov_2.00 = sum(n_pov_2.00)) %>% 
  dplyr::ungroup()

##########################################
# create new data frame with proportions #
##########################################

zip.pk.compare.prop <- apply(zip.pk.compare[,3:ncol(zip.pk.compare)], 2,
                             function(x) {
                               x / zip.pk.compare[,2]
                             }) %>% 
  dplyr::bind_cols()

# fix column names
names(zip.pk.compare.prop) <- gsub("n_", "prop_", names(zip.pk.compare)[3:ncol(zip.pk.compare)])

# add indicator column
zip.pk.compare.prop <- dplyr::bind_cols(zip.pk.compare[,1],
                                        zip.pk.compare.prop)

#####################################
# create data frame to look at race #
#####################################

zip.pk.compare.race <- zip.pk.compare.prop %>% 
  dplyr::select(killings_indicator,
                prop_white_alone:prop_multi_no_other) %>% 
  gather(key = race,
         value = percentage,
         prop_white_alone:prop_multi_no_other)

ggplot(zip.pk.compare.race,
       aes(x = race,
           y = percentage,
           fill = killings_indicator)) +
  geom_bar(stat = "identity",
           position = position_dodge())

##############################################
# create data frame to look at poverty level #
##############################################

zip.pk.compare.poverty <- zip.pk.compare.prop %>% 
  dplyr::select(killings_indicator,
                prop_pov_0_0.49:prop_pov_2.00) %>% 
  gather(key = poverty_level,
         value = percentage,
         prop_pov_0_0.49:prop_pov_2.00)

ggplot(zip.pk.compare.poverty,
       aes(x = poverty_level,
           y = percentage,
           fill = killings_indicator)) +
  geom_bar(stat = "identity",
           position = position_dodge())
