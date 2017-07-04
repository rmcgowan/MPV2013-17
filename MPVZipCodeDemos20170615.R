# goal: add zip code level info to dataset documenting police vehicle-related deaths
# topics to add:
# - race demographics
# - median income
# - poverty rate

library(dplyr) # version 0.5.0
library(tidyr) # version 0.3.1

###################################################
# dataset 1: 2012 deaths related to police chases #
###################################################

# downloaded as tsv from shared google doc
mpv.data <- read.csv("police_killings_2013-17_updated.csv",
                     stringsAsFactors = FALSE)

# add leading 0s to zip codes
mpv.data$Location.of.death..zip.code. <- ifelse(nchar(mpv.data$Location.of.death..zip.code.) < 5,
                                                    formatC(mpv.data$Location.of.death..zip.code.,
                                                            width = 5,
                                                            format = "d",
                                                            flag = "0"),
                                                    mpv.data$Location.of.death..zip.code.)

##########################################
# dataset 2: race info for all zip codes #
##########################################

# downloaded from data2.nhgis.org "GET DATA"
# geographic levels: ZCTA
# years: 2011-2015
# topics: Race
# selected table name "Race" 
# reference corresponding key in directory
zip.race <- read.csv("nhgis_race_2015_zcta.csv",
                     stringsAsFactors = FALSE)

# keep and rename relevant columns
zip.race <- zip.race %>% 
  dplyr::select(zip_code = ZCTA5A,
                pop_total = ADKXE001,
                pop_white_alone = ADKXE002,
                pop_black_aa = ADKXE003,
                pop_amer_ind = ADKXE004,
                pop_asian = ADKXE005,
                pop_hi_pac = ADKXE006,
                pop_other = ADKXE007,
                pop_multi_other = ADKXE009,
                pop_multi_no_other = ADKXE010)

# convert zip code to five digits
zip.race$zip_code <- ifelse(nchar(zip.race$zip_code) < 5,
                            formatC(zip.race$zip_code,
                                    width = 5,
                                    format = "d",
                                    flag = "0"),
                            zip.race$zip_code)

# number of zips in zip.race vs. in ct2z
length(unique(zip.race$zip_code)) # 33120
length(unique(zip.race$zip_code)) == nrow(zip.race) # TRUE

# make sure rowSums equal totals
zip.race.sums <- rowSums(zip.race[,c(3:10)])
identical(as.numeric(zip.race$pop_total), as.numeric(zip.race.sums)) # TRUE

# calculate percentage of each race for each zip
zip.race.prop <- zip.race %>% 
  gather(key = race,
         value = count,
         pop_white_alone, pop_black_aa, pop_amer_ind,
         pop_asian, pop_hi_pac, pop_other,
         pop_multi_other,
         pop_multi_no_other) %>% 
  dplyr::group_by(zip_code, race) %>%
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(zip_code) %>% 
  dplyr::mutate(prop = count / sum(count)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(prop)) %>% 
  dplyr::select(-count) %>% 
  spread(key = race,
         value = prop)

# check that row sums all equal 1
zip.race.prop.sums <- rowSums(zip.race.prop[,-1])
sum(zip.race.prop.sums == 1) == nrow(zip.race.prop) # FALSE
# which ones don't?
zip.race.prop.sums[ which(zip.race.prop.sums != 1) ] # obviously some weird rounding thing

###############################################
# dataset 3: income relative to poverty level #
###############################################

# poverty columns are population counts who live in income buckets relative to poverty threshold
# downloaded from data2.nhgis.org "GET DATA"
# geographic levels: ZCTA
# years: 2011-2015
# topics: Poverty (Income Relative to Poverty Level)
# selected table name "Ratio of Income to Poverty Level in the Past 12 Months" 
# reference corresponding key in directory
zip.poverty <- read.csv("nhgis_poverty_2015_zcta.csv",
                        stringsAsFactors = FALSE)

# keep and rename relevant columns
zip.poverty <- zip.poverty %>% 
  dplyr::select(zip_code = ZCTA5A,
                pop_pov_0_0.49 = ADNEE002,
                pop_pov_0.5_0.99 = ADNEE003,
                pop_pov_1.00_1.24 = ADNEE004,
                pop_pov_1.25_1.49 = ADNEE005,
                pop_pov_1.50_1.84 = ADNEE006,
                pop_pov_1.85_1.99 = ADNEE007,
                pop_pov_2.00 = ADNEE008)

# convert zip code to five digits
zip.poverty$zip_code <- ifelse(nchar(zip.poverty$zip_code) < 5,
                               formatC(zip.poverty$zip_code,
                                       width = 5,
                                       format = "d",
                                       flag = "0"),
                               zip.poverty$zip_code)

# calculate percentage of each poverty level ratio for each zip
zip.poverty.prop <- zip.poverty %>% 
  gather(key = poverty_level_ratio,
         value = count,
         pop_pov_0_0.49, pop_pov_0.5_0.99,
         pop_pov_1.00_1.24, pop_pov_1.25_1.49,
         pop_pov_1.50_1.84, pop_pov_1.85_1.99,
         pop_pov_2.00) %>% 
  dplyr::group_by(zip_code, poverty_level_ratio) %>%
  dplyr::summarise(count = sum(count)) %>% 
  dplyr::ungroup() %>% 
  dplyr::group_by(zip_code) %>% 
  dplyr::mutate(prop = count / sum(count)) %>% 
  dplyr::ungroup() %>% 
  dplyr::filter(!is.na(prop)) %>% 
  dplyr::select(-count) %>% 
  spread(key = poverty_level_ratio,
         value = prop)

# check that row sums all equal 1
zip.poverty.prop.sums <- rowSums(zip.poverty.prop[,-1])
sum(zip.poverty.prop.sums == 1) == nrow(zip.poverty.prop) # FALSE
# which ones don't?
zip.poverty.prop.sums[ which(zip.poverty.prop.sums != 1) ] # obviously some weird rounding thing

########################################################
# dataset 4: median household income for each zip code #
########################################################

# poverty columns are population counts who live in income buckets relative to poverty threshold
# downloaded from data2.nhgis.org "GET DATA"
# geographic levels: ZCTA
# years: 2011-2015
# topics: Household and Family Income
# selected table name "Median Household Income in the Past 12 Months (in 2015 Inflation-Adjusted Dollars)" 
# reference corresponding key in directory
zip.income <- read.csv("nhgis_income_2015_zcta.csv",
                       stringsAsFactors = FALSE)

# keep and rename relevant columns
zip.income <- zip.income %>% 
  dplyr::select(zip_code = ZCTA5A,
                median_household_income = ADNKE001)

# convert zip code to five digits
zip.income$zip_code <- ifelse(nchar(zip.income$zip_code) < 5,
                              formatC(zip.income$zip_code,
                                      width = 5,
                                      format = "d",
                                      flag = "0"),
                              zip.income$zip_code)

################################
# dataset 5: income inequality #
################################

# poverty columns are population counts who live in income buckets relative to poverty threshold
# downloaded from data2.nhgis.org "GET DATA"
# geographic levels: ZCTA
# years: 2011-2015
# topics: Income Inequality
# selected table name "Gini Index of Income Inequality" 
# reference corresponding key in directory
zip.gini <- read.csv("nhgis_gini_2015_zcta.csv",
                     stringsAsFactors = FALSE)

# keep and rename relevant columns
zip.gini <- zip.gini %>% 
  dplyr::select(zip_code = ZCTA5A,
                gini = AD4BE001)

# convert zip code to five digits
zip.gini$zip_code <- ifelse(nchar(zip.gini$zip_code) < 5,
                            formatC(zip.gini$zip_code,
                                    width = 5,
                                    format = "d",
                                    flag = "0"),
                            zip.gini$zip_code)

###################################
# dataset 6: household level info #
###################################

# wanted to get the number of households
# downloaded from data2.nhgis.org "GET DATA"
# geographic levels: ZCTA
# years: 2011-2015
# topics: Households (Termed "Families" before 1940)
# selected table name "Household Type (Including Living Alone)" 
# reference corresponding key in directory
zip.households <- read.csv("nhgis_households_2015_zcta.csv",
                           stringsAsFactors = FALSE)

# keep and rename relevant columns
zip.households <- zip.households %>% 
  dplyr::select(zip_code = ZCTA5A,
                households_total = ADLUE001)

# convert zip code to five digits
zip.households$zip_code <- ifelse(nchar(zip.households$zip_code) < 5,
                                  formatC(zip.households$zip_code,
                                          width = 5,
                                          format = "d",
                                          flag = "0"),
                                  zip.households$zip_code)

##########################
# merge into one dataset #
##########################

# IPUMS ones first
# include total population
zip.data <- zip.race %>% 
  dplyr::select(zip_code, pop_total) %>% 
  dplyr::inner_join(zip.race.prop,
                    by = "zip_code") %>% 
  dplyr::inner_join(zip.poverty.prop,
                    by = "zip_code") %>% 
  dplyr::inner_join(zip.income,
                    by = "zip_code") %>% 
  dplyr::inner_join(zip.gini,
                    by = "zip_code") %>% 
  dplyr::inner_join(zip.households,
                    by = "zip_code")

# left join with mpv.data
mpv.data.updated <- dplyr::left_join(mpv.data,
                                     zip.data,
                                     by = c("Location.of.death..zip.code." = "zip_code"))

# write.csv(mpv.data.updated,
#           file = "police_killings_2013-17_updated.csv",
#           row.names = FALSE,
#           na = "")
