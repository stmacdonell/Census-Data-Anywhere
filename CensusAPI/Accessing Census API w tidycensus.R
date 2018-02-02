#Accessing Census API w/ TidyCensus
#Maia Woluchem
#February 2018

#Uses an R package called TidyCensus to access the Census API to pull specific demographic data by race. The resultant dataframe will then be manipulated
#to create counts of the voter eligible population by race. This demographic data will be later added to a given shape file, allowing one to run
#Mayer's test of partisan lean by the given geographies.

#This code pulls the Population Age 18 and Over by race and the Population Housed at Correctional Facilities for Adults (as a proxy for formerly incarcerated
#individuals) by race from the 2010 Decennial Census. These can be considered inputs for a formula that creates a proxy of the count of eligible voters by
#race. To create this proxy, ideally one should also add in a count of citizens in each desired geography, as currently only naturalized citizens and
#citizens by birth are eligible to vote. This method is described on page 13 of Mayer's Expert Testimony in the
#Wisconsin case: https://static1.squarespace.com/static/559c1a7be4b0a2650c6c39b3/t/559d3dcae4b041328d05ed30/1436368330313/Exhibit+2.pdf

#Install packages
install.packages("tidyverse")
install.packages("tidycensus")

library(tidyverse)
library(tidycensus)

#Insert API Key below:
#If you do not have an API key, please request one here: https://api.census.gov/data/key_signup.html
census_api_key("INSERT API KEY HERE", overwrite = FALSE, install = TRUE)


#demographicvars - a list of the variables you desire to include for this table.
#Pop18Plus - Represents the population of the geography 18+ overall
#Pop18PlusXXXX - Represents the population of the geography 18+ by race
#InstitPopXXXX - Represents the population of the geography housed at correctional facitilities for adults

demographicvars <- c(Pop18Plus = "P0100001",
                     Pop18PlusWhite = "P0100003",
                     Pop18PlusBlack = "P0100004",
                     Pop18PlusAmInd = "P0100005",
                     Pop18PlusAsian = "P0100006",
                     Pop18PlusNativeHawPI = "P0100007",
                     Pop18PlusHisp = "P0110002",
                     InstitPopWhite = "PCT020A003",
                     InstitPopBlack = "PCT020B003",
                     InstitPopAmInd = "PCT020C003",
                     InstitPopAsian = "PCT020D003",
                     InstitPopNativeHawPI = "PCT020E003",
                     InstitPopHisp = "PCT020H003"
                     )

#countryvars - A list of counties that hold the desired blocks. Can take input as text (in the format "Travis County" or 'Travis') or as FIPS codes.
countyvars <- c('Travis', 'Harris County')

#This step creates a dataframe that includes the variables at the particular geography you'd like. In this case, it pulls the blocks for Travis & Harris
#counties in Texas, along with the shapes

dataframe_name <- get_decennial(geography = "block", variables = demographicvars,
                        state = "Texas", county = countyvars, geometry = TRUE, output="wide",
                        summary_var = "P0010001")




