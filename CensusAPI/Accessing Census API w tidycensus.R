#Accessing Census API w/ TidyCensus
#Maia Woluchem

install.packages("tidyverse")
install.packages("tidycensus")

library(tidyverse)
library(tidycensus)

census_api_key("d9174b06d5ec03a39d5f89f411614913e72556e6", overwrite = FALSE, install = TRUE)

#census_api_key("d9174b06d5ec03a39d5f89f411614913e72556e6")
demographicvars <- c(Pop18Plus = "P0100001",
                     Pop18PlusWhite = "P0100003",
                     Pop18PlusBlack = "P0100004",
                     Pop18PlusAmInd = "P0100005",
                     Pop18PlusAsian = "P0100006",
                     Pop18PlusNativeHawPI = "P0100007",
                     Pop18PlusHisp = "P0110002")

countyvars <- c('Travis County', 'Harris County', 'Williamson County')

harris <- get_decennial(geography = "block", variables = demographicvars,
                        state = "Texas", county = countyvars, geometry = TRUE,
                        summary_var = "P0010001")

harris_noshape <- get_decennial(geography = "block", variables = demographicvars,
                        state = "Texas", county = "201", geometry = FALSE,
                        summary_var = "P0010001")


