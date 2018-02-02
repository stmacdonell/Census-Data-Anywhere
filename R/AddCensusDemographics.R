# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(sf)

#This takes a shapefile, state (abbrevation, or FIPS code), and if desired a vector of county FIPS codes and
#returns the same shapefile with Census Demographics attached
AddCensusDemographics <- function(map,state,county=NULL){

  #make sure the state is valid
  if(!((state%in%StateAndCountyFIPScodes2010$StateAbr)|(state%in%StateAndCountyFIPScodes2010$StateFIPS))){
    stop("Invalid State")
  }

  #convert abbrevation to FIPS
  if(state%in%StateAndCountyFIPScodes2010$StateAbr){
    states <- unique(StateAndCountyFIPScodes2010[,1:2])
    state <- states$StateFIPS[states$StateAbr==state]
    rm(states)
  }

  if(is.null(county)){
    county <- unique(StateAndCountyFIPScodes2010$CountyFIPS[StateAndCountyFIPScodes2010$StateFIPS==state])
  }else{

  }

}
