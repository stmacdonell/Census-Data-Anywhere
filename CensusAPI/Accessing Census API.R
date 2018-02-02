#Accessing Census API
#Maia Woluchem
#Adapted from pre-existing code documented here: https://rstudio-pubs-static.s3.amazonaws.com/19337_2e7f827190514c569ea136db788ce850.html

#master_geographies <- read.csv(file='County_Fips_Codes_by_County.csv',
               #header = FALSE)

key = 'd9174b06d5ec03a39d5f89f411614913e72556e6'

getCensusApi <- function(data_url,key, vars, region, numeric=TRUE){
  if(length(vars)>50){
    vars <- vecToChunk(vars) # Split vars into a list
    get <- lapply(vars, function(x) paste(x, sep='', collapse=","))
    data <- lapply(vars, function(x) getCensusApi2(data_url,key, x, region, numeric=TRUE))
  } else {
    get <- paste(vars, sep='', collapse=',')
    data <- list(getCensusApi2(data_url,key, get, region, numeric=TRUE))
  }
  # Format output.  If there were no errors, than paste the data together
  # If there is an error, just return the unformatted list.
  if(all(sapply(data, is.data.frame))){
    colnames <- unlist(lapply(data, names))
    data <- do.call(cbind,data)
    names(data) <- colnames
    # Prettify the output
    # If there are nonunique colums, remove them
    data <- data[,unique(colnames, fromLast=TRUE)]
    # Reorder columns so that numeric fields follow non-numeric fields
    data <- data[,c(which(sapply(data, class)!='numeric'), which(sapply(data, class)=='numeric'))]
    return(data)
  }else{
    print('unable to create single data.frame in getCensusApi')
    return(data)
  }
}

getCensusApi2 <- function(data_url,key, get, region, numeric=TRUE){
  if(length(get)>1) get <- paste(get, collapse=',', sep='')
  api_call <- paste(data_url, 
                    'key=', key, 
                    '&get=', get,
                    '&', region,
                    sep='')
  
  dat_raw <- try(readLines(api_call, warn="F"))
  if(class(dat_raw)=='try-error') {
    print(api_call)
    return}
  dat_df <- data.frame()
  
  #split the datastream into a list with each row as an element
  # Thanks to roodmichael on github
  tmp <- strsplit(gsub("[^[:alnum:], _]", '', dat_raw), "\\,")
  #dat_df <- rbind(dat_df, t(sapply(tmp, '[')))
  #names(dat_df) <- sapply(dat_df[1,], as.character)
  #dat_df <- dat_df[-1,]
  dat_df <- as.data.frame(do.call(rbind, tmp[-1]), stringsAsFactors=FALSE)
  names(dat_df) <- tmp[[1]]
  # convert to numeric
  # The fips should stay as character... so how to distinguish fips from data?
  # I think all of the data have numbers in the names, the fips do not
  #  Example: field names of B01001_001E vs state
  if(numeric==TRUE){
    value_cols <- grep("[0-9]", names(dat_df), value=TRUE)
    for(col in value_cols) dat_df[,col] <- as.numeric(as.character(dat_df[,col]))
  }
  return(dat_df)
}

vecToChunk <- function(x, max=50){
  s <- seq_along(x)
  x1 <- split(x, ceiling(s/max))
  return(x1)
}

study_area <- data.frame(county = c('Cannon', 'Cheatham', 'Davidson', 'Dickson', 'Hickman', 'Macon', 'Maury', 'Robertson', 'Rutherford', 'Smith', 'Sumner', 'Trousdale', 'Williamson', 'Wilson'),
                         fips = c('015', '021', '037', '043', '081', '111', '119', 
                                  '147', '149', '159', '165', '169', '187', '189'),
                         stringsAsFactors=FALSE)

vars <- c('P0010001','P0030001')

df <- NULL
for(cty in study_area$fips){# For each county
  #Construct the regions part of the API Call
  region = paste("for=block:*&in=state:47+county:", cty, sep='')
  # Pull data
  temp.df <- getCensusApi('http://api.census.gov/data/2010/sf1?', key=key, vars=vars, region=region)
  df <- rbind(df, temp.df)
}

rm(region,temp.df)
head(df)


for(cty in study_area$fips){# For each county
  #Construct the regions part of the API Call
  region = paste("for=tract:*&in=state:47+county:", cty, sep='')
  print(region)
}