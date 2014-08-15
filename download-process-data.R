
library(plyr)
library(rgdal)

# Years
years <- c(1994:2012)

# Zip Codes
zip_codes <- read.csv("data/geocorr2k.csv", colClasses = "character")
zip_codes <- zip_codes[zip_codes$cbsa == "41180", ]
zip_codes <- unique(zip_codes$zcta5)


get_cbp_zip_totals <- function(year, zip){
  # Create the temporary folder
  temp_file <- tempfile()
  
  # Download file depending on year
  if(year <= 2001){
    prefix <- "http://www2.census.gov/Econ2001_And_Earlier/CBP_CSV/zbp"
    web_file <- paste0(
      prefix, 
      substr(year, 3, 4), 
      "totals.zip"
      )
  }else{
    prefix <- "http://www2.census.gov/econ"
    web_file <- paste0(
      prefix, 
      year,
      "/CBP_CSV/zbp",
      substr(year, 3, 4), 
      "totals.zip"
      )
  }
  download.file(web_file, temp_file) 
  
  # Import data
  cbp <- read.csv(unzip(temp_file))
  colnames(cbp) <- tolower(colnames(cbp))
  
  # Format Data
  cbp <- droplevels(cbp[cbp$zip %in% as.character(zip_codes), ])
  cbp <- cbp[, c("zip", "name", "emp", "empflag", "qp1", "ap", "est")]
  cbp <- transform(cbp, year = year)
  
  # Delete Temp File
  unlink(temp_file)
  
  return(cbp)
}

# Download Files
cbp <- lapply(years, function(x){get_cbp_zip_totals(x, zip_codes)})
cbp <- do.call("rbind", cbp)

# Drop Temp txt file
zip_files <- dir()
zip_files <- lapply(zip_files[grep("zbp", zip_files)], unlink)
rm(zip_files)

# Covert Zero Values to NA
cbp$emp[cbp$emp == 0] <- NA
cbp$qp1[cbp$qp1 == 0] <- NA
cbp$ap[cbp$ap == 0]   <- NA
cbp$est[cbp$est == 0] <- NA

# Download Zip Map
web_file <- "http://www2.census.gov/geo/tiger/TIGER2012/ZCTA5/tl_2012_us_zcta510.zip"
temp_file <- tempfile()
download.file(web_file, temp_file) 
unzip(temp_file, exdir = "shape")
unlink(temp_file)
zip_map <- readOGR("shape", "tl_2012_us_zcta510")

# Drop Temp shp files
shp_files <- dir("shape")
shp_files <- lapply(
  paste("shape", shp_files[grep("tl_2012_us_zcta510", shp_files)], sep = "/"), 
  unlink
  )
rm(shp_files)

names(zip_map) <- tolower(names(zip_map))

zip_map@data <- transform(zip_map@data, zcta5ce10 = as.character(zcta5ce10))
zip_map <- subset(
  zip_map, 
  zcta5ce10 %in% zip_codes, 
  select = c("zcta5ce10", "geoid10", "intptlat10", "intptlon10")
  )

names(zip_map) <- c("zcta", "geo_id", "lat", "long")

# Save
writeOGR(zip_map, "shape", "stl-zcta-map", "ESRI Shapefile")
save(cbp, file = "data/cbp.RData")
