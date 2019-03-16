#' Download OSM data for whole countries.
#' 
#' @param countries The query to be searched.
#' @return Used only for its side effects (downloads osm data).
#' @examples
#' 
#' Download_OSM(search = "Romania")
#' 
#' @export
#' 

Download_OSM <- function(countries) {
  countries <- tolower(countries)
  
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "shp_zip"), showWarnings = FALSE)
  
  for (i in countries) {
    file_location <- file.path("data", "shp_zip", paste0(i, "-latest-free.shp.zip"))
    if (file.exists(file_location)==FALSE) {
      download.file(url = paste0("http://download.geofabrik.de/europe/", i, "-latest-free.shp.zip"),
                    destfile = file_location)
    }
  }
}

#' Extract shape files of roads from previously downloaded 
#' 
#' @param countries The query to be searched.
#' @return Used only for its side effects (unzips shapefiles of roads from previously downloaded osm data).
#' @examples
#' 
#' Extract_roads(search = "Romania")
#' 
#' @export
#' 

Extract_roads <- function(countries) {
  dir.create(path = file.path("data", "shp_roads"), showWarnings = FALSE)
  
  countries <- tolower(countries)
  for (i in countries) {
    file_location <- file.path("data", "shp_zip", paste0(i, "-latest-free.shp.zip"))
    if (file.exists(file_location) == FALSE) {
      warning(paste0("File not available. Please download the data first with `Download_OSM('", i, "')`" ))
    } else {
      files_to_extract <- unzip(zipfile = file_location, list = TRUE) %>% 
        filter(str_detect(string = Name, pattern = "roads"))
      
      unzip(zipfile = file_location,
            files = files_to_extract %>% pull(Name),
            exdir = file.path("data", "shp_roads", i))
    }
  }
}
