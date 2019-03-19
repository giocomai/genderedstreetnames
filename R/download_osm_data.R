#' Download OSM data for whole countries.
#' 
#' @param countries One or more country names. For details on the country names see http://download.geofabrik.de/ 
#' @param continent Defaults to `Europe`. Available options are: `africa`, `asia`, `australia-oceania`, `central-america`, `europe`, `north-america`, `south-america`. Only one continent at a time can be chosen. 
#' @return Used only for its side effects (downloads osm data).
#' @examples
#' 
#' Download_OSM(search = "Romania")
#' 
#' @export
#' 

Download_OSM <- function(countries, continent = "Europe") {
  countries <- tolower(countries)
  continent <- tolower(continent)
  
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "shp_zip"), showWarnings = FALSE)
  for (i in countries) {
    if (is.element(i, big_countries)==TRUE) {
      dir.create(path = file.path("data", "shp_zip", i), showWarnings = FALSE)
      links <- paste0("http://download.geofabrik.de/europe/", 
                      xml2::read_html(x = paste0("http://download.geofabrik.de/europe/", 
                                                 i,
                                                 ".html")) %>% 
                        rvest::html_nodes(xpath = paste0("//a")) %>% 
                        xml2::xml_attr("href") %>% 
                        stringr::str_subset(pattern = stringr::fixed(".shp.zip")))
      filenames <- file.path("data", "shp_zip", stringr::str_extract(string = links, pattern = paste0(i, "/.*shp.zip")))
      for (j in seq_along(links)) {
        file_location <- filenames[j]
        if (file.exists(file_location)==FALSE) {
          download.file(url = links[j],
                        destfile = file_location)
          files_to_extract <- unzip(zipfile = file_location, list = TRUE) %>% 
            filter(stringr::str_detect(string = Name, pattern = "roads"))
          unzip(zipfile = file_location, files = files_to_extract %>% pull(Name), exdir = file.path("data", "roads_shp_big_countries", i, str_remove(string = filenames[j], pattern = fixed(paste0("data/shp_zip/", i, "/"))) %>% 
                                                                                                     str_remove(pattern = "-latest-free.shp.zip")))
        }
      }
      
    } else {
      file_location <- file.path("data", "shp_zip", paste0(i, "-latest-free.shp.zip"))
      if (file.exists(file_location)==FALSE) {
        download.file(url = paste0("http://download.geofabrik.de/", continent, "/", i, "-latest-free.shp.zip"),
                      destfile = file_location)
      }
      
    }
    
  }
}

#' Extract shape files of roads from previously downloaded 
#' 
#' @param countries The query to be searched.
#' @param export_rds Stores imported shape files as an rds file locally.
#' @param export_csv Stores imported shape files (excluding the geographic information) as a csv file locally.
#' @return A data.frame with geographic data (sf).
#' @examples
#' 
#' Extract_roads(search = "Romania")
#' 
#' @export
#' 

Extract_roads <- function(countries, export_rds = FALSE, export_csv = FALSE) {
  dir.create(path = file.path("data", "roads_shp"), showWarnings = FALSE)
  countries <- tolower(countries)
  
  for (i in countries) { 
    if (is.element(i, big_countries)==TRUE) {
      filenames <- list.files(path = file.path("data", "shp_zip", i), pattern = "shp.zip")
      for (j in seq_along(filenames)) {
        file_location <- filenames[j]
        
        files_to_extract <- unzip(zipfile = file_location, list = TRUE) %>% 
          tibble::as_tibble() %>% 
          dplyr::pull(Name) 
        
        unzip(zipfile = file_location,
              files = files_to_extract[stringr::str_detect(string = files_to_extract, pattern = "roads")],
              exdir = file.path("data",
                                "roads_shp",
                                i,
                                stringr::str_remove(string = filenames[j],
                                                    pattern = stringr::fixed(paste0("data/shp_zip/", i, "/"))) %>% 
                                  stringr::str_remove(pattern = "-latest-free.shp.zip")))
      }
      regions <- list.files(path = file.path("data", "shp_zip", i))
      roads <- purrr::map_dfr(.x = regions, .f = function(x) sf::st_read(dsn = file.path("data", "shp_zip", i, x)))
    } else {
      file_location <- file.path("data", "shp_zip", paste0(i, "-latest-free.shp.zip"))
      if (file.exists(file_location) == FALSE) {
        warning(paste0("File not available. Please download the data first with `Download_OSM('", i, "')`" ))
      } else {
        files_to_extract <- unzip(zipfile = file_location, list = TRUE) %>% 
          tibble::as_tibble() %>% 
          dplyr::pull(Name) 
        unzip(zipfile = file_location,
              files = files_to_extract[stringr::str_detect(string = files_to_extract, pattern = "roads")],
              exdir = file.path("data", "roads_shp", i))
        
        roads <- sf::st_read(dsn = file.path("data", "roads_shp", i)) 
      }
    }
    if (export_rds == TRUE) {
      dir.create(path = file.path("data", "roads_rds"), showWarnings = FALSE)
      dir.create(path = file.path("data", "roads_rds", i), showWarnings = FALSE)
      saveRDS(object = roads,
              file = file.path(file.path("data", "roads_rds", paste0(i, "_roads.rds"))))
    }
    if (export_csv == TRUE) {
      dir.create(path = file.path("data", "roads_csv"), showWarnings = FALSE)
      dir.create(path = file.path("data", "roads_csv", i), showWarnings = FALSE)
      readr::write_csv(x = roads %>% st_set_geometry(NULL),
                       path = file.path(file.path("data", "roads_csv", paste0(i, "_roads.csv"))))
    }
    return(roads)
  }
}

#' Get city boundaries 
#' 
#' @param city The name of a city/municipality.
#' @param country The name of the country. Requested to ensure correct identification of city. 
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return An sf polygon.
#' @examples
#' 
#' Get_city_boundaries(search = "Sibiu, Romania")
#' 
#' @export
#' 

Get_city_boundaries <- function(city, country, cache = TRUE) {
  query <- paste(city, country, sep = ", ")
  
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "city_boundaries"), showWarnings = FALSE)
  dir.create(path = file.path("data", "city_boundaries",  tolower(country)), showWarnings = FALSE)
  
  file_location <- file.path("data", "city_boundaries", tolower(country), paste0(tolower(city), ".rds"))
  if(file.exists(file_location)==FALSE) {
    city_boundary <- osmdata::opq(bbox = query) %>% 
      osmdata::add_osm_feature(key = "boundary", value = "administrative") %>% 
      osmdata::add_osm_feature(key = "admin_level", value = "6") %>% 
      osmdata::add_osm_feature(key = "place", value = "city") %>% 
      osmdata::osmdata_sf() %>% 
      .$osm_polygons
    if (cache == TRUE) {
      dir.create(path = file.path("data", "city_boundaries"), showWarnings = FALSE)
      dir.create(path = file.path("data", "city_boundaries", tolower(city)), showWarnings = FALSE)
      saveRDS(object = city_boundary, file = file_location)
    }
  } else {
    city_boundary <- readRDS(file = file_location)
  }
  return(city_boundary)
}

#' Keep only roads within a given boundary.
#' 
#' @param boundary An object typically created with `Get_city_boundaries()`
#' @param country The name of the country. Requested to ensure correct identification of city. 
#' @return A data frame of the sf class including all roads insidet the given boundary
#' @examples
#' 
#' Subset_roads(city_boundary, roads)
#' 
#' @export
#' 
Subset_roads <- function(boundary, roads) {
  roads[sf::st_within(roads, city_boundary) %>% lengths > 0,]
}
