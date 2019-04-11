#' Download OSM data for whole countries.
#' 
#' @param countries One or more country names. For details on the country names see http://download.geofabrik.de/ 
#' @param continent Defaults to `Europe`. Available options are: `africa`, `asia`, `australia-oceania`, `central-america`, `europe`, `north-america`, `south-america`. Only one continent at a time can be chosen. 
#' @return Used only for its side effects (downloads osm data).
#' @examples
#' 
#' download_OSM(search = "Romania")
#' 
#' @export
#' 

download_OSM <- function(countries, continent = "Europe") {
  countries <- tolower(countries)
  continent <- tolower(continent)
  
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "shp_zip"), showWarnings = FALSE)
  for (i in countries) {
    # big countries
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
        }
      }
      # small countries
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
#' extract_roads(search = "Romania")
#' 
#' @export
#' 

extract_roads <- function(countries, export_rds = FALSE, export_csv = FALSE) {
  dir.create(path = file.path("data", "roads_shp"), showWarnings = FALSE)
  countries <- tolower(countries)
  
  for (i in countries) { 
    if (is.element(i, big_countries)==TRUE) {
      filenames <- list.files(path = file.path("data", "shp_zip", i), pattern = "shp.zip", full.names = TRUE)
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
      regions <- list.files(path = file.path("data", "shp_zip", i)) %>% stringr::str_remove(pattern = stringr::fixed("-latest-free.shp.zip"))
      roads <- purrr::map_dfr(.x = regions, .f = function(x) sf::st_read(dsn = file.path("data", "roads_shp", i, x)))
    } else {
      file_location <- file.path("data", "shp_zip", paste0(i, "-latest-free.shp.zip"))
      if (file.exists(file_location) == FALSE) {
        warning(paste0("File not available. Please download the data first with `download_OSM('", i, "')`" ))
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
#' @param administrative Defaults to NULL. If TRUE, filters only boundaries recorded as administrative. For more information, see: https://wiki.openstreetmap.org/wiki/Tag%3aboundary=administrative
#' @param admin_level Defaults to 6. For more information see: https://wiki.openstreetmap.org/wiki/Tag:boundary%3Dadministrative#10_admin_level_values_for_specific_countries
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return An sf polygon.
#' @examples
#' 
#' get_city_boundaries(search = "Sibiu, Romania")
#' 
#' @export
#' 

get_city_boundaries <- function(city, country, admin_level = 6, administrative = NULL, cache = TRUE) {
  query <- paste(city, country, sep = ", ")
  
  dir.create(path = "data", showWarnings = FALSE)
  dir.create(path = file.path("data", "city_boundaries"), showWarnings = FALSE)
  dir.create(path = file.path("data", "city_boundaries",  tolower(country)), showWarnings = FALSE)

  file_location <- file.path("data", "city_boundaries", tolower(country), paste0(tolower(iconv(x = city, to = "ASCII//TRANSLIT")), ".rds"))
  if(file.exists(file_location)==FALSE) {
    if (is.null(administrative)) {
      
       temp <- osmdata::opq(bbox = query) %>% 
        osmdata::add_osm_feature(key = "admin_level", value = admin_level) %>% 
        osmdata::add_osm_feature(key = "place", value = "city") %>% 
        osmdata::osmdata_sf() 
       
       if (is.null(temp$osm_polygons)==FALSE) {
         city_boundary <- temp$osm_polygons
       } else if (is.null(temp$osm_multipolygons)==FALSE) {
         city_boundary <- temp$osm_multipolygons
       }

    } else {
      temp <- osmdata::opq(bbox = query) %>% 
        osmdata::add_osm_feature(key = "boundary", value = "administrative") %>% 
        osmdata::add_osm_feature(key = "admin_level", value = admin_level) %>% 
        osmdata::add_osm_feature(key = "place", value = "city") %>% 
        osmdata::osmdata_sf() 
      
      if (is.null(temp$osm_polygons)==FALSE) {
        city_boundary <- temp$osm_polygons
      } else if (is.null(temp$osm_multipolygons)==FALSE) {
        city_boundary <- temp$osm_multipolygons
      }
    }
    if (is.null(city_boundary)) {
      usethis::ui_oops("City boundary not found.")
    } else {
      if (cache == TRUE) {
        dir.create(path = file.path("data", "city_boundaries"), showWarnings = FALSE)
        saveRDS(object = city_boundary, file = file_location)
      }
    }
  } else {
    city_boundary <- readRDS(file = file_location)
  }
  return(city_boundary)
}


#' Get boundary by id
#' 
#' @param id A numeric vector of length 1, must correspond to the id of a boundary object on OpenStreetMap. 
#' @param type Defaults to "way".
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return An sf polygon.
#' @examples
#' 
#' get_boundary_by_id(id = c(Arad = 45422208))
#' 
#' @export
#' 
get_boundary_by_id <- function(id,
                               type = "way",
                               cache = TRUE) {
  fs::dir_create(path = file.path("data", "city_boundaries",  "by_id"),
                 recursive = TRUE)
  
  if (is.numeric(id)==TRUE) {
    query_by_id <- genderedstreetnames::create_city_boundary_id_combo(id = id, type = type)
  } else if (class(id)=="list") {
    query_by_id <- id
  } else if (is.na(as.numeric(id)==FALSE)) {
    query_by_id <- genderedstreetnames::create_city_boundary_id_combo(id = as.numeric(id), type = type)
  } else {
    stop("Wrong id format. `id` must be an integer or an object created with `create_city_boundary_id_combo()`.")
  }
  
  file_location <- file.path("data", "city_boundaries", "by_id", paste0(tolower(query_by_id$type), "-", query_by_id$id, ".rds"))
  
  if(file.exists(file_location)==FALSE) {
    temp <- osmdata::opq_osm_id(type = query_by_id$type,
                                id = query_by_id$id) %>%
      osmdata::opq_string() %>%
      osmdata::osmdata_sf()
    
    if (is.null(temp$osm_polygons)==FALSE) {
      city_boundary <- temp$osm_polygons
    } else if (is.null(temp$osm_multipolygons)==FALSE) {
      city_boundary <- temp$osm_multipolygons
    }
       
    if (cache == TRUE) {
      saveRDS(object = city_boundary, file = file_location)
      if (is.null(query_by_id$city)==FALSE&is.null(query_by_id$country)==FALSE) {
        file_city_location <- file.path("data", "city_boundaries", tolower(query_by_id$country), paste0(tolower(iconv(x = query_by_id$city, to = "ASCII//TRANSLIT")), ".rds"))
        saveRDS(object = city_boundary, file = file_city_location)
      }
    }
  } else {
    city_boundary <- readRDS(file = file_location)
  }
  return(city_boundary)
}

#' Create a city/id combination for proper caching
#' 
#' @param id A numeric vector of length 1, must correspond to the id of a boundary object on OpenStreetMap. 
#' @param city The name of a city/municipality.
#' @param country The name of the country. Requested to ensure correct identification of city. 
#' @param type One of either "way", "relation", or "node". 
#' @param cache Logical, defaults to TRUE. If TRUE, stores data in local subfolder data/cities/country_name/city_name.rds
#' @return A list, typically to be fed into `get_boundary_by_id()``
#' @examples
#' 
#' create_city_boundary_id_combo(id = 45422208, type = "way", city = "Arad", country = "Romania")
#' 
#' # https://www.openstreetmap.org/relation/46663 
#' create_city_boundary_id_combo(id = 46663, type = "relation", city = "Trento", country = "Italy", )
#' 
#' @export  
create_city_boundary_id_combo <- function(id,
                                          type,
                                          city = NULL,
                                          country = NULL,
                                          cache = TRUE) {
  combo <- list(city = city,
                country = country,
                id = as.numeric(id),
                type = type)
  if (cache==TRUE) {
    fs::dir_create(path = file.path("data", "city_boundary_id_combo", country),
                   recursive = TRUE)
    saveRDS(object = combo,
            file = file.path("data",
                             "city_boundary_id_combo",
                             country,
                             paste0(tolower(iconv(x = city, to = "ASCII//TRANSLIT")),
                                    ".rds")))
  }
  return(combo)
}

#' Keep only roads within a given boundary.
#' 
#' @param boundary An object typically created with `get_city_boundaries()`
#' @param country The name of the country. Requested to ensure correct identification of city. 
#' @return A data frame of the sf class including all roads insidet the given boundary
#' @examples
#' 
#' subset_roads(city_boundary, roads)
#' 
#' @export
#' 
subset_roads <- function(boundary, roads) {
  roads[sf::st_within(roads, boundary) %>% lengths > 0,]
}
