#' Starts a Shiny app that facilitates fixing categorisation
#'
#' Starts a Shiny app that facilitates fixing categorisation
#'
#'
#' @param city The name of a city/municipality.
#' @param country The name of the country. Requested to ensure correct identification of city. 
#' @export
#' 
#' @examples
#' \dontrun{
#' Fix_street_names()
#' }

Fix_street_names <- function(city, country) {
  if (requireNamespace("shiny", quietly = TRUE)==FALSE) {
    stop("You need to install the `shiny` package with `install.packages('shiny')` to use this function.")
  }
  
  shiny::shinyApp(
    ui = shiny::fluidPage(
      shiny::h1(paste0("Fix categorisation of streets in ", city, ", ", country)),
      DT::dataTableOutput('street_names'),
      shiny::downloadButton(outputId = "download_rds", label = "Download data as .rds"),
      shiny::downloadButton(outputId = "download_csv", label = "Download data as .csv")
    ),
    server = function(input, output, session) {
      
      city_roads_gender <- {
        temp <- readRDS(file = file.path("data", "gendered_street_names", tolower(country), paste0("city_roads_gender-", tolower(city), ".rds"))) %>% 
          dplyr::mutate(name = as.character(name)) %>% 
          dplyr::filter(is.na(name)==FALSE) %>% 
          dplyr::distinct(name, .keep_all = TRUE) %>% 
          dplyr::mutate(female = dplyr::if_else(condition = is.na(Gender)==FALSE&Gender=="female",
                                                true =  paste0('<input type="radio" name="', name, '" value="Female" checked="checked"/>'),
                                                false =  paste0('<input type="radio" name="', name, '" value="Female"/>')),
                        male = dplyr::if_else(condition = is.na(Gender)==FALSE&Gender=="male",
                                              true =  paste0('<input type="radio" name="', name, '" value="Male" checked="checked"/>'),
                                              false =  paste0('<input type="radio" name="', name, '" value="Male"/>')),
                        other = dplyr::if_else(condition = is.na(Gender)==TRUE,
                                               true =  paste0('<input type="radio" name="', name, '" value="Other" checked="checked"/>'),
                                               false =  paste0('<input type="radio" name="', name, '" value="Other"/>'))) %>% 
          dplyr::transmute(`Street name` = name, female, male, other, `Automatic gender` = Gender, `Automatic description` = Description)
        m <- as.matrix(temp)
        rownames(m) <- temp$`Street name`
        m[,-1]
      }
      
      output$street_names <- DT::renderDataTable(
        city_roads_gender, escape = FALSE, selection = 'none', server = FALSE,
        options = list(dom = 't', paging = FALSE, ordering = FALSE),
        callback = htmlwidgets::JS("table.rows().every(function(i, tab, row) {
          var $this = $(this.node());
          $this.attr('id', this.data()[0]);
          $this.addClass('shiny-input-radiogroup');
        });
        Shiny.unbindAll(table.table().node());
        Shiny.bindAll(table.table().node());")
      )
      
      selected <- reactive({
        tibble::tibble(Street_name = rownames(city_roads_gender),
                       Gender = purrr::map_chr(.x = rownames(city_roads_gender), .f = function(i) input[[i]]))
      })
      
      # output$selected <- shiny::renderPrint({
      #   tibble::tibble(Street_name = rownames(city_roads_gender),
      #                  Gender = purrr::map_chr(.x = rownames(city_roads_gender), .f = function(i) input[[i]]))
      # })
      
      
      output$download_rds <- downloadHandler(
        filename = function() {
          filename <- paste0("city_roads_gender_fixed-", tolower(city), ".rds")
          dir.create(path = file.path("data", "gendered_street_names_fixed"), showWarnings = FALSE)
          dir.create(path = file.path("data", "gendered_street_names_fixed", tolower(country)), showWarnings = FALSE)
          saveRDS(object = selected(), file.path("data", "gendered_street_names_fixed", tolower(country), filename))
          filename
        },
        content = function(con) {
          saveRDS(selected(), con)
        }
      )
      
      output$download_csv <- downloadHandler(
        filename = function() {
          filename <- paste0("city_roads_gender_fixed-", tolower(city), ".csv")
          dir.create(path = file.path("data", "gendered_street_names_fixed"), showWarnings = FALSE)
          dir.create(path = file.path("data", "gendered_street_names_fixed", tolower(country)), showWarnings = FALSE)
          readr::write_csv(x = selected(), path = file.path("data", "gendered_street_names_fixed", tolower(country), filename))
          filename
        },
        content = function(con) {
          readr::write_csv(x = selected(), con)
        }
      )
    }
  )
}
