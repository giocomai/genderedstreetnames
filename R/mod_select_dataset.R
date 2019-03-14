# Module UI
  
#' @title   mod_select_dataset_ui and mod_select_dataset_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#' @param dataset_folder_name Datasets will be searched from the given folder. 
#'
#' @rdname mod_select_dataset
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_select_dataset_ui <- function(id){
  ns <- NS(id)
  shiny::uiOutput(outputId = ns("select_dataset_UI"))
}
    
# Module Server
    
#' @rdname mod_select_dataset
#' @export
#' @keywords internal
    
mod_select_dataset <- function(input, output, session, dataset_folder_name){
  ns <- session$ns
  datasets <- list.dirs(path = dataset_folder_name,
                        full.names = FALSE,
                        recursive = FALSE)
  
  output$select_dataset_UI <- shiny::renderUI({
    shiny::selectizeInput(inputId = "selected_datasets",
                          label = "Available dataset",
                          choices = as.list(c("", datasets)),
                          selected = "",
                          multiple = FALSE,
                          width = "95%")
    
  })
}
    
## To be copied in the UI
# mod_select_dataset_ui("select_dataset_ui_1")
    
## To be copied in the server
# callModule(mod_select_dataset, "select_dataset_ui_1")
 
