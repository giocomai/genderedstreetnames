#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(module = mod_select_dataset, id =  "select_dataset_ui_1", dataset_folder_name = "data")
  
  #output$current_folder <- shiny::renderText(expr = getwd())
  
}