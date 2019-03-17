#' @import shiny
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(module = mod_select_dataset, id =  "select_dataset_ui_1", dataset_folder_name = "data/cities")
  callModule(mod_radio_datatable, "radio_datatable_ui_1")
  
  
  #debugging
  observeEvent(input$browser,{
    browser()
  })
  
  
}