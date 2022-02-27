#' getReports UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_getReports_ui <- function(id){
  ns <- NS(id)
  tagList(
    h1(" FIBION Summary")
    ,
    sidebarPanel(
      shinyjs::useShinyjs(),
      div(
        fileInput(ns("datadir"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
      )
    ),
    mainPanel(
      fluidRow(
        col_6(
          div(style = 'overflow-x: scroll; overflow-y: scroll;height:600px;',
              tableOutput(ns("daySummary")),
              width = 6, height = "600px")
        ),
        col_6(
          div(style = 'overflow-x: scroll; overflow-y: scroll;height:600px;',
              tableOutput(ns("weekSummary")),
              width = 6, height = "600px")
        )
      )
    )
  )
}

#' getReports Server Functions
#'
#' @noRd 
mod_getReports_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    # weekSummary -----------------------------------------------------------------
    # Process files and show output
    output$weekSummary <- renderTable({
      req(input$datadir)
      upload = list()
      id = list()
      withProgress(message = 'Aggregating data', value = 0, {
        for (nr in 1:length(input$datadir[, 1])) {
          upload[[nr]] <- read.csv(
            file = input$datadir[[nr, 'datapath']]
          )
          id[[nr]] <- tools::file_path_sans_ext(
            x = basename(input$datadir[[nr, 'datapath']])
          )
          
          incProgress(0.5/length(input$datadir[, 1]), 
                      detail = paste("Processing \n", nr, sep = ""))
          # Run function
          output = FIBIONsummary::get.report_FIBION(data = upload[[nr]], outputdir = outputdir, ID = id[[nr]])
          if (nr == 1) {
            daySummary = output$daySummary
            weekSummary = output$weekSummary
          } else if (nr > 1) {
            daySummary =  plyr::rbind.fill(daySummary, output$daySummary)
            weekSummary = plyr::rbind.fill(weekSummary, output$weekSummary)
          }
          incProgress(0.5/length(datadir), 
                      detail = paste("Done \n", nr, sep = ""))
        }
        # disable_inputs(input_list,T)
        # utils::write.csv(daySummary, file = file.path(outputdir, "daySummary.csv"), row.names = F)
        # utils::write.csv(weekSummary, file = file.path(outputdir, "weekSummary.csv"), row.names = F)
      })
      return(weekSummary)
    })
    
    # daySummary -----------------------------------------------------------------
    # Process files and show output
    output$daySummary <- renderTable({
      req(input$datadir)
      upload = list()
      withProgress(message = 'Aggregating data', value = 0, {
        for (nr in 1:length(input$datadir[, 1])) {
          upload[[nr]] <- read.csv(
            file = input$datadir[[nr, 'datapath']]
          )
          id[[nr]] <- tools::file_path_sans_ext(
            x = basename(input$datadir[[nr, 'datapath']])
          )
          
          incProgress(0.5/length(input$datadir[, 1]), 
                      detail = paste("Processing \n", nr, sep = ""))
          # Run function
          output = FIBIONsummary::get.report_FIBION(data = upload[[nr]], outputdir = outputdir, ID = id[[nr]])
          if (nr == 1) {
            daySummary = output$daySummary
            weekSummary = output$weekSummary
          } else if (nr > 1) {
            daySummary =  plyr::rbind.fill(daySummary, output$daySummary)
            weekSummary = plyr::rbind.fill(weekSummary, output$weekSummary)
          }
          incProgress(0.5/length(datadir), 
                      detail = paste("Done \n", nr, sep = ""))
        }
        # disable_inputs(input_list,T)
        # utils::write.csv(daySummary, file = file.path(outputdir, "daySummary.csv"), row.names = F)
        # utils::write.csv(weekSummary, file = file.path(outputdir, "weekSummary.csv"), row.names = F)
      })
      return(daySummary)
    })
  })
}