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
    sidebarPanel(
      shinyjs::useShinyjs(),
      div(
        actionButton(inputId = ns("datadir"), 
                     label = "Select the files to process",
                     width = "80%")
      ),
      br(),
      div(
        actionButton(inputId = ns("outputdir"), 
                     label = "Select directory to save output",
                     width = "80%")
      ),
      fluidRow(
        col_6(
          shinyjs::disabled(
            actionButton(inputId = ns("run"), icon = icon("play"),
                         label = "Run",
                         width = "80%")
          )
        )
      )
    ),
    mainPanel(
      fluidRow(
        h1("FIBION Summary")
      ),
      fluidRow(
        col_6(
          div(style = 'overflow-x: scroll; overflow-y: scroll;height:300px;',
              tableOutput(ns("selected")), 
              width = 6, height = "300px")
        ),
        col_6(
          div(style = 'overflow-x: scroll; overflow-y: scroll;height:300px;',
              tableOutput(ns("outdir")), 
              width = 6, height = "300px")
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
    
    # Datadir -----------------------------------------------------------------
    datadir = NULL
    makeReactiveBinding("datadir")
    observeEvent(input$datadir,{
      # Reactive datadir
      datadir <<- tcltk::tk_choose.files(getwd())
      # Enable run
      if((length(datadir) > 0 & length(outputdir) > 0)) shinyjs::enable("run")
    })
    # Show the selected files
    output$selected = renderTable({
      if(length(datadir) < 1) {
        data.frame(Files = "No selected files to process")
      } else {
        data.frame(Files = datadir)
      }
    })
    
    # Outputdir -----------------------------------------------------------------
    outputdir = NULL
    makeReactiveBinding("outputdir")
    observeEvent(input$outputdir,{
      # Define outputdir
      outputdir <<- tcltk::tk_choose.dir(getwd())
      # Enable run
      if((length(datadir) > 0 & length(outputdir) > 0)) shinyjs::enable("run")
    })
    # Show the selected directory
    output$outdir = renderTable({
      if(length(outputdir) < 1) {
        data.frame(Directory = "No directory selected to save output")
      } else {
        data.frame(Directory = outputdir)
      }
    })
    
    # Run get.Reports -----------------------------------------------------------
    observeEvent(input$run,{
      # Disable inputs while running
      input_list <- reactiveValuesToList(input)
      # disable_inputs(input_list, F)
      
      # RUN function ------------------------------------------------------------
      withProgress(message = 'Aggregating data', value = 0, {
        for (i in 1:length(datadir)) {
          fname = basename(datadir[i])
          incProgress(0.5/length(datadir), 
                      detail = paste("Processing \n", fname, sep = ""))
          # Run function
          output = FIBIONsummary::get.report_FIBION(datadir = datadir[i], outputdir = outputdir)
          if (i == 1) {
            daySummary = output$daySummary
            weekSummary = output$weekSummary
          } else if (i > 1) {
            daySummary =  plyr::rbind.fill(daySummary, output$daySummary)
            weekSummary = plyr::rbind.fill(weekSummary, output$weekSummary)
          }
          incProgress(0.5/length(datadir), 
                      detail = paste("Done \n", fname, sep = ""))
        }
        # disable_inputs(input_list,T)
        utils::write.csv(daySummary, file = file.path(outputdir, "daySummary.csv"), row.names = F)
        utils::write.csv(weekSummary, file = file.path(outputdir, "weekSummary.csv"), row.names = F)
      })
    })
  })
}