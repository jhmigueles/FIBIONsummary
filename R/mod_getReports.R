#' getReports UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_getReports_ui = function(id){
  ns = NS(id)
  tagList(
    # h1(" FIBION Summary")
    # ,
    sidebarPanel(
      shinyjs::useShinyjs(),
      div(
        fileInput(ns("datadir"), "Choose CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv"))
      ),
      hr(),
      div(downloadButton(ns("downloadData"), "Download"))
    ),
    mainPanel(
      fluidRow(
        col_6(
          div(style = 'overflow-x: scroll; overflow-y: scroll;height:400px;',
              tableOutput(ns("daySummary")),
              width = 6, height = "400px")
        ),
        col_6(
          div(style = 'overflow-x: scroll; overflow-y: scroll;height:400px;',
              tableOutput(ns("weekSummary")),
              width = 6, height = "400px")
        )
      )
    )
  )
}

#' getReports Server Functions
#'
#' @noRd 
mod_getReports_server = function(id){
  moduleServer( id, function(input, output, session){
    ns = session$ns
    
    # weekSummary -----------------------------------------------------------------
    # Process files and show output
    weekSummary <- shiny::reactive({
      req(input$datadir)
      upload = list()
      id = list()
      withProgress(message = 'Aggregating data', value = 0, {
        for (nr in 1:length(input$datadir[, 1])) {
          filename_split = unlist(strsplit(input$datadir[[nr, 'datapath']], ".", fixed = T))
          format = filename_split[length(filename_split)]
          if (format == "csv") {
            upload[[nr]] = read.csv(
              file = input$datadir[[nr, 'datapath']]
            )
          }
          
          if (format == "xlsx") {
            upload[[nr]] = openxlsx::read.xlsx(
              file = input$datadir[[nr, 'datapath']]
            )
          }
          
          id[[nr]] = tools::file_path_sans_ext(
            x = basename(input$datadir[[nr, 'datapath']])
          )
          
          incProgress(0.5/length(input$datadir[, 1]),
                      detail = paste("Processing \n", nr, sep = ""))
          # Run function
          output = FIBIONsummary::get.report_FIBION(data = upload[[nr]], outputdir = outputdir, ID = id[[nr]])
          if (nr == 1) {
            daySummary = output$daySummary
            daySummary_clean = output$dayCleanSummary
            weekSummary = output$weekSummary
          } else if (nr > 1) {
            daySummary =  plyr::rbind.fill(daySummary, output$daySummary)
            daySummary_clean =  plyr::rbind.fill(daySummary_clean, output$dayCleanSummary)
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
    
    output$weekSummary = renderTable(weekSummary())
    
    # daySummary -----------------------------------------------------------------
    # Process files and show output
    daySummary <- shiny::reactive({
      req(input$datadir)
      upload = list()
      id = list()
      withProgress(message = 'Aggregating data', value = 0, {
        for (nr in 1:length(input$datadir[, 1])) {
          upload[[nr]] = read.csv(
            file = input$datadir[[nr, 'datapath']]
          )
          id[[nr]] = tools::file_path_sans_ext(
            x = basename(input$datadir[[nr, 'datapath']])
          )
          
          incProgress(0.5/length(input$datadir[, 1]),
                      detail = paste("Processing \n", nr, sep = ""))
          # Run function
          output = FIBIONsummary::get.report_FIBION(data = upload[[nr]], outputdir = outputdir, ID = id[[nr]])
          if (nr == 1) {
            daySummary = output$daySummary
            daySummary_clean = output$dayCleanSummary
            weekSummary = output$weekSummary
          } else if (nr > 1) {
            daySummary =  plyr::rbind.fill(daySummary, output$daySummary)
            daySummary_clean =  plyr::rbind.fill(daySummary_clean, output$dayCleanSummary)
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
    
    output$daySummary = renderTable(daySummary())
    output$daySummary_clean = renderTable(daySummary_clean())
    
    
    # Download button ---------------------------------------------------------
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("output (", Sys.Date(), ").zip")
      },
      content = function(file) {
        # temporary directory to avoid permission issues
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        
        openxlsx::write.xlsx(weekSummary(), file.path(temp_directory, "weekSummary.xlsx"), row.names = FALSE)
        openxlsx::write.xlsx(daySummary_clean(), file.path(temp_directory, "daySummary_clean.xlsx"), row.names = FALSE)
        openxlsx::write.xlsx(daySummary(), file.path(temp_directory, "daySummary.xlsx"), row.names = FALSE)
        
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        
      },
      contentType = "application/zip"
    )
    
  })
}