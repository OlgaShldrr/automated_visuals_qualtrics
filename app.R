library(qualtRics)
library(shinymaterial)
library(shiny)
library(shinyjs)
library(tidyverse)
library(knitr)
library(zip)
library(openxlsx)
library(V8)

#clear the folder with unzipped file every time the app is run

file.remove(file=list.files(path="unzipped", full.names = TRUE))
jsCode <- "shinyjs.spinShow = function() {$('#cover-spin').show(0);}; shinyjs.spinStop = function() {$('#cover-spin').hide(0);}"

# Define UI for application 
ui <- material_page(
    includeCSS("www/css/style.css"),
    includeScript("www/js/functions.js"),
    shinyjs::useShinyjs(),
    extendShinyjs(text = jsCode),
    shinyjs::inlineCSS(list(.big = "color: red;")),
    
    # Application title
    title = "Visualizations for Qualtrics surveys",
    tags$br(),
    # Sidebar with select and text inputs for which survey and which institutions 
    material_row(
        material_column(width = 6,
                        tags$div(id = "cover-spin"),
                                      material_file_input("upload",
                                                        "Upload file..."),
                                      # Download button 
                                      downloadButton(outputId = "download", label = "Generate a report")
                        )
        ),
        material_column(width = 9,  
                        # Show the initial text and the progress of files
                        material_card(title = "Output", depth = 4,
                                      uiOutput("initial_text"),
                                      div(id = "text")
                        )
        )
    )

server = function(input, output, session) {
    
    output$download <- downloadHandler(
        # For PDF output, change this to "report.pdf"
        filename = paste(Sys.Date(),"-reports.zip", sep=""),
        content = function(file) {
            # Copy the report file to a temporary directory before processing it, in
            # case we don't have write permissions to the current working dir (which
            # can happen when deployed).
            
            if (grepl(x=input$upload$datapath, pattern = ".zip")) {
                unzip(zipfile = input$upload$datapath, exdir = "unzipped")
                evaluations <-  read_survey(list.files(path="unzipped", full.names = TRUE))
                
            } else {
                evaluations <- read_survey(input$upload$datapath)
            }
            
            assign("evaluations", evaluations, envir= .GlobalEnv)
            
    #        validate(
    #            need(length(select(read_survey(input$upload$datapath), starts_with("Q"), -contains("."))) == 0, "Please autonumber your survey"),
    #            need(input$upload != "", "Please select a data set")
    #        )
            
            
            source("tidy_evaluations.R")
            
            # Create temp directory
            
            
            # Name of folder in zip
            out_dir <- tempfile()
            
            dir.create(out_dir)
            
            # fileName of section errors
            out_xlsx <<- NULL
            
            rmd_file <- "visualizations.Rmd"
            

            #evaluations_tidy <- evaluations_tidy[rowSums(is.na(dplyr::select(evaluations_tidy, dplyr::starts_with("q")))) != ncol(dplyr::select(evaluations_tidy, dplyr::starts_with("q"))),]
            #assign("evaluations_tidy", evaluations_tidy, envir= .GlobalEnv)
            meetings <- unique(evaluations_tidy$meeting)
            
            for_loop <- function() {
                for (i in 1:length(meetings)) { # Cycle through all unique users
                    tryCatch({
                        # Name of file
                        out_file <- paste(meetings[i], ".pdf", sep="")
                        
                        knitr::knit_meta(class=NULL, clean = TRUE) # Clean knitr meta
                        rmarkdown::render(input = rmd_file, # Create the document
                                          output_format = "pdf_document", # As Word document
                                          # File name as unitID
                                          output_file = out_file,
                                          output_dir = out_dir,
                                          # Output directory from above
                                          run_pandoc = TRUE, 
                                          params = list(meeting=meetings[i] # Loop through user's unitID, # From
                                                        )) # Set encoding as UTF8
                        
                        
                        # Create text to show once each file is created
                        shinyjs::html("text", 
                                      paste("Document", i, "of", length(meetings), "created. Meeting: ",  meetings[i], "<br>"), 
                                      add = TRUE)
                    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
                }
            }
            
            # Call function
            for_loop()
            
            # Files to be zipped
            fs <- dir(out_dir, full.names = TRUE)
            # Create zip folder with file name and files selected
            utils::zip(zipfile=file, files = fs, flags = "-j")
            
            unlink(out_dir)
            
        },
        contentType = "application/zip"
        
    )
    
    output$initial_text <-
        renderUI(
            strong(
                "Once you've uploaded a raw file from Qualtrics, please click GENERATE A REPORT. The creation of the reports will take some time depending on the number of meetings."
            )
        )
    
   # If IDs' field is empty, don't allow for the download button to be clicked  
   observe({
       if (is.null(input$upload) || input$upload == "") {
           shinyjs::disable("download")
       } else {
           shinyjs::enable("download")
       }
   })
   
   # Once download button is clicked, print out text once each file is created
   observeEvent(input$download, {
       shinyjs::html("text", "")
   })
   
  
   
   shinyjs::onclick("download", "$('#cover-spin').show(0)")
   
}

# Run the application 
shinyApp(ui = ui, server = server)