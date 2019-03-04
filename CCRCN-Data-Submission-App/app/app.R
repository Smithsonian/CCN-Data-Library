library(shiny)
library(shinyjs)
library(rdrop2)

mandatory_fields <- c("names", "title", "study", "data_types")

mandatory_label <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

appCSS <-
  ".mandatory_star { color: red; }"

fieldsAll <- c("names", "title", "study", "doi_data", "doi_pubs", "need_doi", "data_types")

#responsesDir <- file.path("./user_information")

epochTime <- function() {
  as.integer(Sys.time())
}

shinyApp(
  ui = fluidPage(
    shinyjs::useShinyjs(),
    shinyjs::inlineCSS(appCSS),
    titlePanel("CCRCN Data Submission Application"),
    
    sidebarLayout(
      sidebarPanel(
        div(
          id = "form",
          
          textInput("names", mandatory_label("Dataset authors"), ""),
          textInput("title", mandatory_label("Title of dataset")),
          textInput("study", mandatory_label("Title of associated publication")),
          textInput("doi_data", "DOI for dataset, if applicable"),
          textInput("doi_pubs", "DOIs for associated publications, if applicable"),
          checkboxInput("need_doi", "If your dataset does not have a DOI, would you like CCRCN assistance in publically releasing your data?", FALSE),
          #sliderInput("r_num_years", "Number of years using R", 0, 25, 2, ticks = FALSE),
          selectInput("data_types", mandatory_label("Choose the data-types represented by your data"),
                      c("Carbon Stocks", "Age Depth", "Biomass", "Elevation", "SSC"),
                      multiple = TRUE),
          actionButton("submit", "Submit", class = "btn-primary")
        ),
        
        shinyjs::hidden(
          div(
            id = "thankyou_msg",
            h3("Thanks, your data was submitted successfully!"),
            actionLink("submit_another", "Submit more data")
          )
        )
      ),
      
      mainPanel(
        # Input: Select a file ----
        fileInput("fileCSV", "Upload CSV File",
                  multiple = TRUE,
                  accept = c("text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Checkbox if file has header ----
        checkboxInput("header", "Header", TRUE),
        
        # Input: Select separator ----
        radioButtons("sep", "Separator",
                     choices = c(Comma = ",",
                                 Semicolon = ";",
                                 Tab = "\t"),
                     selected = ","), 
        
        # Horizontal line ----
        tags$hr(),
        
        # Input: Select a file ----
        fileInput("fileEML", "Upload EML Metadata File",
                  multiple = TRUE,
                  accept = c("application/xml",
                             "text/xml",
                             ".xml"))
      )
    )
  ),
  
  server = function(input, output, session) {
    ## Check to see if mandatory fields have been filled out
    observe({
      mandatoryFilled <-
        vapply(mandatory_fields,
               function(x) {
                 !is.null(input[[x]]) && input[[x]] != ""
               },
               logical(1))
      mandatoryFilled <- all(mandatoryFilled)
      
      shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    })    
    
    # write user inputs to a table and attach a time stamp
    formData <- reactive({
      data <- sapply(fieldsAll, function(x) input[[x]])
      data <- c(data, timestamp = epochTime())
      data <- t(data)
      data
    })
    
    saveData <- function(data) {
      data <- t(data)
      # Create a unique file name
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          digest::digest(data))      
      
      # Write the data to a temporary file locally
      filePath <- file.path(tempdir(), fileName)
      write.csv(data, filePath, row.names = FALSE, quote = TRUE)
      
      # Upload the data/publication data to Dropbox
      drop_upload(filePath, path = "user_information")
      # Upload the CSV to Dropbox
      drop_upload(input$fileCSV$datapath, path = "user_information")
      
    }
    
    # action to take when submit button is pressed
    observeEvent(input$submit, {
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    })
    
    # show form if user wants to submit additional data
    observeEvent(input$submit_another, {
      shinyjs::show("form")
      shinyjs::hide("thankyou_msg")
    })    
    
  }
)