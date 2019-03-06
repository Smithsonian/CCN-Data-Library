# Coastal Carbon Research Coordination Network
# This is the server script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu


function(input, output, session) {
  
  # Each of these reactive data objects will track user inputs
  # and influence what options will appear throughout the submission process
  study_information <- reactiveValues(df = study_information)
  authors <- reactiveValues(df = authors)
  datatypes <- reactiveValues(vector = c())
  
  # action to take when submit button is pressed
  observeEvent(input$add_study_information, {
    # Each time submit is clicked, re-create the study information table 
    # This will allow users to update one or more variables after initial submission of the table
    study_information$df <- as.data.frame(t(sapply(study_information_var, function(x) input[[x]])))
  })
  
  # When an author is added, add information to new row and clear fields to add another author
  observeEvent(input$add_author, {
    # gather up author information and organize in a table
    author <- as.data.frame(t(sapply(authors_var, function(x) input[[x]])))
    # add new row to authors table
    authors$df <- bind_rows(authors$df, author)
    
    # reset form info
    shinyjs::reset("authors_table")
    
  })
  
  observeEvent(input$confirm_datatypes, {
    # save data type selection within a vector that will be used to generate methods options
    # data types can be updated but we will need to test outcomes if methods are filled out and then
    # data types available is changed by the user 
    datatypes$vector <- input$data_types
    
  })
  
  
  # Send data to dropbox
  # saveData <- function(data) {
  #   #data <- t(data)
  #   # Create a unique file name
  #   fileName <- sprintf("%s_study_information.csv",
  #                       humanTime())   
  #   
  #   # Write the data to a temporary file locally
  #   filePath <- file.path(tempdir(), fileName)
  #   write.csv(data, filePath, row.names = FALSE, quote = TRUE)
  #   
  #   # Upload the data/publication data to Dropbox
  #   drop_upload(filePath, path = "user_information")
  # }
  
}