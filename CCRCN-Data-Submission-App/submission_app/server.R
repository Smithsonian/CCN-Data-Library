# Coastal Carbon Research Coordination Network
# This is the server script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu


function(input, output, session) {
  
  # Each of these reactive data objects will track user inputs
  # and influence what options will appear throughout the submission process
  study_information <- reactiveValues(df = study_information)
  authors <- reactiveValues(df = authors)
  keywords <- reactiveValues(df = data.frame())
  associated_publications <- reactiveValues(df = associated_publications)
  
  # datatypes <- reactiveValues(vector = c())
  
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
  
  # I'm not sure we'll actually need this, may be self-evident from other queries
  # observeEvent(input$confirm_datatypes, {
  #   # save data type selection within a vector that will be used to generate methods options
  #   # data types can be updated but we will need to test outcomes if methods are filled out and then
  #   # data types available is changed by the user 
  #   datatypes$vector <- input$data_types
  #   
  # })
  
  observeEvent(input$add_keywords, {
    # gather up keywords and organize in a table
    keywords_vector <- strsplit(input$keywords, ",")
    keywords$df <- as.data.frame(keywords_vector, col.names = c("key_words"))
    
  })
  
  # When an associated publication is added, add information to new row and clear fields to add another associated publication
  observeEvent(input$add_pub, {
    # gather up publication information and organize in a table
    publication <- as.data.frame(t(sapply(associated_publications_var, function(x) input[[x]])))
    # add new row to publication table
    associated_publications$df <- bind_rows(associated_publications$df, publication)
    
    # reset form info
    shinyjs::reset("associated_publications_table")
    
  })
  
  ## 3. Send data to dropbox ##############
  uploadData <- function(data) {
    
    ## ... 3A Study Information ##########
    # Create a unique file name for each data table
    upload_study_information <- sprintf("%s_study_information.csv", humanTime())

    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_study_information)
    write.csv(study_information$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 3B Authors ##########
    # Create a unique file name for each data table
    upload_authors <- sprintf("%s_authors.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_authors)
    write.csv(authors$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 3C Keywords ##########
    # Create a unique file name for each data table
    upload_keywords <- sprintf("%s_keywords.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_keywords)
    write.csv(keywords$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 3D Associated Publications ########
    # Create a unique file name for each data table
    upload_associated_publications <- sprintf("%s_associated_publications.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_associated_publications)
    write.csv(associated_publications$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
  }

}