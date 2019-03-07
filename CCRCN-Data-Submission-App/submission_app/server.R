# Coastal Carbon Research Coordination Network
# This is the server script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu


function(input, output, session) {
  
  ## 1. Initialize data tables ###############
  # Each of these reactive data objects will track user inputs
  # and influence what options will appear throughout the submission process
  # By default, they are blank. 
  # If a user selects to continue a past data submission entry or to view/edit
  # the metadata of a study that is already in the Library, observeEvent calls 
  # will populate these tables with the relevant data. 
  study_information <- reactiveValues(df = study_information)
  authors <- reactiveValues(df = authors)
  keywords <- reactiveValues(df = data.frame())
  associated_publications <- reactiveValues(df = associated_publications)
  
  # datatypes <- reactiveValues(vector = c())
  
  ## 2. Render UI elements ################
  # All of the input box contents must be handled by the server since users 
  # can either submit new data or revise past submissions. 
  
  ## ... 2a Render Study Information ######
  # Title, one-liner, absract, start and end date
  output$title <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("title", mandatoryLabel("Title of dataset"), width="80%")

    # Continue new data submission:   
    } else if (TRUE) {
      
    # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$one_liner <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("one_liner", "Provide a one-line description of your data", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$doi_data <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("doi_data", "DOI for dataset, if applicable", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$data_year_published <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("data_year_published", "If data has DOI, enter year published", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$abstract <- renderUI({
    # New data submission: 
    if(TRUE){
      textAreaInput("abstract", "Provide the abstract for your data", width="145%",
                    rows=6)
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$start_date <- renderUI({
    # New data submission: 
    if(TRUE){
      dateInput("start_date", "Start date of study", format = "yyyy-mm-dd")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$end_date <- renderUI({
    # New data submission: 
    if(TRUE){
      dateInput("end_date", "End date of study", format = "yyyy-mm-dd")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... 2B Authors & Contact Information #############
  output$last_name <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("last_name", "Last name of author", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$given_name <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("given_name", "Given name of author", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$institution <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("institution", "Institutional affiliation", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$email <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("email", "Email address", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$address <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("address", "Mailing address", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$phone <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("phone", "Phone number", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$corresponding_author <- renderUI({
    # New data submission: 
    if(TRUE){
      checkboxInput("corresponding_author", "Is this the corresponding author for your data release?", FALSE)
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... 2C Keywords ###########
  output$keywords <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("keywords", "Enter keywords associated with your data. 
                Separate multiple keywords with commas",
                width="80%")      
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... 2d Associated Publications ###########
  
  output$title_pubs <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("title_pubs", "Title", width="80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$doi_pubs <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("doi_pubs", "DOI", width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$bibtex_pubs <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("bibtex_pubs", "BibTeX citation", width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## 3. Saving data to tables ################
  # action to take when submit button is pressed
  
  ## ... 3a Confirm Study Information ########
  observeEvent(input$add_study_information, {
    # Each time submit is clicked, re-create the study information table 
    # This will allow users to update one or more variables after initial submission of the table
    study_information$df <- as.data.frame(t(sapply(study_information_var, function(x) input[[x]])))
    
    # Open the authors panel and close the study information panel
    updateCollapse(session, id = "study_information_bspanel", close = "Study Information")
    updateCollapse(session, id = "authors_bspanel", open = "Authors")
  })
  
  ## ... 3b i Confirm author and add additional authors #######
  # When an author is added, add information to new row and clear fields to add another author
  observeEvent(input$add_author, {
    # gather up author information and organize in a table
    author <- as.data.frame(t(sapply(authors_var, function(x) input[[x]])))
    # add new row to authors table
    authors$df <- bind_rows(authors$df, author)
    
    # reset form info
    shinyjs::reset("authors_table")
    
  })
  
  ## ... 3b ii Confirm author and move on to keywords #######
  # When an author is added, add information to new row and clear fields to add another author
  observeEvent(input$add_last_author, {
    # gather up author information and organize in a table
    author <- as.data.frame(t(sapply(authors_var, function(x) input[[x]])))
    # add new row to authors table
    authors$df <- bind_rows(authors$df, author)
    
    # reset form info
    shinyjs::reset("authors_table")
    updateCollapse(session, id = "authors_bspanel", close = "Authors")
    updateCollapse(session, id = "keywords_bspanel", open = "Keywords")
    
  })
  # I'm not sure we'll actually need this, may be self-evident from other queries
  # observeEvent(input$confirm_datatypes, {
  #   # save data type selection within a vector that will be used to generate methods options
  #   # data types can be updated but we will need to test outcomes if methods are filled out and then
  #   # data types available is changed by the user 
  #   datatypes$vector <- input$data_types
  #   
  # })
  
  ## ... 3c Confirm keywords and add more #######
  observeEvent(input$add_keywords, {
    # gather up keywords and organize in a table
    keywords_vector <- strsplit(input$keywords, ",")
    keywords$df <- as.data.frame(keywords_vector, col.names = c("key_words"))
    
    updateCollapse(session, id = "keywords_bspanel", close = "Keywords")
    updateCollapse(session, id = "associated_pubs_bspanel", open = "Associated Publications")
  })
  
  ## ... 3d Confirm associated publications and add additional publications ########
  # When an associated publication is added, add information to new row and clear fields to add another associated publication
  observeEvent(input$add_pub, {
    # gather up publication information and organize in a table
    publication <- as.data.frame(t(sapply(associated_publications_var, function(x) input[[x]])))
    # add new row to publication table
    associated_publications$df <- bind_rows(associated_publications$df, publication)
    
    # reset form info
    shinyjs::reset("associated_publications_table")
    
    updateCollapse(session, id = "associated_pubs_bspanel", close = "Associated Publications")
  })
  
  ## 4. Send data to dropbox ##############
  uploadData <- function(data) {
    
    ## ... 4A Study Information ##########
    # Create a unique file name for each data table
    upload_study_information <- sprintf("%s_study_information.csv", humanTime())

    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_study_information)
    write.csv(study_information$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4B Authors ##########
    # Create a unique file name for each data table
    upload_authors <- sprintf("%s_authors.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_authors)
    write.csv(authors$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4C Keywords ##########
    # Create a unique file name for each data table
    upload_keywords <- sprintf("%s_keywords.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_keywords)
    write.csv(keywords$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    ## ... 4D Associated Publications ########
    # Create a unique file name for each data table
    upload_associated_publications <- sprintf("%s_associated_publications.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_associated_publications)
    write.csv(associated_publications$df, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
  }

}