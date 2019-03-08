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
  
  ## ... 2e Render Methods Metadata ############
  
  ## ... ... 2e i Universal variables ######
  output$coring_method <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("coring_method", choices = coring_methods_var, 
                  label=NULL, selected = NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$roots_flag <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("roots_flag", choices = c("not specified", "roots and rhizomes included", "roots and rhizomes separated"), 
                  label=NULL, selected = NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$sediment_sieve_size <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("sediment_sieve_size", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$compaction_flag <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("compaction_flag", choices = compaction_flag_var,
                  label=NULL, selected = NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$carbon_profile_notes <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("carbon_profile_notes", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... ... 2e ii LOI #####################
  output$loi_temp <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("loi_temp", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$loi_time <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("loi_time", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$loi_sample_volume <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("loi_sample_volume", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$loi_sample_mass <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("loi_sample_mass", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$loi_approx <- renderUI({
    # New data submission: 
    if(TRUE){
      checkboxInput("loi_approx", "LOI time recorded herein is an approximate estimation")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... ... 2e iii bulk density #####################
  output$dbd_temp <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("dbd_temp", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$dbd_time <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("dbd_time", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$dbd_sample_volume <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("dbd_sample_volume", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$dbd_sample_mass <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("dbd_sample_mass", label=NULL, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$dbd_density <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("dbd_density", label=NULL, choices = dbd_density_var, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... ... 2e iv Fraction organic matter #############
  
  output$carbonates_removed <- renderUI({
    # New data submission: 
    if(TRUE){
      checkboxInput("carbonates_removed", 
                    "Were carbonates removed prior to calculating fraction organic carbon?",
                    width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
    
  })
  
  output$carbonate_removal_method <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("carbonate_removal_method", label=NULL, choices = c("not specified", "direct acid treatment", "acid fumigation", "low carbonate soil", 
                                                                      "carbonates not removed"), width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... ... 2e v Fraction Carbon ############
  output$carbon_measured_or_modeled <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("carbon_measured_or_modeled", label=NULL, choices = c("not specified", "measured", "modeled"), width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$fraction_carbon_method <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("fraction_carbon_method", label=NULL, choices = fraction_carbon_method_var, width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$fraction_carbon_type <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("fraction_carbon_type", label=NULL, choices = c("not specified", "organic carbon", "total carbon"), width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  ## ... ... 2e vi Age-Depth Models ############
  output$cs137_counting_method <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("cs137_counting_method", label=NULL, choices = c("not specified", "alpha", "gamma"), width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$pb210_counting_method <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "alpha", "gamma"), width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$excess_pb210_rate <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "mass accumulation", "accretion"), 
                  width = "80%")
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$excess_pb210_model <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "CRS", "CIC", "CFCS")) 
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$ra226_assumption <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("ra226_assumption", label=NULL, choices = c("not specified", "each sample", 
                                                                   "total core", "at asymptote")) 
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
  })
  
  output$c14_counting_method <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("c14_counting_method", label=NULL, choices = c("not specified", "AMS", "beta")) 
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
    
  })
  
  output$dating_notes <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("dating_notes", label=NULL) 
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
    
  })
  
  output$age_depth_model_reference <- renderUI({
    # New data submission: 
    if(TRUE){
      selectInput("age_depth_model_reference", label=NULL, choices = c("YBP", "CE", "core collection date")) 
      
      # Continue new data submission:   
    } else if (TRUE) {
      
      # Revise study metadata from library:  
    } else {
      
    }
    
  })
  
  output$age_depth_model_notes <- renderUI({
    # New data submission: 
    if(TRUE){
      textInput("age_depth_model_notes", label=NULL) 
      
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
  
  ## ... 3e Methods Metadata ##############
  ## ... ... i Universal Metadata #########
  observeEvent(input$confirm_coring_methods, {
    updateCollapse(session, id="core_methods_bspanel", close = "Coring Methods")
  })
  
  ## ... ... ii Carbon Stock Metadata #####
  observeEvent(input$confirm_organic_carbon, {
    updateCollapse(session, id="carbon_measurements_bspanel", close = "Carbon Stocks")
  })
  
  observeEvent(input$cancel_organic_carbon, {
    # reset variables
    shinyjs::reset("loi")
    shinyjs::reset("dbd")
    shinyjs::reset("fc")
    shinyjs::reset("foc")
    
    # close the panel
    
    updateCollapse(session, id="carbon_measurements_bspanel", close = "Carbon Stocks")
  })
  
  # LOI variables: 
  observe({
    if(input$loi == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "loi_var_bspanel")
    }  else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "loi_var_bspanel")
    }
  })
  
  observeEvent(input$confirm_loi, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "loi_var_bspanel")
  })
  
  observeEvent(input$cancel_loi, {
    # reset LOI variables
    shinyjs::reset("loi_table")
    # unselect the LOI checkbox
    shinyjs::reset("loi")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "loi_var_bspanel")
  })
  
  # DBD variables: 
  observe({
    if(input$dbd == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "dbd_bspanel")
    } else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "dbd_bspanel")
    } 
  })
  
  observeEvent(input$confirm_dbd, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "dbd_bspanel")
  })
  
  observeEvent(input$cancel_dbd, {
    # reset dbd variables
    shinyjs::reset("dbd_table")
    # unselect the dbd checkbox
    shinyjs::reset("dbd")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "dbd_bspanel")
  })
  
  # Fraction organic matter: 
  observe({
    if(input$fraction_organic_matter == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "organic_matter_bspanel")
    } else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "organic_matter_bspanel")
    } 
  })
  
  observeEvent(input$confirm_fom, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "organic_matter_bspanel")
  })
  
  observeEvent(input$cancel_fom, {
    # reset dbd variables
    shinyjs::reset("fraction_organic_matter_table")
    # unselect the dbd checkbox
    shinyjs::reset("fraction_organic_matter")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "organic_matter_bspanel")
  })
  
  # Fraction Carbon:
  observe({
    if(input$fraction_carbon == TRUE){
      updateCollapse(session, id = "carbon_measurements_bspanel", open = "fraction_carbon_bspanel")
    } else {
      updateCollapse(session, id = "carbon_measurements_bspanel", close = "fraction_carbon_bspanel")
    } 
  })
  
  observeEvent(input$confirm_fc, {
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "fraction_carbon_bspanel")
  })
  
  observeEvent(input$cancel_fc, {
    # reset dbd variables
    shinyjs::reset("fraction_carbon_table")
    # unselect the dbd checkbox
    shinyjs::reset("fraction_carbon")
    # close the panel
    updateCollapse(session, id = "carbon_measurements_bspanel", close = "fraction_carbon_bspanel")
  })
  
  ## ... ... iii Age-Depth Metadata #######
  observe({
    if(input$cs137 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "cs137_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
    } 
  })
  
  observeEvent(input$confirm_cs137, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
  })
  
  observeEvent(input$cancel_cs137, {
    # reset dbd variables
    shinyjs::reset("cs137_table")
    # unselect the dbd checkbox
    shinyjs::reset("cs137")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
  })
  
  observeEvent(input$cancel_age_depth, {
    # reset LOI variables
    shinyjs::reset("age_depth_types")
    # unselect the LOI checkbox
    shinyjs::reset("age_depth")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "ad_types_bspanel")
  })
  
  
  ## 4. Send data to dropbox ##############
  observeEvent(input$submit_study_information, {
    uploadData()
  })
  
  uploadData <- function() {
    
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