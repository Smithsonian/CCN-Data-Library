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
  
  # The datatype tracker monitors which datatypes the user has data for in order to populate the templates
  datatype_tracker <- reactiveValues(df = datatype_tracker)

  # Blank depthseries template
  depthseries_template <- reactiveValues(df = data.frame())
  
  # Objective tracks the use case of the user: new data, revise existing metadata, continue data submission
  objective <- reactiveVal("new_submission")
  
  # If the user wants to submit new data, all UI prompts are empty or listed as "not specified"
  observeEvent( input$new_submission, {
    objective("new_submission")
    updateTabsetPanel(session, inputId = "nav", selected = "Study Information")
  })
  
  observeEvent(input$edit_metadata, {
    objective("edit_metadata")
  })
  
  output$edit_study <- renderUI({
    if(objective() == "edit_metadata"){
      selectInput("edit_study", "Select a study", choices = unique(metadata$study_id))
    }
  })
  
  ## 2. Render UI elements ################
  # All of the input box contents must be handled by the server since users 
  # can either submit new data or revise past submissions. 
  
  ## ... 2a Render Study Information ######
  # Title, one-liner, absract, start and end date
  output$title <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("title", mandatoryLabel("Title of dataset"), width="80%")

    # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
    # Continue new data submission:
    } else {
      
    }
  })
  
  output$one_liner <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("one_liner", "Provide a one-line description of your data", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$doi_data <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("doi_data", "DOI for dataset, if applicable", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$data_year_published <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("data_year_published", "If data has DOI, enter year published", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$abstract <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textAreaInput("abstract", "Provide the abstract for your data", width="145%",
                    rows=6)
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$start_date <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      dateInput("start_date", "Start date of study", format = "yyyy-mm-dd")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$end_date <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      dateInput("end_date", "End date of study", format = "yyyy-mm-dd")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2B Authors & Contact Information #############
  output$last_name <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("last_name", "Last name of author", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$given_name <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("given_name", "Given name of author", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$institution <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("institution", "Institutional affiliation", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$email <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("email", "Email address", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$address <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("address", "Mailing address", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$phone <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("phone", "Phone number", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$corresponding_author <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("corresponding_author", "Is this the corresponding author for your data release?", FALSE)
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2C Keywords ###########
  output$keywords <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("keywords", "Enter keywords associated with your data. 
                Separate multiple keywords with commas",
                width="80%")      
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2d Associated Publications ###########
  
  output$title_pubs <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("title_pubs", "Title", width="80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$doi_pubs <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("doi_pubs", "DOI", width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$bibtex_pubs <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("bibtex_pubs", "BibTeX citation", width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... 2e Render Methods Metadata ############
  
  ## ... ... 2e i Universal variables ######
  output$coring_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("coring_method", choices = coring_methods_var, 
                  label=NULL, selected = NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      
      selectInput("coring_method", choices = coring_methods_var, 
                  label=NULL, width = "80%",
                  selected = filter(metadata, study_id == input$edit_study)$coring_method)
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$roots_flag <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("roots_flag", choices = c("not specified", "roots and rhizomes included", "roots and rhizomes separated"), 
                  label=NULL, selected = NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$sediment_sieve_size <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("sediment_sieve_size", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$compaction_flag <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("compaction_flag", choices = compaction_flag_var,
                  label=NULL, selected = NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$carbon_profile_notes <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("carbon_profile_notes", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... 2e ii LOI #####################
  output$loi_temp <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_temp", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_time <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_time", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_sample_volume <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_sample_volume", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_sample_mass <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("loi_sample_mass", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$loi_approx <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("loi_approx", "LOI time recorded herein is an approximate estimation")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... 2e iii bulk density #####################
  output$dbd_temp <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_temp", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_time <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_time", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_sample_volume <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_sample_volume", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_sample_mass <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dbd_sample_mass", label=NULL, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$dbd_density <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("dbd_density", label=NULL, choices = dbd_density_var, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... 2e iv Fraction organic matter #############
  
  output$carbonates_removed <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("carbonates_removed", 
                    "Were carbonates removed prior to calculating fraction organic carbon?",
                    width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$carbonate_removal_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("carbonate_removal_method", label=NULL, choices = c("not specified", "direct acid treatment", "acid fumigation", "low carbonate soil", 
                                                                      "carbonates not removed"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... 2e v Fraction Carbon ############
  output$carbon_measured_or_modeled <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("carbon_measured_or_modeled", label=NULL, choices = c("not specified", "measured", "modeled"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$fraction_carbon_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("fraction_carbon_method", label=NULL, choices = fraction_carbon_method_var, width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$fraction_carbon_type <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("fraction_carbon_type", label=NULL, choices = c("not specified", "organic carbon", "total carbon"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  ## ... ... 2e vi Age-Depth Models ############
  output$cs137_counting_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("cs137_counting_method", label=NULL, choices = c("not specified", "alpha", "gamma"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$pb210_counting_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "alpha", "gamma"), width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$excess_pb210_rate <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "mass accumulation", "accretion"), 
                  width = "80%")
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$excess_pb210_model <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("pb210_counting_method", label=NULL, choices = c("not specified", "CRS", "CIC", "CFCS")) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$ra226_assumption <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("ra226_assumption", label=NULL, choices = c("not specified", "each sample", 
                                                                   "total core", "at asymptote")) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
  })
  
  output$c14_counting_method <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("c14_counting_method", label=NULL, choices = c("not specified", "AMS", "beta")) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$other_marker <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      checkboxInput("other_marker", label="Did you date any other depth horizons, such as an artificial marker, pollen horizon, 
                    or pollution horizon?") 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$dating_notes <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("dating_notes", label=NULL) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$age_depth_model_reference <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      selectInput("age_depth_model_reference", label=NULL, choices = c("YBP", "CE", "core collection date")) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
    } else {
      
    }
    
  })
  
  output$age_depth_model_notes <- renderUI({
    # New data submission: 
    if(objective() == "new_submission"){
      textInput("age_depth_model_notes", label=NULL) 
      
      # Revise metadata for study in CCRCN clearinghouse   
    } else if (objective() == "edit_metadata") {
      
      # Continue new data submission:
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
    
    # updateCollapse(session, id = "associated_pubs_bspanel", close = "Associated Publications")
  })
  
  observeEvent(input$add_last_pub, {
    # gather up publication information and organize in a table
    publication <- as.data.frame(t(sapply(associated_publications_var, function(x) input[[x]])))
    # add new row to authors table
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
    datatype_tracker$df$loi <- TRUE
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
    datatype_tracker$df$dbd <- TRUE
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
    datatype_tracker$df$fraction_organic_matter <- TRUE
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
    datatype_tracker$df$fraction_carbon <- TRUE
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
  # cs137
  
  observe({
    if(input$cs137 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "cs137_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
    } 
  })
  
  observeEvent(input$confirm_cs137, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
    datatype_tracker$df$cs137 <- TRUE
  })
  
  observeEvent(input$cancel_cs137, {
    # reset cs137 variables
    shinyjs::reset("cs137_table")
    # unselect the cs137 checkbox
    shinyjs::reset("cs137")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "cs137_bspanel")
  })
  
  # pb210
  observe({
    if(input$pb210 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "pb210_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "pb210_bspanel")
    } 
  })
  
  observeEvent(input$confirm_pb210, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "pb210_bspanel")
    datatype_tracker$df$pb210 <- TRUE
  })
  
  observeEvent(input$cancel_pb210, {
    # reset pb210 variables
    shinyjs::reset("pb210_table")
    # unselect the pb210 checkbox
    shinyjs::reset("pb210")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "pb210_bspanel")
  })
  
  #226ra
  observe({
    if(input$ra226 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "226ra_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "226ra_bspanel")
    } 
  })
  
  observeEvent(input$confirm_226ra, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "226ra_bspanel")
    datatype_tracker$df$ra226 <- TRUE
  })
  
  observeEvent(input$cancel_226ra, {
    # reset dbd variables
    shinyjs::reset("226ra_table")
    # unselect the dbd checkbox
    shinyjs::reset("ra226")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "226ra_bspanel")
  })
  
  # c14
  observe({
    if(input$c14 == TRUE){
      updateCollapse(session, id = "dating_methods_bspanel", open = "c14_bspanel")
    } else {
      updateCollapse(session, id = "dating_methods_bspanel", close = "c14_bspanel")
    } 
  })
  
  observeEvent(input$confirm_c14, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "c14_bspanel")
    datatype_tracker$df$c14 <- TRUE
    
  })
  
  observeEvent(input$cancel_c14, {
    # reset dbd variables
    shinyjs::reset("c14_table")
    # unselect the dbd checkbox
    shinyjs::reset("c14")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "c14_bspanel")
  })
  
  observeEvent(input$be7, {
    if(input$be7 == TRUE) {
      datatype_tracker$df$be7 <- TRUE
    } else {     
      datatype_tracker$df$be7 <- FALSE
    }
  })
  
  observeEvent(input$am241, {
    if(input$am241 == TRUE) {
      datatype_tracker$df$am241 <- TRUE
    } else {     
      datatype_tracker$df$am241 <- FALSE
    }
  })
  
  # cancel or confirm all age depth choices
  
  observeEvent(input$cancel_age_depth, {
    # reset LOI variables
    shinyjs::reset("age_depth_types")
    # unselect the LOI checkbox
    shinyjs::reset("age_depth")
    # close the panel
    updateCollapse(session, id = "dating_methods_bspanel", close = "ad_types_bspanel")
  })
  
  observeEvent(input$confirm_age_depth, {
    updateCollapse(session, id = "dating_methods_bspanel", close = "ad_types_bspanel")
  })
  
  # confirm all material and method metadata selections
  observeEvent(input$submit_methods_metadata, {

    updateTabsetPanel(session, inputId = "nav", selected = "Download Templates")
    
    methods_metadata <- methods_metadata %>%
      mutate(coring_method = input$coring_method, roots_flag = input$roots_flag, sediment_sieved_flag = input$sediment_sieved_flag, compaction_flag = input$compaction_flag,
             dry_bulk_density_temperature = input$dbd_temp, dry_bulk_density_time = input$dbd_time, 
             dry_bulk_density_sample_volume = input$dbd_sample_volume, dry_bulk_density_sample_mass = input$dbd_sample_mass, dry_bulk_density_flag = input$dbd_density,
             loss_on_ignition_temperature = input$loi_temp, loss_on_ignition_time = input$loi_time, 
             loss_on_ignition_flag = input$loi_approx, loss_on_ignition_sample_volume = input$loi_sample_volume, loss_on_ignition_sample_mass = input$loi_sample_mass,
             carbonates_removed = input$carbonates_removed, carbonate_removal_method = input$carbonate_removal_method,
             carbon_measured_or_modeled = input$carbon_measured_or_modeled, fraction_carbon_method = input$fraction_carbon_method, 
             fraction_carbon_type = input$fraction_carbon_type, carbon_profile_notes = input$carbon_profile_notes,
             cs137_counting_method = input$cs137_counting_method,              
             pb210_counting_method = input$pb210_counting_method, excess_pb210_rate = input$excess_pb210_rate, excess_pb210_model = input$excess_pb210_model, 
             ra226_assumption = input$ra226_assumption, 
             cs14_counting_method = input$c14_counting_method, 
             dating_notes = input$dating_notes, age_depth_model_reference = input$age_depth_model_reference, age_depth_model_notes = input$age_depth_model_notes
            )
    
    # Create a unique file name for each data table
    upload_methods_metadata <- sprintf("%s_material_and_methods.csv", humanTime())
    
    # Write the data to a temporary file locally
    filePath <- file.path(tempdir(), upload_methods_metadata)
    write.csv(methods_metadata, filePath, row.names = FALSE, quote = TRUE)
    
    # Upload the data/publication data to Dropbox
    drop_upload(filePath, path = "user_information")
    
    generate_depthseries_template()
    
    })
  
  ## 4. Send data to dropbox ##############
  observeEvent(input$submit_study_information, {
    updateTabsetPanel(session, inputId = "nav", selected = "Methods and Materials Metadata")
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

  ## 5. Generate Templates ###################
  
  ## ... 5a core level template ##############
  generate_core_template <- function(){
    
  core_var <- c(
    "study_id", "site_id", "core_id", 
    'core_date', "core_notes",
    "core_latitude", "core_longitude", "core_position_accuracy", "core_position_method", "core_position_notes", 
    "core_elevation", "core_elevation_datum", "core_elevation_accuracy", "core_elevation_method", "core_elevation_notes",
    "salinity_class", "salinity_method", "salinity_notes", 
    "vegetation_class", "vegetation_method", "vegetation_notes", 
    "inundation_class", "inundation_method", "inundation_notes", 
    "core_length_flag"
  )
  
  
  }
  
  ## ... 5b depthseries template #############
  generate_depthseries_template <- function(){
    
    template_variables <- c("study_id", "site_id", "core_id",
                            "depth_min", "depth_max", "depth_interval_notes")
    
    age_depth_tracker <- FALSE
    
    if(datatype_tracker$df$dbd == TRUE){
      template_variables <- append(template_variables, "dry_bulk_density")
    }
    
    if(datatype_tracker$df$fraction_carbon == TRUE){
      template_variables <- append(template_variables, "fraction_carbon")
    }
    
    if(datatype_tracker$df$fraction_organic_matter == TRUE){
      template_variables <- append(template_variables, "fraction_organic_matter")
    }
    
    if(datatype_tracker$df$cs137 == TRUE) {
      template_variables <- append(template_variables, c("cs137_activity", "cs137_activity_sd"))
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$pb210 == TRUE) {
      template_variables <- append(template_variables, c("total_pb210_activity", "total_pb210_activity_sd",
                                                         "excess_pb210_activity", "excess_pb210_activity_sd"))
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$ra226 == TRUE) {
      template_variables <- append(template_variables, c("ra226_activity", "ra226_activity_sd")) 
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$c14 == TRUE) {
      template_variables <- append(template_variables, c("c14_age", "c14_age_sd", "c14_material", "c14_notes", "delta_c13")) 
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$be7 == TRUE) {
      template_variables <- append(template_variables, c("be7_activity", "be7_activity_sd")) 
      age_depth_tracker <- TRUE
    }
    
    if(datatype_tracker$df$am241 == TRUE) {
      template_variables <- append(template_variables, c("am241_activity", "am241_activity_sd")) 
      age_depth_tracker <- TRUE
    }
    
    if(age_depth_tracker == TRUE){
      template_variables <- append(template_variables, c("age", "age_min", "age_max", "age_sd")) 
    }
    
    if(is.null(input$other_marker) == FALSE){
      if(input$other_marker == TRUE){
        template_variables <- append(template_variables, c("marker_date", "marker_type", "marker_notes")) 
      }
    }
    
    if(is.null(input$compaction_flag) == FALSE){
      if(input$compaction_flag == "compaction_qualified" | input$compaction_flag == "compaction_quantified"){
        template_variables <- append(template_variables, c("compaction_fraction", "compaction_notes")) 
      }
    }
    
    template_depthseries <- data.frame(matrix(data = FALSE, ncol=length(template_variables), nrow = 0))
    colnames(template_depthseries) <- template_variables
    depthseries_template$df <- template_depthseries
  }  
 
  output$depthseries_template <- downloadHandler(
    filename = function() {
      paste("CCRCN_depthseries_template.csv")
    },
    
    content = function(file) {
      write.csv(depthseries_template$df, file, row.names=F)
    }
  )
}