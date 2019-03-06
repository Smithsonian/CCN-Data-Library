# Coastal Carbon Research Coordination Network
# This is the UI script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu


## 1 Introduction ######################################
# The UI interface is designed to effectively gather study metadata
# and generate detailed templates to faciliate data submissions. 
# Users will be asked broad questions about their data and more specific 
# queries will be generated based on their answers to the first set 
# of questions. 

# Additionally, study metadata will be used to create custom CSV templates 
# that can be downloaded and filled out. 

navbarPage("Coastal Carbon Data Submission Application", id="nav",
           
           ## 2 Study Metadata ####################
           tabPanel("Study Information",
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "30%", height = "auto", style="padding:10px;",
                                  
                                  div(
                                    id = "study_info_table",
                                    
                                    ## ... 2A Study Information ####################
                                    # Title, one-liner, absract, start and end date
                                    tags$h3("Study Information"), tags$br(), 
                                    
                                    textInput("title", mandatoryLabel("Title of dataset"), width="80%"),
                                    textInput("doi_data", "DOI for dataset, if applicable", width="80%"),
                                    textInput("data_year_published", "If data has DOI, enter year published", width="80%"),
                                    textInput("one_liner", "Provide a one-line description of your data", width="80%"),
                                    textAreaInput("abstract", "Provide the abstract for your data", width="145%",
                                                  rows=6),
                                    dateInput("start_date", "Start date of study", format = "yyyy-mm-dd"),
                                    dateInput("end_date", "End date of study", format = "yyyy-mm-dd"),
                                    actionButton("add_study_information", "Confirm study information", class = "btn-primary")
                                  )
                    ),
                    
                    ## ... 2B Authors & Contact Information #############
                    absolutePanel(id = "authors_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "35%", right = "auto", bottom = "auto",
                                  width = "30%", height = "auto", style="padding:10px;",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  div(
                                    id = "authors_table",
                                    
                                    tags$h3("Author Information"), tags$br(), 
                                    
                                    textInput("last_name", "Last name of author", width="80%"),
                                    textInput("given_name", "Given name of author", width="80%"),
                                    textInput("institution", "Institutional affiliation", width="80%"),
                                    textInput("email", "Email address", width="80%"),
                                    textInput("address", "Mailing address", width="80%"),
                                    textInput("phone", "Phone number", width="80%"),
                                    checkboxInput("corresponding_author", "Is this the corresponding author for your data release?", FALSE),
                                    
                                    actionButton("add_author", "Confirm and add additional author", class = "btn-primary")
                                  )
                    ), 
                    
                    ## ... 2C Available Datatypes #############
                    absolutePanel(id = "datatypes_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "68%", right = "auto", bottom = "auto",
                                  width = "30%", height = "auto", style="padding:10px;",
                                  
                                  div(
                                    id = "datatypes_table",
                                    
                                    tags$h3("Data Types"), tags$br(), 
                                    
                                    selectInput("data_types", mandatoryLabel("Choose the data-types represented by your data"),
                                                c("Carbon Stocks", "Age Depth", "Biomass", "Elevation", "SSC"),
                                                multiple = TRUE, width = "80%"),
                                    
                                    actionButton("confirm_datatypes", "Confirm data type selections", class = "btn-primary")
                                  )
                    )
           )
)

# checkboxInput("need_doi", "If your dataset does not have a DOI, would you like CCRCN assistance in publically releasing your data?", FALSE),
# selectInput("data_types", mandatory_label("Choose the data-types represented by your data"),
#             c("Carbon Stocks", "Age Depth", "Biomass", "Elevation", "SSC"),
#             multiple = TRUE),
# actionButton("submit", "Submit", class = "btn-primary")