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
                    
                shinyjs::useShinyjs(),
                
                absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                              draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                              width = "30%", height = "auto", style="padding:10px;",
                              
                              bsCollapse(id = "study_information_bspanel",
                                         
                                         ## ... 2A Study Information ####################
                                         # Title, one-liner, absract, start and end date
                                         bsCollapsePanel(           
                                           title = "Study Information",
                                           div(
                                             id = "study_info_table",

                                             uiOutput("title"),
                                             uiOutput("doi_data"),
                                             uiOutput("data_year_published"),
                                             uiOutput("one_liner"), 
                                             uiOutput("abstract"), 
                                             uiOutput("start_date"),
                                             uiOutput("end_date"),
                                             
                                             actionButton("add_study_information", "Confirm study information", class = "btn-primary")
                                           )
                                         )
                              )
                ),
                    
                    ## ... 2B Authors & Contact Information #############
                    absolutePanel(id = "authors_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "35%", right = "auto", bottom = "auto",
                                  width = "30%", height = "auto", style="padding:10px;",
                                  
                                  shinyjs::useShinyjs(),
                                  bsCollapse(id = "authors_bspanel",
                                             
                                             bsCollapsePanel(title="Authors",           
                                                             
                                                             div(
                                                               id = "authors_table",
                                                               
                                                               uiOutput("last_name"),
                                                               uiOutput("given_name"),
                                                               uiOutput("institution"),
                                                               uiOutput("email"),
                                                               uiOutput("address"),
                                                               uiOutput("phone"),
                                                               uiOutput("corresponding_author"),
                                                               
                                                               actionButton("add_author", "Confirm and add additional author", 
                                                                            class = "btn-primary"),
                                                               actionButton("add_last_author", "Confirm and close author's table",
                                                                            class = "btn-primary")
                                                             )))
                    ), 
                    
                    ## ... 2C Available Datatypes #############
                    # I suspect I won't need this because it will be self-evident based on methods responses
                    # But we may need it for elevation/biomass/etc 
                    # absolutePanel(id = "datatypes_panel", class = "panel panel-default", fixed = FALSE,
                    #               draggable = TRUE, top = "10%", left = "68%", right = "auto", bottom = "auto",
                    #               width = "30%", height = "auto", style="padding:10px;",
                    #               
                    #               div(
                    #                 id = "datatypes_table",
                    #                 
                    #                 tags$h3("Data Types"), tags$br(), 
                    #                 
                    #                 selectInput("data_types", mandatoryLabel("Choose the data-types represented by your data"),
                    #                             c("Carbon Stocks", "Age Depth", "Biomass", "Elevation", "SSC"),
                    #                             multiple = TRUE, width = "80%"),
                    #                 
                    #                 actionButton("confirm_datatypes", "Confirm data type selections", class = "btn-primary")
                    #               )
                    # )
                    
                    ## ... 2D Keywords ###########
                    absolutePanel(id = "keywords_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "68%", right = "auto", bottom = "auto",
                                  width = "30%", height = "auto", style="padding:10px;",
                                  
                                  bsCollapse(id = "keywords_bspanel",
                                             
                                             bsCollapsePanel(title="Keywords",           
                                                             
                                                             div(id = "keywords_table",
                                                                 uiOutput("keywords"),
                                                                 
                                                                 actionButton("add_keywords", "Submit keywords", 
                                                                              class = "btn-primary")
                                                             )))
                    ),
                    
                    ## ... 2E Associated Publications ###########
                absolutePanel(id = "pubs_panel", class = "panel panel-default", fixed = FALSE,
                              draggable = TRUE, top = "50%", left = "68%", right = "auto", bottom = "auto",
                              width = "30%", height = "auto", style="padding:10px;",
                              
                              bsCollapse(id = "associated_pubs_bspanel",
                                         
                                         bsCollapsePanel(title="Associated Publications",           
                                                         
                                                         div(id = "associated_publications_table",
                                                             tags$h3("Associated Publications"), 
                                                             "Enter either a DOI or bibtex citation and click 'submit citation'.",
                                                             
                                                             uiOutput("title_pubs"),
                                                             uiOutput("doi_pubs"),
                                                             uiOutput("bibtex_pubs"),
                                                             
                                                             actionButton("add_pub", "Confirm and add additional publications", class = "btn-primary")
                                                         )
                                         )))
                                         
           ),
           
           ## 3. Methods Metadata
           tabPanel("Methods Metadata", 
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "30%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "methods_all"
                                      
                                      
                                  )
                    )
           )
)