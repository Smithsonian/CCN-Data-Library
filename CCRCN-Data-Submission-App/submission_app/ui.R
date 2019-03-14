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
           
           tabPanel("Welcome", 
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "25%", right = "auto", bottom = "auto",
                                  width = "50%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "welcome_message", 
                                      "Welcome to the Coastal Carbon Research Coordination Network's data submission application! 
                                      This application allows you to submit data and metadata for inclusion into the coastal carbon 
                                      data clearinghouse and revise existing metadata in the clearinghouse.", tags$br(), tags$br(), 
                                      
                                      "New data submissions: Tell us about your data and we will generate a custom set of templates 
                                      to organize your data for submission.", tags$br(), tags$br(), 
                                      
                                      actionButton("new_submission", "Submit new data to the CCRCN data clearinghouse", 
                                                   class = "btn-primary"), tags$br(), tags$br(), 
                                      
                                      "Revise existing metadata: We need help completing metadata for the data already stored in our 
                                      clearinghouse. This includes information on authors, associated publications, and methods and 
                                      materials. Click the button below, and select a study to begin.", tags$br(), tags$br(), 
                                      
                                      actionButton("edit_metadata", "Edit metadata for data in the CCRCN clearinghouse", 
                                                   class = "btn-primary"), tags$br(), tags$br(),
                                      
                                      uiOutput("edit_study")
                                      )
                    )
           ),
                    
           ## 2 Study Metadata ####################
           tabPanel("Study Information",
                    
                    shinyjs::useShinyjs(),
                    
                    absolutePanel(id = "study_info_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  div(id = "study_info_introduction", 
                                      tags$h3("Study Information"), 
                                      "Enter metadata associated with your study, including authors, associated publications, and keywords.
                                  This information is essential for creating Ecological Metadata Language (EML), and will ensure your data
                                  data is easily discoverable, you are correctly cited when your data is used, and
                                  that the history, context, and originality of your research is clear to others", 
                                      tags$br(), tags$br()
                                  ),
                                  
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
                                  ),
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
                                                             ))),
                                  
                                  
                                  ## ... 2C Keywords ###########
                                  
                                  bsCollapse(id = "keywords_bspanel",
                                             
                                             bsCollapsePanel(title="Keywords",           
                                                             
                                                             div(id = "keywords_table",
                                                                 uiOutput("keywords"),
                                                                 
                                                                 actionButton("add_keywords", "Submit keywords", 
                                                                              class = "btn-primary")
                                                             ))),
                                  
                                  
                                  ## ... 2D Associated Publications ###########
                                  
                                  bsCollapse(id = "associated_pubs_bspanel",
                                             
                                             bsCollapsePanel(title="Associated Publications",           
                                                             
                                                             div(id = "associated_publications_table",
                                                                 tags$h3("Associated Publications"), 
                                                                 "Enter either a DOI or bibtex citation and click 'submit citation'.",
                                                                 
                                                                 uiOutput("title_pubs"),
                                                                 uiOutput("doi_pubs"),
                                                                 uiOutput("bibtex_pubs"),
                                                                 
                                                                 actionButton("add_pub", "Confirm and add additional publications", class = "btn-primary"),
                                                                 actionButton("add_last_pub", "Confirm and close additional publication table",
                                                                              class = "btn-primary")
                                                                 
                                                             )
                                             )), 
                                  div(id="end_study_information", 
                                      "Once you have entered all of basic information associated with your data, let's move on 
                                      to the materials and methods metadata. Select the 'submit study information' button below to finalize your 
                                      study information metadata. Then click on the 'Methods and Materials Metadata' tab at the top of the
                                      website navigation bar to begin entering metadata associated with methodologies", tags$br(), tags$br(), 
                                      
                                      actionButton("submit_study_information", "Confirm all study information metadata",
                                                   class = "btn-primary")))
                    
           ),
           
           ## 3. Methods and Materials Metadata ########
           tabPanel("Methods and Materials Metadata", 
                    
                    absolutePanel(id = "universal_methods_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  div(id="intro_methods", 
                                      tags$h3("Methods and Materials Metadata"),
                                      "For each study please fill out key data regarding materials and methods that are 
                                      important to the soil carbon stocks meta-analysis. 
                                      Some users may want to include or exclude certain methodologies, 
                                      or see your commentary on the methods. Let’s make it easy for them.",
                                      tags$br(), tags$br()
                                      
                                  ),
                                  
                                  ## ... 3A Universal methods and material metadata ############

                                  bsCollapse(id = "core_methods_bspanel", 
                                             bsCollapsePanel(
                                               title="Coring Methods", 
                                               
                                               div(id = "core_methods_table", 
                                                   "Indicate the type of device used to collect soil depth profiles",
                                                   uiOutput("coring_method"), 
                                                   "Indicate whether live roots were included or excluded from carbon assessments",
                                                   uiOutput("roots_flag"),
                                                   
                                                   "Indicate the size of the sieve used on sediments prior to carbon measurements", 
                                                   tags$br(), "(millimeters)", 
                                                   uiOutput("sediment_sieve_size"),
                                                   checkboxInput("sediment_sieved_flag", "The sediment was not sieved prior to carbon measurements",
                                                                 width = "80%"),
                                                   "Indicate how the study qualified or quantified compaction of the core:",
                                                   uiOutput("compaction_flag"),
                                                   
                                                   actionButton("confirm_coring_methods", "Confirm coring methods metadata",
                                                                class = "btn-primary")
                                              
                                                   )
                                           )),
                                  
                                  ## ... 3B Carbon stock methods ###################
                                  # LOI, Bulk density 
                                  
                                  div(id = "data_types", 
                                      "Select which types of data were collected in this study:",
                                      tags$br(), tags$br()
                                  ),
                                  
                                  bsCollapse(id = "carbon_measurements_bspanel",
                                             
                                             bsCollapsePanel(title = "Carbon Stocks",
                                                             
                                                             div(id = "loi",
                                                                 checkboxInput("loi", "Loss-on-ignition"),
                                                                 
                                                                 bsCollapsePanel(           
                                                                   title = "", 
                                                                   value = "loi_var_bspanel",
                                                                   div(
                                                                     id = "loi_table",
                                                                     "Temperature at which samples were combusted to estimate fraction organic matter",
                                                                     tags$br(), "(degrees celsius)", 
                                                                     uiOutput("loi_temp"),
                                                                     "Time over which samples were combusted to estimate fraction organic matter",
                                                                     tags$br(), "(hours)", 
                                                                     uiOutput("loi_time"),
                                                                     uiOutput("loi_approx"),
                                                                     "Sample volume used for LOI, if held constant", tags$br(),
                                                                     "(cubic centimeters)", 
                                                                     uiOutput("loi_sample_volume"), 
                                                                     "Sample mass used for LOI, if held constant", tags$br(), 
                                                                     "(grams)",
                                                                     uiOutput("loi_sample_mass"),
                                                                     actionButton("confirm_loi", "Confirm and save LOI metadata", class = "btn-primary"),
                                                                     actionButton("cancel_loi", "This study did not measure LOI", class = "btn-danger")
                                                                     
                                                                   )
                                                                 ),
                                                                 checkboxInput("dbd", "Dry Bulk density"),
                                                                 bsCollapsePanel(title = "",
                                                                                 value = "dbd_bspanel",
                                                                                 div(
                                                                                   id = "dbd_table",
                                                                                   
                                                                                   "Temperature at which samples were dried to measure dry bulk density,",
                                                                                   tags$br(), "(degrees celsius)",
                                                                                   uiOutput("dbd_temp"),
                                                                                   "Time over which samples were dried to measure dry bulk density,",
                                                                                   tags$br(), "(hours)", 
                                                                                   uiOutput("dbd_time"), 
                                                                                   "Sample volume used for dry bulk density measurements, if held constant,",
                                                                                   tags$br(), "(cubic centimeters)", 
                                                                                   uiOutput("dbd_sample_volume"), 
                                                                                   "Sample mass used for dry bulk density measurements, if held constant,",
                                                                                   tags$br(), "(grams)",
                                                                                   uiOutput("dbd_sample_mass"),
                                                                                   "How did the study quantify dry bulk density?",
                                                                                   uiOutput("dbd_density"),
                                                                                   
                                                                                   actionButton("confirm_dbd", "Confirm and save dry bulk density metadata", class = "btn-primary"),
                                                                                   actionButton("cancel_dbd", "This study did not measure dry bulk density", class = "btn-danger")
                                                                                   
                                                                                 )
                                                                 ),
                                                                 checkboxInput("fraction_organic_matter", "Fraction of organic matter", width="80%"),
                                                                 
                                                                 bsCollapsePanel(title = "", value = "organic_matter_bspanel", 
                                                                               
                                                                                 div(id="fraction_organic_matter_table", 
                                                                                     uiOutput("carbonates_removed"),
                                                                                     "Specify the method used to remove carbonates:",
                                                                                     uiOutput("carbonate_removal_method"),
                                                                                     
                                                                                     actionButton("confirm_fom", "Confirm metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_fom", "This study did not measure fraction organic matter",
                                                                                                  class = "btn-danger")
                                                                                 )
                                                                 ),
                                                                               
                                                                 checkboxInput("fraction_carbon", "Fraction carbon", width = "80%"),
                                                                 bsCollapsePanel(title = "", value = "fraction_carbon_bspanel", 
                                                                                 
                                                                                 div(id="fraction_carbon_table", 
                                                                                     "Was fraction carbon measured or estimated as a function of organic matter?",
                                                                                     uiOutput("carbon_measured_or_modeled"),
                                                                                     "Specify the method used to measure or model carbon:",
                                                                                     uiOutput("fraction_carbon_method"),
                                                                                     "Does the fraction carbon measurement refer to organic or total carbon?",
                                                                                     uiOutput("fraction_carbon_type"),
                                                                                     
                                                                                     actionButton("confirm_fc", "Confirm fraction carbon metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_fc", "This study did not measure fraction carbon",
                                                                                                  class = "btn-danger")
                                                                                 )
                                                                 ),
                                                                 
                                                                 div(id = "end_organic_carbon_measurements", tags$br(), tags$br(),
                                                                     
                                                                    "Specify any other notes describing methodologies for determining dry bulk density, organic matter fraction, 
                                                                    and carbon fraction",
                                                                    uiOutput("carbon_profile_notes"), 
                                                                 
                                                                    actionButton("confirm_organic_carbon", 
                                                                                 "Confirm organic carbon measurement metadata",
                                                                                 class = "btn-primary"),
                                                                    actionButton("cancel_organic_carbon", 
                                                                                 "This study did not measure organic carbon",
                                                                                 class = "btn-danger")
                                                                 )
                                                          )       
                                             )
                                  ),
                                             
                                  ## ... 3C Age-depth model metadata ###########
                                  bsCollapse(id = "dating_methods_bspanel",
                                  
                                             bsCollapsePanel(title = "Age Depth Models", 
                                                             value = "ad_types_bspanel", 
                                                             
                                                             div(id="age_depth_types", 
                                                                 "Select each dating method used in your study:",
                                                                 checkboxInput("cs137", "Radiocesium (cs 137)"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "cs137_bspanel",
                                                                                 div(id="cs137_table", 
                                                                                     "Specify the method used for determining radiocesium activity",
                                                                                     uiOutput("cs137_counting_method"),
                                                                                      actionButton("confirm_cs137", "Confirm and save cs137 metadata", class = "btn-primary"),
                                                                                      actionButton("cancel_cs137", "This study did not date using cs137", class = "btn-danger")
                                                                                 )),
                                                                 
                                                                 checkboxInput("pb210", "Lead 210 (pb 210)"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "pb210_bspanel",
                                                                                 div(id="pb210_table", 
                                                                                     "Specify the method used for determining lead 210 actvity",
                                                                                     uiOutput("pb210_counting_method"),
                                                                                     "Specify the mass or accretion rate used in the excess lead 210 model",
                                                                                     uiOutput("excess_pb210_rate"), 
                                                                                     "Specify the model used to estimate excess lead 210",
                                                                                     uiOutput("excess_pb210_model"),
                                                                                     actionButton("confirm_pb210", "Confirm and save pb210 metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_pb210", "This study did not date using pb210", class = "btn-danger")
                                                                                  )
                                                                                 ),

                                                                 checkboxInput("ra226", "Radium-226 (226Ra)"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "226ra_bspanel",
                                                                                 div(id="226ra_table", 
                                                                                     "Specify the assumption used to estimate core's background Radium-226 activity",
                                                                                     uiOutput("ra226_assumption"),
                                                                                     actionButton("confirm_226ra", "Confirm and save 226ra metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_226ra", "This study did not date using 226ra", class = "btn-danger")
                                                                                 )
                                                                 ),
                                                                 
                                                                 checkboxInput("c14", "Carbon 14"),
                                                                 bsCollapsePanel(title="", 
                                                                                 value = "c14_bspanel",
                                                                                 div(id="c14_table", 
                                                                                     "Specify the method used for determining radiocarbon activity",
                                                                                     uiOutput("c14_counting_method"),
                                                                                     actionButton("confirm_c14", "Confirm and save c14 metadata", class = "btn-primary"),
                                                                                     actionButton("cancel_c14", "This study did not date using c14", class = "btn-danger")
                                                                                     )
                                                                 ),
                                                                 
                                                                 checkboxInput("7be", "Beryillium-7 (7be)"),
                                                                 checkboxInput("241am", "Americium-241 (241am)"),
                                                                 
                                                                 uiOutput("other_marker"),
                                                                 "Provide any additional notes on the process of dating the core",
                                                                 uiOutput("dating_notes"), 
                                                                 "Indicate the reference or 0 year of the age depth model", 
                                                                 uiOutput("age_depth_model_reference"),
                                                                 "Provide any additional notes on how the age depth model was created",
                                                                 uiOutput("age_depth_model_notes"),
                                                                 
                                                                 actionButton("confirm_age_depth", "Confirm age-depth model metadata", class = "btn-primary"),
                                                                 actionButton("cancel_age_depth", "This study did not generate age-depth models", 
                                                                              class = "btn-danger")
                                                                 
                                                             ) 
                                             )
                                  ),
                                  
                                  div(id="end_methods_metadata", 
                                      "Next we will ask you about metadata associated with your site, including vegetation and human impacts. 
                                      When you're ready, select the 'Confirm materials and method metadata' button below, then click the 'Site Metadata' tab at the top of the
                                      website navigation bar.", tags$br(), tags$br(), 
                                      
                                      actionButton("submit_methods_metadata", "Confirm materials and method metadata",
                                                   class = "btn-primary"))
                                  
                    )
                    
           ),
           tabPanel("Download Templates", 
                    absolutePanel(id = "universal_methods_panel", class = "panel panel-default", fixed = FALSE,
                                  draggable = TRUE, top = "10%", left = "3%", right = "auto", bottom = "auto",
                                  width = "94%", height = "auto", style="padding:10px;",
                                  
                                  shinyjs::useShinyjs(),
                                  
                                  downloadButton("depthseries_template", "Download soil depthseries template", class = "button")
                    )
           )
)