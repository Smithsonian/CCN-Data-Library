# Coastal Carbon Research Coordination Network
# This is the UI script for two-file version of the CCRCN Map GUI interactive
# interface.
# Contact: klingesd@si.edu

## Workspace Prep ################


## UI ###############
# Create header
navbarPage( "CCRCN Data Library", id="nav",
            
            

  # Tab for contributing to the library
  tabPanel("Add to Bibliography",
           
           absolutePanel(
             id = "logo", class = "panel panel-default", fixed = TRUE,
             draggable = FALSE, top = 80, left = "auto", right = 25, bottom = "auto",
             height = "auto", style = "background: rgba(255,0,0,.0); border: none;",
             
             # tells UI to use image's style CSS file 
             # Note: if we use the style.CSS file for other features, 
             # we'll probably need to move the below line out of this absolute panel block
             
             includeCSS("www/style.css"),
             
             tags$div(
               tags$a(href="https://serc.si.edu/coastalcarbon",tags$img(src = "ccrcn_logo.png", 
                                                                        id= "ccrcn_logo", width="100px", height="100px"),
                      target="_blank")
             )
           ),
           
    tags$head(tags$script(src = "message-handler.js")),
           

    # Let's do a sidebar panel for inputs and main panel for outputs
    sidebarLayout(
      # Inputs
      sidebarPanel(
                
        textInput("DOI", "Enter your DOI here"),
              
        textInput("style", "What style citation", "apa"),
        
        actionButton("search_DOI", "Search DOI", 
          style='width: 250px; height: 80px; font-size: 200%; text-align:center;
          background-color: #8bb4ef;'
                     ),
        
        # Line breaks
        tags$br(),
        tags$br(),
        
        conditionalPanel(
          
          condition = "output.bibtex_out",
          
          textInput("tags", "Enter your tags here separated by commas"),
          
          actionButton("add_DOI", HTML("Add DOI to <br/> CCRCN bibliography"),
                       style ='width: 250; height: 100px; text-align:center; font-size: 170%;
                       background-color: #8bb4ef;')
          
          )
        
      ),

      # Outputs
      mainPanel(
        
        conditionalPanel(
          
          condition = "input.add_DOI == 0",
          
          tags$br(),
          tags$br(),

          textOutput("citation_display"),
          
          tags$head(
            tags$style("#citation_display{color: black;
                       font-size: 18px;
                       font-style: bold;
                       }"
            )
          ),

          tags$br(),
      
          textOutput("citation_out"),
      
          tags$br(),
          tags$br(),
           
          textOutput("bibtex_display"),
          
          tags$head(
            tags$style("#bibtex_display{color: black;
                       font-size: 18px;
                       font-style: bold;
                       }"
            )
            ),
      
          tags$br(),
      
      
          textOutput("bibtex_out")
      
        ),
        
        conditionalPanel(
        
          condition = "input.add_DOI > 0",
        
          textOutput("thankyou"),
          
          tags$head(
            tags$style("#thankyou{color: black;
                                 font-size: 20px;
                               font-style: bold;
                               }"
            )
          )
        ),
        
        width = 6
      
      )
    )
    

    

  ),
  
 
  tabPanel("Search our Bibliography",

                    fluidRow(
                      column(3,
                             selectInput("states", "States", c("All states"="", 
                            structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("cities", "Cities", c("All cities"=""), 
                                                          multiple=TRUE)
                             )
                      ),
                      column(3,
                             conditionalPanel("input.states",
                                              selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), 
                                                          multiple=TRUE)
                             )
                      )
                    ),
                    fluidRow(
                      column(1,
                             numericInput("minScore", "Min score", min=0, max=100, value=0)
                      ),
                      column(1,
                             numericInput("maxScore", "Max score", min=0, max=100, value=100)
                      )
                    ),
                    hr(),
                    DT::dataTableOutput("ziptable")
           )

)