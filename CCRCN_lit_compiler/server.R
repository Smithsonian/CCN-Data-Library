# Coastal Carbon Research Coordination Network
# This is the server script for two-file version of the CCRCN Map GUI 
# interactive interface.
# Contact: klingesd@si.edu

## Workspace Prep ################




## Server ####################
function(input, output, session) {
  
  
  observeEvent(
    input$search_DOI, {
    #DOI_in <- "10.1038/s41598-018-26948-7"
    DOI_in <- input$DOI
    style_in <- input$style

    if(DOI_in != "") {
      
      if(style_in != "") {
        
        output$citation_display <- renderText("Citation of DOI:")
        
        # Save the citation to an R object
        citation <- renderText(cr_cn(dois = DOI_in, format = "text",
                                                 style = style_in))
        
        # Pass that R object to the UI output
        output$citation_out <- citation
      
      }
      
      output$bibtex_display <- renderText("BibTex format of DOI:")
      output$bibtex_out <- renderText(cr_cn(dois = DOI_in, format = "bibtex"))
      
    }
    
    
  })
  
  observeEvent(
    input$add_DOI, {
      
      output$thankyou <- renderText("Thank you for submitting to the CCRCN!")
    }
    
  )
  

  
}