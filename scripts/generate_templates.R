## CCRCRN Data Library
## Jaxine Wolfe, wolfejax@si.edu

## Create and output CCRCN data entry templates with glossaries
# Inspired by the MGEO protocols repo workflow
# designed by Michael Lonneman, mlonneman@si.edu

library(tidyverse)
library(openxlsx)

# Workflow:
# input table names
# read in guidance and create main template and glossary
# assemble workbooks for individual tables type
# output to templates folder

generateTemplates <- function(table_names){
  
  # Read in and prepare guidance 
  guidance <- read_csv("docs/ccrcn_database_structure.csv") %>% 
    # filter out attributes added during post-processing
    filter(required != "added automatically")
  
  # load controlled attributes
  attributes <- read_csv("docs/controlled_attributes.csv") %>% 
    # duplicated year and month attributes causing table expansion
    filter(attribute_name != "year" | data_type != "character") %>% 
    filter(attribute_name != "month" | data_type != "character")
  
  # load controlled variables
  variables <- read_csv("docs/controlled_variables.csv") %>% 
    group_by(attribute_name) %>% 
    summarise(variable_code = paste(variable_name, collapse="; "))
  
  # create general glossary to subset by table
  main_glossary <- guidance %>% 
    select(table, attribute_name, data_type) %>% 
    filter(table != "study_citations") %>% 
    left_join(variables) %>% 
    left_join(attributes %>% select(-data_type)) %>% 
    mutate(format_unit_codes = case_when(!is.na(variable_code) ~ variable_code, 
                                         !is.na(format_string) ~ format_string,
                                         is.na(format_unit_codes) ~ "text",
                                         TRUE ~ format_unit_codes)) %>% 
    select(table, attribute_name, attribute_definition, everything()) %>% 
    # easier column names for people to understand
    rename(column_name = attribute_name, definition = attribute_definition, 
           field_type = data_type, text_format = format_string) %>% 
    select(-variable_code, -number_type)
  
  # loop through tables
  for(tabletype in table_names){
    
    # Generate individual templates from main glossary
    template <- main_glossary %>%
      filter(table == tabletype) %>% 
      select(column_name, format_unit_codes) %>%
      pivot_wider(names_from = column_name, values_from = format_unit_codes) %>% 
      mutate(column_name = "format, units, or variable codes") %>% 
      select(column_name, everything())
    
    # create individual table glossary
    glossary <- main_glossary %>% 
      filter(table == tabletype) %>% 
      # select_if(function(x){!all(is.na(x))}) %>% 
      select(-table, -format_unit_codes)
    
    ## Create Workbook ####
    ## still under construction...
    
    # Workbook styling
    # needs work
    style_gloss <- createStyle(wrapText = TRUE)
    
    style_col <- createStyle(
      fontName = "Calibri",
      fontSize = 11,
      fontColour = "black",
      textDecoration = "bold",
      border = "bottom",
      # fgFill = "#C7EAFE",
      halign = "center"
    )
    
    # create workbook to house template and glossary
    wb <- createWorkbook()
    addWorksheet(wb, "glossary")
    addWorksheet(wb, tabletype)

    # write table to glossary sheet  
    writeData(wb, sheet = "glossary", x = glossary, headerStyle = style_col)
    addStyle(wb, sheet = "glossary", style_gloss, 
             rows = 1:(4+nrow(glossary)), cols = 1:ncol(glossary), 
             gridExpand = T)
    setColWidths(wb, sheet = "glossary", cols = 1:ncol(glossary), 
                 widths = c(20, 30, 10, 10, 10, 60))
    
    # write table data entry sheet
    writeData(wb, sheet = tabletype, x = template, headerStyle = style_col) 
    # width should show the full column name
    # setColWidths(wb, sheet = tabletype, cols = 1:ncol(template), widths = "auto")
    
    # save template workbook to templates folder
    saveWorkbook(wb, file = paste0("./docs/templates/ccrcn_", tabletype, "_template.xlsx"),
                 overwrite = T)
  }
}

table_names <- c("methods", "sites", "cores", "depthseries", "species", "impacts")
# table_names <- c("methods", "depthseries") # for tests

# make the templates
generateTemplates(table_names)


