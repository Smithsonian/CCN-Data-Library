## CCRCN Data Library

## script for generating a pretty HTML version of the database guidance
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(knitr)
library(kableExtra)
library(reactable)
library(reactablefmtr)

# load controlled variables for table nesting
variables <- read_csv("docs/controlled_variables.csv", col_types = cols())
  # remove vars that are undefined/obsolete
  # filter(!(variable_name %in% c("mass accumulation", "accretion")))

# create shortened version of variables to display
vars_collapse <- variables %>% 
  group_by(attribute_name) %>% 
  summarise(variable_names = paste(variable_name, collapse="; "))

# read in guidance 
guidance <- read_csv("docs/ccrcn_database_structure.csv", col_types = cols()) %>% 
  # filter out attributes added during post-processing
  filter(required != "added automatically") %>% 
  mutate(required = case_when(required %in% c("required", "encouraged") ~ "Y",
                                   attribute_name %in% c("study_id", "site_id", "core_id", "method_id", "habitat",
                                                         "salinity_class", "vegetation_class", "inundation_class", "impact_class",
                                                         "salinity_method", "vegetation_method", "inundation_method") ~ "Y", 
                                   TRUE ~ NA_character_)) %>% 
  # mutate(required = recode(required, "conditional" = "encouraged")) %>% 
  select(table, attribute_name, attribute_definition, required,  data_type, format_unit_codes) %>% 
  left_join(vars_collapse) # join the collapsed variable names



# write function to generate interactive tables
renderTable <- function(tabletype){
  
  table_guidance <- guidance %>%
    filter(table == tabletype) %>% 
    mutate(units = ifelse(data_type == "factor", NA, format_unit_codes)) %>% 
    select(-table, -format_unit_codes) %>% select(-variable_names, variable_names) %>% 
    rename(`column name` = attribute_name, `data type` = data_type, 
           definition = attribute_definition, `variable names` = variable_names)
  # need to make column names more user friendly
  
  # conditionally displayed table nesting
  factor_rows <- which(table_guidance$`data type` == "factor")
  # conditional emphasis of key attributes
  key_rows <- which(table_guidance$required == "Y")
  
  # create interactive table
  reactable(table_guidance %>% select(-required), searchable = TRUE, highlight = TRUE, 
            pagination = FALSE,
            # bold key attributes
            columns = list(`column name` = colDef(style = cell_style(rows = key_rows, 
                                                                     font_weight = "bold",
                                                                     horizontal_align = "left"),
                                                  width = 210),
                           definition = colDef(width = 200),
                           `data type` = colDef(width =  80)),
            # rowClass = function(index) {
            #   if (table_guidance$required[index] == "required") {
            #     "bold"
            #   }
            # },
            # columns = list(
            #   Make = colDef(minWidth = 200),   # 50% width, 200px minimum
            #   Type = colDef(minWidth = 100),   # 25% width, 100px minimum
            #   Weight = colDef(minWidth = 100)  # 25% width, 100px minimum
            # ),
            details = function(index) {
    if (index %in% factor_rows) {
      # store variable name at index
      attribute <- table_guidance$`column name`[index]
      # create table of variable codes for given attribute
      reactable(variables %>% 
                  filter(attribute_name == attribute) %>% 
                  select(-attribute_name), 
                # specify styling
                bordered = TRUE, highlight = TRUE, fullWidth = TRUE)
    }
  })
}

# render the html document
rmarkdown::render(input = "scripts/housekeeping/generate_table.Rmd",
                  # output_format = "html_document",
                  output_file = "ccrcn_database_structure.html",
                  output_dir = "./docs")

