## Methane Working Group Chamber Data Synthesis

## script for generating a pretty HTML version of the database guidance
## contact: Jaxine Wolfe, wolfejax@si.edu

library(tidyverse)
library(knitr)
library(kableExtra)

guidance <- read_csv("docs/ccrcn_database_structure.csv") %>% 
  select(table, attribute_name, attribute_definition, required, data_type, format_unit_codes) %>% 
  rename(table_name = table, 
         # 'column name' = attribute_name,
         definition = attribute_definition, 
         'data type' = data_type,
         'unit, format, or codes' = format_unit_codes)

rmarkdown::render(input = "./scripts/generate_table.Rmd",
                  # output_format = "html_document",
                  output_file = "ccrcn_database_structure.html",
                  output_dir = "./docs")
