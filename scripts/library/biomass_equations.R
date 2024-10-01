


library(tidyverse)

source("scripts/1_data_formatting/cifor_utility_functions.R") 

# read in reference tables for the CIFOR SWAMP database
refs <- readExcelWorkbook("./data/primary_studies/CIFOR/CIFOR_docs/Ref_Tables_2020-05-12.xlsx")

citation <- refs$Ref_Citation %>% rename(CITID = ID)
species <- refs$Ref_Species %>% rename(SPECID = ID) %>% select(SPECID, SCIENTIFIC_NAME)
comp <- refs$Ref_Component %>% rename(COMPID = ID)
comp_bio <- refs$Ref_Component_BioGrp %>% rename(COMP_BIOGRPID = ID)

eq <- refs$Ref_Equation %>% 
  left_join(species) %>% 
  left_join(citation) %>% 
  left_join(comp) %>% 
  left_join(comp_bio) %>% 
  filter(EQUATION != "NA") %>% 
  select(-c(ID, SPECID, CITID, COMPID, COMP_BIOGRPID, COMP_GRPID, EQUAT, OUTPUT, COUNTRYID)) %>% 
  select(SCIENTIFIC_NAME, COMP_BIOGRP, COMP, COMPONENT_DESCR, everything()) %>% 
  arrange(SCIENTIFIC_NAME, COMP_BIOGRP)
  
write_excel_csv(eq, "docs/cifor_biomass_equations.csv")
  