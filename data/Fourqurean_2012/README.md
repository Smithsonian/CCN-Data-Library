# Fourqurean et al. 2012 Hook Notes and Progress

Citation: 
Fourqurean, James W., et al. "Seagrass ecosystems as a globally significant carbon stock." Nature geoscience 5.7 (2012): 505.

## Progress 

Object | Status | Notes
------------- | ------------- | -------------
Material and Methods Metadata | in progress | 
Study Information table | not started | 
Keywords table | not started | 
Author table | not started |
funding sources table | not started |
site-level table | in progress | 
core-level table | needs review | 
species table | in progress | 
impact table | in progress | 
depthseries table | needs review |
biomass table | needs review |
define study-specific attributes | not started |
define study-specific values or codes | not started |
user defined attributes | not started | 
user define variables | not started | 

## Notes and Assumptions


#### Biomass data 

attribute | unit | description 
----------------------------------
BGB_prod  |g/m2/yr | 
BGB_turnover | yr-1 | 
percent_dry_mass | unitless | 
decay_constant | unitless | 
stem_density | # / m2 | 
total_AGB | g / m2| total of dry living, dead, and fine aboveground biomass
95_rooting_depth | g / m2 |
live_BGB | g / m2 | dry living coarse organic matter (primarily stem bases and rhizomes)
dead_BGB | g / m2 | dry dead coarse organic matter (primarily stem bases and rhizomes)
fine_BGB | g / m2 | dry living and dead organic matter caught within a 1mm mesh sieve
total_BGB | g / m2 | total of dry living, dead, and fine belowground biomass
total_biomass | g / m2 | 
AG_carbon | g / m2 | 
BG_carbon | g / m2 | 

#### Manual Changes to raw data
1. 177 coreserial values in the raw dataset did not get a study ID. One row of each unique core was given a reference in the raw data. Occasionally, a new core started within the same study and the reference was not moved to the new core. For those cases, manual entry of the reference was done. 
2. 11 coreserial numbers actually represented 22 cores. New coreserial values were manually assigned to these cases. The original coreserial numbers were: 340, 670:676, 678:681. 
3. 5 cores were associated with 20 coreserial numbers. That is, each depthseries entry in the core was assigned a unique coreserial number. They are represented by coreserial numbers 922:946 in the original dataset. These values have been collapsed into five coreserial numbers. 
4. Depthseries intervals for Marba unpublished and Fourqurean unpublished could be derived from depth at mid slice and slice size variables
5. Depthseries intervals for Holmer et al. 2016 and Vichkovitten and Holmer 2005 were in a separate column than the rest of the cores. The data was moved to the correct column. 

All of these changes were made manually and saved to the "intermediate" folder. The raw data can still be viewed in the "original" folder. The edited intermediate file is used in the hook script. 

#### Methods and Materials

Assumption | Justification | Notes
------------- | ------------- | -------------
soil profiles represent full sediment profile (`core_length_flag` == "core depth represents deposit depth " | In original data, "Depth of accumulated sediment (cm)" for each core equals the depth of the last sampled slice | 

## Uncontrolled Attributes/Variables and Potential Guidance Updates

#### Site Data

Attribute | Status | Notes
------------- | ------------- | -------------
vegetation_class | new variable: "unvegetated marine" | 


#### Depthseries data

Attribute | New Attribute Name | Unit | Definition | Notes
------------- | ------------- | ------------- | ------------- | ------------
Soil organic carbon density (mg/mL) | none given | mg/mL | self-explanatory |
Soil organic matter density (mg/mL) | none given| mg/mL | self-explanatory  |
Porosity (%) | none given | dimensionless | Porosity of soil |
Soil organic matter density | mg/mL | excluded from derivative dataset, but may want to include in RCN guidance

