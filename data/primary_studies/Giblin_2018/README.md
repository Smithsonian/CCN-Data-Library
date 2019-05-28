## Data citation: 
Giblin A., I. Forbrich. 2018. PIE LTER high marsh sediment chemistry and activity measurements, Nelson Island Creek marsh, Rowley, MA. Environmental Data Initiative. https://doi.org/10.6073/pasta/d1d5cbf87602ccf51de30b87b8e46d01. 

## Journal article citation: 
Forbrich, I., A. E. Giblin, and C. S. Hopkinson. 2018. “Constraining Marsh Carbon Budgets Using Long‐Term C Burial and Contemporary Atmospheric CO2 Fluxes.” Journal of Geophysical Research: Biogeosciences 123 (3): 867–78. https://doi.org/10.1002/2017JG004336.

## Site level, core level, core level species and soil depth series data

Four derived .csv files:

1. “Giblin_and_Forbrich_2018_cores.csv"

2. "Giblin_and_Forbrich_2018_depthseries.csv"

3. "Giblin_and_Forbrich_2018_species.csv"

4. "Giblin_and_Forbrich_2018_sites.csv"

Datasets follow the structure and definitions given at CCRCN-Soils-Working-Group/data/data-syntheses/data_templates/ in the group’s GitHub, and at https://serc.si.edu/coastalcarbon/database-structure

## Issues or notes: 
2019-02-05 JH notes: Hook script still has a few issues. Depth series has inconsistent formatting of NA's. We need a housekeeping script that will detect 'NA' and convert to consistent accross all tables and all attributes.

``` file[is.na(file)] <- NA```

Species table needs columns ordered correctly.

DOI in study citations still has a rogue `};` at the end.

2019-03-05 ML: Fixed NA format, species table ordering. The DOI issue will be fixed for all scripts once the new bibliography workflow is established. 

