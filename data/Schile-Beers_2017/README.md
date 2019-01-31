## Data citation: 
Schile, Lisa M. and Megonigal, J. Patrick. 2017. [Dataset] "Abu Dhabi Blue Carbon Demonstration Project." Distributed by Smithsonian Environmental Research Center. https://doi.org/10.5479/data_serc/10088/31949

## Journal article citation: 
Schile, L. M., Kauffman, J. B., Crooks, S., Fourqurean, J. W., Glavan, J. and Megonigal, J. P. (2017), Limits on carbon sequestration in arid blue carbon ecosystems. Ecol Appl, 27: 859–874. doi:10.1002/eap.1489

## Site level, core level and soil depth series data

Three derived .csv files:

1. “Schile-Beers_etal_2017_core_data.csv"

2. "Schile-Beers_etal_2017_depth_series_data.csv"

3. "Schile-Beers_etal_2017_site_data.csv"

Datasets follow the structure and definitions given at CCRCN-Soils-Working-Group/data/data-syntheses/data_templates/ in the group’s GitHub, and at https://serc.si.edu/coastalcarbon/database-structure

## Issues: 

1. Two sites have multi-year entries for cores that either do not have matching depth series data or have depth series data but no clear core-level metadata (and therefore no location data). The specific site_ids are: Eastern Mangrove 10 yr, Eastern Mangrove 7 yr, Eastern Mangrove 3 yr, Jubail Is. 10 yr, Jubail Is. 7 yr, Jubail Is. 3 yr

2. According to the methods in the publication, core depth was to either 3 m (the corer was 1 m long) or until parent material. There is no clear core_depth_flag code for the former, and I have coded it as "core depth limited by length of corer"

3. There are biomass data available in the original data but at this moment there are no guidelines for incorporating them into the CCRCN. 

4. There are NAs for around 25 depth series sections' depth_max. The section length was listed as >100.  