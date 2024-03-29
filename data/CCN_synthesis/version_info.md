## Coastal Carbon Network Data Library Versioning Documentation

This versioning document tracks the changes made to the CCN Data Library, including additions of new data or corrections to previously synthesized datasets.

***

### Synthesis Version 1.2.0

#### Version 1.2.0 Contents 

This database contains tidal wetland soil carbon profiles and associated metadata. The data itself is housed in separate comma separated value (.csv) spreadsheets linked by common identifier attributes (study_id, site_id, and core_id).

*Data Tables*

- CCN_methods.csv - Contains information on materials and methods broken down by study 
- CCN_sites.csv - Contains positional and descriptive information of the coring sites.
- CCN_cores.csv - Contains positional and descriptive information on core locations.
- CCN_depthseries.csv - Contains raw depth-series information for all cores.
- CCN_species.csv - Contains information on the dominant plant species at coring locations.
- CCN_impacts.csv - Contains information on the anthropogenic impacts at site or coring locations.

*Metadata Tables*

- CCN_database_structure.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
- CCN_study_citations.csv - Contains bibliographic information for publications and associated sources, which can be related to the database tables via study_id.
- CCN_bibliography.bib - Contains bibliographic information for publications and sources associated with this database.

#### Version 1.2.0 Updates

*New Datasets 1.2.0*

This update adds 26 new studies and 5,164 new soil cores to the CCN Data Library, bringing the total number of cores to 14,958, with 526 unique studies. 

Bost et al 2024
Brown et al 2024 
Craft 2024
Cifuentes et al 2024 (Nicoya)
Darienzo and Peterson 1990
Dontis et al 2023
Drake et al 2024
Everhart et al 2020
Gillen et al 2018
Kemp et al 2024
Langston et al 2022
Loomis and Craft 2024
Machite and Adams 2024
Maxwell et al 2023
Messerschmidt et al 2020
Morgan et al 2024 
Palinkas and Cornwell 2024
Palinkas and Engelhardt 2024
Radabaugh et al 2017
Radabaugh et al 2018
Radabaugh et al 2021
Radabaugh et al 2023
Schieder and Kirwan 2019
Smith and Kirwan 2021
Stahl et al 2024 
Stevens et al 2024
Vinent and Kirwan 2017


*Corrections and Additions to previously synthesized datasets 1.2.0*

1. Belshe et al 2019: Updated core position and position_notes for core_id = "SM10" in _cores.csv. Updated and reformatted depthseries table to reflect V1.2.0 structure. 
2. Breithaupt et al 2014: Removed 'date' attribute from depthseries table. This value is represented in 'age' and 'age_se'.
3. Bukoski et al 2017: Corrected latitude and longitude values. 
4. Callaway et al 2019: Removed 'pb210_background_assumption' attribute from methods table. Value recoded in 'dating_notes'. Resolved duplicate core_ids. 
5. Cifuentes et al 2023 (Panama): Resolved encoding issue with site names.
6. Eagle et al 2021: Resolved accent encoding issue.
7. Everhart et al 2020: Removed the following uncontrolled attributes from depthseries table: th234_activity, th234_activity_se, k40_activity, k40_activity_se.
8. Fourqurean et al 2012: Added site-level position data in _cores.csv.
9. Holmquist et al 2018: Removed Drake et al 2015 and Radaubaugh et al 2018 data contained in synthesis, now represented in Drake et al 2024 and Radaugbaugh et al 2018. Added 'site_id' to _impacts.csv and _species.csv. 
10. Howard and Fourqurean 2020: Resolved duplicate core_ids. 
11. Osland et al 2016: Added habitat informations in _cores.csv.
12. Peck et al 2020: Removed cores that do not contain dissaggregated depthseries data.
13. Poppe and Rybczyk 2019: Removed duplicate impact classes in _impacts.csv. 
14. Marot et al 2020: Resolved position and updated habitat classification in _cores.csv. 
15. Miller et al 2022: Updated study_id in all associated .csv files. Study originally represented as Rodriguez et al 2022.
16. Morrisette et al 2023: Removed uncontrolled attribute in methods table: 'ground_or_sieved_flag'.
17. Schile-Beers 2017: Updated missing core positions in _cores.csv.
18. Snedden 2017: Removed modeled carbon values from _depthseries.csv.
19. St Laurent 2020: Resolved duplicate core_ids.  
20. Thom et al 2019: Addition of core "PB1" replicate in _cores.csv to represent samples present in _depthseries.csv. 
21. Trettin et al 2020: Updated name of study citations csv file. Now named Trettin_et_al_2020_study_citations.csv.
22. Turck 2014: Resolved error with vegetation_class in _cores.csv.



***

### Synthesis Version 1.1.1

#### Version 1.1.1 Contents

This database contains tidal wetland soil carbon profiles and associated metadata. The data itself is housed in separate comma separated value (.csv) spreadsheets linked by common identifier attributes (study_id, site_id, and core_id).

*Data Tables*

- CCN_methods.csv - Contains information on materials and methods broken down by study 
- CCN_sites.csv - Contains positional and descriptive information of the coring sites.
- CCN_cores.csv - Contains positional and descriptive information on core locations.
- CCN_depthseries.csv - Contains raw depth-series information for all cores.
- CCN_species.csv - Contains information on the dominant plant species at coring locations.
- CCN_impacts.csv - Contains information on the anthropogenic impacts at site or coring locations.

*Metadata Tables*

- CCN_database_structure.html - Is a simplified, visual and interactive version of metadata.xml for display purposes.
- CCN_study_citations.csv - Contains bibliographic information for publications and associated sources, which can be related to the database tables via study_id.
- CCN_bibliography.bib - Contains bibliographic information for publications and sources associated with this database.

#### Version 1.1.1 Updates

*Corrections and Additions to previously synthesized datasets*

1. OKeefe-Suttles et al 2021: added year of sampling to _cores.csv
2. Updates in datasets collected from the CIFOR SWAMP project. Updates include corrected core_id values and core position data fixes. 
SWAMP Data-Soil carbon-Barreto-2017-Brazil                  
SWAMP Data-Soil carbon-Boca Grande-2017-Brazil              
SWAMP Data-Soil carbon-Caetano-2017-Brazil                   
SWAMP Data-Soil carbon-Furo Grande-2017-Brazil              
SWAMP Data-Soil carbon-Mangue Sul-2017-Brazil               
SWAMP Data-Soil carbon-Mauripe-2017-Brazil                  
SWAMP Data-Soil carbon-Salina-2017-Brazil                    
SWAMP Data-Soil carbon-Berahan kulon-2019                    
SWAMP Data-Soil carbon-Timbulsloko-2019                     
SWAMP Data-Soil carbon-Bhitakarnika-2013-India              
SWAMP Data-Soil carbon-Acarau Boca-2016-Brazil              
SWAMP Data-Soil carbon-Baouth-2014-Senegal                   
SWAMP Data-Soil carbon-Caete-2017-Brazil                    
SWAMP Data-Soil carbon-Cauassu Leste Shrimp-2016-Brazil     
SWAMP Data-Soil carbon-Cauassu Oeste Shrimp-2016-Brazil      
SWAMP Data-Soil carbon-Cumbe Leste Camaro-2016-Brazil        
SWAMP Data-Soil carbon-Cumbe norte Camarao-2016-Brazil      
SWAMP Data-Soil carbon-Diamniadio-2014-Senegal              
SWAMP Data-Soil carbon-Djirnda-2014-Senegal                  
SWAMP Data-Soil carbon-Fambine-2014-Senegal                  
SWAMP Data-Soil carbon-Furo de Chato-2017-Brazil            
SWAMP Data-Soil carbon-Mangizal Cauassu-2016-Brazil          
SWAMP Data-Soil carbon-Manguinho-2016-Brazil                 
SWAMP Data-Soil carbon-Mounde-2014-Senegal                   
SWAMP Data-Soil carbon-Porto Ceu Mangrove-2016-Brazil       
SWAMP Data-Soil carbon-Porto Ceu Shrimp-2016-Brazil         
SWAMP Data-Soil carbon-Sang-2014-Senegal                    
SWAMP Data-Soil carbon-Case Shell-2014                       
SWAMP Data-Soil carbon-Jardin Du Elephant-2014              
SWAMP Data-Soil carbon-Lac Simba Deux-2014                  
SWAMP Data-Soil carbon-Lac Simba-2014                       
SWAMP Data-Soil carbon-Lac Sounga Deux-2014                
SWAMP Data-Soil carbon-Lac Sounga-2014                
SWAMP Data-Soil carbon-Mwana Mouele South-2014               
SWAMP Data-Soil carbon-Mwana Mouele-2014                     
SWAMP Data-Soil carbon-Ndougou-2014                          
SWAMP Data-Soil carbon-Paga-2014                             
SWAMP Data-Soil carbon-BRM10-2014                            
SWAMP Data-Soil carbon-MRM8-2014                             
SWAMP Data-Soil carbon-MRT7-2014                             
SWAMP Data-Soil carbon-MRT9-2014                             
SWAMP Data-Soil carbon-NCM1-2014                             
SWAMP Data-Soil carbon-NCM4-2014                             
SWAMP Data-Soil carbon-NCM5-2014                             
SWAMP Data-Soil carbon-NCT2-2014                             
SWAMP Data-Soil carbon-NCT3-2014                             
SWAMP Data-Soil carbon-NCT6-2014                             
SWAMP Data-Soil carbon-Marisma High-2017-Brazil            
SWAMP Data-Soil carbon-Marisma Low-2017-Brazil            
SWAMP Data-Soil carbon-Marisma Medium-2017-Brazil          
SWAMP Data-Soil carbon-Bunaken-2011                          
SWAMP Data-Soil carbon-Cilacap-2011                         
SWAMP Data-Soil carbon-Kubu Raya-2011-Indonesia             
SWAMP Data-Soil carbon-Sembilang-2011-Indonesia             
SWAMP Data-Soil carbon-Tanjung Puting-2009-Indonesia      
SWAMP Data-Soil carbon-Teminabuan-2011-Indonesia             
SWAMP Data-Soil carbon-Timika-2011-Indonesia                 
SWAMP Data-Soil carbon-Ca Mau-2012-Vietnam                  
SWAMP Data-Soil carbon-Arguni Bay-West Papua-2015-Indonesia  
SWAMP Data-Soil carbon-Bintuni Bay-West Papua-2018-Indonesia 
SWAMP Data-Soil carbon-Buruway-West Papua-2016-Indonesia    
SWAMP Data-Soil carbon-Etna Bay-West Papua-2017-Indonesia    
SWAMP Data-Soil carbon-Kaimana City-West Papua-2017-Indonesia
SWAMP Data-Soil carbon-CanGio-2012-Vietnam  
3. Removal of modeled fraction carbon values in Drexler et al 2009
4. Corrected a typo to include 5 cores that were previously dropped from the Fourqurean synthesis (Copertino unpublished study)
5. Corrected the vegetation class for Turck 2014 (changed from seagrass to emergent)
6. Added site-level coordinates for three studies in the Sanderman synthesis (Nsombo et al 2016, Eid and Staltout 2016 Egypt, and Eid et al 2016 Saudi Arabia). Also added associated publications for Eid and Staltout 2016 Egypt and Eid et al 2016 Saudi Arabia.
7. Resolved a post-processing bug that prevented taxa resolution of the synthesis species table.
8. Standardized activity units for several studies
 
***

### Synthesis Version 1.1.0

#### Version 1.1.0 Contents

This synthesis is comprised of this README file, a .bib file containing BibTex formatted citations for studies included in the download, and several comma separated value (.csv) spreadsheets linked by common attributes study_id, site_id, and core_id. The data tables present in each download may vary depending on available information for the selected sediment profiles. No data values are represented using 'NA'. Please refer to the "CCN_data_dictionary.csv" file or our online [CCN Database Structure](https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html) for detail on attribute and variable definitions and descriptions. 

- CCN_methods: Contains descriptions of methods and materials used during collection and processing of samples in the field and lab. Included if present for the downloaded data.
- CCN_sites: Contains positional and descriptive information associated with each sampling site.
- CCN_cores: Core level data includes the positional, descriptive, and environmental information associated with each core.
- CCN_depthseries: Contains information and measurements associated with sampling intervals along each core profile.
- CCN_impacts: Classification of anthropogenic or other impacts at the site or sampling location where data was collected. Included if present for the downloaded data.
- CCN_species: Identification of dominant plant species present at locations of coring. Included if present for the downloaded data.

#### Version 1.1.0 Updates

This synthesis update contains changes to the CCN Data Library, including new datasets and added categorical variables. These additions help to better represent a wide variety of data types and to describe coring locations across coastal ecosystems. 

**Database Structure**

*Updated Attributes 1.1.0:*

- geomorphic_id: Located in CCN_cores.csv

*Updated Variables 1.1.0*

- Located in CCN_sites.csv: "unvegetated", under vegetation_class attribute
- Located in CCN_cores.csv: "unvegetated", under vegetation_class attribute
- Located in CCN_depthseries.csv: "dredge horizon", under marker_type attribute 
- Located in CCN_impacts.csv: "degraded", under impact_class
- Located in CCN_impacts.csv: "hurricane", under impact_class

**New Datasets 1.1.0**

This update adds 94 new studies and 3,498 new cores to the CCN Data Library, bringing the total number of cores to 10,198, with 401 unique studies. 

Akther et al 2021  
Beers et al 2023  
Cifuentes et al 2023  
Costa et al 2023  
Curtis et al 2022  
Dai et al 2022  
Giblin 2018  
Githaiga et al 2017  
Hamzeh and Lahijani 2022  
Howard and Fourqurean 2020  
Kusumaningtyas et al 2018  
Lafratta et al 2018  
Marot et al 2020  
McClellan et al 2021  
Morrissette et al 2023  
Piazza et al 2020  
Rodriguez_et_al_2022  
Rovai et al 2022  
Saunders 2013  
Senger et al 2020  
Shaw et al 2021  
Snedden et al 2018  
Snedden et al 2021  
Turck et al 2014  
van Ardenne et al 2018  
Vincent and Dionne 2023  
Wang et al 2023  

*Datasets included from the CIFOR (Center for International Forestry Research) SWAMP Project:*

SWAMP Data-Soil carbon-Barreto-2017-Brazil                  
SWAMP Data-Soil carbon-Boca Grande-2017-Brazil              
SWAMP Data-Soil carbon-Caetano-2017-Brazil                   
SWAMP Data-Soil carbon-Furo Grande-2017-Brazil              
SWAMP Data-Soil carbon-Mangue Sul-2017-Brazil               
SWAMP Data-Soil carbon-Mauripe-2017-Brazil                  
SWAMP Data-Soil carbon-Salina-2017-Brazil                    
SWAMP Data-Soil carbon-Berahan kulon-2019                    
SWAMP Data-Soil carbon-Timbulsloko-2019                     
SWAMP Data-Soil carbon-Bhitakarnika-2013-India              
SWAMP Data-Soil carbon-Acarau Boca-2016-Brazil              
SWAMP Data-Soil carbon-Baouth-2014-Senegal                   
SWAMP Data-Soil carbon-Caete-2017-Brazil                    
SWAMP Data-Soil carbon-Cauassu Leste Shrimp-2016-Brazil     
SWAMP Data-Soil carbon-Cauassu Oeste Shrimp-2016-Brazil      
SWAMP Data-Soil carbon-Cumbe Leste Camaro-2016-Brazil        
SWAMP Data-Soil carbon-Cumbe norte Camarao-2016-Brazil      
SWAMP Data-Soil carbon-Diamniadio-2014-Senegal              
SWAMP Data-Soil carbon-Djirnda-2014-Senegal                  
SWAMP Data-Soil carbon-Fambine-2014-Senegal                  
SWAMP Data-Soil carbon-Furo de Chato-2017-Brazil            
SWAMP Data-Soil carbon-Mangizal Cauassu-2016-Brazil          
SWAMP Data-Soil carbon-Manguinho-2016-Brazil                 
SWAMP Data-Soil carbon-Mounde-2014-Senegal                   
SWAMP Data-Soil carbon-Porto Ceu Mangrove-2016-Brazil       
SWAMP Data-Soil carbon-Porto Ceu Shrimp-2016-Brazil         
SWAMP Data-Soil carbon-Sang-2014-Senegal                    
SWAMP Data-Soil carbon-Case Shell-2014                       
SWAMP Data-Soil carbon-Jardin Du Elephant-2014              
SWAMP Data-Soil carbon-Lac Simba Deux-2014                  
SWAMP Data-Soil carbon-Lac Simba-2014                       
SWAMP Data-Soil carbon-Lac Sounga Deux-2014                
SWAMP Data-Soil carbon-Lac Sounga-2014                
SWAMP Data-Soil carbon-Mwana Mouele South-2014               
SWAMP Data-Soil carbon-Mwana Mouele-2014                     
SWAMP Data-Soil carbon-Ndougou-2014                          
SWAMP Data-Soil carbon-Paga-2014                             
SWAMP Data-Soil carbon-BRM10-2014                            
SWAMP Data-Soil carbon-MRM8-2014                             
SWAMP Data-Soil carbon-MRT7-2014                             
SWAMP Data-Soil carbon-MRT9-2014                             
SWAMP Data-Soil carbon-NCM1-2014                             
SWAMP Data-Soil carbon-NCM4-2014                             
SWAMP Data-Soil carbon-NCM5-2014                             
SWAMP Data-Soil carbon-NCT2-2014                             
SWAMP Data-Soil carbon-NCT3-2014                             
SWAMP Data-Soil carbon-NCT6-2014                             
SWAMP Data-Soil carbon-Marisma High-2017-Brazil            
SWAMP Data-Soil carbon-Marisma Low-2017-Brazil            
SWAMP Data-Soil carbon-Marisma Medium-2017-Brazil          
SWAMP Data-Soil carbon-Bunaken-2011                          
SWAMP Data-Soil carbon-Cilacap-2011                         
SWAMP Data-Soil carbon-Kubu Raya-2011-Indonesia             
SWAMP Data-Soil carbon-Sembilang-2011-Indonesia             
SWAMP Data-Soil carbon-Tanjung Puting-2009-Indonesia      
SWAMP Data-Soil carbon-Teminabuan-2011-Indonesia             
SWAMP Data-Soil carbon-Timika-2011-Indonesia                 
SWAMP Data-Soil carbon-Ca Mau-2012-Vietnam                  
SWAMP Data-Soil carbon-Arguni Bay-West Papua-2015-Indonesia  
SWAMP Data-Soil carbon-Bintuni Bay-West Papua-2018-Indonesia 
SWAMP Data-Soil carbon-Buruway-West Papua-2016-Indonesia    
SWAMP Data-Soil carbon-Etna Bay-West Papua-2017-Indonesia    
SWAMP Data-Soil carbon-Kaimana City-West Papua-2017-Indonesia
SWAMP Data-Soil carbon-CanGio-2012-Vietnam   
Sharma et al 2021
MacKenzie et al 2021
Bukoski et al 2020
Trettin et al 2020
Hribljan et al 2020

**Corrections and Additions to datasets included in previous update (V1.0.0)**

1. Abbott et al 2019: corrected site names to match with associated paper.
2. Boyd et al 2019: assigned habitat to cores. 
3. Buffington et al 2020: corrected depth increments for Cs137 activity. 
4. Carlin 2021: assigned habitats to cores.
5. Drexler et al 2009: added year of core collection. 
6. Drexler et al 2013: corrections to Pb210 and Cs137 units. 
7. Drexler 2019: assigned habitats to cores. 
8. Ensign et al 2020: assigned habitats to sites, added positional data for cores
9. Gonneea et al 2018: corrected Pb210 unit. 
10. Johnson et al 2007: added month and year of core collection. 
11. Kauffman et al 2020: dropped modeled fraction carbon values from depthseries data, assigned habitat to cores.
12. Keshta et al 2020: dropped modeled fraction carbon values from depthseries data.
13. Krauss et al 2018: assigned habitats to cores, added year of core collection, changed impact class from "restoring" to "natural", corrected site names to match with associated paper.
14. Nahlik and Fenessey 2016: assigned habitats to cores.
15. Noe et al 2016: corrected Pb210 unit, added month and year of core collection. 
16. Messerschmidt and Kirwan 2020: corrected an error in Pb210 standard error.
17. Okeefe-Suttles et al 2021: Correction in Pb2130 activity values. 
18. Vaughn et al 2020: added missing core position data. 
19. Watson and Bryne 2013: added year of core collection and added missing site names.  
20. Weston et al 2020: Updated to Weston et al 2023, revised data. 

***

### Synthesis Version 1.0.0

*Note: This database was initialized in 2018, however, this release marks the official adoption of semantic versioning for the CCN Data Library.*

#### Major New Features

- Added features and metrics to increase database utility for end users
- Internal database and ingestion workflow restructuring
- Updated database documentation and instructions for contribution

**Database Structure Updates**

- Updates and additions to controlled attributes and variables
- Standardization of bibliography IDs and publication type assignment

**Hook Script Updates**

- Reviewed and updated all hook scripts in accordance with revised database guidance #65 
- Updated and standardized QAQC for data ingestion

**Synthesis Post-Processing**

Developed workflows to provide the following additional information for every soil core in the synthesis:

- Assign geography to the resolution of country and administrative division 
- Assign habitat type if not defined in the original hook script
- Assign quality codes based on utility and completeness of the information present for each profile

**Other Updates**

- Creation of a CCN taxonomic database

***

### About the CCN

The [Coastal Carbon Network](https://serc.si.edu/coastalcarbon) seeks to accelerate the pace of discovery in coastal wetland carbon science by providing our community with access to data, analysis tools, and synthesis opportunities. Our activities include bringing data libraries online, creating open source analysis and modeling tools, providing training and outreach opportunities, hosting data synthesis workshops targeted at strategically reducing uncertainty in coastal carbon science issues, and to create a community of practice. For more information please contact: CoastalCarbon@si.edu. Thank you for your interest in the Coastal Carbon Data Library and Atlas.
