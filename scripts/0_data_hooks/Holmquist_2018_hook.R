## CCRCN Data Library
# contact: klingesd@si.edu

# This script hooks in data from the Holmquist et al 2018 Scientific Reports
#   data release. 

# Holmquist, James R., Windham-Myers, Lisamarie, Bliss, Norman, Crooks, Stephen, Morris, James T., Megonigal, 
# J. Patrick, Troxler, Tiffany, Weller, Donald, Callaway, John, Drexler, Judith, Ferner, Matthew C., Gonneea, 
# Meagan E., Kroeger, Kevin D., Schile-Beers, Lisa, Woo, Isa, Buffington, Kevin, Boyd, Brandon M., Breithaupt, 
# Joshua, Brown, Lauren N., Dix, Nicole, Hice, Lyndie, Horton, Benjamin P., MacDonald, Glen M., Moyer, 
# Ryan P., Reay, William et al. 2018. 
# [Dataset] "Accuracy and Precision of Tidal Wetland Soil Carbon Mapping in the Conterminous United States: 
# Public Soil Carbon Data Release." Distributed by Smithsonian Research Online.


## 1. Download data ################
# Load RCurl, a package used to download files from a URL
library(RCurl)

# Create a list of the URLs for each data file
url_list <- list("https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_core_data.csv?sequence=7&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_depth_series_data.csv?sequence=8&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_impact_data.csv?sequence=9&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_methods_data.csv?sequence=10&isAllowed=y",
                 "https://repository.si.edu/bitstream/handle/10088/35684/V1_Holmquist_2018_species_data.csv?sequence=11&isAllowed=y"
)

# Apply a function, which downloads each of the data files, over url_list
lapply(url_list, function(x) {
  # Extract the file name from each URL
  filename <- as.character(x)
  filename <- substring(filename, 56)
  filename <- gsub("\\..*","", filename)
  # Now download the file into the "data" folder
  download.file(x, paste0(getwd(), "./data/Holmquist_2018/", filename, ".csv"))
})

## 2. Import data to convert codes to common plain language ####

cores <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_core_data.csv")
depthseries <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_depth_series_data.csv")
impacts <-read.csv("./data/Holmquist_2018/V1_Holmquist_2018_impact_data.csv")
species <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_species_data.csv")
methods <- read.csv("./data/Holmquist_2018/V1_Holmquist_2018_methods_data.csv")

## 3. Recode factors #################
cores <- cores %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014"))

depthseries <- depthseries %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id,
                                  "Crooks_et_al_2013" = "Crooks_et_al_2014"))

# There are 9 impact codes to convert to plain english
impacts <- impacts %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014"),
         impact_code = recode_factor(impact_code,
           "Can" = "Canalled",
           "Dik" = "Diked",
           "Dit" = "Ditched",
           "Eut" = "Eutrophic",
           "Imp" = "Impounded",
           "Man" = "Managed", 
           "Nat" = "Natural", 
           "Res" = "Restoring",
           "SalImp" = "Salt Impacted"
         ))

# There are 77 species codes

species <- species %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014"), 
         species_code = recode_factor(species_code,
            "AgSp" = "Agrostis spp.", "AlPh" = "Alternanthera philoxeroides", "AmCa" = "Amaranthus cannabinus", 
            "AmTr" = "Ambrosia trifida", "ArAr" = "Arrow arum.", "AtFi" = "Athyrium filix-femina",
            "AvGe" = "Avicennia germinans", "BaHa" = "Baccharis halimifolia", "BaMa" = "Batis maritima",
            "BiLa" = "Bidens laevis", "BoMa" = "Bolboschoenus maritimus", "CaLy" = "Carex lyngbyei", "CoSe" = "Cornus sericea",  
            "CuSa" = "Cuscuta salina", "DiSp" = "Distichlis spicata", "EcSpp" = "Echinochloa spp", "ElPa" = "Eleocharis palustris",
            "ElSpp" = "Eleocharis spp.", "FrSa" = "Frankenia salina", "GaSh" = "Gaultheria shallon",
            "GrSt" = "Grindelia stricta", "HiSpp" = "Hibiscus spp.", "ImCa" = "Impatiens capensis",
            "IvFr" = "Iva frutescens","JaCa" = "Jaumea carnosa", "JuBa" = "Juncus balticus", 
            "JuRo" = "Juncus roemerianus", "LoIn" = "Lonicera involucrata", "LuSpp" = "Ludwigia spp",
            "LyAm" = "Lysichiton americanus", "MyGa" = "Myrica gale", 
            "NuAd" = "Nuphar advena", "NyAq" = "Nyssa aquatica", "OeSa" = "Oenanthe sarmentosa",
            "PaHe" = "Panicum hemitomon", "PaVa" = "Paspalum vaginatum", "PeVi" = "Peltandra virginica",
            "PhAr" = "Phalaris arundinacea", "PhAu" = "Phragmites australis", "PiSi" = "Picea sitchensis", 
            "PoAr" = "Polygonum arifolium", "PoPu" = "Polygonum punctatum", "PoSa" = "Polygonum sagittatum",
            "PoSpp" = "Polygonum spp.","RiMa" = "Rhizophora mangle", "RoCa" = "Rosa californica",
            "RoNu" = "Rosa nutkana", "RoPi" = "Rosa pisocarpa", "RuSp" = "Rubus spectabilis", 
            "RuUr" = "Rubus ursinus", "SaLa" = "Sagittaria latifolia", "SaLan" = "Sagittaria lancifolia",
            "SaLas" = "Salix lasiolepis", "SaPa" = "Salicornia pacifica", "SaVi" = "Salicornia virginica",
            "ScAc" = "Scirpus acutus", "ScAm" = "Scirpus americanus", "ScCa" = "Schoenoplectus californicus", 
            "ScTa" = "Schoenoplectus tabernaemontani", "SpAl" = "Spartina alterniflora", "SpCy" = "Spartina cynosuroides", 
            "SpDo" = "Spiraea douglasii", "SpFo" = "Spartina foliosa", "SpPa" = "Spartina patens", 
            "SpSpp" = "Spartina spp.", "Swamp" = "Swamp","TaDi" = "Taxodium distichum",
            "TrMa" = "Triglochin maritima", "TrNa" = "Trapa natis", "TyAg" = "Typa angustifolia", 
            "TyDo" = "Typa domingensis", "TyLa" = "Typha latifolia", "TySpp" = "Typha spp.",
            "UnVeg" = "Un-vegetated", "ZiAq" = "Zizania aquatica", "ZiMi" = "Zizaniopsis milaceae", "Mix" = "Mix"))
              
methods <- methods %>%
  # The Crooks study ID should be 2014, not 2013. 
  mutate(study_id = recode_factor(study_id, "Crooks_et_al_2013" = "Crooks_et_al_2014"))

## 4. Write to folder ########
write.csv(cores, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_core_data.csv")
write.csv(depthseries, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_depth_series_data.csv")
write.csv(impacts, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_impact_data.csv")
write.csv(species, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_species_data.csv")
write.csv(methods, "./data/Holmquist_2018/derivative/V1_Holmquist_2018_methods_data.csv")
