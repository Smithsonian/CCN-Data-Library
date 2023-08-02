## CCRCN Data Library
# contact: wolfejax@si.edu

# This script contains simple functions built to curate datasets

## Remove all empty columns 
# https://stackoverflow.com/questions/2643939/remove-columns-from-dataframe-where-all-values-are-na
notAllNA <- function(x) any(!is.na(x))


## Convert disintegration/min/gram to becquerel/kilogram ###########
convert_dpm_g_to_bec_kg <- function(col_input) {
  col_output <- as.numeric(col_input)/60 * 1000
  return(col_output)
}


# Convert UTM to decimal degree ##########
# assumed columns: core_id, easting, northing, zone
UTM_to_DD <- function(df, core_id, easting, northing, zone){
  
  require(sf)
  
  # define latlong projection
  dd_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  zones <- unique(df$zone) # isolate specific zones
  dd_results <- data.frame()
    
  # loop through zones
  # create spatial object and assign coords utm coordinate system
  for(z in zones){
    
    # define UTM projection with specified zone
    utm_prj <- paste0("+proj=utm +zone=", z, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
    # subset dataframe into specific zone
    
    utm_df <- df %>% filter(zone == z)
    
    utm_coords <- st_as_sf(utm_df, coords = c("easting", "northing"), crs = utm_prj) 
    # st_crs(utm_coords) # check projection
    dd_coords <- st_transform(utm_coords, crs = dd_prj) %>% 
      # extract lat lon from geometry
      mutate(longitude = sf::st_coordinates(.)[,1],
             latitude = sf::st_coordinates(.)[,2]) %>% 
      # extract(geometry, into = c('longitude', 'latitude'), '\\((.*),(.*)\\)', convert = T) %>% 
      dplyr::select(core_id, longitude, latitude) %>% 
      sf::st_set_geometry(NULL)
    
    dd_results <- bind_rows(dd_results, dd_coords)
  }

  # join back to the dataframe
  df_decimal <- left_join(df, dd_results)
  
  return(df_decimal)
}

# testing
# df <- data.frame(core_id = c('Wilmington', 'New Bern', "Shallotte"),
#                  zone = c(18, 18, 17),
#                  easting = c(228739.63048392, 313704.36, 741519.99),
#                  northing = c(3791107.69823704, 3886986.48, 3762267.73),
#                  other_info = "blah blah")
# df_decimal <- df %>% UTM_to_DD()
# leaflet(df_decimal) %>% addTiles() %>% addCircleMarkers(lng = ~longitude, lat = ~latitude)

## Convert UTM to lat/long ###############
# only used in Thorne hook
convert_UTM_to_latlong <- function(easting, northing, zone, core_id) {
  warning('This function has been superceded by UTM_to_DD().')
  
  # Remove non-numeric characters from zone attribute
  zone <- gsub("[^0-9]", "", zone)
  
  # Change value of digits to ensure as.numeric does not round
  options(digits=22)
  
  # Ensure that easting, northing, and zone are numeric
  easting <- as.numeric(easting)
  northing <- as.numeric(northing)
  zone <- as.numeric(zone)
  
  # Combine the three attributes
  spatialData <- as.matrix(cbind(easting, northing, zone))
  
  # Some rows may have NA values. We'll want to flag those rows, notify the user,
  #   remove them before transforming, and then add them back at the end.
  NA_rows <- c()
  
  # For each of the columns in spatialData (easting, northing, zone)...
  for (i in 1:3) {
    col <- spatialData[,i]
    
    # ...and for each cell in those columns...
    for (j in 1:length(col)) {
      
      # ...if the cell's value is NA...
      if (is.na(col[[j]])) {
        
        # ...tell the user so...
        warning(paste0("Row ", j, " in column ", colnames(spatialData)[i], " is 'NA.' 
            Removing row from data frame for now but will add back with NA at end
            of function. NOTE: CHECK TO MAKE SURE THE SAME ROWS HAVE 'NA' AND 
            'NA' HAS NOT BEEN ADDED TO OTHER ROWS"))
        
        # And add the row # of that cell to a vector that we initialized before
        NA_rows <- c(NA_rows, j)
        # In case some rows have more than one NA. We only need that row once
        NA_rows <- unique(NA_rows)
      }
    }
    NA_rows <- unique(NA_rows)
  }
  
  # We'll need spatialData as a tibble or data frame going forward
  require(tidyverse)
  spatialData <- as_tibble(cbind(easting, northing, zone))

  # Now that we flagged the NA rows, we can get rid of them
  spatialData <- na.omit(spatialData)
  
  # And get rid of any zone values that were NA
  zone_list <- as.list(unique(na.omit(zone)))
  
  # Establish the projection we'll convert to. NOTE: this could be a user input
  wgs84 = "+init=epsg:4326"
  
  # Initialize our output dataset
  output <- matrix(nrow = 0, ncol = 2)
  colnames(output) <- c("core_longitude", "core_latitude")
  
  # We'll need to transform the projection separately for data that are in
  #   different zones. So we'll need to subset by the zone values, which we
  #   stored in zone_list
  for (i in 1:length(zone_list)) {
    
    # Filter to just one zone
    spatialData_sub <- spatialData %>%
      filter(zone == zone_list[[i]]) %>%
      select(easting, northing) %>% # We don't need the zone attribute anymore
      na.omit() # Just in case
    
    # Create a dataframe for out subsetted data
    output_sub <- spatialData_sub
    
    # Define the proj4string, using the zone that the subsetted data are in
    proj4string <- CRS(as.character(paste0("+proj=utm +zone=", zone_list[[i]],
                                           " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")))
    
    # Finally, perform the transformation
    require(sp)
    sp <-  sp::spTransform(sp::SpatialPoints(list(spatialData_sub$easting, 
              spatialData_sub$northing), proj4string=proj4string),sp::CRS(wgs84))
  

  output_sub <- na.omit(sp@coords) # Get rid of NAs again
  colnames(output_sub) <- c("core_longitude", "core_latitude") # Rename the output columns
  output <- rbind(output, output_sub) # And slap the data subset into our final output
  }
  
  output <- data.frame(output) # Turn our output into a dataframe
  
  # Now let's add those NA rows back in
  require(DataCombine)
  if(length(NA_rows) > 0){
    for (i in 1:length(NA_rows)) {
      output <- InsertRow(data = output, NewRow = rep(NA, 2), RowNum = NA_rows[[i]])
    }
  }
  # And add the core_id attribute back for joining purposes
  output <- cbind(output, core_id)
  
  # And output the output
  output
}

## Create site-level bounding box from core-level locations #############

create_multiple_geographic_coverages <- function(core_table) {
  
  subsite_bounding_box <- core_table %>%
    # group by subsite
    group_by(site_id) %>% 
    # get min and max latitudes for each subsite
    summarise(site_longitude_max = max(core_longitude),
              site_longitude_min = min(core_longitude),
              site_latitude_max = max(core_latitude),
              site_latitude_min = min(core_latitude)
    ) %>%
    # Make a 10% buffered bounding box from the site data table
    mutate(lon_buffer = (site_longitude_max - site_longitude_min) / 10,
           lat_buffer = (site_latitude_max - site_latitude_min) / 10,
           site_longitude_max = site_longitude_max + lon_buffer,
           site_longitude_min = site_longitude_min - lon_buffer,
           site_latitude_max = site_latitude_max + lat_buffer,
           site_latitude_min = site_latitude_min - lat_buffer) %>%
    dplyr::select(site_id, site_longitude_max, site_longitude_min, 
                  site_latitude_max, site_latitude_min)
}

## Create IDs from study IDs FOR CORE-LEVEL DATA ############
# only used in Fourqurean synthesis hook

create_IDs_from_study_IDs_corelevel <- function(df, # dataframe
                                      new_ID # string for the name of the new ID column
                                      ) {
  # Extract all study IDs
  studies <- unique(df$study_id)
  
  # Iterate through old IDs
  for (i in 1:length(studies)) {
    
    # Subset to a particular study
    study <- df %>%
      filter(study_id == studies[[i]])
    
    # Create a unique ID from the row number
    study <- study %>%
      rowid_to_column("ID")
    
    # Combine the old ID with the row number separated by underscore
    study <- study %>%
      mutate(new = paste0(study_id, "_", ID))
    
    # Replace spaces with underscores in ID
    study$new <- gsub(" ", "_", study$new)
    
    # Rename the 'new' column with the new_ID string given by the user
    colName <- new_ID
    study <- study %>%
      mutate(!!quo_name(colName) := new)
    
    # Now remove temporary columns
    study <- study %>%
      select(-ID, -new)
    
    # Combine back together
    if (i == 1) {
      df_out <- study
    } else {
      df_out <- bind_rows(df_out, study)
    }
  }
  
  # Return final data frame
  df_out
}


## Recode variables ##################

## ... Recode vegetation classes to full names
# used in Drexler 2009 and Holmquist synthesis

recode_vegetation <- function(df, vegetation_class) {
  output_df <- df %>%
    mutate(vegetation_class = recode_factor(vegetation_class,
                                            "EM" = "emergent", 
                                            "FO" = "forested",
                                            "SS" = "scrub shrub",
                                            "FO/SS" = "forested to shrub"
    ))
  output_df
}

## ... Recode impact classes to full names 
# used in Holmquist synthesis

recode_impact <- function(df, impact_class) {
  output_df <- df %>%
    mutate(impact_class = recode_factor(impact_class,
                                       "Can" = "canalled",
                                       "Dik" = "diked",
                                       "Dit" = "ditched",
                                       "Eut" = "eutrophic",
                                       "Imp" = "impounded",
                                       "Man" = "managed", 
                                       "Nat" = "natural", 
                                       "Res" = "restoring",
                                       "SalImp" = "salt impacted"
    ))
  output_df
}
 
## ... Recode species codes to full names
## used in Holmquist synth and Osland 2016

recode_species <- function(df, species_code) {
  output_df <- df %>%
    mutate(species_code = recode_factor(species_code,
                                        "AgSp" = "Agrostis spp.", "AlPh" = "Alternanthera philoxeroides", "AmCa" = "Amaranthus cannabinus", 
                                        "AmTr" = "Ambrosia trifida", "ArAr" = "Peltandra virginica", "AtFi" = "Athyrium filix-femina",
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
                                        "UnVeg" = "Un-vegetated", "ZiAq" = "Zizania aquatica", "ZiMi" = "Zizaniopsis milaceae", "Mix" = "Mix",
                                        "AcAu" = "Acrostichium aureum", "ALGMAT" = "Algal Mat", "ALGSRF" = "Surface Algae", 
                                        "AmpArb" = "Ampelopsis arborea", "AmPs" = "Ambrosia psilostachya", "BBF3" = "Unidentified forb", 
                                        "BBG2" = "Unidentified grass", "BoFr" = "Borrichia frutescens", "ChPi" = "Chrysopsis pilosa",
                                        "ClMa" = "Cladium mariscus", "CoEr" = "Conocarpus erectus", "CrVi" = "Crassostrea virginica",
                                        "CuSp" = "Cuscuta sp.", "DaEc" = "Dalbergia ecastaphyllum", "ElCe" = "Eleocharis cellulosa" , 
                                        "FimCas" = "Fimbristylis castanea", "HaWr" = "Halodule wrightii" , "HyMu" = "Hypericum mutilum" , "IlDe" = "Ilex decidua" ,
                                        "IlVo" = "Ilex vomitoria" , "IpSa" = "Ipomoea sagittata" , "KoVi" = "Kosteletzkya virginica" , "LaRa" = "Laguncularia racemosa",
                                        "LuAl" = "Ludwigia alternifolia" , "LyCa" = "Lycium carolinianum" , "MoCe" = "Morella cerifera",
                                        "MoLi" = "Monanthochloe littoralis" , "PaHa" = "Panicum hallii" , "PaRe" = "Panicum repens" , "PaVi" = "Panicum virgatum" , 
                                        "PoHy" = "Polygonum hydropiperoides" , "RhMa" = "Rhizophora mangle" , "RuTr" = "Rubus trivialis" , "RuVe" = "Rumex verticillatus" ,
                                        "SaAr" = "Sabatia arenicola" , "SaBi" = "Salicornia bigelovii" , "SaDe" = "Salicornia depressa" , 
                                        "SaV" = "submerged aquatic vegetation","ScRo" = "Schoenoplectus robustus" , "ScSc" = "Schizachyrium scoparium" , "SeHe" = "Sesbania herbacea",
                                        "SePo" = "Sesuvium portulacastrum" , "SpBa" = "Spartina bakeri" , "SpSp" = "Spartina spp.",
                                        "SuLi" = "Suaeda linearis" , "SyTe" = "Symphyotrichum tenuifolium" , "ThTe" = "Thalassia testudinum",
                                        "Bare" = "unvegetated", "Un-vegetated" = "unvegetated"
    ))
  output_df
}       

## ... Recode salinity codes to full names 
# used in Drexler 2009 and Holmquist synthesis

recode_salinity <- function(df, salinity_class) {
  output_df <- df %>%
    mutate(salinity_class = recode_factor(salinity_class,
                                          "Bra" = "brackish",
                                          "Bra Fre" = "brackish to fresh", 
                                          "Bra Sal" = "bracish to saline",
                                          "Del" = "deltaic", 
                                          "Est" = "estuarine", 
                                          "Fre" = "fresh", 
                                          "Int" = "intermediate salinity", 
                                          "Mes" = "mesohaline", 
                                          "Oli" = "oligohaline",
                                          "Pol" = "polyhaline", 
                                          "Riv" = "riverine", 
                                          "Sal" = "saline"  ))
  
  return(output_df)
}

## Convert Salinity to Salinity Class ----

assignSalinityClass <- function(x) {
  # fresh = <0.5 ppt.; oligohaline = 0.5-5 ppt.; mesohaline = 5-18 ppt.; 
  # polyhaline = 18-30 ppt.; mixoeuhaline = 30-40 ppt.; saline = 30-50 ppt.; brine = >50 ppt.
  
  # nested ifelse statement to assign salinity class from given salinity
  sal_class <- case_when(x <= 0.5 ~ "fresh",
                         x >= 50 ~ "brine", 
                         between(x, 0.5, 5) ~ "oligohaline", 
                         between(x, 5, 18) ~ "mesohaline", 
                         between(x, 18, 30) ~ "polyhaline", 
                         between(x, 30, 40) ~ "mixoeuhaline", 
                         between(x, 40, 50) ~ "saline", 
                         is.na(x) ~ NA_character_,
                         TRUE ~ "other salinity class")
  return(sal_class)
}

## Resolve Taxonomy ####

## function to resolve taxa names using GNR 
# uses taxonomic authorities to resolve spelling rather than recoding everything by hand
resolveTaxa <- function(species, db = NULL) {
  # create unique list of species codes to save time
  taxa <- unique(sort(species$species_code))
  
  resolved <- data.frame()
  unresolved <- vector()
  
  for (i in 1:length(taxa)){
    # gnr_sources <- taxize::gnr_datasources()
    
    # store resolved results
    gnr_result <- taxize::gnr_resolve(sci = as.vector(taxa[i]), 
                                      preferred_data_sources = db, # default no preference
                                      canonical = TRUE) %>%
      # gnr_datasources()
      # preferred_data_sources = c(150, 9, 4, 3)
      
      slice(1) # pick the first result
    
    if (!plyr::empty(gnr_result)) {
      # compile list of resolved taxa
      resolved <- rbind(resolved, gnr_result)
      
    } else {
      # save unresolved taxa
      unresolved <- rbind(unresolved, taxa[i])
      # skip unresolved taxa
      i <- i + 1
      next
    }
  }
  
  if (length(unresolved) > 0) {
    print("The following taxa could not be resolved:")
    print(unresolved)
  }
  # bind results to the species dataframe provided
  resolved_species <- left_join(species, resolved, by = c("species_code" = "user_supplied_name")) %>% 
    rename(resolved_taxa = matched_name2) %>% # more informative colname
    mutate(name_updated = ifelse(species_code == resolved_taxa, FALSE, TRUE)) %>% 
    # could replace the updated species codes with the resolved name
    # but this is tricky if it didn't resolve and the resolved_taxa is NA
    select(-c(submitted_name, data_source_title, score))
  
  return(resolved_species)
}

# this function should only be used on resolved taxa
# the ncbi database doesn't seem super reliable, sometimes it pulls the wrong classification
# classifyTaxa <- function(species, database = "ncbi"){
#   
#   taxa_list <- unique(sort(species$species_code)) # save time with unique list
#   
#   classified_taxa <- data.frame()
#   
#   for(taxa in taxa_list){
#     # determine the taxonomic classification for each taxa
#     classified <- taxize::classification(taxa, db = database, rows = 1)
#     classified_df <- as.data.frame(classified[[1]])
#     
#     # extract the highest taxonomic level
#     taxa_class <- classified_df[nrow(classified_df),]
#     
#     # add to dataframe
#     classified_taxa <- bind_rows(classified_taxa, taxa_class)
#   }
#   
#   # bind results to the species dataframe provided
#   its_classified <- left_join(species, classified_taxa, by = c("species_code" = "name"))
#   # do some renaming
#   
#   return(its_classified)
# }
