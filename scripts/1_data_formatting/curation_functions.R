## CCRCN Data Library
# contact: klingesd@si.edu

# This script contains simple functions built to curate datasets

library(tidyverse)

## Convert mean depth to min and max depth ############
# For conversion of depth interval values: if a dataframe
#   has a mean depth attribute, this will create a min depth
#   and max depth attributes

# NOTE: this function assumes constant sampling interval (as in, each slice of
#   core is of equal length)
convert_mean_depth_to_min_max <- function(dataframe, df_mean.depth) {
  # Set l as th length of the dataframe
  l <- length(df_mean.depth)
  df_mean.depth <- as.numeric(df_mean.depth)
  # For each row....
  for (i in 1:l) {
    min <- (df_mean.depth[i - 1] + df_mean.depth[i])/2 # Take mean of previous cell and current cell
    max <- (df_mean.depth[i + 1] + df_mean.depth[i])/2 # Take mean of current cell and next cell
      # If initial iteration...
      if (i == 1) {
        depth_min = as.vector(min) # create depth_min vector
        depth_max = as.vector(max) # create depth_max vector
      } else { # If not initial iteration, add min/max
               #  value to depth_min/max vector
        
          depth_min[i] <- min
          depth_max[i] <- max
        }
  }

  if (df_mean.depth[1] == 0) {
    depth_min[1] <- 0
    depth_max[1] <- 0
    depth_min[2] <- 0
    depth_max[l] <- depth_min[l] + 2 * (df_mean.depth[l] - depth_min[l])
  } else {
    depth_min[1] <- 0
    depth_max[l] <- depth_min[l] + 2 * (df_mean.depth[l] - depth_min[l])
    }

  dataframe <- dataframe %>%
    mutate(depth_min = depth_min) %>%
    mutate(depth_max = depth_max)

  return(dataframe)
}       

## Convert disintegration/min/gram to becquerel/kilogram ###########
convert_dpm_g_to_bec_kg <- function(col_input) {
  col_output <- as.numeric(col_input)/60 * 1000
  return(col_output)
}

## Convert percent to fraction ###############
convert_percent_to_fraction <- function(col_input) {
  col_output <- as.numeric(col_input)/100
  return(col_output)
}

# Convert UTM to decimal degree ##########
# assumed columns: core_id, easting, northing
UTM_to_DD <- function(df, zone){
  
  require(sf)
  
  # define UTM and latlong projections
  utm_prj <- paste0("+proj=utm +zone=", zone, " +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
  dd_prj <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  
  # isolate UTM coordinates
  # utm_coords <- data.frame(core_id = "Chelmsford", easting = 325205.77, northing = 5734662.8) # test
  utm_coords <- df %>% select(core_id, easting, northing)
  
  # create spatial object and assign coords utm coordinate system
  utm_coords <- st_as_sf(utm_coords, coords = c("easting", "northing"), crs = utm_prj) 
  # st_crs(utm_coords) # check projection
  
  dd_coords <- st_transform(utm_coords, crs = dd_prj) %>% 
    # extract lat lon from geometry
    extract(geometry, into = c('longitude', 'latitude'), '\\((.*),(.*)\\)', convert = T) %>%
    select(core_id, longitude, latitude)
  
  # join back to the dataframe
  df_decimal <- left_join(df, dd_coords)
  
  return(df_decimal)
}

## Convert UTM to lat/long ###############
convert_UTM_to_latlong <- function(easting, northing, zone, core_id) {
  
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

## Customized cr_cn function ##############

# Pulled from ropenscri/crossref
# https://github.com/ropensci/rcrossref

# Required functions from rcrossref for cr_cn (and therefore cr_ccrcn)
`cr_agency` <- function(dois = NULL, .progress="none", ...) {
  foo <- function(x, y, ...){
    cr_GET(
      endpoint = sprintf("works/%s/agency", x),
      args = list(),
      parse = TRUE, ...)
  }
  if (length(dois) > 1) {
    res <- llply(dois, foo, y = .progress, ...)
    res <- lapply(res, "[[", "message")
    names(res) <- dois
    res
  } else {
    foo(dois, ...)$message
  }
}

GET_agency_id <- function(x, ...) {
  if (is.null(x)) {
    stop("no doi for doi agency check provided", call. = FALSE)
  }
  cr_agency(x)$agency$id
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

rcrossref_ua <- function() {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
                paste0("crul/", utils::packageVersion("crul")),
                sprintf("rOpenSci(rcrossref/%s)", 
                        utils::packageVersion("rcrossref")),
                get_email())
  paste0(versions, collapse = " ")
}

get_email <- function() {
  email <- Sys.getenv("crossref_email")
  if (identical(email, "")) {
    NULL
  } else {
    paste0("(mailto:", val_email(email), ")")
  }
}

warn_status <- function(x) {
  if (x$status_code > 202) {
    mssg <- x$parse("UTF-8")
    if (!is.character(mssg)) {
      mssg <- if (x$status_code == 406) {
        "(406) - probably bad format type"
      } else {
        x$status_http()$message
      }
    } else {
      mssg <- paste(sprintf("(%s)", x$status_code), "-", mssg)
    }
    # warning(
    #   sprintf(
    #     "%s w/ %s",
    #     gsub("%2F", "/", crul::url_parse(x$url)$path),
    #     mssg
    #   ),
    #   call. = FALSE
    # )
  }
}

# Supported content types
# See http://www.crosscite.org/cn/
supported_cn_types <- list(
  crossref = c("rdf-xml", "turtle", "citeproc-json", "citeproc-json-ish",
               "text", "ris", "bibtex", "crossref-xml", "bibentry",
               "crossref-tdm"),
  datacite = c("rdf-xml", "turtle", "datacite-xml", "citeproc-json-ish", "text",
               "ris", "bibtex", "bibentry"),
  medra = c("rdf-xml", "turtle", "citeproc-json-ish", "ris", "bibtex",
            "bibentry", "onix-xml")
)

cr_ccrcn <- function (dois, format = "bibtex", style = "apa", locale = "en-US", 
          raw = FALSE, .progress = "none", url = NULL, ...) 
{

  format <- match.arg(format, c("rdf-xml", "turtle", "citeproc-json", 
                                "citeproc-json-ish", "text", "ris", "bibtex", "crossref-xml", 
                                "datacite-xml", "bibentry", "crossref-tdm", "onix-xml"))
  cn <- function(doi, ...) {
    agency_id <- suppressWarnings(GET_agency_id(doi))
    if (is.null(agency_id)) {
      warning(doi, " agency not found - proceeding with 'crossref' ...",
              "cr_ccrcn failed to parse BibTex citation from this DOI. Please manually generate BibTex citation for this DOI.",
              
              call. = FALSE)
      agency_id <- "crossref"
    }
    assert(url, "character")
    if (is.null(url)) 
      url <- "https://doi.org"
    args <- list()
    if (grepl("citation.crosscite.org", url)) {
      args <- cr_compact(list(doi = doi, lang = locale, 
                              style = style))
    }
    else {
      url <- file.path(url, doi)
    }
    if (!format %in% supported_cn_types[[agency_id]]) {
      stop(paste0("Format '", format, "' for '", doi, "' is not supported by the DOI registration agency: '", 
                  agency_id, "'.\nTry one of the following formats: ", 
                  paste0(supported_cn_types[[agency_id]], collapse = ", ")))
    }
    pick <- c(`rdf-xml` = "application/rdf+xml", turtle = "text/turtle", 
              `citeproc-json` = "transform/application/vnd.citationstyles.csl+json", 
              `citeproc-json-ish` = "application/vnd.citationstyles.csl+json", 
              text = "text/x-bibliography", ris = "application/x-research-info-systems", 
              bibtex = "application/x-bibtex", `crossref-xml` = "application/vnd.crossref.unixref+xml", 
              `datacite-xml` = "application/vnd.datacite.datacite+xml", 
              bibentry = "application/x-bibtex", `crossref-tdm` = "application/vnd.crossref.unixsd+xml", 
              `onix-xml` = "application/vnd.medra.onixdoi+xml")
    type <- pick[[format]]
    if (format == "citeproc-json") {
      cli <- crul::HttpClient$new(url = file.path("https://api.crossref.org/works", 
                                                  doi, type), headers = list(`User-Agent` = rcrossref_ua(), 
                                                                             `X-USER-AGENT` = rcrossref_ua()))
      response <- cli$get(...)
    }
    else {
      if (format == "text") {
        type <- paste(type, "; style = ", style, "; locale = ", 
                      locale, sep = "")
      }
      cli <- crul::HttpClient$new(url = url, opts = list(followlocation = 1), 
                                  headers = list(`User-Agent` = rcrossref_ua(), 
                                                 `X-USER-AGENT` = rcrossref_ua(), Accept = type))
      response <- cli$get(query = args, ...)
    }
    warn_status(response)
    if (response$status_code < 202) {
      select <- c(`rdf-xml` = "text/xml", turtle = "text/plain", 
                  `citeproc-json` = "application/json", `citeproc-json-ish` = "application/json", 
                  text = "text/plain", ris = "text/plain", bibtex = "text/plain", 
                  `crossref-xml` = "text/xml", `datacite-xml` = "text/xml", 
                  bibentry = "text/plain", `crossref-tdm` = "text/xml", 
                  `onix-xml` = "text/xml")
      parser <- select[[format]]
      if (raw) {
        response$parse("UTF-8")
      }
      else {
        out <- response$parse("UTF-8")
        if (format == "text") {
          out <- gsub("\n", "", out)
        }
        if (format == "bibentry") {
          out <- parse_bibtex(out)
        }
        if (parser == "application/json") {
          out <- jsonlite::fromJSON(out)
        }
        if (parser == "text/xml") {
          out <- xml2::read_xml(out)
        }
        out
      }
    }
  }
  if (length(dois) > 1) {
    # library(plyr, warn.conflicts = FALSE, quietly = TRUE)
    # library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
    lapply(dois, function(z, ...) {
      out <- try(cn(z, ...), silent = TRUE)
      if ("try-error" %in% class(out)) {
        warning(paste0("Failure in resolving '", z, "'. See error detail in results."),
                call. = FALSE)
        out <- list(doi = z, error = out[[1]])
      }
      return(out)
    }, .progress = .progress)
  }
  else {
    cn(dois, ...)
  }
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



