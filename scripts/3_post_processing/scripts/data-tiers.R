# James Holmquist Pseudo Coded this on 2020-09-30
# Code written on 2020-10-01 and 02.

# The code iterates through the CCN database, which has already been converted to V2 guidance
# It looks at the data available for each core and assigns data quality and completedness tiers
# For stocks (C), date (B), and elevation (A) info.

# C2 - Carbon stock data complete, not confirmed to be a complete profile
# C1 - Carbon stock data complete, confirmed to be a complete profile

# B2 - Dating information present, but not complete
# B1 - Dating information present and complete

# A3 - Elevation data present but of low quality, dating info present.
# A2 - Elevation data is high quality, but dating info present but incomplete.
# A1 - Elevation data is high quality, and dating info is complete.

# Import data files
# materials and methods
# cores
# depth profiles
methods <- read_csv("data/CCRCN_V2/methods.csv")
cores <- read_csv("data/CCRCN_V2/cores.csv", guess_max=6206)
depthseries <- read_csv("data/CCRCN_V2/depthseries.csv", guess_max = 42698)

# Get Unique Study ID's
# Get Unique Site ID's
# Get Unique Core ID's
study_site_core <- cores %>% 
  select(study_id, site_id, core_id) %>% 
  mutate(stocks_qual_code = NA_character_,
         dates_qual_code = NA_character_,
         elevation_qual_code = NA_character_)

# Iterate through study/site/core ID's
for (i in 1:nrow(study_site_core)) {
  # Assigning Tiers
  # One core at a time ...
  
  # Select the core in all the relevant files
  temp_study_id <- study_site_core$study_id[i]
  # temp_site_id <- study_site_core$site_id[i]
  temp_core_id <- study_site_core$core_id[i]
  
  # temp_methods <- methods %>% filter(study_id == temp_study_id & site_id == temp_site_id & core_id == temp_core_id)
  temp_core <- cores %>% filter(study_id == temp_study_id & core_id == temp_core_id)
  temp_depthseries <- depthseries %>% filter(study_id == temp_study_id & core_id == temp_core_id)
  
  # Remove columns from all tables that are all NA
  # temp_methods <- temp_methods[, colSums(is.na(temp_methods)) != nrow(temp_methods)]
  temp_core <- temp_core[, colSums(is.na(temp_core)) != nrow(temp_core)]
  temp_depthseries <- temp_depthseries[, colSums(is.na(temp_depthseries)) != nrow(temp_depthseries)]
  
  # Soil Stocks
  # If there is dry bulk density and either fraction OM or fraction C
  if ("dry_bulk_density" %in% names(temp_depthseries) & 
      # Then it is at least - level C
      any(c("fraction_organic_matter", "fraction_carbon") %in% names(temp_depthseries))) {
    # If the core_length_flag is there
    if ("core_length_flag" %in% names(temp_core)) { 
      # and says the core represents the full deposit
      if (temp_core$core_length_flag[1] == "core depth represents deposit depth") {
        # Then it is C1
        stocks_code <- "C1"
        # Else it is C2
      } else {
        stocks_code <- "C2"
      }
    } else {
      stocks_code <- "C2"
    }
  } else {
    stocks_code <- NA
  }
  # Add C1, C2, or NA to a C column
  study_site_core[i, "stocks_qual_code"] <- stocks_code
  
  date_codes <- c()
  
  # See if it is a dated core
  # If it has one of these data types, then it is at least B
  if (any(c("cs137_peak_present",
          "cs137_peak_age",
          "cs137_activity", "cs137_activity_se",
          "excess_pb210_activity", "excess_pb210_activity_se",
          "total_pb210_activity", "total_pb210_activity_se",
          "ra226_activity", "ra226_activity_se",
          "pb214_activity", "pb214_activity_se",
          "bi214_activity", "bi214_activity_se",
          "c14_age", "c14_age_se", "c14_material",
          "marker_date", "marker_date_se") %in% names(temp_depthseries))) {
    
    # See if the data is fully complete
    # First, any derrived attributes should have their original data traced back
    
    # has to be present for it to be a level B-1
    
    if (any(c("cs137_peak_present", "cs137_peak_age") %in% names(temp_depthseries))) {
      # If cs137_peak_present OR cs137_peak_age is present, then cs137_activity also
      if (all(c("cs137_activity", "cs137_activity_se") %in% names(temp_depthseries))) {
        date_codes <- c(date_codes, "B1") 
      } else {
        date_codes <- c(date_codes, "B2") 
      }
    } else {
      date_codes <- c(date_codes, NA) 
    }
    
    # If total_pb210_activity is present, then excess_pb210_activity has to be there
    # as well as at least one of ra226_activity, pb214_activity, and bi214_activity
    if ("total_pb210_activity" %in% names(temp_depthseries) | "excess_pb210_activity" %in%  names(temp_depthseries)) {
      if (any(c("ra226_activity", "pb214_activity", "bi214_activity") %in% names(temp_depthseries))) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    } else {
      date_codes <- c(date_codes, NA)
    }
    
    # Marker dates and 14C reporting is a lot more straightforward and standardized 
    # in the case of 14C so I won't put any special conditions on that.
    
    # For any radioisotopes included, their respective uncertainties need to be there too.
    # if excess_pb210_activity then excess_pb210_activity_se
    
    if ("cs137_activity" %in% names(temp_depthseries)) {
      if ("cs137_activity_se" %in% names(temp_depthseries)) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    } 
    
    # Excess 210 Pb activity is OK to not have error bars I guess.
    #if ("excess_pb210_activity" %in% names(temp_depthseries)) {
    #  if ("excess_pb210_activity_se" %in% names(temp_depthseries)) {
    #    date_codes <- c(date_codes, "B1")
    #  } else {
    #    date_codes <- c(date_codes, "B2")
    #  }
    #} 
    
    if ("total_pb210_activity" %in% names(temp_depthseries)) {
      # if total_pb210_activity then total_pb210_activity_se
      if ("total_pb210_activity_se" %in% names(temp_depthseries)) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    }
    
    if ("ra226_activity" %in% names(temp_depthseries)) {
      # if ra226_activity then ra226_activity_se
      if ("ra226_activity_se" %in% names(temp_depthseries)) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    }
    
    if ("pb214_activity" %in% names(temp_depthseries)) {
      # if pb214_activity then pb214_activity_se
      if ("pb214_activity_se" %in% names(temp_depthseries)) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    }
    
    if ("bi214_activity" %in% names(temp_depthseries)) {
      # if bi214_activity then bi214_activity_se
      if ("bi214_activity_se" %in% names(temp_depthseries)) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    }
    
    if ("c14_age" %in% names(temp_depthseries)) {
      # if c14_age then c14_age_se
      if ("c14_age_se" %in% names(temp_depthseries)) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    } 
    
    # not really worried about marker horizons uncertainty
    # more worried about whether or not there is enough additional info
    if ("marker_date" %in% names(temp_depthseries)) {
      if (any(c("marker_type", "marker_notes") %in% names(temp_depthseries))) {
        date_codes <- c(date_codes, "B1")
      } else {
        date_codes <- c(date_codes, "B2")
      }
    }
    
    date_codes <- date_codes[!is.na(date_codes)]
    
    # if none of these landmines get tripped it's a B1
    # Add B1, B2, or NA to a B column
    if (all(date_codes == "B1")) {
      study_site_core[i, "dates_qual_code"] <- "B1"
    } else if (any(date_codes == "B2")) {
      # Else, it's a B2
      study_site_core[i, "dates_qual_code"] <- "B2"
    } else {
      study_site_core[i, "dates_qual_code"] <- NA
    }
  } else {
    study_site_core[i, "dates_qual_code"] <- NA
  }
  
  # For top shelf stuff. 
  
  # If C1 or C2 and B1 or B2,
  if (! is.na(study_site_core$dates_qual_code[i]) & ! is.na(study_site_core$stocks_qual_code[i])) {
    # Look at core table, if elevation is not NA, then it's an A.
    if ("elevation" %in% names(temp_core)) {
      if ("elevation_method" %in% names(temp_core)){
        if (grepl("RTK", temp_core$elevation_method[1]) | 
            temp_core$elevation_method[1] == "other high resolution") {
              # If it's B2 AND DEM source is RTK or better then it's an A2
              if (study_site_core$dates_qual_code[i] == "B2") {
                elv_code <- "A2"
              } else if (study_site_core$dates_qual_code[i] == "B1") {
                # If it's B1 AND DEM source is RTK or better, then it's an A1.
                elv_code <- "A1"
              } else {
                elv_code <- NA
              }
               
            } else {
              # If DEM source is not RTK-GPS  then it's an A3
              elv_code <- "A3"
            }
      } else {
        elv_code <- "A3"
      }
    } else {
      elv_code <- NA
    }
    
  } else {
    elv_code <- NA
  }
  
  # Add A1, A2, A3, or NA to an A column 
  study_site_core$elevation_qual_code[i] <- elv_code

}

# !!! write to a directory
# Join to core table, then write over input 
# write_csv(study_site_core, "data/CCRCN_V2/")

