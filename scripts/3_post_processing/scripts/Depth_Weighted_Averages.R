# Load up data
library(data.table)
is_overlapping<-function(x1,x2,y1,y2) { return( max(x1,y1) < min(x2,y2)) } # determine whether two depth itevals are overlapping

methods2 <- read_csv("CCRCN_v2/methods.csv") # methods
cores2 <- read_csv("CCRCN_v2/cores.csv", guess_max=5603)
depthseries2 <- read_csv("CCRCN_v2/depthseries.csv", guess_max = 38878)
species2 <- read_csv("CCRCN_v2/species.csv")

habitat2 <- read_csv("cleaned_up_habitat_classifications.csv")
core_locations <- cores2 %>% select(study_id:core_id, latitude, longitude)

# Filter these out
filter_these_out <- cores2 %>% 
  filter(study_id == "Nahlik_and_Fennessy_2016" & salinity_class == "palustrine")

max(depthseries2$depth_max, na.rm=T)

{
  target_horizons <- data.frame(horizon_min = c(0,20,50, seq(100,850, by = 50)),
                                horizon_max = c(20,50,100,seq(150, 900, by = 50)))
  
  # Depth series integration
  depthseries_paired_down <- depthseries2 %>%
    select(study_id:fraction_carbon)
  
  depth_series_horizons <- merge(depthseries_paired_down, target_horizons) 
  
  depth_series_horizons$overlapping <- mapply(is_overlapping, x1 = depth_series_horizons$depth_min,
                                              x2 = depth_series_horizons$depth_max,
                                              y1 = depth_series_horizons$horizon_min,
                                              y2 = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    filter(overlapping == TRUE) %>% 
    arrange(study_id, site_id, core_id, depth_min, depth_max)
  
  overlapping_depth <- function(depth_min, depth_max, horizon_min, horizon_max) {
    return(min((depth_max-depth_min),
               (horizon_max-depth_min),
               (depth_max-horizon_min), na.rm=T))
  }
  
  depth_series_horizons$overlapping_depth <- mapply(overlapping_depth, depth_min = depth_series_horizons$depth_min,
                                                    depth_max = depth_series_horizons$depth_max,
                                                    horizon_min = depth_series_horizons$horizon_min,
                                                    horizon_max = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>% 
    mutate(total_depth = sum(overlapping_depth),
           weight = overlapping_depth/total_depth)
  
  depth_series_wa_horizons <- depth_series_horizons %>% 
    ungroup() %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    summarise(dry_bulk_density=sum(dry_bulk_density*weight),
              fraction_organic_matter=sum(fraction_organic_matter*weight),
              fraction_carbon = sum(fraction_carbon*weight))
  
  
  # Bind all the tables together
  
  fully_joined_datasets <- core_locations %>%
    full_join(habitat2) %>% 
    full_join(depth_series_wa_horizons) %>% 
    group_by(study_id, site_id, core_id) %>% 
    filter((!all(is.na(fraction_organic_matter))|(!all(is.na(fraction_carbon)))),
           !is.na(habitat),
           !core_id %in% filter_these_out$core_id)
  
  fully_joined_datasets_plot <- fully_joined_datasets %>% 
    mutate(organic_matter_density = fraction_organic_matter * dry_bulk_density,
           study_site_core = paste(study_id, site_id, core_id, sep="_"),
           horizon_med = horizon_min + ((horizon_max-horizon_min)/2),
           relative_latitude = abs(latitude))
  
  ggplot(data = fully_joined_datasets_plot, aes(x=horizon_med, y=organic_matter_density, color = relative_latitude)) +
    geom_point(alpha=0.2) +
    geom_line(alpha=0.2, aes(group=study_site_core)) + 
    scale_colour_gradientn(colors=rainbow(7)) +
    facet_wrap(.~habitat) +
    xlab("Depth (cm)") +
    ylab(expression(paste("Organic Matter Density (g cm"^"-3",")", sep=""))) +
    scale_x_reverse() +
    coord_flip() +
    labs(color = "absv(longitude)")
  
  ggsave("All habitat types depth and omd.pdf", width=8.5, height=6.25)
  
  ggplot(data = fully_joined_datasets_plot, aes(x=longitude, y=latitude)) +
    geom_point(aes(color=habitat, shape=habitat), alpha=0.4) +
    theme_dark()
  
  ggsave("CCN Core Locations 200828.pdf", width=8.5, height=4.25)
  
  
  write_csv(fully_joined_datasets, "soil-profiles-for-saintlan-full-20200827.csv")
  View(fully_joined_datasets %>% 
         filter(habitat == "mangrove") %>% 
         arrange(-latitude))
  
  
  # Get the bibliography subset
  bib <- read_csv("CCRCN/CCRCN_bibliography.csv")
  bib_subset <- bib %>% 
    filter(study_id %in% fully_joined_datasets$study_id) %>%
    select(-study_id, -X1) %>% 
    distinct() %>% 
    arrange(bibliography_id)
  write_csv(bib_subset, "CCN-citations-200828.csv") 
}

# Choose 
# Either confirmed complete profiles

# Or cores with a max depth of > 1 m
# Select dry bulk density, fraction organic matter, and fraction carbon
# Run code the get depth weighted averages

# Classify as mangrove marsh seagrass
# First by the vegetation code
# Then by running a partial string search in the core table
# Then by vegetation species cover
# Convert string to factor in species list, 6 mangrove, 5 swamp, 4 marsh, 3 seagrass, 2 algal mat, 1 unvegetated, NA
# Group by site and core then habitat is the maximum factorized habitat code
# Convert habitat back to string

# Do any extra cleanup
# Don't include Nhalik and Fennesy palustrine cores
# Most are non tidal

# Plot 
# 0-5cm

{
  target_horizons <- data.frame(horizon_min = c(0),
                                horizon_max = c(5))
  
  # Depth series integration
  depthseries_paired_down <- depthseries2 %>%
    select(study_id:fraction_carbon)
  
  depth_series_horizons <- merge(depthseries_paired_down, target_horizons) 
  
  depth_series_horizons$overlapping <- mapply(is_overlapping, x1 = depth_series_horizons$depth_min,
                                              x2 = depth_series_horizons$depth_max,
                                              y1 = depth_series_horizons$horizon_min,
                                              y2 = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    filter(overlapping == TRUE) %>% 
    arrange(study_id, site_id, core_id, depth_min, depth_max)
  
  overlapping_depth <- function(depth_min, depth_max, horizon_min, horizon_max) {
    return(min((depth_max-depth_min),
               (horizon_max-depth_min),
               (depth_max-horizon_min), na.rm=T))
  }
  
  depth_series_horizons$overlapping_depth <- mapply(overlapping_depth, depth_min = depth_series_horizons$depth_min,
                                                    depth_max = depth_series_horizons$depth_max,
                                                    horizon_min = depth_series_horizons$horizon_min,
                                                    horizon_max = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>% 
    mutate(total_depth = sum(overlapping_depth),
           weight = overlapping_depth/total_depth)
  
  depth_series_wa_horizons <- depth_series_horizons %>% 
    ungroup() %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    summarise(dry_bulk_density=sum(dry_bulk_density*weight),
              fraction_organic_matter=sum(fraction_organic_matter*weight),
              fraction_carbon = sum(fraction_carbon*weight))
  
  
  # Bind all the tables together
  
  fully_joined_datasets <- core_locations %>%
    full_join(habitat2) %>% 
    full_join(depth_series_wa_horizons) %>% 
    group_by(study_id, site_id, core_id) %>% 
    filter((!all(is.na(fraction_organic_matter))|(!all(is.na(fraction_carbon)))),
           !is.na(habitat),
           !core_id %in% filter_these_out$core_id)
  
  fully_joined_datasets_plot <- fully_joined_datasets %>% 
    mutate(organic_matter_density = fraction_organic_matter * dry_bulk_density,
           study_site_core = paste(study_id, site_id, core_id, sep="_"),
           horizon_med = horizon_min + ((horizon_max-horizon_min)/2),
           relative_latitude = abs(latitude))
  
  ggplot(data = fully_joined_datasets_plot, aes(x=horizon_med, y=organic_matter_density, color = relative_latitude)) +
    geom_point(alpha=0.2) +
    geom_line(alpha=0.2, aes(group=study_site_core)) + 
    scale_colour_gradientn(colors=rainbow(7)) +
    facet_wrap(.~habitat) +
    xlab("Depth (cm)") +
    ylab(expression(paste("Organic Matter Density (g cm"^"-3",")", sep=""))) +
    scale_x_reverse() +
    coord_flip() +
    labs(color = "absv(longitude)")
  
  write_csv(fully_joined_datasets, "soil-profiles-for-saintlan-0to5cm-20200827.csv")
  
}

# 0-10cm
{
  target_horizons <- data.frame(horizon_min = c(0),
                                horizon_max = c(10))
  
  # Depth series integration
  depthseries_paired_down <- depthseries2 %>%
    select(study_id:fraction_carbon)
  
  depth_series_horizons <- merge(depthseries_paired_down, target_horizons) 
  
  depth_series_horizons$overlapping <- mapply(is_overlapping, x1 = depth_series_horizons$depth_min,
                                              x2 = depth_series_horizons$depth_max,
                                              y1 = depth_series_horizons$horizon_min,
                                              y2 = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    filter(overlapping == TRUE) %>% 
    arrange(study_id, site_id, core_id, depth_min, depth_max)
  
  overlapping_depth <- function(depth_min, depth_max, horizon_min, horizon_max) {
    return(min((depth_max-depth_min),
               (horizon_max-depth_min),
               (depth_max-horizon_min), na.rm=T))
  }
  
  depth_series_horizons$overlapping_depth <- mapply(overlapping_depth, depth_min = depth_series_horizons$depth_min,
                                                    depth_max = depth_series_horizons$depth_max,
                                                    horizon_min = depth_series_horizons$horizon_min,
                                                    horizon_max = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>% 
    mutate(total_depth = sum(overlapping_depth),
           weight = overlapping_depth/total_depth)
  
  depth_series_wa_horizons <- depth_series_horizons %>% 
    ungroup() %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    summarise(dry_bulk_density=sum(dry_bulk_density*weight),
              fraction_organic_matter=sum(fraction_organic_matter*weight),
              fraction_carbon = sum(fraction_carbon*weight))
  
  
  # Bind all the tables together
  
  fully_joined_datasets <- core_locations %>%
    full_join(habitat2) %>% 
    full_join(depth_series_wa_horizons) %>% 
    group_by(study_id, site_id, core_id) %>% 
    filter((!all(is.na(fraction_organic_matter))|(!all(is.na(fraction_carbon)))),
           !is.na(habitat),
           !core_id %in% filter_these_out$core_id)
  
  fully_joined_datasets_plot <- fully_joined_datasets %>% 
    mutate(organic_matter_density = fraction_organic_matter * dry_bulk_density,
           study_site_core = paste(study_id, site_id, core_id, sep="_"),
           horizon_med = horizon_min + ((horizon_max-horizon_min)/2),
           relative_latitude = abs(latitude))
  
  ggplot(data = fully_joined_datasets_plot, aes(x=horizon_med, y=organic_matter_density, color = relative_latitude)) +
    geom_point(alpha=0.2) +
    geom_line(alpha=0.2, aes(group=study_site_core)) + 
    scale_colour_gradientn(colors=rainbow(7)) +
    facet_wrap(.~habitat) +
    xlab("Depth (cm)") +
    ylab(expression(paste("Organic Matter Density (g cm"^"-3",")", sep=""))) +
    scale_x_reverse() +
    coord_flip() +
    labs(color = "absv(longitude)")
  
  write_csv(fully_joined_datasets, "soil-profiles-for-saintlan-0to10cm-20200827.csv")
  
}

# 0-30cm
{
  target_horizons <- data.frame(horizon_min = c(0),
                                horizon_max = c(30))
  
  # Depth series integration
  depthseries_paired_down <- depthseries2 %>%
    select(study_id:fraction_carbon)
  
  depth_series_horizons <- merge(depthseries_paired_down, target_horizons) 
  
  depth_series_horizons$overlapping <- mapply(is_overlapping, x1 = depth_series_horizons$depth_min,
                                              x2 = depth_series_horizons$depth_max,
                                              y1 = depth_series_horizons$horizon_min,
                                              y2 = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    filter(overlapping == TRUE) %>% 
    arrange(study_id, site_id, core_id, depth_min, depth_max)
  
  overlapping_depth <- function(depth_min, depth_max, horizon_min, horizon_max) {
    return(min((depth_max-depth_min),
               (horizon_max-depth_min),
               (depth_max-horizon_min), na.rm=T))
  }
  
  depth_series_horizons$overlapping_depth <- mapply(overlapping_depth, depth_min = depth_series_horizons$depth_min,
                                                    depth_max = depth_series_horizons$depth_max,
                                                    horizon_min = depth_series_horizons$horizon_min,
                                                    horizon_max = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>% 
    mutate(total_depth = sum(overlapping_depth),
           weight = overlapping_depth/total_depth)
  
  depth_series_wa_horizons <- depth_series_horizons %>% 
    ungroup() %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    summarise(dry_bulk_density=sum(dry_bulk_density*weight),
              fraction_organic_matter=sum(fraction_organic_matter*weight),
              fraction_carbon = sum(fraction_carbon*weight))
  
  
  # Bind all the tables together
  
  fully_joined_datasets <- core_locations %>%
    full_join(habitat2) %>% 
    full_join(depth_series_wa_horizons) %>% 
    group_by(study_id, site_id, core_id) %>% 
    filter((!all(is.na(fraction_organic_matter))|(!all(is.na(fraction_carbon)))),
           !is.na(habitat),
           !core_id %in% filter_these_out$core_id)
  
  fully_joined_datasets_plot <- fully_joined_datasets %>% 
    mutate(organic_matter_density = fraction_organic_matter * dry_bulk_density,
           study_site_core = paste(study_id, site_id, core_id, sep="_"),
           horizon_med = horizon_min + ((horizon_max-horizon_min)/2),
           relative_latitude = abs(latitude))
  
  ggplot(data = fully_joined_datasets_plot, aes(x=horizon_med, y=organic_matter_density, color = relative_latitude)) +
    geom_point(alpha=0.2) +
    geom_line(alpha=0.2, aes(group=study_site_core)) + 
    scale_colour_gradientn(colors=rainbow(7)) +
    facet_wrap(.~habitat) +
    xlab("Depth (cm)") +
    ylab(expression(paste("Organic Matter Density (g cm"^"-3",")", sep=""))) +
    scale_x_reverse() +
    coord_flip() +
    labs(color = "absv(longitude)")
  
  write_csv(fully_joined_datasets, "soil-profiles-for-saintlan-0to30cm-20200827.csv")
  
}

# 0-100
{
  target_horizons <- data.frame(horizon_min = c(0),
                                horizon_max = c(100))
  
  # Depth series integration
  depthseries_paired_down <- depthseries2 %>%
    select(study_id:fraction_carbon)
  
  depth_series_horizons <- merge(depthseries_paired_down, target_horizons) 
  
  depth_series_horizons$overlapping <- mapply(is_overlapping, x1 = depth_series_horizons$depth_min,
                                              x2 = depth_series_horizons$depth_max,
                                              y1 = depth_series_horizons$horizon_min,
                                              y2 = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    filter(overlapping == TRUE) %>% 
    arrange(study_id, site_id, core_id, depth_min, depth_max)
  
  overlapping_depth <- function(depth_min, depth_max, horizon_min, horizon_max) {
    return(min((depth_max-depth_min),
               (horizon_max-depth_min),
               (depth_max-horizon_min), na.rm=T))
  }
  
  depth_series_horizons$overlapping_depth <- mapply(overlapping_depth, depth_min = depth_series_horizons$depth_min,
                                                    depth_max = depth_series_horizons$depth_max,
                                                    horizon_min = depth_series_horizons$horizon_min,
                                                    horizon_max = depth_series_horizons$horizon_max)
  
  depth_series_horizons <- depth_series_horizons %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>% 
    mutate(total_depth = sum(overlapping_depth),
           weight = overlapping_depth/total_depth)
  
  depth_series_wa_horizons <- depth_series_horizons %>% 
    ungroup() %>% 
    group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
    summarise(dry_bulk_density=sum(dry_bulk_density*weight),
              fraction_organic_matter=sum(fraction_organic_matter*weight),
              fraction_carbon = sum(fraction_carbon*weight))
  
  
  # Bind all the tables together
  
  fully_joined_datasets <- core_locations %>%
    full_join(habitat2) %>% 
    full_join(depth_series_wa_horizons) %>% 
    group_by(study_id, site_id, core_id) %>% 
    filter((!all(is.na(fraction_organic_matter))|(!all(is.na(fraction_carbon)))),
           !is.na(habitat),
           !core_id %in% filter_these_out$core_id)
  
  fully_joined_datasets_plot <- fully_joined_datasets %>% 
    mutate(organic_matter_density = fraction_organic_matter * dry_bulk_density,
           study_site_core = paste(study_id, site_id, core_id, sep="_"),
           horizon_med = horizon_min + ((horizon_max-horizon_min)/2),
           relative_latitude = abs(latitude))
  
  ggplot(data = fully_joined_datasets_plot, aes(x=horizon_med, y=organic_matter_density, color = relative_latitude)) +
    geom_point(alpha=0.2) +
    geom_line(alpha=0.2, aes(group=study_site_core)) + 
    scale_colour_gradientn(colors=rainbow(7)) +
    facet_wrap(.~habitat) +
    xlab("Depth (cm)") +
    ylab(expression(paste("Organic Matter Density (g cm"^"-3",")", sep=""))) +
    scale_x_reverse() +
    coord_flip() +
    labs(color = "absv(longitude)")
  
  write_csv(fully_joined_datasets, "soil-profiles-for-saintlan-0to100cm-20200827.csv")
  
}


