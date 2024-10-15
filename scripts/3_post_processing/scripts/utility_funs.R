## Utility functions for gapfilling carbon content and calculating carbon stock
## Jaxine Wolfe

## Standard Error ----

se <- function(x, na.rm=TRUE) {
  if (na.rm) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# the following predictive models were developed by James Holmquist

## Predict DBD from LOI ----

predict_dbd_from_loi <- function(fom, k1 = 0.098, k2 = 1.67) {
  return(1/((fom/k1) + ((1-fom)/k2)))
}
# plot(seq(0,1, by=0.01), predict_dbd_from_loi(seq(0,1, by=0.01)))

## Predict OM from C ----

# for mangrove
# this produces a weird curve in the data
predict_om_from_c_mangrove <- function(fraction_carbon, a=0.485667, b=-0.015966) {
  return((-b + sqrt((b^2) - 4*a*(0-fraction_carbon))) / (2*a))
}

# for marsh
predict_om_from_c_marsh <- function(fraction_carbon, b=0.427391, a=0.063454) {
  return((-b + sqrt((b^2) - 4*a*(0-fraction_carbon))) / (2*a))
}

# Gapfill and Calculate C Stock ----

# integrate seagrass? (Howard 2014)
# For seagrass soils with % lOI < 0.20 % OC = -0.21 + 0.40 (% lOI);
# For seagrass soils with % lOI > 0.20 % OC = -0.33 + 0.43 (% lOI)
# needs more verification^

gapfillCarbon <- function(df, dry_bulk_density, fraction_organic_matter, fraction_carbon){

  if("habitat" %in% names(df)){

    gf_ds <- df %>%

      # Case: LOI but no C
      # gapfill carbon values using relationship developed by Jim for the synthesis
      mutate(fraction_carbon = case_when(
        is.na(fraction_carbon) & habitat == "marsh" ~ 0.427 * fraction_organic_matter + 0.0635 * (fraction_organic_matter^2),
        # apply the following from Breithaupt et al.
        # some seagrass classified as carbonate...going to ignore
        is.na(fraction_carbon) & habitat == "mangrove" & Sedimentar == "Carbonate" ~ (0.002 * ((fraction_organic_matter*100)^2) + 0.326 * fraction_organic_matter * 100 + 1.8)/100,
        is.na(fraction_carbon) & habitat == "mangrove" & Sedimentar == "Terrigenous" ~ (0.004 * ((fraction_organic_matter*100)^2) + 0.217 * fraction_organic_matter * 100 + 0.3)/100,

        is.na(fraction_carbon) & habitat %in% c("mangrove", "swamp, scrub/shrub") ~ 0.486 * fraction_organic_matter - 0.016 * (fraction_organic_matter^2),
        # need to double check these two...turning fraction carbon negative, leave out for now
        is.na(fraction_carbon) & habitat == "seagrass" & fraction_organic_matter < 0.002 ~ -0.0021 + 0.40 * fraction_organic_matter,
        is.na(fraction_carbon) & habitat == "seagrass" & fraction_organic_matter > 0.002 ~ -0.0033 + 0.43 * fraction_organic_matter,
        T ~ fraction_carbon)) %>%

      # Case: no DBD but has C
      mutate(OM_flag = case_when(is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat %in% c("mangrove", "swamp, scrub/shrub") ~ "OM modeled from C",
                                 T ~ "no flag")) %>%
      mutate(fraction_organic_matter = case_when(
        is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat == "marsh" ~ predict_om_from_c_marsh(fraction_carbon),
        is.na(dry_bulk_density) & !is.na(fraction_carbon) & habitat %in% c("mangrove", "swamp, scrub/shrub") ~ predict_om_from_c_mangrove(fraction_carbon),
        T ~ fraction_organic_matter)) %>%

      # Case: calculate DBD using LOI (preexisting and gapfilled)
      mutate(dry_bulk_density = case_when(
        is.na(dry_bulk_density) & !is.na(fraction_organic_matter) ~ predict_dbd_from_loi(fraction_organic_matter),
        T ~ dry_bulk_density))

    return(gf_ds)

  } else {
    print("Data table must have a habitat column.")
  }
}

## Standardize depth intervals ----
# what standard depth intervals will give the highest resolution C stock?
# do we standardize the depth intervals before or after calculating cstock for each interval?

# horizons <- data.frame(horizon_min = c(0,100),
#                        horizon_max = c(100,200))
# top meter
# horizons <- data.frame(horizon_min = 0, horizon_max = 100)

# standardizeDepths <- function(df, target_interval){
#   # Note: this function was adapted from Atlas code (written by Michael Lonneman and/or Jim)
#
#   if(missing(target_interval)){
#
#     result <- df %>%
#       dplyr::group_by(study_id, site_id, core_id) %>%
#       dplyr::summarize(stock_MgHa = sum(stock_MgHa))
#
#   } else {
#
#   result <- df %>%
#     # mutate(across(where(cols %in% c("depth_min", "depth_max", "dry_bulk_density", "fraction_organic_matter", "fraction_carbon"))), as.numeric)
#     merge(target_intervals) %>%
#     # Keeps intervals between min and max horizon
#     # If an interval crosses a horizon, it remains
#     dplyr::filter(pmax(depth_min, horizon_min) < pmin(depth_max, horizon_max)) %>%
#     dplyr::arrange(study_id, site_id, core_id, depth_min, depth_max) %>%
#     # Calculate weights for each interval
#     dplyr::mutate(overlapping_depth = pmin((depth_max-depth_min),
#                                            (horizon_max-depth_min),
#                                            (depth_max-horizon_min), na.rm=T)) %>%
#     dplyr::group_by(study_id, site_id, core_id, horizon_min, horizon_max) %>%
#     dplyr::mutate(total_depth = sum(overlapping_depth),
#                   weight = overlapping_depth / total_depth) %>%
#     # Aggregate by horizon intervals
#     dplyr::summarise(dry_bulk_density = sum(dry_bulk_density * weight),
#                      fraction_organic_matter = sum(fraction_organic_matter * weight),
#                      fraction_carbon = sum(fraction_carbon * weight)
#                      # stock_gCm2 = sum(stock_gCm2 * weight)
#     ) %>%
#     ungroup()
#   }
#   return(result)
# }

