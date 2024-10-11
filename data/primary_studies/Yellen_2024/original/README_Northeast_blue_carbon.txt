README – NORTHEAST US BLUE CARBON SURFACE SAMPLES AND CORE DATA
CONTACT – Brian Yellen, byellen@umass.edu
April 14, 2023

We collected sediment cores and surface samples from 19 marshes spread across the Northeast US to characterize geospatial patterns of marsh soil properties within and across sites. All field samples were collected in 2020 and 2021 and surveyed to the NAVD88 vertical datum using real time kinematic (RTK) GPS. We used a public network of GNSS reference stations to correct RTK measurements in real time. At each site, we established 2-4 transects perpendicular to tidal creeks that were designed to capture gradients in elevation and distance from sediment sources. 

Surface samples were collected by rotating a ~20 cm section of 7.3 internal diameter aluminum pipe approximately 15 cm into the marsh platform taking care to minimize soil compaction. We dug the sampling tube out of the marsh with a long-bladed spade inserted next to the corer at an angle to extend below the short core barrel. After digging the corer out of the marsh platform, we partially extruded the core using a marked piston to leave exactly 10 cm of soil in the corer. Excess sediment was cut away with a serrated blade, and the 10 cm by 7.3 cm diameter sample (total volume = 418 cm3) was placed in a sample bag for later processing. Samples were dried and weighed to calculate the dry bulk density of each sample. The large size of these samples was designed to capture centimeter scale variability in soil properties and thereby obtain more accurate estimates of in situ bulk density. Sediment cores were collected at each of our 19 field sites with a 6 cm internal diameter gouge corer, which results in negligible vertical compaction and allows for visual observation in the field. 

Surface samples were dried at 100°C and weighed. Dry mass was divided by the uniform volume of 418 cm3 of our samples to determine dry bulk density. The entire sample was then homogenized in a food processor. An approximately 20g dry mass subsample was placed into a pre-weighed crucible to estimate soil organic matter percentage via loss on ignition following procedures in Dean (1974). Briefly, this process involved combusting the weighed dry sample at 550°C for four hours, with the change in mass equal to the organic fraction. To test whether our subsamples were representative of the entire 418 cm3 sample, we used a sample splitter to divide a sample in half. We combusted one half, and measured LOI on ten subsamples from the other half. The half sample LOI was 42.5%. The mean of the subsamples was 42.1%, with a standard deviation of 1.5%. From this, we determined that measurements from subsamples were representative. 

We used LOI to estimate total organic carbon (TOC), or the fraction of each sample by mass made up of carbon, in order to calculate carbon density in all collected samples. From a global compilation of paired LOI and TOC measurements, Ouyang and Lee (2020) found a close relationship between LOI and TOC in tidal marsh sediments (R2=0.99, p<0.001), which we used to convert LOI to TOC. TOC was then multiplied by BD to determine carbon density in grams of carbon per cubic centimeter (gC cm-3). 

One centimeter thickness subsamples were removed from 1 m sediment cores one core half every five centimeters to obtain LOI depth profiles. The other half of the sediment core was scanned with an automated core scanner for down core elemental abundances, and then sampled for bulk density at the same depths as LOI measurements using a 3 cm internal diameter, 15.5 cm3 volume serrated hole saw. 

The above-described data have been provided in separate csv files as described below: 

1. UMass-NRCS_blue_carbon_surface_samples.csv; 435 surface samples, 0-10 cm, with RTK surveyed locations, measured sediment properties, and derived percent carbon converted from LOI via the relationship in Ouyang and Lee (2020), OC = 0.52*LOI–1.17

2. UMass_BlueCarbonCores_LOI.csv; Sediment core depth series with LOI for 1 cm subsamples corresponding to the listed starting and ending depths.

3. UMass_BlueCarbonCores_BD.csv; Sediment core depth series with bulk density for 15.5 cm^3 subsamples corresponding to the listed starting and ending depths.

4. UMass_sediment_core_locations.csv; locations of sediment cores listed in LOI and BD depth series. Locations were obtained with RTK GPS, except where “GPS_method” notes that a handheld GPS was used. 

Column labels for UMass-NRCS_blue_carbon_surface_samples.csv are as follows
Samp_ID – sample name
Site – site name
Vol_cm3 – sample volume in cubic centimeters
dry_mass_g – dry mass of sample
loi – organic fraction from loss on ignition
BD_g_cm3 – dry bulk density in grams per cubic centimeter
Elev_mNAVD88	 - elevation as surveyed by RTK GPS relative to the NAVD88 datum in meters
Lat – latitude, decimal degrees
Lon – longitude, decimal degrees
perC – percent carbon derived from LOI via Ouyang and Lee (2020), perC = 0.52*LOI–1.17
CD_gC_cm3 = carbon density in grams of carbon per cubic centimeter, the product of perC and BD
Mineral_g_cm3 – the density of mineral sediment in each sample calculated as (1-loi)/BD

CITATIONS

Dean, W. E. Jr. (1974). Determination of carbonate and organic matter in calcareous sediments and sedimentary rocks by loss on ignition: Comparison with other methods. Journal of Sedimentary Research, 44, 242–248. https://doi.org/10.1306/74D729D2-2B21-11D7-8648000102C1865D

Ouyang, X., & Lee, S. Y. (2020). Improved estimates on global carbon stock and carbon pools in tidal wetlands. Nature Communications, 11, 317. https://doi.org/10.1038/s41467-019-14120-2
