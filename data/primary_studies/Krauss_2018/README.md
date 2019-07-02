# Krauss et al. 2018 Hook Notes and Progress

Citation: 
Krauss, K.W., Noe, G.B., Duberstein, J.A., Conner, W.H., Stagg, C.L., Cormier, N., Jones, M.C., Bernhardt, C.J., Lockaby, B.G., From, A.S., Doyle, T.W., Day, R.H., Ensign, S.H., Pierfelice, K.N., Hupp, C.R., Chow, A.T., and Whitbeck, J.L., 2018. The role of the upper tidal estuary in wetland blue carbon storage and flux: Global Biogeochemical Cycles, v. 32, no. 5, p. 817-839. https://doi.org/10.1029/2018GB005897.

Krauss 2018 data release:
10.5066/F7TM7930

**Important note below on conflict between c14 dates. See "Depthseries data"**

## Progress 

Object | Status | Notes
------------- | ------------- | -------------
depthseries table | done, but could compare more against Jones 2017 |
core-level table | needs review | 
site-level table | needs review| 
Material and Methods Metadata | needs review | 
species table | not sure if we want? | 
impact table | in progress | 
biomass table | not sure if we want? |
Study Information table | not started | 
Keywords table | not started | 
Author table | not started |
funding sources table | not started |
define study-specific attributes | not started |
define study-specific values or codes | not started |
user defined attributes | not started | 
user define variables | not started | 

## Notes and Assumptions


#### Depthseries data

Assumption | Justification | Notes
------------- | ------------- | -------------
Soil core samples are 1 cm long | For radiocarbon work, samples were 1 cm long | 
"Depth (cm) in TFFW_soil_core_data.csv is bottom of core sample | Metadata states "Soil depth of core sample in centimeters" |
Renamed "Compression (%) to compaction_fraction | DK is not too certain of difference between compaction and compression, if any...metadata described this attribute as "measure of soil compression resulting from collecting soil core". Converted % to fraction |
Using Krauss c14 dates and not Jones c14 dates | c14 ages from Krauss and Jones appear to be different for the Jones cores reported | ...this is determined from about 10 minutes of inspection but possibly could use more comparison of raw data (which is difficult because raw Jones data corruped). Because of the sets of concerns we have with the Neotoma Jones data release, I would trust the Krauss dates more; plus, the Jones data only includes some of the cores. However, the Jones data release has modeled ages with min and max, so information is lost by using Krauss dataset. Comparable resolution across the depthseries from what I've checked, however.

#### Core data

Assumption | Justification | Notes
------------- | ------------- | -------------
NA | NA | | Don't have coordinates for core W2

#### Site Data

Assumption | Justification | Notes
------------- | ------------- | -------------
core "W2" is from the Waccamaw river site | Because of the "W"...that's the best I've got | 
NA | NA | Added new vegetation_class variable "forested to emergent", which has also been added to docs/controlled_variables.csv and the Database Structure page on the website

#### Methods and Materials

Assumption | Justification | Notes
------------- | ------------- | -------------


## Uncontrolled Attributes/Variables and Potential Guidance Updates






## Personall Communications from Miriam Jones and Ken Krauss


**2019-04-19**

Waccamaw Transect			
Core name	Environment	latitude	longitude	coring device
11-11-2-1	Oligohaline Marsh, Turkey Creek	33.35003	-79.3447	vibracore
11-11-2-3	Turkey Creek heavily salt impacted TFFW	33.34001	-79.34166	vibracore
11-11-3-1	Butler Island, moderately salt-impacted	33.422823	-79.207996	vibracore
11-11-1-2	Richmond Island, fresh TFFW	33.55564	-79.08943	vibracore
				
Savannah transect			
Core name	Environment	latitude	longitude	Coring device
12-12-10-3	Oligohaline Marsh	32.17	-81.14	Russian-style
12-12-10-2	Heavily salt-impacted TFFW	32.18	-81.14	Russian-style
12-12-9-3	Moderately salt-impacted TFFW	32.24	-81.15	Russian-style
12-12-11-1	Upper Fresh TFFW	32.238	-81.155	Russian-style

**2019-04-19**

Here is how the core names relate to the environment:
Waccamaw R transect:
 
11-11-3-1: Middle freshwater tidal
11-11-2-1: Oligohaline marsh
11-11-2-3: Lower salt impacted (stressed)
11-11-1-2: Upper freshwater tidal
 
Savannah R transect:
12-12-10-2: Lower salt impacted (stressed)
12-12-9-3: Middle freshwater tidal
12-12-10-3: Oligohaline marsh
12-12-11-1: Upper freshwater tidal


2019-06-17 JH Review: Study citations table only has paper in it. Needs the data release.  

Under cores. I don't think high, low, mid in this study refer to elevations. I think they refer to relative positions on the river. Double check before removing inundation class.  

Under depth series. Compaction % needs to be converted to compaction fraction.  

Also, needs species data.  

2019-07-02 DK Edits from JH Manager review:  

- changed compaction % to compaction fraction  

- added species data (not all core locations had species information)  

- added impact data  

- looking at paper and map in figure 2, low, middle, and high corresponds to positions on the river and vegetation zones. Looking at guidance on inundation_class, definitions of low, medium and high are "Study-specific definition of an elevation relatively [low, mid, high] in the tidal frame, typically defined by vegetation type". This appears to match the current designations of inundation_class. Assuming that higher on a river is also higher in elevation (because water flows downhill), low, medium, high on a river would also be low, medium, and high elevations. No changes made, but if inappropriate use of inundation_class it can be removed.

