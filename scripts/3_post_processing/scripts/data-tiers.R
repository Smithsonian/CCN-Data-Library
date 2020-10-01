# James Holmquist Pseudo Coded this on 2020-09-30
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

# Get Unique Study ID's
# Get Unique Site ID's
# Get Unique Core ID's

# Iterate through study/site/core ID's

# Assigning Tiers

# One core at a time ...

# Select the core in all the relevant files

# Remove columns from all tables that are all NA

# Soil Stocks

# If there is dry bulk density and either fraction OM or fraction C

# Then it is at least - level C

# If the core_length_flag is there
# and says the core represents the full deposit

# Then it is C1

# Else it is C2

# Add C1, C2, or NA to a C column


# See if it is a dated core

# If it has one of these data types, then it is at least B

# cs137_peak_present, cs137_peak_age, cs137_activity, cs137_activity_se, 
# excess_pb210_activity, excess_pb210_activity_se,
# total_pb210_activity, total_pb210_activity_se
# ra226_activity, ra226_activity_se,
# pb214_activity, pb214_activity_se,
# bi214_activity, bi214_activity_se,
# c14_age, c14_age_se, c14_material,
# marker_date, marker_date_se

# See if the data is fully complete

# First, any derrived attributes should have their original data traced back

# If cs137_peak_present OR cs137_peak_age is present, then cs137_activity also
# has to be present for it to be a level B-1

# If total_pb210_activity is present, then excess_pb210_activity has to be there 
# as well as at least one of ra226_activity, pb214_activity, and bi214_activity

# Marker dates and 14C reporting is a lot more straightforward and standardized 
# in the case of 14C so I won't put any special conditions on that.

# For any radioisotopes included, their respective uncertainties need to be there too.
# if cs137_activity then cs137_activity_se
# if excess_pb210_activity then excess_pb210_activity_se
# if total_pb210_activity then total_pb210_activity_se
# if ra226_activity then ra226_activity_se
# if pb214_activity then pb214_activity_se
# if bi214_activity then bi214_activity_se
# if c14_age then c14_age_se
# not really worried about marker horizons uncertainty

# if none of these landmines get tripped it's a B1
# Else, it's a B2

# Add B1, B2, or NA to a B column


# For top shelf stuff. 

# If B1 or B2, 
# Look at core table, if elevation is not NA, then it's an A.
# If DEM source is not RTK-GPS  then it's an A3
# If it's B2 AND DEM source is RTK or better then it's an A2
# If it's B1 AND DEM source is RTK or better, then it's an A1. 

# Add A1, A2, A3, or NA to an A column
