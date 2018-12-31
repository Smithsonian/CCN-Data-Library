# CCRCN-Data-Library
_Hub for scripts that pull data from the internet, and derivative datasets hosted by CCRCN_


See our [Data Management, Structure, and Products page](https://serc.si.edu/coastalcarbon/data) for more information.

The Coastal Carbon Research Coordination Network (CCRCN) is a consortium of biogeochemists, ecologists, pedologists, and coastal land managers with the goal of **accelerating the pace of discovery in coastal wetland carbon science by providing our community with access to data, analysis tools, and synthesis opportunities.** We have accomplished this goal by growing iteratively with community feedback, facilitating the sharing of open data and analysis products, offering training in data management and analytics, and leading topical working groups aimed at quantitatively reducing uncertainty in coastal greenhouse gas emissions and storage.

<img src="https://github.com/Smithsonian/CCRCN-Data-Library/blob/master/docs/images/spectrum_of_users.PNG?raw=true" alt="user spectrum" width=700 height=380>

The CCRCN serves a broad spectrum of users. As a result, we attempt to make our data products transparent, easy to use, and open-source.

This repository contains the data library and curation scripts used to synthesize diverse data sources.

<img src="https://github.com/Smithsonian/CCRCN-Data-Library/blob/master/docs/images/CCRCN_network_activities.PNG?raw=true" alt="user spectrum" width=500 height=380>


## Study level metadata
Study-level information is essential for formatting the Ecological Metadata Language, and is a great way for you to express your project’s history, context, and originality. Including this metadata will enable us to provide a more clear trail of breadcrumbs back to the original data source of yours.

## Materials and Methods
For each study please fill out key data regarding materials and methods that are important to the soil carbon stocks meta-analysis. Some users may want to include or exclude certain methodologies, or see your commentary on the methods. Let’s make it easy for them.

## Site level
Site information provides important context for your study. You should describe the site and how it fits into your broader study, provide geographic information (although this can be generated automatically from the cores as well), and add any relevant tags and notes regarding site vegetation and inundation. Vegetation and inundation can alternatively be incorporated into the core-level data, whatever makes the most sense for your study design.

## Core level
Note that positional data can be assigned at the core level, or at the site level. However, it is important that this is specified, _that site coordinates are not attributed as core coordinates_, and that the method of measurement and precision is noted. Vegetation and inundation can alternatively be incorporated into the site-level data, whatever makes the most sense to your study design. In the future this level of hierarchy will be complemented by a ‘subsite level’ as this level of hierarchy can handle any sublocation information such as vegetation plot, and instrument location/description.

## Soil Depth Series
This level of hierarchy contains the actual depth series information. At minimum a submission needs to specify minimum and maximum depth increments, dry bulk density, and either fraction organic matter or fraction carbon. Sample ID’s should be used in the case that there are multiple replicates of a measurements. There is plenty of room for recording raw data from various dating techniques as well as age depth models.
