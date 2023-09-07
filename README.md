<p> <img src="https://github.com/Smithsonian/CCRCN-Data-Library/blob/master/docs/images/CCRCN icon.png?raw=true" alt="user spectrum" width=100 height=100 style="float:right;"> </img></p>

# Coastal Carbon Network Data Library
_Hub for scripts that pull data from the internet, and derivative datasets hosted by CCN_


The Coastal Carbon Network (CCN) is a consortium of biogeochemists, ecologists, pedologists, and coastal land managers with the goal of **accelerating the pace of discovery in coastal wetland carbon science by providing our community with access to data, analysis tools, and synthesis opportunities.** We have accomplished this goal by growing iteratively with community feedback, facilitating the sharing of open data and analysis products, offering training in data management and analytics, and leading topical working groups aimed at quantitatively reducing uncertainty in coastal greenhouse gas emissions and storage.

This repository contains the workflows and products of the effort the CCN has undertaken to synthesize coastal carbon data from tidal wetland habitats around the world. The current version is CCN Data Library Version 1.0.0.

<img src="https://github.com/Smithsonian/CCRCN-Data-Library/blob/master/docs/images/spectrum_of_users.PNG?raw=true" alt="user spectrum" width=700 height=380>

The CCN serves a broad spectrum of users. As a result, we attempt to make our data products transparent, easy to use, and open-source.

<img src="https://github.com/Smithsonian/CCRCN-Data-Library/blob/master/docs/images/CCRCN_network_activities.PNG?raw=true" alt="user spectrum" width=500 height=380>

### Resources

Below are some additional resources related to CCN data archival:
- [About the CCN at the Smithsonian Environmental Research Center (SERC)](https://serc.si.edu/coastalcarbon)
- The [Coastal Carbon Atlas](https://ccrcn.shinyapps.io/CoastalCarbonAtlas/), an interactive map interface that enables you to visualize, query, and download data from the CCN Data Library.
- See our [Data Management Plan](https://serc.si.edu/coastalcarbon/data-management-plan) for more information on how we archive and version our Data Library 
- See our [Database Structure](https://smithsonian.github.io/CCN-Community-Resources/soil_carbon_guidance.html) for detailed information on how the CCN Data Library is organized

## How to cite the CCN Data Library

**Recommended Citation**

Coastal Carbon Network (2023). Coastal Carbon Library (Version 1.0.0). DOI.ORG/10.25573/serc.21565671. Accessed (YYYY-MM-DD).

We refer to users as anyone using either data we curate, or synthesis products we create. Data that is curated, but not created, by the Network, should not be attributed to the Network. Users should cite all dataset DOIs and credit the datasets’ original authors. All synthesis products created by the Network and associated collaborators will be listed under a Creative Commons With Attribution license. The Network should be acknowledged and cited appropriately if users utilize any of the data structures, tools, or scripts developed by Network and associated collaborators. We will develop additional tools to assist users in generating lists of citations, but users will be ultimately responsible for correctly citing all data used. See the [CCN Principles and Governance](https://serc.si.edu/coastalcarbon/principles-and-governance) for more information.

## Contribution

Please see the [Data Contribution](https://smithsonian.github.io/CCN-Community-Resources/contributing.html) page on our Community Resources Portal for more details on how to submit data to the CCN Data Library and Atlas.

## Data Structure

### Study level metadata
Study-level information is essential for formatting the Ecological Metadata Language, and is a great way for you to express your project’s history, context, and originality. Including this metadata will enable us to provide a more clear trail of breadcrumbs back to the original data source of yours.

### Materials and Methods
For each study please fill out key data regarding materials and methods that are important to the soil carbon stocks meta-analysis. Some users may want to include or exclude certain methodologies, or see your commentary on the methods. Let’s make it easy for them.

### Site level
Site information provides important context for your study. You should describe the site and how it fits into your broader study, provide geographic information (although this can be generated automatically from the cores as well), and add any relevant tags and notes regarding site vegetation and inundation. Vegetation and inundation can alternatively be incorporated into the core-level data, whatever makes the most sense for your study design.

### Core level
Note that positional data can be assigned at the core level, or at the site level. However, it is important that this is specified, _that site coordinates are not attributed as core coordinates_, and that the method of measurement and precision is noted. Vegetation and inundation can alternatively be incorporated into the site-level data, whatever makes the most sense to your study design. In the future this level of hierarchy will be complemented by a ‘subsite level’ as this level of hierarchy can handle any sublocation information such as vegetation plot, and instrument location/description.

### Soil Depth Series
This level of hierarchy contains the actual depth series information. At minimum a submission needs to specify minimum and maximum depth increments, dry bulk density, and either fraction organic matter or fraction carbon. Sample ID’s should be used in the case that there are multiple replicates of a measurements. There is plenty of room for recording raw data from various dating techniques as well as age depth models.


