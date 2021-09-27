## CCRCN Data Library ####
## contact: Jaxine Wolfe, wolfejax@si.edu

## Create a bibtex file of all the data releases hooks since May 2020 

library(RefManageR)

dois <- c("10.25573/serc.13315472", # St Laurent et al 2020
          "10.25573/serc.13483332", # Ensign et al 2020
          "10.25573/serc.13187549", # Keshta et al 2020
          "10.25573/serc.12636404", # Whighham et al 2020 (biomass)
          "10.25573/serc.12640172", # Kauffman et al 2020
          "10.25573/serc.12252005", # Sanborn and Coxson 2020
          "10.25573/serc.12855323") # Mueller et al 2020 (biomass)

release_bibs <- GetBibEntryWithDOI(dois)

WriteBib(release_bibs, "docs/data_release_bibs_2020.bib")
