## CCRCN Data Library
# contact: klingesd@si.edu

# This script writes a BibTeX file for each data source in
#   the CCRCN library


## Workspace prep ########
library(RefManageR)


# Drake et al 2015
cr_cn("10.1007/s00267-015-0568-z")

# Radabaugh et al 2017
cr_cn("10.1007/s12237-017-0362-7")

cr_cn("")

@article{crooks2014coastal,
  title={Coastal blue carbon opportunity assessment for the Snohomish Estuary: The climate benefits of estuary restoration},
  author={Crooks, S and Rybczyk, J and O’Connell, Keeley and Devier, DL and Poppe, K and Emmett-Mattox, S},
  journal={Report by Environmental Science Associates, Western Washington University, EarthCorps, and Restore America’s Estuaries},
  year={2014}
}

bib <- ReadCrossRef(query = '10.1080/01621459.2012.699793')



## Write BibTeX file
WriteBib(bib, "./docs/biblio.bib", biblatex = TRUE)


