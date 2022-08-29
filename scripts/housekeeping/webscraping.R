## CCRCN Data Library ####
## Jaxine Wolfe, wolfejax@si.edu

## Webscraping workflow for identifying data that is not in the Library

## EDI ####

# Searching and accessing data from Environmental Data Initiative

library(EDIutils)

res <- search_data_packages('q="soil+carbon"&fl=packageid,title,author,abstract&')
res2 <- search_data_packages('q="carbon+accretion"&fl=packageid,title,abstract,author&')
# ore+accretion+bulk+density+carbon+wetland

res3 <- search_data_packages('q=subject:("soil+carbon")+OR+subject:accretion+OR+subject:biomass&fl=packageid,title,abstract&')
res3_refined <- res3 %>% filter(grepl("mangrove", abstract))
         
res4 <- search_data_packages('q=subject:("soil+core")+subject:("marsh")+subject("pb+210")+subject:("carbon")&fl=packageid,title,abstract&')
res4_refined <- res4 %>% filter(grepl("tidal|marsh", abstract))


## USGS uses 