# Coastal Carbon Research Coordination Network
# This is the global script for three-file version of the CCRCN data submission application
# Contact: Michael Lonneman, lonnemanM@si.edu
#          Dave Klinges, klingesD@si.edu

# Load necessary packages
library(shiny)
library(shinyjs)
library(shinyBS)
library(tidyverse)
library(rdrop2)

mandatory_fields <- c("names", "title", "study", "data_types")

mandatoryLabel <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

# study information variables and inital table creation
study_information_var <- c("title", "one_liner", "abstract", "start_date", "end_date", "doi_data")
study_information <- data.frame(matrix(ncol=length(study_information_var), nrow = 0))
colnames(study_information) <- study_information_var

# author variables and initial author table creation 
authors_var <- c("last_name", "given_name", "institution", "email", "address", "phone", "corresponding_author")
authors <- data.frame(matrix(ncol=length(authors_var), nrow = 0))
colnames(authors) <- authors_var

associated_publications_var <- c("title_pubs", "doi_pubs", "bibtex_pubs")
associated_publications <- data.frame(matrix(ncol = length(associated_publications_var), nrow = 0))
colnames(associated_publications) <- associated_publications_var

# function to stamp files with the time of submission
humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

coring_methods_var <- c("gouge auger", "hargas corer", "mcauley corer", "mcaffrey peat cutter", 
                    "none specified", "other shallow corer", "piston corer", "push core", 
                    "pvc and hammer", "russian corer", "vibracorer", "surface sample")