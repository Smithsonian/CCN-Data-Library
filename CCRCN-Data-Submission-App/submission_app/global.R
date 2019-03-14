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

coring_methods_var <- c("none specified", "gouge auger", "hargas corer", "mcauley corer", "mcaffrey peat cutter", 
                     "other shallow corer", "piston corer", "push core", 
                    "pvc and hammer", "russian corer", "vibracorer", "surface sample")

dbd_density_var <- c("not specified", "air dried to constant mass", "modeled", "freeze dried", "removed non structural water", 
                     "time approximate", "to constant mass")

compaction_flag_var <- c("not specified", "compaction qualified", "compaction quantified", 
                         "corer limits compaction", "no obvious compaction")

fraction_carbon_method_var <- c("not specified", "Craft regression", "EA", "Fourqurean regression", "Holmquist regression", "kjeldahl digestion", 
                                "local regression", "wet oxidation")

methods_var <- c("coring_method", "roots_flag", "sediment_sieved_flag", "compaction_flag", 
                    "dry_bulk_density_temperature", "dry_bulk_density_time", "dry_bulk_density_sample_volume", "dry_bulk_density_sample_mass", "dry_bulk_density_flag",
                    "loss_on_ignition_temperature", "loss_on_ignition_time", "loss_on_ignition_flag", "loss_on_ignition_sample_volume", "loss_on_ignition_sample_mass", 
                    "carbonates_removed", "carbonate_removal_method", 
                    "carbon_measured_or_modeled", "fraction_carbon_method", "fraction_carbon_type",
                    "carbon_profile_notes", 
                    "cs137_counting_method", 
                    "pb210_counting_method", "excess_pb210_rate", "excess_pb210_model", 
                    "ra226_assumption", 
                    "c14_counting_method", 
                    "dating_notes", "age_depth_model_reference", "age_depth_model_notes")

methods_metadata <- data.frame(matrix(ncol=length(methods_var), nrow = 1))
colnames(methods_metadata) <- methods_var

datatypes <- c("dbd", "fraction_carbon", "loi", "fraction_organic_matter", 
                "cs137", "pb210", "ra226", "c14", "be7", "am241")
datatype_tracker <- data.frame(matrix(data = FALSE, ncol=length(datatypes), nrow = 1))
colnames(datatype_tracker) <- datatypes

metadata <- read.csv("./data/CCRCN_methods_data.csv")
