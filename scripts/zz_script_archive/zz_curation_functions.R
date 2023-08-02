## CCN Data Library

# This script contains an archive of obsolete or superceded curation functions

## Convert percent to fraction ###############
convert_percent_to_fraction <- function(col_input) {
  col_output <- as.numeric(col_input)/100
  return(col_output)
}

## Convert mean depth to min and max depth #####
# For conversion of depth interval values: if a dataframe
#   has a mean depth attribute, this will create a min depth
#   and max depth attributes

# NOTE: this function assumes constant sampling interval (as in, each slice of
#   core is of equal length)
convert_mean_depth_to_min_max <- function(dataframe, df_mean.depth) {
  # Set l as th length of the dataframe
  l <- length(df_mean.depth)
  df_mean.depth <- as.numeric(df_mean.depth)
  # For each row....
  for (i in 1:l) {
    min <- (df_mean.depth[i - 1] + df_mean.depth[i])/2 # Take mean of previous cell and current cell
    max <- (df_mean.depth[i + 1] + df_mean.depth[i])/2 # Take mean of current cell and next cell
    # If initial iteration...
    if (i == 1) {
      depth_min = as.vector(min) # create depth_min vector
      depth_max = as.vector(max) # create depth_max vector
    } else { # If not initial iteration, add min/max
      #  value to depth_min/max vector
      
      depth_min[i] <- min
      depth_max[i] <- max
    }
  }
  
  if (df_mean.depth[1] == 0) {
    depth_min[1] <- 0
    depth_max[1] <- 0
    depth_min[2] <- 0
    depth_max[l] <- depth_min[l] + 2 * (df_mean.depth[l] - depth_min[l])
  } else {
    depth_min[1] <- 0
    depth_max[l] <- depth_min[l] + 2 * (df_mean.depth[l] - depth_min[l])
  }
  
  dataframe <- dataframe %>%
    mutate(depth_min = depth_min) %>%
    mutate(depth_max = depth_max)
  
  return(dataframe)
}       

## Customized cr_cn function ######

# Pulled from ropenscri/crossref
# https://github.com/ropensci/rcrossref

# Required functions from rcrossref for cr_cn (and therefore cr_ccrcn)
`cr_agency` <- function(dois = NULL, .progress="none", ...) {
  foo <- function(x, y, ...){
    cr_GET(
      endpoint = sprintf("works/%s/agency", x),
      args = list(),
      parse = TRUE, ...)
  }
  if (length(dois) > 1) {
    res <- llply(dois, foo, y = .progress, ...)
    res <- lapply(res, "[[", "message")
    names(res) <- dois
    res
  } else {
    foo(dois, ...)$message
  }
}

GET_agency_id <- function(x, ...) {
  if (is.null(x)) {
    stop("no doi for doi agency check provided", call. = FALSE)
  }
  cr_agency(x)$agency$id
}

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

rcrossref_ua <- function() {
  versions <- c(paste0("r-curl/", utils::packageVersion("curl")),
                paste0("crul/", utils::packageVersion("crul")),
                sprintf("rOpenSci(rcrossref/%s)", 
                        utils::packageVersion("rcrossref")),
                get_email())
  paste0(versions, collapse = " ")
}

get_email <- function() {
  email <- Sys.getenv("crossref_email")
  if (identical(email, "")) {
    NULL
  } else {
    paste0("(mailto:", val_email(email), ")")
  }
}

warn_status <- function(x) {
  if (x$status_code > 202) {
    mssg <- x$parse("UTF-8")
    if (!is.character(mssg)) {
      mssg <- if (x$status_code == 406) {
        "(406) - probably bad format type"
      } else {
        x$status_http()$message
      }
    } else {
      mssg <- paste(sprintf("(%s)", x$status_code), "-", mssg)
    }
    # warning(
    #   sprintf(
    #     "%s w/ %s",
    #     gsub("%2F", "/", crul::url_parse(x$url)$path),
    #     mssg
    #   ),
    #   call. = FALSE
    # )
  }
}

# Supported content types
# See http://www.crosscite.org/cn/
supported_cn_types <- list(
  crossref = c("rdf-xml", "turtle", "citeproc-json", "citeproc-json-ish",
               "text", "ris", "bibtex", "crossref-xml", "bibentry",
               "crossref-tdm"),
  datacite = c("rdf-xml", "turtle", "datacite-xml", "citeproc-json-ish", "text",
               "ris", "bibtex", "bibentry"),
  medra = c("rdf-xml", "turtle", "citeproc-json-ish", "ris", "bibtex",
            "bibentry", "onix-xml")
)

cr_ccrcn <- function (dois, format = "bibtex", style = "apa", locale = "en-US", 
                      raw = FALSE, .progress = "none", url = NULL, ...) 
{
  
  format <- match.arg(format, c("rdf-xml", "turtle", "citeproc-json", 
                                "citeproc-json-ish", "text", "ris", "bibtex", "crossref-xml", 
                                "datacite-xml", "bibentry", "crossref-tdm", "onix-xml"))
  cn <- function(doi, ...) {
    agency_id <- suppressWarnings(GET_agency_id(doi))
    if (is.null(agency_id)) {
      warning(doi, " agency not found - proceeding with 'crossref' ...",
              "cr_ccrcn failed to parse BibTex citation from this DOI. Please manually generate BibTex citation for this DOI.",
              
              call. = FALSE)
      agency_id <- "crossref"
    }
    assert(url, "character")
    if (is.null(url)) 
      url <- "https://doi.org"
    args <- list()
    if (grepl("citation.crosscite.org", url)) {
      args <- cr_compact(list(doi = doi, lang = locale, 
                              style = style))
    }
    else {
      url <- file.path(url, doi)
    }
    if (!format %in% supported_cn_types[[agency_id]]) {
      stop(paste0("Format '", format, "' for '", doi, "' is not supported by the DOI registration agency: '", 
                  agency_id, "'.\nTry one of the following formats: ", 
                  paste0(supported_cn_types[[agency_id]], collapse = ", ")))
    }
    pick <- c(`rdf-xml` = "application/rdf+xml", turtle = "text/turtle", 
              `citeproc-json` = "transform/application/vnd.citationstyles.csl+json", 
              `citeproc-json-ish` = "application/vnd.citationstyles.csl+json", 
              text = "text/x-bibliography", ris = "application/x-research-info-systems", 
              bibtex = "application/x-bibtex", `crossref-xml` = "application/vnd.crossref.unixref+xml", 
              `datacite-xml` = "application/vnd.datacite.datacite+xml", 
              bibentry = "application/x-bibtex", `crossref-tdm` = "application/vnd.crossref.unixsd+xml", 
              `onix-xml` = "application/vnd.medra.onixdoi+xml")
    type <- pick[[format]]
    if (format == "citeproc-json") {
      cli <- crul::HttpClient$new(url = file.path("https://api.crossref.org/works", 
                                                  doi, type), headers = list(`User-Agent` = rcrossref_ua(), 
                                                                             `X-USER-AGENT` = rcrossref_ua()))
      response <- cli$get(...)
    }
    else {
      if (format == "text") {
        type <- paste(type, "; style = ", style, "; locale = ", 
                      locale, sep = "")
      }
      cli <- crul::HttpClient$new(url = url, opts = list(followlocation = 1), 
                                  headers = list(`User-Agent` = rcrossref_ua(), 
                                                 `X-USER-AGENT` = rcrossref_ua(), Accept = type))
      response <- cli$get(query = args, ...)
    }
    warn_status(response)
    if (response$status_code < 202) {
      select <- c(`rdf-xml` = "text/xml", turtle = "text/plain", 
                  `citeproc-json` = "application/json", `citeproc-json-ish` = "application/json", 
                  text = "text/plain", ris = "text/plain", bibtex = "text/plain", 
                  `crossref-xml` = "text/xml", `datacite-xml` = "text/xml", 
                  bibentry = "text/plain", `crossref-tdm` = "text/xml", 
                  `onix-xml` = "text/xml")
      parser <- select[[format]]
      if (raw) {
        response$parse("UTF-8")
      }
      else {
        out <- response$parse("UTF-8")
        if (format == "text") {
          out <- gsub("\n", "", out)
        }
        if (format == "bibentry") {
          out <- parse_bibtex(out)
        }
        if (parser == "application/json") {
          out <- jsonlite::fromJSON(out)
        }
        if (parser == "text/xml") {
          out <- xml2::read_xml(out)
        }
        out
      }
    }
  }
  if (length(dois) > 1) {
    # library(plyr, warn.conflicts = FALSE, quietly = TRUE)
    # library(dplyr, warn.conflicts = FALSE, quietly = TRUE)
    lapply(dois, function(z, ...) {
      out <- try(cn(z, ...), silent = TRUE)
      if ("try-error" %in% class(out)) {
        warning(paste0("Failure in resolving '", z, "'. See error detail in results."),
                call. = FALSE)
        out <- list(doi = z, error = out[[1]])
      }
      return(out)
    }, .progress = .progress)
  }
  else {
    cn(dois, ...)
  }
}
