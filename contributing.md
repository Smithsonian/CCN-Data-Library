
# Contributing to the Coastal Carbon Data Library

_DRAFT VERSION_




### Reading in data files 
We do a lot of importing of datasets, primarily excel files and .csv. For excel files, I know that I am careful to see how column types are parsed and if data is lost. But for .csv files, especially ones that we already curated...I admit that I am not. This can sometimes be a serious issue if the first 1,000 values of a column are `NA` and parsed as logical...this may cause all of the values of the column to be turned to `NA`, which may go unnoticed later on. I want to improve my practices as well as all of ours. Four steps that I think we should be taking:

1. we should parse individual columns more carefully. Each read_csv() command should very likely be accompanied by a few col_numeric() commands on cols that are parsed otherwise. 

2. Each read_csv() command should be followed by a stop_for_problems() command. This will stop the script from running if there are problems, and rather than letting those warnings slide on by, **we should start taking them more seriously.**

3. We should change default parsers if needed; typically, changing to number.

The three of these practices together could look like this:

```
CCRCN_depthseries <- read_csv("./data/CCRCN_synthesis/CCRCN_depthseries_data.csv",
                              col_types = cols(.default = col_number(),
                                               study_id = col_character(),
                                               site_id = col_character(),
                                               core_id = col_character(),
                                               fraction_carbon_type = col_factor(),
                                               DBD_measured_or_modeled = col_factor(),
                                               OC_measured_or_modeled = col_factor(),
                                               CD_measured_or_modeled = col_factor(),
                                               CD_reported = col_factor()
                                               ))

stop_for_problems(CCRCN_depthseries)
```

The issue with reading in this dataset using defaults was that some of the columns (e.g. total_pb210_activity) were parsed as logical because of many `NA`s, which dropped all of the actual values. So, changing the default to number, and then specifying what columns should not be numeric, fixes this. 

4. Before exporting a curated dataset, sort it so that the first few dozen lines are representative of what we actually want the column to be parsed as.

Hadley's [Data Import chapter in R for Data Science](https://r4ds.had.co.nz/data-import.html) has some helpful content that helped inform this. Let's keep these practices in mind when hooking new data, but also _just as importantly_ for formatting and joining already-curated data.
