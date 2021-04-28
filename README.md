# CLIMAR

Data processing for use in a UKCP18 visualisation app - https://covid19modelling.shinyapps.io/UKCP18_explore/

ukcpm_rasters.R is used to extract monthly quantiles from the UKCP18 data for each UK MSOA.

explore_uk_cp.R filters these summaries and stores them as .RDS files

app.R is the code for the shiny app which is currently pulling the .RDS files from googledrive. 

