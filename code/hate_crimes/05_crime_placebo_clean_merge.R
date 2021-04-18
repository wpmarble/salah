

## Clean up crime data for placebo analysis in Fig 3. Take the input data and
## count how many total crimes are reported and how many crimes of each type
## are committed.

# The raw data archive is large so we are only including the -output- of
# this file on Dataverse. To run this file, you will need to download two
# datasets from the UK Police website:
# 1) https://data.police.uk/data/archive/2018-09.zip
# 2) https://data.police.uk/data/archive/2015-10.zip

# Unzip the archives, delete the overlapping months from the earlier archive
# (2015-Oct), and put them in the directory data/crime-archive, with
# subdirectories corresponding to months in the format YYYY-MM. For example, this
# is what the folder corresponding to Jan. 2015 looks like:
#   > head(list.files("03data/crime-archive/2015-01/"), 10 )
#   [1] "2015-01-avon-and-somerset-street.csv" 
#   [2] "2015-01-bedfordshire-street.csv"      
#   [3] "2015-01-btp-street.csv"               
#   [4] "2015-01-cambridgeshire-street.csv"    
#   [5] "2015-01-cheshire-street.csv"          
#   [6] "2015-01-city-of-london-street.csv"    
#   [7] "2015-01-cleveland-street.csv"         
#   [8] "2015-01-cumbria-street.csv"           
#   [9] "2015-01-derbyshire-street.csv"        
#   [10] "2015-01-devon-and-cornwall-street.csv"


library(dplyr)
library(lubridate)

basedir = file.path("data/crime-archive")
dirs = list.dirs(basedir, full.names = F)
dirs = dirs[dirs!=""]

results = list()
j = 1
for (dir in dirs){
  
  cat("Starting on directory", dir, "...\n\n")
  
  # first delete stop-and-search and "outcome" data -- don't need it and
  #  taking up too much space
  stopsearch = list.files(file.path(basedir, dir), pattern = "search\\.csv$")
  file.remove(file.path(basedir, dir, stopsearch))
  outcome = list.files(file.path(basedir, dir), pattern = "outcomes\\.csv$")
  file.remove(file.path(basedir, dir, outcome))
  
  # get report files, ending in *street.csv
  street = list.files(file.path(basedir, dir), pattern = "street\\.csv$")
  
  # get month from directory
  year = substr(dir, 1, 4)
  month = gsub(paste0(year, "-"), "", dir)
  date = paste(year, month, "01", sep = "/")
  
  
  out = list()
  i = 1
  for (s in street){
    # get county from file name
    county = gsub(paste0(dir, "|-street\\.csv$"), "", s)
    county = gsub("^-","",county)
    
    # read in data and count how many total crimes are reported + how many
    # incidents of each crime type
    dat = read.csv(file.path(basedir, dir, s), stringsAsFactors = F)
    names(dat) = tolower(names(dat))
    
    sum = dat %>% 
      group_by(crime.type) %>% 
      summarise(num_crimes = n()) %>% 
      mutate(county = county, month = date) 
    all = dat %>% 
      summarise(num_crimes = n()) %>% 
      mutate(county = county, month = date,
             crime.type = "All") 
    sum = bind_rows(sum, all)
    
    out[[i]] = sum
    i = i + 1
  }
  out = bind_rows(out)
  
  results[[j]] = out
  j = j + 1
}
results = bind_rows(results)

# clean up and mark what observations overlap with the hate crime analysis
results = results %>% 
  mutate(month = as.Date(month))
results$county[results$county == "avon-and-somerset"] = "avon_somerset"
results$county[results$county == "metropolitan"] = "london"
results$county[results$county == "greater-manchester"] = "manchester"
results$county[results$county == "west-mercia"] = "west_marcia"
results$county[results$county == "west-yorkshire"] = "west_yorkshire"

est_dat = readRDS("data/hate-crimes/estimation_sample.rds")
est_dat$hate_crime_sample = 1
est_dat = dplyr::rename(est_dat, county = county_str, month = date)

results = left_join(results, est_dat, by = c("county", "month"))
results$hate_crime_sample[is.na(results$hate_crime_sample)] = 0

# merge in population data for normalized outcomes
pop_dat = read.csv("data/hate-crimes/police-area-population.csv",stringsAsFactors = FALSE)
results = left_join(results, select(pop_dat, county, police_area_pop), by = "county")

results = results %>% 
  mutate(crimes_pc = 12 * 1000 * num_crimes / police_area_pop)

saveRDS(results, file = "data/crime-archive/cleaned_all_crime.rds")
