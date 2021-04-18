*** DATA MERGING ***
// merge all the individual hate crime files together, make sure there's no 
// gaps in the time series, create variables, etc. 

clear all
set more off






// read in cleaned hate crime data
tempfile temp
fs "data/hate-crimes/police-reports/*.csv"
local i = 1
foreach f in `r(files)' {
	import delimited data/hate-crimes/police-reports/`f', clear  stringcols(_all) varnames(1)
		
	if `i' == 1 {
		save `temp', replace
		local ++i
	}
	else {
		append using `temp'
		save `temp', replace
	}
}


// clean up variable names
replace date = ïdate if !missing(ïdate) 
drop ïdate
destring hate_crimes muslim_crimes, ignore("NA") replace

gen date2 = date(date, "YMD") 
format %td date2

encode county, gen(county2)
drop county
rename county2 county



// see data range for each county
sort county date2
bys county: egen lastmonth = max(date2)
bys county: egen firstmonth = min(date2)
format %td *month



// fill in time series 
gen month = mofd(date2)
format %tm month
tsset county month
tsfill
replace hate_crimes = 0 if missing(hate_crimes) & !missing(hate_crimes[_n-1]) & county == county[_n-1]
replace muslim_crimes = 0 if missing(muslim_crimes) & !missing(muslim_crimes[_n-1]) & county == county[_n-1]


// fill in date variable after tsfill
replace date2 = dofm(month)
replace date = string(yofd(date2)) + "-" + string(mod(mofd(date2), 12) + 1) + "-01" 

// merge with population data
decode county, gen(county_str)
tempfile temp2
save `temp2', replace
import delimited data/hate-crimes/police-area-population.csv, clear 
rename ïcounty county
rename county county_str
merge 1:n county_str using `temp2'

count if _merge == 2
assert `r(N)' == 0 
drop if _merge == 1
drop _merge

// create annualized per capita crime variable by dividing by population, then
// multiplying by 1000 to get monthly rate per 1000 people then by 12 to get
// annualized rate per 1000 people
cap destring police_area_pop, replace
gen hate_crimes_pc = 12 * 1000 * hate_crimes / police_area_pop
gen muslim_crimes_pc = 12 * 1000 * muslim_crimes / police_area_pop

tsset county date2



// define treatment variable
gen treated = 0 
replace treated = 1 if county_str == "merseyside" & date2 >= `=date("2017-07-01", "YMD")'


// only keep data that is within the range of merseyside data - starting in Jan 2015
sum firstmonth if county_str == "merseyside"
assert `r(sd)' == 0
drop if date2 < `r(mean)'

save data/hate-crimes/merged_hate_crimes.dta, replace
export delimited data/hate-crimes/merged_hate_crimes.csv, delimiter(",") replace


*** FINISHED DATA MERGING ***
