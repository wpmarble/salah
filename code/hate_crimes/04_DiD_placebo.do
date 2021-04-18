/* 
Replication file for "Can Celebrities Reduce Prejudice" 

This file run a generalized diff-in-diff analysis of hate crimes in the UK, with
the treated unit being Merseyside after July 2017 (inclusive). In the first part,
it runs a number of specifications (two way FE, unit-specific time trends, with
and without population weights). 

Since there is only one treated unit, the standard errors are not reliable. To 
do inference, implement a placebo procedure which randomly shuffles the treated
county and time that treatment is implemented. Repeatedly re-run the DiD model
with the placebo treatment, then compare the actual coefficient to the distribution
of placebo coefficients. 

Requires the reghdfe command and outreg2 for regression tables. If not installed
type "ssc install reghdfe" and "ssc install outreg2". 
*/ 

clear all
set more off


// set wd to replication directory
//cd "~/Dropbox/Mo_Salah/replication_files"


use data/hate-crimes/merged_hate_crimes.dta, clear

**********************
// Initial Analysis // 
**********************

* regular two-way FE * 
reghdfe hate_crimes_pc treated, absorb(month county)  cluster(county) 
outreg2 using "tables/hate_crime_did.tex", replace keep(treated) adds("Unit FE", 1, "Month FE", 1, "Unit-specific time trend", 0, "Weights", 0) nocons

* include unit-specific time trend
reghdfe hate_crimes_pc treated, absorb(county##c.month month) cluster(county)
outreg2 using "tables/hate_crime_did.tex", append keep(treated) adds("Unit FE", 1, "Month FE", 1, "Unit-specific time trend", 1, "Weights", 0) nocons

* two-way FE with population weights
reghdfe hate_crimes_pc treated [aw=police_area_pop], absorb(month county)  cluster(county) 
outreg2 using "tables/hate_crime_did.tex", append keep(treated) adds("Unit FE", 1, "Month FE", 1, "Unit-specific time trend", 0, "Weights", 1) nocons

* unit-specific time trend with population weights
reghdfe hate_crimes_pc treated [aw=police_area_pop], absorb(county##c.month month) cluster(county)
outreg2 using "tables/hate_crime_did.tex", append keep(treated) adds("Unit FE", 1, "Month FE", 1, "Unit-specific time trend", 1, "Weights", 1) nocons

***********************
// Placebo Inference //
***********************

/* 
Implement a randomization-based approach to inference. Randomly assign 
the treated unit and treatment date, estimate the two way FE regression,
then save the estimates to approximate the randomization distribution. 
Constrain time of treatment to be no earlier than 4 months after the first
period observed in the data and no later than the actual treatment date. 
*/
set seed 499203   // from random.org
local sims = 1e4  // number of placebo simulations

// create a matrix to store simulated coefficient estimates
mata: b_2way = J(`sims', 1, 0) 
mata: b_time = J(`sims', 1, 0) 

// repeat placebo procedure `sims' times 
qui forvalues i = 1/`sims' {
	
	cap drop sim_treated
	cap drop sim_treated_unit
	cap drop temp 
	cap drop temp2
	
	// assign the placebo treated unit. code is kind of convoluted -- first 
	// assign each county-month observation a random number, then assign the
	// county with the highest random number to be treated. 
	gen temp = runiform()
	bys county: egen temp2 = max(temp)
	sum temp2
	gen sim_treated_unit = 0
	replace sim_treated_unit = 1 if temp2 == `r(max)'
	drop temp temp2
	
	// find the treatment start month; restrict it to be no later than the actual 
	// treatment and no earlier than 4 months after the first month in the 
	// dataset. 
	sum month
	local minmonth = `r(min)' + 4
	local maxmonth = `=mofd(date("2017-07-01", "YMD"))'
	
	// generate a random number for the observations corresponding to the 
	// placebo treated unit (found above) and months falling between the first
	// and last possible month for treatment to switch on (as defined by 
	// `minmonth' and `maxmonth'). Treat the month with the smallest random number
	// as the month that treatment switches on.
	gen temp = runiform() if sim_treated_unit == 1 & month >= `minmonth' & month <= `maxmonth'
	egen temp2 = min(temp) if sim_treated_unit == 1
	gen sim_treated = 0
	replace sim_treated = 1 if sim_treated_unit == 1 & temp == temp2
	sort county month
	replace sim_treated = 1 if sim_treated[_n-1] == 1 & county[_n-1] == county
	drop temp temp2
	
	// run the two-way FE regression and extract coefficient
	reghdfe hate_crimes_pc sim_treated, absorb(month county) 
	mat b = e(b)
	local coef = b[1,1]
	
	// store estimate in the matrix defined above
	mata: b_2way[`i',1] = `coef'
	
	// run the regression with unit specific linear time trends
	reghdfe hate_crimes_pc sim_treated, absorb(county##c.month month) 
	mat b = e(b)
	local coef = b[1,1]
	
	// store estimate in the matrix defined above
	mata: b_time[`i',1] = `coef'
}

** Analysis of Placebo Distribution **

// Get actual coefficients (same as first two regressions above) 
reghdfe hate_crimes_pc treated, absorb(month county) 
mat b = e(b)
local actual_2way = b[1,1]

reghdfe hate_crimes_pc treated, absorb(county##c.month month) cluster(county)
mat b = e(b)
local actual_time = b[1,1]


// put simulated coefficient matrix into dataset
drop _all 
getmata sim_coefs_2way = b_2way, force replace
getmata sim_coefs_time = b_time, force replace


*** Vanilla 2-way FE model ***

// generate one-sided p-value by seeing proportion less than actual estimate
gen less_than_actual_2way = sim_coefs_2way <= `actual_2way'
sum less_than_actual_2way
local p_onesided_2way = `r(mean)'

// Make a histogram plotting the distribution of placebo coefficients along with
// the observed coefficient
hist sim_coefs_2way, /*
	*/ scheme(s1mono) xtitle("Placebo coefficient estimate") fcolor(gs10) lcolor(gs10) /*
	*/ addplot(pci 0 `actual_2way' 3 `actual_2way', lcolor(black) lwidth(.65) legend(off)) /* 
	*/ text(2.75 `=`actual_2way'+.24' "Observed estimate" "One sided p = `=round(`p_onesided_2way', .001)'", just(left))
graph export figs/hate_crime_randomization_dist_2wayfe.pdf, replace


*** With Unit-Specific Time Trends ***

// generate one-sided p-value by seeing proportion less than actual estimate
gen less_than_actual_time = sim_coefs_time <= `actual_time'
sum less_than_actual_time
local p_onesided_time = `r(mean)'

// Make a histogram plotting the distribution of placebo coefficients along with
// the observed coefficient
hist sim_coefs_time, /*
	*/ scheme(s1mono) xtitle("Placebo coefficient estimate") fcolor(gs10) lcolor(gs10) /*
	*/ addplot(pci 0 `actual_time' 3 `actual_time', lcolor(black) lwidth(.65) legend(off)) /* 
	*/ text(2.75 `=`actual_time'+.24' "Observed estimate" "One sided p = `=round(`p_onesided_time', .001)'", just(left))
graph export figs/hate_crime_randomization_dist_unitspec_trend.pdf, replace





keep *coef*
save data/hate-crimes/placebo_coefs.dta, replace
