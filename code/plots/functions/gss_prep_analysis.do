********************************************************************************
*Gender + Wellbeing
********************************************************************************
*Trend figures
********************************************************************************

set scheme plottig

*=====================================
*	1. Setup
*=====================================

*-------------------------------------
*	1.0. Set path and load
*-------------------------------------

global path "/Users/casparkaiser/Library/CloudStorage/OneDrive-UniversityofWarwick/gender_wellbeing/gss_analysis"
use "$path/gss7222_r3a.dta", clear

*-------------------------------------
*	1.1. Gender & covariates
*-------------------------------------

*Gender
replace sex = . if !inlist(sex,1,2)
replace sex = sex -1
label define SEX 0 "male" 1 "female", replace
label values sex SEX

*Age
gen agesq=age^2

global covars ""

*-------------------------------------
*	1.2. happy
*-------------------------------------

replace happy = . if !inlist(happy, 1,2,3)
replace happy = happy*-1
replace happy = happy + 4
label define HAPPY 1 "not too happy" 2 "pretty happy" 3 "very happy" , replace
label values happy HAPPY

*-------------------------------------
*	1.3. life
*-------------------------------------

replace life = . if !inlist(life, 1,2,3)
replace life = life*-1
replace life = life + 4
label define LIFE 1 "dull" 2 "routine" 3 "exciting" , replace
label values life LIFE

*-------------------------------------
*	1.4. satjob
*-------------------------------------

replace satjob = . if !inlist(satjob, 1,2,3,4)
replace satjob = satjob*-1
replace satjob = satjob + 5
label define SATJOB 1 "vary dissatisfied" 2 "a little dissatisfied" 3 "moderately satisfied" 4 "very satisfied" , replace
label values satjob SATJOB

*-------------------------------------
*	1.5. satfin
*-------------------------------------

replace satfin = . if !inlist(satfin, 1,2,3)
replace satfin = satfin*-1
replace satfin = satfin + 4
label define SATFIN 1 "not satisfied at all" 2 "more or less satisfied" 3 "pretty well satisfied" , replace
label values satfin SATFIN

*-------------------------------------
*	1.5. class and classy -> pooled to class 
*-------------------------------------

replace class = . if class ==5
recode classy (5=4)
 replace class = classy if class==.

*=====================================
*	2. Run regressions
*=====================================
	 
local depvars happy life satjob satfin class
foreach var of local depvars {
	reg `var' i.year i.sex i.year $covars [pw=wtssps], vce(robust) // just to check overall difference
	eststo `var': reg `var' i.year i.sex#i.year $covars [pw=wtssps], vce(robust)
	reg `var' i.sex##c.year $covars [pw=wtssps], vce(robust)
	gen `var'_b_trend_d = _b[1.sex#c.year]
	gen `var'_se_trend_d = _se[1.sex#c.year]
	gen `var'_b_trend_m = _b[year]
	gen `var'_se_trend_m = _se[year]	 
}

*=====================================
*	3. Get differences
*=====================================

local depvars happy life satjob satfin class
foreach var of local depvars {

	*-------------------------------------
	*	3.0. Set some variable specific options
	*-------------------------------------
	
	if "`var'"=="happy" {
		local startyear=1972
		local start2year=1973
	}
	if "`var'"=="life" {
		local startyear=1973
		local start2year=1974
	}
	if "`var'"=="satfin" {
		local startyear=1972
		local start2year=1973
	}	
	if "`var'"=="satjob" {
		local startyear=1972
		local start2year=1973
	}		
	if "`var'"=="class" {
		local startyear=1972
		local start2year=1973
	}		
	
	*-------------------------------------
	*	3.1. For pooled model
	*-------------------------------------
	
	estimates restore `var'
	predict `var'_hat
	predict `var'_hat_se, stdp
	gen `var'_hat_l = `var'_hat - invnormal(0.975)*`var'_hat_se
	gen `var'_hat_u = `var'_hat + invnormal(0.975)*`var'_hat_se

	gen `var'_diff =	_b[1.sex#`startyear'b.year] if year==`startyear'
	gen `var'_diff_se = invnormal(0.975)*_se[1.sex#`startyear'b.year] if year==`startyear'
	forvalues year=`start2year'(1)2022 {
		cap replace `var'_diff = _b[1.sex#`year'.year] if year==`year'
		cap replace `var'_diff_se = invnormal(0.975)*_se[1.sex#`year'.year] if year==`year'	
	}
	gen `var'_diff_l = `var'_diff - invnormal(0.975)*`var'_diff_se
	gen `var'_diff_u = `var'_diff + invnormal(0.975)*`var'_diff_se
	
}

*=====================================
*	4. Make plots
*=====================================

*To speed up the picture drawing and save for use in R
cap drop tmp 
bys year sex: gen tmp = _n
keep if tmp == 1

keep year sex ///
happy_hat happy_hat_u happy_hat_l happy_diff happy_diff_u happy_diff_l happy_b_trend_d happy_se_trend_d happy_b_trend_m happy_se_trend_m ///
life_hat life_hat_u life_hat_l life_diff life_diff_u life_diff_l life_b_trend_d life_se_trend_d life_b_trend_m life_se_trend_m /// 
satjob_hat satjob_hat_u satjob_hat_l satjob_diff satjob_diff_u satjob_diff_l satjob_b_trend_d satjob_se_trend_d satjob_b_trend_m satjob_se_trend_m /// 
satfin_hat satfin_hat_u satfin_hat_l satfin_diff satfin_diff_u satfin_diff_l satfin_b_trend_d satfin_se_trend_d satfin_b_trend_m satfin_se_trend_m /// 
class_hat class_hat_u class_hat_l class_diff class_diff_u class_diff_l class_b_trend_d class_se_trend_d class_b_trend_m class_se_trend_m //

save "$path/gss_predictions.dta", replace




local depvars happy life satjob satfin class
foreach var of local depvars {
	
	preserve

	*-------------------------------------
	*	4.0. Setups
	*-------------------------------------
	
	if "`var'"=="happy" {
		local title "Happiness"
	}
	if "`var'"=="life" {
		local title "Life exciting"
	}
	if "`var'"=="satfin" {
		local title "Financial sat."
	}	
	if "`var'"=="satjob" {
		local title "Job sat."
	}		
	if "`var'"=="class" {
		local title "Subj. class"
	}
	
	*-------------------------------------
	*	4.1. Draw plot
	*-------------------------------------

	twoway ///
	(line `var'_hat year if sex==0, lp(solid) lc(black)) ///
	(rarea `var'_hat_u `var'_hat_l year if sex==0, color(plb1%40)) ///
	(line `var'_hat year if sex==1, lp("-.") lc(black)) ///
	(rarea `var'_hat_u `var'_hat_l year if sex==1, color(plg1%40)) ///
	, ///
	plotregion(fcolor(white)) ///
	yscale(lcolor(black)) ylabel(, format(%4.1f) glcolor(black) glpattern(dot) gmin gmax) ytitle("Predicted values") ///
	xscale(lcolor(black)) xlabel(1972(2)2022, format(%4.0f) nogrid angle(45)) xtitle("Year") ///
	legend(pos(6) order(1 "Men" 3 "Women") cols(2)) name(absolute, replace) nodraw ///
	title("`title' levels across years in the USA") scale(0.8) 

	twoway ///
	(line `var'_diff year, lp(solid) lc(black)) ///
	(rarea `var'_diff_u `var'_diff_l year, color(plb1%40)) ///
	, ///
	plotregion(fcolor(white)) ///
	yscale(lcolor(black)) ylabel(, format(%4.1f) glcolor(black) glpattern(dot) gmin gmax) ytitle("Predicted values") ///
	xscale(lcolor(black)) xlabel(1972(2)2022, format(%4.0f) nogrid angle(45)) xtitle("Year") ///
	yline(0, lc(red)) ///
	legend(pos(6) order(1 "Difference (women - men)") cols(2)) name(diff, replace) nodraw ///
	title("`title' difference between genders across years in the USA") scale(0.8)

	graph combine absolute diff ///
	, ysize(2) name(`var'_pooled, replace)
		
	restore
}

*-------------------------------------
*	4.2. Combine
*-------------------------------------

graph combine ///
happy_pooled life_pooled satjob_pooled satfin_pooled class_pooled ///
, cols(1) ysize(8)

graph export "$path/combined.png", replace width(2000)



