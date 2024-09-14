*==========================================================================
*==========================================================================
*	TWO PARADOXES IN WOMEN'S WELLBEING PROJECT
*==========================================================================
*==========================================================================
* 	Eurobarometer data cleaning and analysis
*==========================================================================
*==========================================================================

*=====================================
*	0. Setup
*=====================================

*-------------------------------------
*0.1. Set file path
*-------------------------------------

*Work
*global path "/Users/u2272701/Library/CloudStorage/OneDrive-UniversityofWarwick/gender_wellbeing/gss_and_eurobarometer/eurobarometer/data"
*Private
global path "/Users/casparkaiser/Library/CloudStorage/OneDrive-UniversityofWarwick/gender_wellbeing/gss_and_eurobarometer/eurobarometer/data"

*=====================================
*	1. Merge waves
*=====================================

*-------------------------------------
*1.1. Mannheim Trendfile (1974-2002)
*-------------------------------------

use "$path/ZA3521.dta", replace

keep  happinss satislfe ///
sex ///
wsample  wnation weuro ///
nation1 nation2 year ///
age income married educ sizehh occup

save "$path/eb_trend_1.dta", replace

*-------------------------------------
*1.2. Trendfile for 2004-2021
*-------------------------------------

use "$path/harmonised_EB_2004-2021_v3-0-0.dta", replace
rename satislife satislfe
rename gender sex
rename country nation3
rename married married_2
keep  satislfe ///
sex ///
wsample wnation wnationGE wnationGB weuro w_all wex wpol ///
nation3 year ///
age income_q married educ sizehh occup
save "$path/eb_trend_2.dta", replace

*-------------------------------------
*1.3. Individual waves for 2022-2023
*-------------------------------------

*2022 1
use "$path/ZA7848.dta", replace

rename d70 satislfe
rename d10 sex
rename w92 w_all

gen nation4 = .
local countries "6 1 10 3 8 9 2 7 12 12 4 5 11 3 39 15 14 13"
forvalues i=1/18 {
	local n: word `i' of `countries'
	cap replace nation4 = `i' if q1_`n'==1
}
gen year = 2022
gen month_start = 1
gen month_end = 2

rename d11 age
rename d7r mastat
rename d8r1 educ
rename d40a sizehh
rename d15a occup_2

keep  satislfe ///
sex ///
w_all ///
nation4 year ///
age mastat educ sizehh occup_2 region_germany

save "$path/eb2022_1.dta", replace

*2022 2
use "$path/ZA7888.dta", replace
rename d70 satislfe
rename d10 sex
rename w92 w_all

gen nation4 = .
local countries "6 1 10 3 8 9 2 7 12 12 4 5 11 3 39 15 14 13"
forvalues i=1/18 {
	local n: word `i' of `countries'
	cap replace nation4 = `i' if q1_`n'==1
}
gen year = 2022
gen month_start = 4
gen month_end = 5

rename d11 age
rename d7r mastat
rename d8r1 educ
rename d40a sizehh
rename d15a occup_2

keep  satislfe ///
sex ///
w_all ///
nation4 year ///
age mastat educ sizehh occup_2 region_germany

save "$path/eb2022_2.dta", replace

*2022 3
use "$path/ZA7902.dta", replace
rename d70 satislfe
rename d10 sex
rename w92 w_all

gen nation4 = .
local countries "6 1 10 3 8 9 2 7 12 12 4 5 11 3 39 15 14 13"
forvalues i=1/18 {
	local n: word `i' of `countries'
	cap replace nation4 = `i' if q1_`n'==1
}
gen year = 2022
gen month_start = 6
gen month_end = 7

rename d11 age
rename d7r mastat
rename d8r1 educ
rename d40a sizehh
rename d15a occup_2

keep  satislfe ///
sex ///
w_all ///
nation4 year ///
age mastat educ sizehh occup_2 region_germany

save "$path/eb2022_3.dta", replace

*2023 1
use "$path/ZA7953.dta", replace
rename d70 satislfe
rename d10 sex
rename w92 w_all

gen nation4 = .
local countries "6 1 10 3 8 9 2 7 12 12 4 5 11 3 39 15 14 13"
forvalues i=1/18 {
	local n: word `i' of `countries'
	cap replace nation4 = `i' if q1_`n'==1
}
gen year = 2023
gen month_start = 1
gen month_end = 2

rename d11 age
rename d7r mastat
rename d8r1 educ
rename d40a sizehh
rename d15a occup_2

keep  satislfe ///
sex ///
w_all ///
nation4 year ///
age mastat educ sizehh occup_2 region_germany

save "$path/eb2023_1.dta", replace

*2023 2
use "$path/ZA7997.dta", replace
rename d70 satislfe
rename d10 sex
rename w92 w_all

gen nation4 = .
local countries "6 1 10 3 8 9 2 7 12 12 4 5 11 3 39 15 14 13"
forvalues i=1/18 {
	local n: word `i' of `countries'
	cap replace nation4 = `i' if q1_`n'==1
}
gen year = 2023
gen month_start = 5
gen month_end = 6

rename d11 age
rename d7r mastat
rename d8r1 educ
rename d40a sizehh
rename d15a occup_2

keep  satislfe ///
sex ///
w_all ///
nation4 year ///
age mastat educ sizehh occup_2 region_germany

save "$path/eb2023_2.dta", replace

*2023 3
use "$path/ZA8779.dta", replace
rename d70 satislfe
rename d10 sex
rename w92 w_all

gen nation4 = .
local countries "6 1 10 3 8 9 2 7 12 12 4 5 11 3 39 15 14 13"
forvalues i=1/18 {
	local n: word `i' of `countries'
	cap replace nation4 = `i' if q1_`n'==1
}
gen year = 2023
gen month_start = 10
gen month_end = 11

rename d11 age
rename d7r mastat
rename d8r1 educ
rename d40a sizehh
rename d15a occup_2

keep  satislfe ///
sex ///
w_all ///
nation4 year ///
age mastat educ sizehh occup_2 region_germany

save "$path/eb2023_3.dta", replace


*-------------------------------------
*1.4. Individual wave for 2003
*-------------------------------------

use "$path/ZA3938.dta", replace
rename v38 satislfe
rename v597 sex
rename v19 w_all // "Weight result from target"

rename isocntry nation5

gen month_start = 10
gen month_end = 11
gen year = 2003

rename v598 age
rename v594 mastat_2
rename v596 educ
rename v625 income_q
rename v601 occup_2

keep  satislfe ///
sex ///
w_all ///
nation5 year ///
age mastat_2 educ income_q occup_2

*-------------------------------------
*1.5. Append them all together
*-------------------------------------

save "$path/eb2003_1.dta", replace

use "$path/eb_trend_1.dta", replace
append using "$path/eb_trend_2.dta"
append using "$path/eb2022_1.dta"
append using "$path/eb2022_2.dta"
append using "$path/eb2022_3.dta"
append using "$path/eb2023_1.dta"
append using "$path/eb2023_2.dta"
append using "$path/eb2023_3.dta"
append using "$path/eb2003_1.dta"

save "$path/eb_trend_full.dta" , replace

*=====================================
*	2. Clean data
*=====================================

use "$path/eb_trend_full.dta" , replace

*-------------------------------------
*2.1. Life satisfaction
*-------------------------------------

keep if satislfe <= 4
replace satislfe = satislfe*-1 + 5
label define SATISLFE 1 "Not at all satisfied" 2 "Not very satisfied" 3 "Fairly satisfied" 4 "Very satisfied", replace
label values satislfe SATISLFE

*-------------------------------------
*2.2. Sex
*-------------------------------------

keep if sex <= 2
replace sex = sex - 1
label define SEX 0 "Male" 1 "Female", replace
label values sex SEX

*-------------------------------------
*2.3. Covariates for m3
*-------------------------------------

keep if age >= 15 & age != .
gen agesq = age^2
 
replace married = 1 if inlist(married_2,301,302)
replace married = 2 if inlist(married_2,101,102,103,104)
replace married = 3 if inlist(married_2,201,202,203,204)
replace married = 4 if inlist(married_2,401,402)
replace married = 5 if inlist(married_2,501,502)
replace married = 9 if inlist(married_2,-99981)
replace married = 9 if inlist(married,10,98)
replace married = 4 if married==5

replace married = 1 if inlist(mastat,2,3)
replace married = 2 if inlist(mastat,1)

replace married = 4 if inlist(mastat,4)
replace married = 6 if inlist(mastat,5)
replace married = 9 if inlist(mastat,6,7)

replace married = 1 if inlist(mastat_2,4,5)
replace married = 2 if inlist(mastat_2,1,2)
replace married = 3 if inlist(mastat_2,3)
replace married = 4 if inlist(mastat_2,6,7)
replace married = 6 if inlist(mastat_2,8)
replace married = 9 if inlist(mastat_2,9, .r)

replace married = 9 if married == .

replace educ = 12 if inlist(educ, -99951, 11, 97, 98, 99, ., .a, .b, .d, .n, .r) 

cap drop empstat
gen empstat = 1 if (occup >=110 & occup < 500) | (occup_2 >= 5 & occup <=18) // working
replace empstat = 2 if inlist(occup,500,510) | (occup_2 == 1) // homemaker
replace empstat = 3 if inlist(occup,530) | (occup_2 ==4) // retired
replace empstat = 4 if inlist(occup,520,521,522) | (occup_2 ==2) // student and/or military service
replace empstat = 5 if inlist(occup,540) | (occup_2 ==3) // unemployed
replace empstat = 6 if empstat == . // other/missing

label define EMPSTAT 1 "Working" 2 "Homemaker" 3 "Retired" 4 "Student" 5 "Unemployed" 6 "Other/missing", replace
label values empstat EMPSTAT

*-------------------------------------
*2.4. Countries
*-------------------------------------

replace nation2 = nation3 if nation2==.
replace nation2 = nation4 if nation2==.
replace nation2 = 1 if nation5 == "FR"
replace nation2 = 2 if nation5 == "BE"
replace nation2 = 3 if nation5 == "NL"
replace nation2 = 4 if nation5 == "DE-W"
replace nation2 = 5 if nation5 == "IT"
replace nation2 = 6 if nation5 == "LU"
replace nation2 = 7 if nation5 == "DK"
replace nation2 = 8 if nation5 == "IE"
replace nation2 = 9 if nation5 == "GB-GBN"
replace nation2 = 10 if nation5 == "GB-NIR"
replace nation2 = 11 if nation5 == "GR"
replace nation2 = 12 if nation5 == "ES"
replace nation2 = 13 if nation5 == "PT"
replace nation2 = 14 if nation5 == "DE-E"
replace nation2 = 14 if nation1 == 14
replace nation2 = 15 if nation5 == "NO" // none
replace nation2 = 16 if nation5 == "FI"
replace nation2 = 17 if nation5 == "SE"
replace nation2 = 18 if nation5 == "AT"
rename nation2 cntry
keep if cntry!=.
drop if cntry > 18
replace cntry = 9 if cntry==10

replace cntry = 14 if inlist(region_germany, 4, 8, 13, 14, 16)
label define NATION2 14 "germany (east)", modify

*-------------------------------------
*2.5. Weights
*-------------------------------------

gen tmp1 = w_all
replace tmp1 = wnation if tmp1==.

bys cntry year: egen double tmp2 = total(tmp1)
gen double weight = tmp1/tmp2

*-------------------------------------
*2.6. Save
*-------------------------------------

save "$path/eb_trend_full_clean.dta" , replace

*=====================================
*=====================================
*		THINGS TO DO FOR M1
*=====================================
*=====================================

*=====================================
*	3. Run regressions for m1
*=====================================

use "$path/eb_trend_full_clean.dta" , replace

local CNTRYS "France Belgium Netherlands Germany Italy Luxembourg Denmark Ireland UK BLUB Greece Spain Portugal Germany_East Norway Finland Sweden Austria"	 
forvalues cntry=1/18  {
	if `cntry' == 10 continue
	local CNTRY: word `cntry' of `CNTRYS' 
	dis "This is `CNTRY'"
	qui reg satislfe i.year i.sex i.year if cntry==`cntry' [pw=weight], vce(robust) // just to check overall difference
	qui eststo cntry_`CNTRY': reg satislfe i.year i.sex#i.year if cntry==`cntry'  [pw=weight], vce(robust)
	reg satislfe i.sex##c.year if cntry==`cntry'  [pw=weight], vce(robust)
	gen `CNTRY'_b_trend_d = _b[1.sex#c.year]
	gen `CNTRY'_se_trend_d = _se[1.sex#c.year]
	gen `CNTRY'_b_trend_m = _b[year]
	gen `CNTRY'_se_trend_m = _se[year] 
}

*=====================================
*	4. Get differences for m1
*=====================================

cap drop tmp 
bys cntry year sex: gen tmp = _n
keep if tmp == 1

local CNTRYS "France Belgium Netherlands Germany Italy Luxembourg Denmark Ireland UK BLUB Greece Spain Portugal Germany_East Norway Finland Sweden Austria"	 
forvalues cntry=1/18  {
	if `cntry' == 10 continue
	local CNTRY: word `cntry' of `CNTRYS' 
		
	estimates restore cntry_`CNTRY'
	sum year if e(sample)
	local startyear = r(min)
	local start2year = `startyear' + 1
	predict `CNTRY'_hat
	predict `CNTRY'_hat_se, stdp
	gen `CNTRY'_hat_l = `CNTRY'_hat - invnormal(0.975)*`CNTRY'_hat_se
	gen `CNTRY'_hat_u = `CNTRY'_hat + invnormal(0.975)*`CNTRY'_hat_se

	gen `CNTRY'_diff =	_b[1.sex#`startyear'b.year] if year==`startyear'
	gen `CNTRY'_diff_se = invnormal(0.975)*_se[1.sex#`startyear'b.year] if year==`startyear'
	forvalues year=`start2year'(1)2023 {
		cap replace `CNTRY'_diff = _b[1.sex#`year'.year] if year==`year'
		cap replace `CNTRY'_diff_se = invnormal(0.975)*_se[1.sex#`year'.year] if year==`year'	
	}
	gen `CNTRY'_diff_l = `CNTRY'_diff - invnormal(0.975)*`CNTRY'_diff_se
	gen `CNTRY'_diff_u = `CNTRY'_diff + invnormal(0.975)*`CNTRY'_diff_se	
}

*=====================================
*	5. Save for m1 for R
*=====================================

cap drop tmp 
bys year sex: gen tmp = _n
keep if tmp == 1

drop _est_cntry_France _est_cntry_Belgium _est_cntry_Netherlands _est_cntry_Germany _est_cntry_Italy _est_cntry_Luxembourg _est_cntry_Denmark _est_cntry_Ireland _est_cntry_UK _est_cntry_Greece _est_cntry_Spain _est_cntry_Portugal _est_cntry_Germany_East _est_cntry_Norway _est_cntry_Finland _est_cntry_Sweden _est_cntry_Austria nation1 cntry wsample wnation weuro satislfe happinss married educ age sizehh occup income nation3 income_q married_2 wnationGE wnationGB w_all wex wpol mastat occup_2 region_germany nation4 nation5 mastat_2 empstat

save "$path/eb_predictions_m1.dta", replace


*=====================================
*=====================================
*		THINGS TO DO FOR M3
*=====================================
*=====================================

*=====================================
*	6. Run regressions for m3
*=====================================

use "$path/eb_trend_full_clean.dta" , replace
local controls age agesq i.married i.educ i.empstat

local CNTRYS "France Belgium Netherlands Germany Italy Luxembourg Denmark Ireland UK BLUB Greece Spain Portugal Germany_East Norway Finland Sweden Austria"	 
forvalues cntry=1/18  {
	if `cntry' == 10 continue
	local CNTRY: word `cntry' of `CNTRYS' 
	dis "This is `CNTRY'"
	qui reg satislfe i.year i.sex i.year `controls' if cntry==`cntry' [pw=weight], vce(robust) // just to check overall difference
	qui eststo cntry_`CNTRY': reg satislfe i.year i.sex#i.year `controls' if cntry==`cntry'  [pw=weight], vce(robust)
	reg satislfe i.sex##c.year `controls' if cntry==`cntry'  [pw=weight], vce(robust)
	gen `CNTRY'_b_trend_d = _b[1.sex#c.year]
	gen `CNTRY'_se_trend_d = _se[1.sex#c.year]
	gen `CNTRY'_b_trend_m = _b[year]
	gen `CNTRY'_se_trend_m = _se[year] 
}

*=====================================
*	7. Get predictions for m3
*=====================================

local controls age agesq married educ empstat
foreach control of local controls {
	sum `control' [aw=weight]
	replace `control' =  r(mean)
}

cap drop tmp 
bys cntry year sex: gen tmp = _n
keep if tmp == 1

local CNTRYS "France Belgium Netherlands Germany Italy Luxembourg Denmark Ireland UK BLUB Greece Spain Portugal Germany_East Norway Finland Sweden Austria"	 
forvalues cntry=1/18  {
	if `cntry' == 10 continue
	local CNTRY: word `cntry' of `CNTRYS' 
		
	estimates restore cntry_`CNTRY'
	sum year if e(sample)
	local startyear = r(min)
	local start2year = `startyear' + 1
	predict `CNTRY'_hat
	predict `CNTRY'_hat_se, stdp
	gen `CNTRY'_hat_l = `CNTRY'_hat - invnormal(0.975)*`CNTRY'_hat_se
	gen `CNTRY'_hat_u = `CNTRY'_hat + invnormal(0.975)*`CNTRY'_hat_se

	gen `CNTRY'_diff =	_b[1.sex#`startyear'b.year] if year==`startyear'
	gen `CNTRY'_diff_se = invnormal(0.975)*_se[1.sex#`startyear'b.year] if year==`startyear'
	forvalues year=`start2year'(1)2023 {
		cap replace `CNTRY'_diff = _b[1.sex#`year'.year] if year==`year'
		cap replace `CNTRY'_diff_se = invnormal(0.975)*_se[1.sex#`year'.year] if year==`year'	
	}
	gen `CNTRY'_diff_l = `CNTRY'_diff - invnormal(0.975)*`CNTRY'_diff_se
	gen `CNTRY'_diff_u = `CNTRY'_diff + invnormal(0.975)*`CNTRY'_diff_se	
}

*=====================================
*	8. Save for m3 for R
*=====================================

cap drop tmp 
bys year sex: gen tmp = _n
keep if tmp == 1

drop _est_cntry_France _est_cntry_Belgium _est_cntry_Netherlands _est_cntry_Germany _est_cntry_Italy _est_cntry_Luxembourg _est_cntry_Denmark _est_cntry_Ireland _est_cntry_UK _est_cntry_Greece _est_cntry_Spain _est_cntry_Portugal _est_cntry_Germany_East _est_cntry_Norway _est_cntry_Finland _est_cntry_Sweden _est_cntry_Austria nation1 cntry wsample wnation weuro satislfe happinss married educ age sizehh occup income nation3 income_q married_2 wnationGE wnationGB w_all wex wpol mastat occup_2 region_germany nation4 nation5 mastat_2 empstat

save "$path/eb_predictions_m3.dta", replace


