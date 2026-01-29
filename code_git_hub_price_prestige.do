/*==============================================================================
The Price of Prestige: Fees and Rankings in European Higher Education 
 
Descriptive analysis
Correlation and Regression analysis 
 
Author Rosario Scandurra 
==============================================================================*/

*reading data

import delimited "C:\Users\Rosario.Scandurra\Dropbox\DO FILE STATA\0_RICERCA FINITA\2022_rankings\hei_price_prestige.csv", clear

sum

***deriving controls for regression cnt, mean disposable income of the region, stud_enrolled
gen th_stud=stud_enrolled/1000
egen ln_stud=std(th_stud)
gen income_th=median_income_imputed/1000
encode cnt, gen (country)
egen std_income=std(income_th)

*****labeling variable****
lab var ln_stud "Total Student ISCED 5-7 (z-score)"
lab var std_income "HEI reg. median income (z-score)"
lab var country "Country of the HEI"
lab var pub "Private university"
lab var rank_fees "HEI fees, rank"
lab var glob_rank "HEI rank ARWU & THES"
lab var arwu_rank "HEI rank ARWU"
lab var thes_rank "HEI rank ARWU"

*tables
tabstat glob_rank arwu_rank thes_rank rank_fees ln_stud std_income, stats(n median min max sd var)

graph twoway scatter glob_rank rank_fees , mlabel(englishinstitutionname)mlabsize(tiny) mlabposition(6) msymbol(p)

*graph export "XXXXXXXXXXXXXXXX", as(png) replace


* ============================================================
* CORRELATION TABLE: PEARSON, SPEARMAN, AND SAVAGE SCORES
* ============================================================

* Define variables for correlation analysis
local rankvars glob_rank arwu_rank thes_rank rank_fees

* --- Pearson correlations ---
quietly correlate `rankvars'
matrix Pearson = r(C)
matrix rownames Pearson = `rankvars'
matrix colnames Pearson = `rankvars'

* --- Spearman correlations ---
spearman `rankvars', stats(rho) matrix
matrix Spearman = r(Rho)

* --- Savage scores correlations ---
* Create Savage scores for each variable
foreach v of local rankvars {
    quietly count if !missing(`v')
    local n = r(N)
    egen temprank_`v' = rank(`v'), unique
    gen savage_`v' = .
    forvalues i = 1/`n' {
        local savage = 0
        forvalues j = `i'/`n' {
            local savage = `savage' + 1/`j'
        }
        quietly replace savage_`v' = `savage' if temprank_`v' == `i'
    }
    drop temprank_`v'
}

quietly correlate savage_*
matrix Savage = r(C)
matrix rownames Savage = `rankvars'
matrix colnames Savage = `rankvars'

* --- Display correlation matrices ---
di _n "{hline 60}"
di "PEARSON CORRELATIONS"
di "{hline 60}"
matrix list Pearson, format(%6.3f) noheader

di _n "{hline 60}"
di "SPEARMAN RANK CORRELATIONS"
di "{hline 60}"
matrix list Spearman, format(%6.3f) noheader

di _n "{hline 60}"
di "SAVAGE SCORE CORRELATIONS"
di "{hline 60}"
matrix list Savage, format(%6.3f) noheader

* --- Export to Excel ---
putexcel set "correlation_table.xlsx", replace
putexcel A1 = "Pearson Correlations"
putexcel A2 = matrix(Pearson), names nformat("0.000")
putexcel A8 = "Spearman Correlations"  
putexcel A9 = matrix(Spearman), names nformat("0.000")
putexcel A15 = "Savage Score Correlations"
putexcel A16 = matrix(Savage), names nformat("0.000")

* ============================================================
* REGRESSION ANALYSIS WITH MULTIPLE RANKINGS
* ============================================================

eststo clear

* Global rank models
eststo glob1: reg glob_rank rank_fees, robust
estadd local cfe "No", replace: glob1
eststo glob2: reg glob_rank rank_fees i.country, robust
estadd local cfe "Yes", replace: glob2
eststo glob3: reg glob_rank rank_fees i.country ln_stud c.std_income, robust
estadd local cfe "Yes", replace: glob3

* ARWU rank models
eststo arwu1: reg arwu_rank rank_fees, robust
estadd local cfe "No", replace: arwu1
eststo arwu2: reg arwu_rank rank_fees i.country, robust
estadd local cfe "Yes", replace: arwu2
eststo arwu3: reg arwu_rank rank_fees i.country ln_stud c.std_income, robust
estadd local cfe "Yes", replace: arwu3

* THE rank models
eststo thes1: reg thes_rank rank_fees, robust
estadd local cfe "No", replace: thes1
eststo thes2: reg thes_rank rank_fees i.country, robust
estadd local cfe "Yes", replace: thes2
eststo thes3: reg thes_rank rank_fees i.country ln_stud c.std_income, robust
estadd local cfe "Yes", replace: thes3

* Export regression table
esttab glob* arwu* thes* using "regression_table.rtf", replace ///
    label ///
    b(3) se(3) star(* 0.10 ** 0.05 *** 0.01) ///
    keep(rank_fees ln_stud std_income _cons) ///
    order(rank_fees ln_stud std_income _cons) ///
    title("University Rankings and Fees") ///
    mtitles("Global" "Global" "Global" "ARWU" "ARWU" "ARWU" "THE" "THE" "THE") ///
    scalars("cfe Country FE" "r2 R-squared" "N Observations") ///
    note("Robust standard errors in parentheses. * p<0.10, ** p<0.05, *** p<0.01")
  
* Add beta option to esttab
esttab glob* arwu* thes* using "regression_table_beta.rtf", replace ///
    label beta ///
    star(* 0.10 ** 0.05 *** 0.01) ///
    keep(rank_fees ln_stud std_income _cons) ///
    order(rank_fees ln_stud std_income _cons) ///
    title("University Rankings and Fees") ///
    mtitles("Global" "Global" "Global" "ARWU" "ARWU" "ARWU" "THE" "THE" "THE") ///
    scalars("cfe Country FE" "r2 R-squared" "N Observations") ///
    note("Standardized beta coefficients. * p<0.10, ** p<0.05, *** p<0.01")	


* ============================================================
* BOOTSTRAP REGRESSION WITH BCa INTERVALS - GLOBAL RANKING
* ============================================================

eststo clear

* Model 1: Baseline
eststo m1: bootstrap, reps(1000) seed(12345) bca nodots: reg glob_rank rank_fees
estat bootstrap, bca
matrix ci1 = e(ci_bca)

* Model 2: Country FE
eststo m2: bootstrap, reps(1000) seed(12345) bca nodots: reg glob_rank rank_fees i.country
estat bootstrap, bca
matrix ci2 = e(ci_bca)

* Model 3: +Size
eststo m3: bootstrap, reps(1000) seed(12345) bca nodots: reg glob_rank rank_fees i.country ln_stud 
estat bootstrap, bca
matrix ci3 = e(ci_bca)

* Model 4: Full model
eststo m4: bootstrap, reps(1000) seed(12345) bca nodots: reg glob_rank rank_fees i.country ln_stud std_income
estat bootstrap, bca
matrix ci4 = e(ci_bca)

* Export table with BCa confidence intervals
esttab m1 m2 m3 m4 using "bootstrap_results.rtf", replace ///
    label ///
    b(3) ci(3) ///
    keep(rank_fees ln_stud std_income _cons) ///
    order(rank_fees ln_stud std_income _cons) ///
    title("Table X. Bootstrap Regression Results: Global University Rankings") ///
    mtitles("Model 1" "Model 2" "Model 3" "Model 4") ///
    nonumbers ///
    stats(N r2, labels("Observations" "R-squared") fmt(0 3)) ///
    addnotes("Bias-corrected accelerated (BCa) 95% confidence intervals in brackets." ///
             "Bootstrap replications: 1,000." ///
             "Country fixed effects included in Models 2, 3 and 4.")