* Note: activate this do file from within Stata
clear
set more off
cd "/Users/your_repository/Documents/TL_dementia/"

use "/Users/your_repository/Documents/TL_dementia/HC16/HC16sta/hc16hp_r.dta"

keep hhid pn R1HCAPDX HCAP16WGTR 

save "HCAP_diagnosis.dta", replace



