* Note: activate this do file from within Stata
clear
set more off

cd "/Users/your_repository/Documents/Second_paper/HCAP code"


*Unzip the data before using them.
global path "/Users/your_repository/Documents/Second_paper/HCAP code"
infile using "$path/h14core/h14sta/H14D_R.dct", using("$path/h14core/h14da/H14D_R.da")
gen HHIDPN = HHID + PN
destring HHIDPN, replace
save "core2014.dta", replace
clear

clear
set more off

cd "/Users/your_repository/Documents/Second_paper/HCAP code"

*Unzip the data before using them.
global path "/Users/your_repository/Documents/Second_paper/HCAP code"
infile using "$path/h16core/h16sta/H16D_R.dct", using("$path/h16core/h16da/H16D_R.da")
gen HHIDPN = HHID + PN
destring HHIDPN, replace
save "core2016.dta", replace
clear

