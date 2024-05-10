
clear all
set maxvar 120000
query memory

*Designate your own repository place.

cd "/Users/your_repository/Documents/TL_dementia/"
use "/Users/your_repository/Documents/TL_dementia/randhrs1992_2018v1_archive_STATA/randhrs1992_2018v1.dta"

*Pick variables of interest.

keep hhid pn ragender raracem rahispan raedyrs raedegrm rabyear inw* r*agey_e r*adla r*iadlza r*proxy r*bwc20 r*ser7 r*aimr10 r*adlr10 r*imrc r*dlrc r*mo r*dy r*yr r*dw r*cact r*scis r*pres r*vp r*wtcrnh 

reshape long inw@ r@agey_e r@adla r@iadlza r@proxy r@bwc20 r@ser7 r@aimr10 r@adlr10 r@imrc  r@dlrc r@mo r@dy r@yr r@dw r@cact r@scis r@pres r@vp r@wtcrnh, i(hhid pn ragender raracem rahispan raedyrs raedegrm rabyear) j(wave)

save RAND_HRS
