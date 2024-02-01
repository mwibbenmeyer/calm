
ssc install maptile
ssc install spmap
ssc install shp2dta
maptile_install using "http://files.michaelstepner.com/geo_county2010.zip"
capture net install grc1leg2.pkg




use "M:\GitRepos\land-use\processing\combined\countypanel.dta", clear

gen PastureRange_pcnt = Pastureland_pcnt + Rangeland_pcnt
gen PastureRange_ratio = Pastureland_pcnt / (Rangeland_pcnt + 0.00000001)
gsort - PastureRange_pcnt
order PastureRange_pcnt Pastureland_pcnt Rangeland_pcnt *ratio
su PastureRange_pcnt Pastureland_pcnt Rangeland_pcnt PastureRange_ratio
gsort - PastureRange_ratio

*collapse(mean) pasture_nr PastureRange_pcnt Pastureland_pcnt Rangeland_pcnt PastureRange_ratio, by (fips)
rename fips county
maptile PastureRange_ratio if year == 2012, geo(county2010)
maptile pasture_nr if year == 2012, geo(county2010)



use "M:\GitRepos\land-use\processing\combined\countypanel.dta", clear

gen PastureRange_pcnt = Pastureland_pcnt + Rangeland_pcnt
gen PastureRange_ratio = Pastureland_pcnt / (Rangeland_pcnt + 0.00000001)
gsort - PastureRange_pcnt
order PastureRange_pcnt Pastureland_pcnt Rangeland_pcnt *ratio
su PastureRange_pcnt Pastureland_pcnt Rangeland_pcnt PastureRange_ratio
gsort - PastureRange_ratio

levelsof year, local(years)
foreach y of local years {
di `y'
scatter PastureRange_ratio pasture_nr if year == `y'
pause
}
