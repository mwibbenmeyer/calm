/* Programmer: Alexandra Thompson
Start Date: November 25, 2020
Objective: Explore NASS data
Variable of interest: survey-economics-expenses-rent: rent, cash, pastureland, expense, measured in $/acre
*/

********************************************************************************
************SETUP************
********************************************************************************
set more off
clear

* working dir
global workingdir "M:\GitRepos\land-use"
cd $workingdir

********************************************************************************
************EXPLORE DATA AVAILABILITY************
********************************************************************************
use processing\NASS\pasturerents, clear
ta pasture_nr_level, sort

/** tag years of intersest
gen tag = year == 1991 ///
		| year == 1992 ///
		| year == 1993 ///
		| year == 1996 ///
		| year == 1997 ///
		| year == 1998 ///
		| year == 2001 ///
		| year == 2002 ///
		| year == 2003 ///
		| year == 2006 ///
		| year == 2007 ///
		| year == 2008 ///
		| year == 2011 ///
		| year == 2012 ///
		| year == 2013 ///
		| year == 2016 ///
		| year == 2017 ///
		| year == 2018
*/
* pie graphs
	use processing\NASS\pasturerents, clear
	/*graph pie, over(pasture_nr_level) sort(pasture_nr_level) by(, title(test) subtitle(test) caption(test) note(test)) by(year, total)*/
	graph pie, over(pasture_nr_level) ///
		sort(pasture_nr_level) ///
		by(, title(County pasture_nr Value Data Level) ///
		caption(Precision: county > Ag.StatisticsDistrict(asd)/othercombcounties > state > multistate > nodata)) ///
		by(year, total)
		gr_edit legend.Edit , style(cols(3)) style(rows(0)) style(key_ysize(small)) keepstyles 
		gr_edit legend.Edit, style(labelstyle(size(small)))
		gr_edit note.draw_view.setstyle, style(no)
		gr_edit note.fill_if_undrawn.setstyle, style(no)
		gr_edit caption.style.editstyle size(small) editcopy
		gr_edit title.style.editstyle size(medium) editcopy
		gr_edit subtitle.style.editstyle size(small) editcopy
		qui graph export results\initial_descriptives\NASS\pasturenr_datalevel_year_pie.png, replace

* maps
	* mapping setup
	ssc install maptile
	ssc install spmap
	ssc install shp2dta
	maptile_install using "http://files.michaelstepner.com/geo_county2010.zip"
	capture net install grc1leg2.pkg
	* load dataset
	use processing\NASS\pasturerents, clear
	rename fips county
	keep county pasture_nr_level year
	* replace pasture_nr_level = "." if pasture_nr_level == "nodata"
	* encode pasture_nr_level /*if pasture_nr_level != "nodata", gen(level)*/
	encode pasture_nr_level, gen(level)
	* replace level = . if pasture_nr_level == "nodata"
	gen leveln = level
	* colorpalette
	local color2 navy
	local color3 maroon
	local color4 forest_green
	local color5 dkorange
	local color6 teal
	local color7 cranberry
	* map
	levelsof year, local(levels)
	foreach y of local levels {
	maptile level if year == `y', geo(county2010) cutvalues(1(1)6) /*fcolor(Rainbow)*/ ///
		fcolor(""`color2'" "`color3'" "`color4'" "`color5'" "`color6'" "`color7'"") ///
		twopt( ///
		legend(order(2 3 4 5 6 7) lab(2 "Ag. Stat. Dist.") lab(3 "County") ///
		lab(4 "Multistate") lab(5 "No Data") lab(6 "Other Comb. Counties") lab(7 "State")))
	gr_edit title.text.Arrpush "`y'"
	* pause
	graph save processing\NASS\tempgraphs\pasturenr_datalevel_`y'_map, replace
	}

grc1leg2 ///
		processing\NASS\tempgraphs\pasturenr_datalevel_1994_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_1995_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_1996_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_1997_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_1998_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_1999_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2000_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2001_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2002_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2003_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2004_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2005_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2006_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2007_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2008_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2009_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2010_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2011_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2012_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2013_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2014_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2015_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2016_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2017_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2018_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2019_map.gph ///
		processing\NASS\tempgraphs\pasturenr_datalevel_2020_map.gph, ///
		title(County pasture_nr Value Data Level)
		gr_edit legend.Edit , style(rows(1)) style(cols(0)) keepstyles 
		gr_edit style.editstyle boxstyle(shadestyle(color(white))) editcopy
		gr_edit style.editstyle boxstyle(linestyle(color(white))) editcopy
		gr_edit legend.Edit, style(labelstyle(size(small)))
		* pause
		graph export "results\initial_descriptives\NASS\pasturenr_datalevel_year_map.png", replace

/* export legend (no longer needed, labeled in maps)
keep *level*
duplicates drop
drop level
order leveln pasture*
sort leveln
export delimited results\initial_descriptives\NASS\pasturenr_datalevel_year_map_LEGEND.csv, replace*/


********************************************************************************
************EXPLORE DATA VALUES************
********************************************************************************
* box plots
	use processing\NASS\pasturerents, clear
	graph box pasture_nr, by(year, total)
	gr_edit l1title.style.editstyle size(small) editcopy
	graph export "results\initial_descriptives\NASS\pasture_nr_boxplots.png", replace

* maps
	* mapping setup
	ssc install maptile
	ssc install spmap
	ssc install shp2dta
	maptile_install using "http://files.michaelstepner.com/geo_county2010.zip"
	capture net install grc1leg2.pkg
	
	* load dataset
	use processing\NASS\pasturerents, clear
	rename fips county
	keep county year pasture_nr

	* calculate percentile breaks for all values
	pctile pasturenr_nq5breaks = pasture_nr, nq(5)
	pctile pasturenr_nq6breaks = pasture_nr, nq(6)
	pctile pasturenr_nq9breaks = pasture_nr, nq(9)
	
	* generate temp min & max values for legend: year is arbitrary. just want values that capture full range of values.
	egen pasture_nr_min = min(pasture_nr)
	egen pasture_nr_max = max(pasture_nr)
	gen pasture_nr_display = pasture_nr if year == 2012
	egen pasture_nr_min_2012 = min(pasture_nr) if year == 2012
	replace pasture_nr_display = pasture_nr_min if pasture_nr_display == pasture_nr_min_2012 & year == 2012
	egen pasture_nr_max_2012 = max(pasture_nr) if year == 2012
	replace pasture_nr_display = pasture_nr_max if pasture_nr_display == pasture_nr_max_2012 & year == 2012
	drop *max* *min*

	levelsof year, local(levels)
		foreach y of local levels {
			maptile pasture_nr if year == `y', geo(county2010) cutp(pasturenr_nq9breaks ) fcolor(Greens)
			gr_edit subtitle.text.Arrpush "`y'"
			* pause
			graph save "processing\NASS\tempgraphs/pasture_nr_`y'", replace
		}
	maptile pasture_nr_display if year == 2012, geo(county2010) cutp(pasturenr_nq9breaks ) fcolor(Greens) // mean county-level value map for legend
	graph save "processing\NASS\tempgraphs\pasture_nr", replace

	* combine, save graphs
	cd "$workingdir\processing\NASS\tempgraphs"
	grc1leg2 ///
		pasture_nr_1994.gph ///
		pasture_nr_1995.gph ///
		pasture_nr_1996.gph ///
		pasture_nr_1997.gph ///
		pasture_nr_1998.gph ///
		pasture_nr_1999.gph ///
		pasture_nr_2000.gph ///
		pasture_nr_2001.gph ///
		pasture_nr_2002.gph ///
		pasture_nr_2003.gph ///
		pasture_nr_2004.gph ///
		pasture_nr_2005.gph ///
		pasture_nr_2006.gph ///
		pasture_nr_2007.gph ///
		pasture_nr_2008.gph ///
		pasture_nr_2009.gph ///
		pasture_nr_2010.gph ///
		pasture_nr_2011.gph ///
		pasture_nr_2012.gph ///
		pasture_nr_2013.gph ///
		pasture_nr_2014.gph ///
		pasture_nr_2015.gph ///
		pasture_nr_2016.gph ///
		pasture_nr_2017.gph ///
		pasture_nr_2018.gph ///
		pasture_nr_2019.gph ///
		pasture_nr_2020.gph ///
		pasture_nr.gph, ///
		legendfrom(pasture_nr.gph)
	gr_edit title.text.Arrpush "pasture_nr"
	gr_edit plotregion1.graph28.draw_view.setstyle, style(no) // hide map created for legend only
	gr_edit style.editstyle boxstyle(shadestyle(color(white))) editcopy
	gr_edit style.editstyle boxstyle(linestyle(color(white))) editcopy
	gr_edit legend.Edit , style(rows(2)) style(cols(0)) keepstyles 
	gr_edit legend.Edit, style(labelstyle(size(small)))

	cd $workingdir
	graph export "results\initial_descriptives\NASS\pasture_nr_maps.png", replace


**********
*********** SUMMARY STATISTICS ***********
**********
use processing\NASS\pasturerents, clear

* generate year-level values
levelsof year, local(levels)
	foreach y of local levels {
		gen pasturenr_`y' = pasture_nr if year == `y'
	}

* summary statistics tables
estpost summarize pasturenr*
esttab using "results\initial_descriptives\NASS\pasture_nr.rtf", cells("count(fmt(%12.0fc)) mean(fmt(%12.1fc)) sd(fmt(%12.1fc)) min(fmt(%12.1fc)) max(fmt(%12.1fc))") replace	

**********
*********** CLEAN UP ***********
**********
cd $workingdir
cd processing\NASS\tempgraphs
forvalues y = 1994/2020 {
erase pasture_nr_`y'.gph
}
erase pasture_nr.gph
forvalues y = 1994/2020 {
erase pasturenr_datalevel_`y'_map.gph
}
cd $workingdir
