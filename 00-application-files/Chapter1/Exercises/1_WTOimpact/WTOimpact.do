********************************************************************************
****************  CHAPTER 1 - PARTIAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

************************  EXERCISE 1: GATT/WTO EFFECTS  ************************ 

* The aim of this exercise is to assess the impact on trade of the accession to
* the GATT/WTO.    

* Data source: The database reports bilateral trade, including international and
*              intra-national trade, at the aggregated manufacturing level for 
*              69 countries for the period 1986-2006, provided by Thomas Zylkin,
*              based on UN COMTRADE, CEPII TradeProd and UN UNIDO INDSTAT 
*              databases. Information on RTAs come from Mario Larch's Regional 
*			   Trade Agreements Database (http://www.ewf.uni-bayreuth.de/en/
*			   research/RTA-data/index.html). Standard gravity variables such as
*			   distance, continuous borders, and common language, are taken from 
*              the CEPII GeoDist database.


******************************* PRELIMINARY STEP *******************************

* IMPORTANT: The "directory_definition.do" do-files has to be executed before
*            running this do-file.
*            do "directory_definition.do"

* Clear memory and set parameters
	clear all
	set more off
	clear matrix
	set memory 500m
	set matsize 8000
	set maxvar 30000
	
* Set directory path, where "$input" refers to the path of the main folder 
* "Practical Guide to Gravity"	
	cd "$input/Chapter1"	
		
* Close and create log	
	capture log close
	log using "Exercises\2_UnilateralPolicyImpact/Results/UnilateralPolicyImpact.log", text replace


* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the ppml_panel_sg command if necessary	
	* ssc install ppml_panel_sg

* Install or update the hdfe command used by the ppml_panel_sg command if necessary	
	* ssc install hdfe

* Install or update the esttab command if necessary
	* findit esttab
	
	
****************************** (i) PRELIMINARIES *******************************

* a. Open the data file 
	use "Datasets/Chapter1Exercise1.dta", clear

* b. Create a histogram reporting the frequency of the number of countries 
*    members of the GATT/WTO by year of accession.
		drop if WTO == 0			
		collapse(min) year, by(exporter WTO)
		histogram year, frequency ytitle(Frequency) xtitle(Year) title(Histogram of country members of the GATT/WTO)


********************** (ii) Benchmark gravity estimation ***********************

* a. Generate exporter-time and importer-time effects

	use "Datasets/Chapter1Exercise1.dta", clear

	* Create exporter-time fixed effects
		egen exp_time = group(exporter year)
		quietly tabulate exp_time, gen(EXPORTER_TIME_FE)

	* Create importer-time fixed effects
		egen imp_time = group(importer year)
		quietly tabulate imp_time, gen(IMPORTER_TIME_FE)

* b. Estimate the following standard gravity specification by considering only 
*    international trade flows (i.e. for i=/j ) and applying the OLS estimator:
*    Xij = pi_it + xsi_jt + b*GRAVITY + b5*RTA + b6*WTO

	* Create the symmetric pair id
		egen pair_id = group(DIST)

	* Alternatively create asymmetric pair id
	*	egen pair_id = group(exporter importer)
		
	* Create the log of distance
		gen ln_DIST = ln(DIST)
	
	* Estimate the gravity model with the OLS estimator and store the results
 		regress trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY RTA WTO if exporter != importer, cluster(pair_id)
			estimates store ols		

* c. Re-estimate the same specification expressed in multiplicative form with 
*    the PPML estimator. 

	* Estimate the gravity model with the PPML estimator and store the results
 		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY RTA WTO if exporter != importer, cluster(pair_id)
			estimates store ppml		


********* (iii) Gravity estimation accounting for intra-national trade *********

* a. Generate pair fixed effects

	* Create intra-trade pair fixed effects
		generate intrapair_id = 0
			replace intrapair_id = pair_id if exporter == importer
		quietly tabulate intrapair_id, gen(INTRAPAIR_FE)
		
	* Create pair fixed effects
		quietly tabulate pair_id, gen(PAIR_FE)	
		
* b. Re-estimate the specification with the PPML estimator but this time by 
*    considering international and intra-national trade. 

	* Estimate the gravity model with the PPML estimator and store the results
		ppml trade  INTRAPAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY RTA WTO, cluster(pair_id)
			estimates store intra		

* c. In order to correct for potential endogeneity of the RTAs variable, 
*    estimate the following gravity specification with the PPML estimator			
*    Xij = pi_it + xsi_jt + mu_ij + b*GRAVITY + b5*RTA + b6*WTO

	* Estimate the gravity model with the PPML estimator and store the results
 		ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA WTO, cluster(pair_id)
	* Alternative faster PPML command available (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
	*	ppml_panel_sg trade RTA WTO, ex(exporter) im(importer) y(year) sym cluster(pair_id) 	
			estimates store ppml_fes

************* (iv) Gravity estimation accounting for globalization *************

* a. Generate international border dummies for the years 1986, 1990, 1994, 1998 
*    and 2002.
	
	* Create the international border dummy variable
		gen INTL_BRDR = 1 if exporter ! = importer
			replace INTL_BRDR = 0 if INTL_BRDR == .
			
	* Create an international border dummy variable for each year
		forvalues i = 1986(1)2006{
			generate INTL_BRDR_`i' = INTL_BRDR if year == `i'
				replace INTL_BRDR_`i' = 0 if INTL_BRDR_`i' == .
		}	
		
* b. Estimate with the PPML estimator the following structural gravity 
*    specification:	
*    Xij = pi_it + xsi_jt + mu_ij + b*GRAVITY + b5*RTA + b6*WTO + 
*          b7*INT_BRDR_1986 + b8*INT_BRDR_1990 + b9*INT_BRDR_1994 +
*          b10*INT_BRDR_1998 + b11*INT_BRDR_2002

	* Estimate the gravity model with the PPML estimator and store the results
 		ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA WTO INTL_BRDR_1986 INTL_BRDR_1990 INTL_BRDR_1994 INTL_BRDR_1998 INTL_BRDR_2002, cluster(pair_id)
	* Alternative faster PPML command (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
	* 	ppml_panel_sg trade RTA WTO INTL_BRDR_1986 INTL_BRDR_1990 INTL_BRDR_1994 INTL_BRDR_1998 INTL_BRDR_2002, ex(exporter) im(importer) y(year) sym cluster(pair_id) 	
			estimates store ppml_glbzn

	
******************************* EXPORT ESTIMATES *******************************

	* Export estimates in text format
		esttab ols ppml intra ppml_fes ppml_glbzn using "Exercises/1_WTOimpact/Results/WTOmpact.txt", append title(Estimating The Effects of WTO) mtitles(OLS PPML INTRA PAIR_FEs GLBZN) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* IMPORTER_TIME_FE*) staraux nogaps 

	* Export estimates in Excel format
		esttab ols ppml intra ppml_fes ppml_glbzn using "Exercises/1_WTOimpact/Results/WTOImpact.csv", append title(Estimating The Effects of WTO) mtitles(OLS PPML INTRA PAIR_FEs GLBZN) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* IMPORTER_TIME_FE*) staraux nogaps 

