********************************************************************************
****************  CHAPTER 1 - PARTIAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

**************  APPLICATION 3: REGIONAL TRADE AGREEMENTS EFFECTS  ************** 

* This application aims at estimating the effects of RTAs on trade.  

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
	log using "Applications/3_RTAsEffects/Results/RTAsEffects.log", text replace


* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the ppml_panel_sg command if necessary	
	* ssc install ppml_panel_sg

* Install or update the hdfe command used by the ppml_panel_sg command if necessary	
	* ssc install hdfe

* Install or update the esttab command if necessary
	* findit esttab
	
	
************************* OPEN AND MANAGE THE DATABASE *************************
* Open the database according to the Stata version you are using
	use "Datasets/Chapter1Application3.dta", clear
	
* The analysis considers panel data with 4 year interval (1986, 1990, ..., 2006)
		keep if year == 1986 | year == 1990 | year == 1994 | year == 1998 | year == 2002 | year == 2006 

* Create the dependent variable	
		generate ln_trade = ln(trade)

* Create the log of distance variable 		
		generate ln_DIST = ln(DIST)

* Create exporter-time fixed effects
		egen exp_time = group(exporter year)
			quietly tabulate exp_time, gen(EXPORTER_TIME_FE)

* Create importer-time fixed effects
		egen imp_time = group(importer year)
			quietly tabulate imp_time, gen(IMPORTER_TIME_FE)

* Create pair fixed effects
		summarize pair_id
			replace pair_id = pair_id + 100000 if exporter == importer
			quietly tabulate pair_id, gen(PAIR_FE)
		
		
*********************************** ANALYSIS ***********************************

*********** (a) Standard RTA estimates with international trade only ***********

* Estimate the gravity model (1-34) with the OLS estimator
		regress ln_trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY RTA if exporter != importer, cluster(pair_id)

* Store the results
			estimates store ols

* Estimate the gravity model (1-35) with the PPML estimator and store the results
		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY RTA if exporter != importer, cluster(pair_id)
			estimates store ppml


************** (b)	Addressing potential domestic trade diversion **************

* Estimate the gravity model (1-35) with the PPML estimator by considering
* intra-national trade as well as international trade and store the results
		ppml trade PAIR_FE2347-PAIR_FE2415 EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY RTA, cluster(pair_id)
			estimates store ppml_intra


***************** (c) Addressing potential endogeneity of RTAs *****************

* Estimate the gravity model (1-36) with the PPML estimator by applying the
* average treatment effects and store the results
		ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA, cluster(pair_id) 
			estimates store ppml_bb
* Alternative faster PPML command available (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
	*	ppml_panel_sg trade RTA, ex(exporter) im(importer) y(year) sym cluster(pair_id) 
			parmest, saving(pair_fes, replace)


***** (d) Testing for potential "reverse causality" between trade and RTAs *****

* Estimate the gravity model (1-37) with the PPML estimator and store the results
		ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA RTA_LEAD4, cluster(pair_id)
			estimates store ppml_lead
* Alternative faster PPML command (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
	*	ppml_panel_sg trade RTA RTA_LEAD4, ex(exporter) im(importer) y(year) sym cluster(pair_id) 			
			

****** (e) Addressing potential non-linear and phasing-in effects of RTAs ******

* Estimate the gravity model (1-38) with the PPML estimator and store the results
		ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA RTA_LAG4 RTA_LAG8 RTA_LAG12, cluster(pair_id)
			estimates store ppml_lags
* Alternative faster PPML command (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
	*	ppml_panel_sg trade RTA RTA_LAG4 RTA_LAG8 RTA_LAG12, ex(exporter) im(importer) y(year) sym cluster(pair_id) 
			
* Compute the total RTA effects and associated standard errors with delta method		
			lincom _b[RTA]+_b[RTA_LAG4]+_b[RTA_LAG8]+_b[RTA_LAG12]


********************* (f) Addressing globalization effects *********************

* Create border variables for each year
		generate INTL_BRDR = 1 if exporter != importer
			replace INTL_BRDR = 0 if INTL_BRDR == .
		forvalues i = 1986(4)2006{
			generate INTL_BRDR_`i' = 1 if INTL_BRDR == 1 & year == `i'
				replace INTL_BRDR_`i' = 0 if INTL_BRDR_`i' == .
		}

* Estimate the gravity model (1-39) with the PPML estimator 
		ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA RTA_LAG4 RTA_LAG8 RTA_LAG12 INTL_BRDR_1986 INTL_BRDR_1990 INTL_BRDR_1994 INTL_BRDR_1998 INTL_BRDR_2002 , cluster(pair_id)
			estimates store ppml_bly
* Alternative faster PPML command (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
	*	ppml_panel_sg trade RTA RTA_LAG4 RTA_LAG8 RTA_LAG12 INTL_BRDR_1986 INTL_BRDR_1990 INTL_BRDR_1994 INTL_BRDR_1998 INTL_BRDR_2002, ex(exporter) im(importer) y(year) sym cluster(pair_id) 
			
* Compute the total RTA effects and associated standard errors with delta method		
			lincom _b[RTA]+_b[RTA_LAG4]+_b[RTA_LAG8]+_b[RTA_LAG12]


******************************* EXPORT ESTIMATES *******************************

* Export estimates in text format
		esttab ols ppml ppml_intra ppml_bb ppml_lead ppml_lags ppml_bly using "Applications/3_RTAsEffects/Results/RTAsEffects.rtf", append title(Estimating the Effects of Regional Trade Agreements) mtitles(OLS PPML INTRA PAIRFE LEAD PHSNG GLOB) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE*) staraux nogaps

* Export estimates in Excel format
		esttab ols ppml ppml_intra ppml_bb ppml_lead ppml_lags ppml_bly using "Applications/3_RTAsEffects/Results/RTAsEffects.csv", append title(Estimating the Effects of Regional Trade Agreements) mtitles(OLS PPML INTRA PAIRFE LEAD PHSNG GLOB) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE*) staraux nogaps
		
* Save the data used for this application 
	save "Datasets/3_RTAsEffects.dta", replace

