********************************************************************************
****************  CHAPTER 1 - PARTIAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

***************  APPLICATION 2: THE "DISTANCE PUZZLE" RESOLVED  **************** 

* This application applies the methods developed by Yotov (2012) in order to 
* solve the "distance puzzle" of trade by controlling for internal trade and 
* internal distance   

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
	log using "Applications/2_DistancePuzzle/Results/DistancePuzzle.log", text replace


* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the esttab command if necessary
	* findit esttab
	
	
************************* OPEN AND MANAGE THE DATABASE *************************
* Open the database according to the Stata version you are using
	use "Datasets/Chapter1Application2.dta", clear
	
* The analysis considers panel data with 4 year interval (1986, 1990, ..., 2006)
		keep if year == 1986 | year == 1990 | year == 1994 | year == 1998 | year == 2002 | year == 2006 

* Create the dependent variable	
		generate ln_trade = ln(trade)

* Create the log of distance variable for each year		
		generate ln_DIST = ln(DIST)

		forvalues i = 1986(1)2006{
			generate ln_DIST_`i' = ln_DIST if year == `i'
				replace ln_DIST_`i' = 0 if ln_DIST_`i' == .
		}		
		
* Create exporter-time fixed effects
		egen exp_time = group(exporter year)
			quietly tabulate exp_time, gen(EXPORTER_TIME_FE)

* Create importer-time fixed effects
		egen imp_time = group(importer year)
			quietly tabulate imp_time, gen(IMPORTER_TIME_FE)

		
*********************************** ANALYSIS ***********************************

********************* (a) Uncovering the "distance puzzle" ********************* 

* Estimate the gravity model (1-29) with the OLS estimator
		regress ln_trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST_1986 ln_DIST_1990 ln_DIST_1994 ln_DIST_1998 ln_DIST_2002 ln_DIST_2006 CNTG LANG CLNY if exporter != importer, cluster(pair_id)

* Store the results
			estimates store dist_puzzl_ols
		
* Compute the percentage change in the estimates of the effects of bilateral
* distance between 1986 and 2006, and associated standard errors
			nlcom 100*(_b[ln_DIST_2006] - _b[ln_DIST_1986]) / _b[ln_DIST_1986]

* Estimate the gravity model (1-30) with the PPML estimator and store the results
		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST_1986 ln_DIST_1990 ln_DIST_1994 ln_DIST_1998 ln_DIST_2002 ln_DIST_2006 CNTG LANG CLNY if exporter != importer, cluster(pair_id)
			estimates store dist_puzzl_ppml

* Compute the percentage change in the estimates of the effects of bilateral
* distance between 1986 and 2006, and associated standard errors 
* Note that nlcom is very slow and consider commenting it out for rest of analysis
			nlcom 100*(_b[ln_DIST_2006] - _b[ln_DIST_1986]) / _b[ln_DIST_1986]


********************** (b) Solving the "distance puzzle" *********************** 

*** Accounting for internal distance

* Create a dummy variable for intra-national trade
		generate SMCTRY = 1 if exporter == importer
			replace SMCTRY = 0 if SMCTRY == .

* Create a variable for the log of internal distance
		generate ln_DIST_INTRA = ln_DIST*SMCTRY

* Re-adjust the logarithm of the distance for each year by removing the internal
* distance
		forvalues i = 1986(1)2006{
			replace ln_DIST_`i' = 0 if SMCTRY == 1
		}

* Estimate the gravity model (1-31) with the PPML estimator and store the results
		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST_1986 ln_DIST_1990 ln_DIST_1994 ln_DIST_1998 ln_DIST_2002 ln_DIST_2006 CNTG LANG CLNY ln_DIST_INTRA, cluster(pair_id)
			estimates store dist_puzzl1		

* Compute the percentage change in the estimates of the effects of bilateral
* distance between 1986 and 2006, and associated standard errors with delta methods
* Note that nlcom is very slow and consider commenting it out for rest of analysis
			nlcom 100*(_b[ln_DIST_2006] - _b[ln_DIST_1986]) / _b[ln_DIST_1986]


*** Accounting for internal distance and home bias

* Estimate the gravity model (1-32) with the PPML estimator and store the results
		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST_1986 ln_DIST_1990 ln_DIST_1994 ln_DIST_1998 ln_DIST_2002 ln_DIST_2006 CNTG LANG CLNY ln_DIST_INTRA SMCTRY, cluster(pair_id)
			estimates store dist_puzzl2

* Compute the percentage change in the estimates of the effects of bilateral
* distance between 1986 and 2006, and associated standard errors 
* Note that nlcom is very slow and consider commenting it out for rest of analysis
			nlcom 100*(_b[ln_DIST_2006] - _b[ln_DIST_1986]) / _b[ln_DIST_1986]


*** Accounting for country-sepcific fixed effects

* Create pair fixed effects
		egen intra_pair = group(exporter) if exporter == importer
			replace intra_pair = 0 if intra_pair == .
			quietly tabulate intra_pair, generate(INTRA_FE)

* Estimate the gravity model (1-33) with the PPML estimator and store the results
		ppml trade INTRA_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST_1986 ln_DIST_1990 ln_DIST_1994 ln_DIST_1998 ln_DIST_2002 ln_DIST_2006 CNTG LANG CLNY, cluster(pair_id)
			estimates store dist_puzzl3

* Compute the percentage change in the estimates of the effects of bilateral
* distance between 1986 and 2006, and associated standard errors 
* Note that nlcom is very slow and consider commenting it out for rest of analysis
			nlcom 100*(_b[ln_DIST_2006] - _b[ln_DIST_1986]) / _b[ln_DIST_1986]


******************************* EXPORT ESTIMATES *******************************

* Export estimates in text format
		esttab dist_puzzl_ols dist_puzzl_ppml dist_puzzl1 dist_puzzl2 dist_puzzl3 using "Applications/2_DistancePuzzle/Results/Application_DistancePuzzle.rtf", append title(A Simple Solution to the Distance Puzzle In Trade) mtitles(OLS PPML INTRA ENDG LEAD PHSNG) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* IMPORTER_TIME_FE* INTRA_FE*) staraux nogaps 

* Export estimates in Excel format
		esttab dist_puzzl_ols dist_puzzl_ppml dist_puzzl1 dist_puzzl2 dist_puzzl3 using "Applications/2_DistancePuzzle/Results/Application_DistancePuzzle.csv", append title(A Simple Solution to the Distance Puzzle In Trade) mtitles(OLS PPML INTRA ENDG LEAD PHSNG) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* IMPORTER_TIME_FE* INTRA_FE*) staraux nogaps 

* Save the data used for this application 
	save "Datasets/2_DistancePuzzle.dta", replace
