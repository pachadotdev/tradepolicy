********************************************************************************
****************  CHAPTER 1 - PARTIAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

****************  APPLICATION 1: TRADITIONAL GRAVITY ESTIMATES  **************** 

* This application estimates the effects of traditional gravity variables by 
* applying different methods to account for the multilateral resistance terms 
* and different estimators.  

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
	log using "Applications/1_TraditionalGravity/Results/TraditionalGravity.log", text replace


* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the esttab command if necessary
	* findit esttab
	
	
************************* OPEN AND MANAGE THE DATABASE *************************
* Open the database according to the Stata version you are using
	use "Datasets/Chapter1Application1.dta", clear

* The analysis considers panel data with 4 year interval (1986, 1990, ..., 2006)
		keep if year == 1986 | year == 1990 | year == 1994 | year == 1998 | year == 2002 | year == 2006 

* Create the dependent variable	
		generate ln_trade = ln(trade)

* Create the independent variables		
		generate ln_DIST = ln(DIST)
		bysort exporter year: egen Y = sum(trade)
			generate ln_Y = ln(Y)
		bysort importer year: egen E = sum(trade)
			generate ln_E = ln(E)	

		
*********************************** ANALYSIS ***********************************

***************************** (a) OLS IGNORING MRs ***************************** 

* Estimate the gravity model (1-23)
		regress ln_trade ln_DIST CNTG LANG CLNY ln_Y ln_E if exporter != importer, cluster(pair_id) 

* Store the results
			estimates store ols

* Perform the RESET Test
		predict fit, xb
			generate fit2 = fit^2
		regress ln_trade ln_DIST CNTG LANG CLNY ln_Y ln_E fit2 if exporter != importer, cluster(pair_id)
			test fit2 = 0
			drop fit*
		
* Save the data used for this application 
	save "Datasets/1_TraditionalGravityData.dta", replace


************* (b) OLS CONTROLLING FOR MRs WITH REMOTENESS INDEXES ************** 

* Option 1: creation of the remotness indexes with the total command
	use "Datasets/1_TraditionalGravityData.dta", clear
		
* Create the remoteness indexes on the exporter side, defined as the logarithms
* of expenditure-weighted averages of bilateral distance:
		bysort exporter year: egen TotEj = total(E)
		bysort year: egen TotE = max(TotEj)
		bysort exporter year: egen REM_EXP = total(DIST / (E / TotE))
			gen ln_REM_EXP = ln(REM_EXP)
			
* Create the remoteness indexes on the importer side defined as the logarithms
* of output-weighted averages of bilateral distance:	
		bysort importer year: egen TotYi = total(Y)
		bysort year: egen TotY = max(TotYi)
		bysort importer year: egen REM_IMP = total(DIST / (Y / TotY))
			gen ln_REM_IMP = ln(REM_IMP)

			
* Option 2: creation of the remotness indexes with the collapse command			
* Create the remoteness indexes on the exporter side, defined as the logarithms
* of expenditure-weighted averages of bilateral distance:
	use "Datasets/1_TraditionalGravityData.dta", clear
		keep importer year E
		duplicates drop
		collapse(sum) E, by(year)
		rename E TotE
		sort year
	save "Datasets/RMTNS_EXP.dta", replace

	use "Datasets/1_TraditionalGravityData.dta", clear
		keep exporter year Y
		duplicates drop
		collapse(sum) Y, by(year)
		rename Y TotY
		sort year
	save "Datasets/RMTNS_IMP.dta", replace
* Merge the data on total output and expenditures with the database created above
	use "Datasets/1_TraditionalGravityData.dta", clear
		sort year
		merge year using "Datasets/RMTNS_EXP.dta"
			drop _merge
		sort year
		merge year using "Datasets/RMTNS_IMP.dta"
			drop _merge
		
* Create the remoteness indexes on the exporter side, defined as the logarithms
* of expenditure-weighted averages of bilateral distance:
		bysort exporter year: egen REM_EXP = total(DIST / (E / TotE))
			generate ln_REM_EXP = ln(REM_EXP)
			
* Create the remoteness indexes on the importer side defined as the logarithms
* of output-weighted averages of bilateral distance:	
		bysort importer year: egen REM_IMP = total(DIST / (Y / TotY))
			generate ln_REM_IMP = ln(REM_IMP)
		
* Estimate the gravity model (1-24) and store the results
		regress ln_trade ln_DIST CNTG LANG CLNY ln_Y ln_E ln_REM_EXP ln_REM_IMP if exporter != importer, cluster(pair_id) 
			estimates store rmtns

* Perform the RESET test (taking form the "Log of Gravity” web page)
		predict fit, xb
			generate fit2 = fit^2
		regress ln_trade ln_DIST CNTG LANG CLNY ln_Y ln_E REM_EXP REM_IMP fit2 if exporter != importer, cluster(pair_id) 
			test fit2 = 0
			drop fit*


**************** (c) OLS CONTROLLING FOR MRs WITH FIXED EFFECTS **************** 	

* Create exporter-time fixed effects
		egen exp_time = group(exporter year)
			quietly tabulate exp_time, gen(EXPORTER_TIME_FE)

* Create importer-time fixed effects
		egen imp_time = group(importer year)
			quietly tabulate imp_time, gen(IMPORTER_TIME_FE)

* Estimate the gravity model (1-27) and store the results
		regress ln_trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY if exporter != importer, cluster(pair_id)
			estimates store fes

* A convenient and faster fixed effects estimator is given by the command reghdfe
		* ssc install reghdfe
		* reghdfe ln_trade ln_DIST CNTG LANG CLNY if exporter != importer, absorb(exp_time imp_time) cluster(pair_id)
	
* Perform the RESET test (taking form the "Log of Gravity” web page)
		predict fit, xb
			generate fit2 = fit^2
		regress ln_trade ln_DIST CNTG LANG CLNY ln_Y ln_E ln_REM_EXP ln_REM_IMP fit2 if exporter != importer, cluster(pair_id) 
			test fit2 = 0
			drop fit*


*************** (d) PPML CONTROLLING FOR MRs WITH FIXED EFFECTS ****************

* Estimate the gravity model (1-28) with the PPML estimator and store the results
		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY if exporter != importer, cluster(pair_id)
			estimates store ppml

		* An alternative command to apply the PPML is given by glm		
		* glm trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY if exporter != importer, cluster(pair_id) family(poisson) 

* Perform the RESET test
		predict fit, xb
			generate fit2 = fit^2
		ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY fit2 if exporter != importer, cluster(pair_id) 
			test fit2 = 0
			drop fit*

* Construct the R2, given that the ppml command reports only the pseudo R2
		* ppml trade EXPORTER_TIME_FE* IMPORTER_TIME_FE* ln_DIST CNTG LANG CLNY if exporter != importer, cluster(pair_id)
		estimates restore ppml
		predict trade_hat
			qui cor trade_hat trade 
			display "R-squared 0" (`r(rho)')^2 
			drop trade_hat

		
******************************* EXPORT ESTIMATES *******************************

* Export estimates in text format
		esttab ols rmtns fes ppml using "Applications/1_TraditionalGravity/Results/Application_TraditionalGravity.rtf", append title(Traditional Gravity Estimates) mtitles(OLS RMTNS FES PPML) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* IMPORTER_TIME_FE*) staraux nogaps 

* Export estimates in Excel format
		esttab ols rmtns fes ppml using "Applications/1_TraditionalGravity/Results/Application_TraditionalGravity.csv", append title(Traditional Gravity Estimates) mtitles(OLS RMTNS FES PPML) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* IMPORTER_TIME_FE*) staraux nogaps 

* Save the data used for this application 
	save "Datasets/1_TraditionalGravity.dta", replace
