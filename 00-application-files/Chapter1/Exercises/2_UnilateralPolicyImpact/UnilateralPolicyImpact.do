********************************************************************************
****************  CHAPTER 1 - PARTIAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

****************  EXERCISE 2: UNILATERAL TRADE POLICY EFFECTS  *****************

* The aim of this exercise is to demonstrate that the gravity model can be used 
* to estimate the effects of non-discriminatory (across trading partners) MFN
* tariffs.   

* Data source: The database reports bilateral trade, including international and
*              intra-national trade, at the aggregated manufacturing level for 
*              69 countries for the period 1986-2006, provided by Thomas Zylkin,
*              based on UN COMTRADE, CEPII TradeProd and UN UNIDO INDSTAT 
*              databases. Information on RTAs come from Mario Larch's Regional 
*			   Trade Agreements Database (httppml_panel_sg trade RTA RTA_LAG3p://www.ewf.uni-bayreuth.de/en/
*			   research/RTA-data/index.html).


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
	*log using "Exercises\2_UnilateralPolicyImpact/Results/UnilateralPolicyImpact.log", text replace


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
	use "Datasets/Chapter1Exercise2.dta", clear

* b. Determine how many countries and which years for which data on MFN tariffs 
*    are available
		summarize MFN_TARIFF
		keep exporter  importer year MFN_TARIFF
			drop if MFN_TARIFF == .
			duplicates drop
		reshape wide MFN_TARIFF, i(year importer) j(exporter) string

		
********************** (ii) Benchmark gravity estimation ***********************

* a. Generate exporter-time, importer-time and pair fixed effects
	use "Datasets/Chapter1Exercise2.dta", clear
		
	* Create exporter-time fixed effects
		egen exp_time = group(exporter year)
			quietly tabulate exp_time, gen(EXPORTER_TIME_FE)

	* Create importer-time fixed effects
		egen imp_time = group(importer year)
			quietly tabulate imp_time, gen(IMPORTER_TIME_FE)

	* Create pair fixed effects
		egen pair_id = group(exporter importer)
			quietly tabulate pair_id, gen(PAIR_FE)

* b. Estimate with the PPML estimator the following structural gravity 
*    specification:
*    Xij = pi_it + xsi_jt + mu_ij + b1*RTA + b2*RTA_t-3 + b3*RTA_t-6 + b4*RTA_t-9
	
	* Estimate the gravity model with the PPML estimator and store the results
	* This command will take hours to estimate the model
	*	ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA RTA_LAG3 RTA_LAG6 RTA_LAG9, cluster(pair_id) diff iter(30)
	* Alternative faster PPML command (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
		ppml_panel_sg trade RTA RTA_LAG3 RTA_LAG6 RTA_LAG9, ex(exporter) im(importer) y(year) cluster(pair_id) 
			estimates store ppml_lags		
		
* d. Compute the total effects of the RTAs and comment	
	
 	* Compute the linear combinations of the RTAs variables
			lincom _b[RTA]+_b[RTA_LAG3]+_b[RTA_LAG6]+_b[RTA_LAG9]
 

************ (iii) Gravity estimation with unilateral trade policy *************

* a. Create the logarithm of the MFN tariffs variable (ln_MFN), the dummy
*    variable taking the value of one for international trade flows and zero 
*    otherwise (INTL), and the product of the variables ln_MFN and INTL.

	* Create the logarithm  of MFN Tariffs 
		generate ln_MFN = ln(MFN_TARIFF + 1)
			replace ln_MFN = 0 if exporter == importer
		
* b. Estimate with the PPML estimator the modified structural gravity 
*    specification: 
*    Xij = pi_it + xsi_jt + mu_ij + b1*RTA + b2*RTA_t-3 + b3*RTA_t-6 + b4*RTA_t-9
*          + ln_MFN	

	* Estimate the gravity model with the PPML estimator and store the results
	* This command will take hours to estimate the model
	*	ppml trade PAIR_FE* EXPORTER_TIME_FE* IMPORTER_TIME_FE* RTA RTA_LAG3 RTA_LAG6 RTA_LAG9 ln_MFN, cluster(pair_id) diff iter(30)
	* Alternative faster PPML command (more info at http://www.tomzylkin.com/uploads/4/1/0/4/41048809/help_file.pdf)	
		ppml_panel_sg trade RTA RTA_LAG3 RTA_LAG6 RTA_LAG9 ln_MFN, ex(exporter) im(importer) y(year) cluster(pair_id) 
			estimates store ppml_tars

* c. Compute the total effects of the RTAs, compare the result and comment 

 	* Compute the linear combinations of the RTAs variables
			lincom _b[RTA]+_b[RTA_LAG3]+_b[RTA_LAG6]+_b[RTA_LAG9] 
  
* d. Compute the trade elasticity of substitution based on the estimated 
*    obtained in 3.b. Discuss the result, noting that the elasticity estimates 
*	 from the related trade literature usually vary between 2 and 12 (Eaton and
*    Kortum, 2002; Anderson and van Wincoop, 2003; Broda et al., 2006)
			scalar sigma = -_b[ln_MFN]
				display sigma
	

******************************* EXPORT ESTIMATES *******************************

	* Export estimates in text format
		* esttab ppml_lags ppml_tars using "Exercises\2_UnilateralPolicyImpact/Results/UnilateralPolicyImpact.txt", append title(Estimating The Effects of Non-discriminatory Trade Policy) mtitles(RTAs TRFFS GLBZN) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* EXPORTER_TIME_FE* PAIR_FE*) staraux nogaps 
		* If the ppml_panel_sg command is used
		esttab ppml_lags ppml_tars using "Exercises\2_UnilateralPolicyImpact/Results/UnilateralPolicyImpact.txt", append title(Estimating The Effects of Non-discriminatory Trade Policy) mtitles(RTAs TRFFS GLBZN) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) staraux nogaps 

		
	* Export estimates in Excel format
		* esttab ppml_lags ppml_tars using "Exercises\2_UnilateralPolicyImpact/Results/UnilateralPolicyImpact.csv", append title(Estimating The Effects of Non-discriminatory Trade Policy) mtitles(RTAs TRFFS GLBZN) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) drop(EXPORTER_TIME_FE* EXPORTER_TIME_FE* PAIR_FE*) staraux nogaps 
		* If the ppml_panel_sg command is used
		esttab ppml_lags ppml_tars using "Exercises\2_UnilateralPolicyImpact/Results/UnilateralPolicyImpact.csv", append title(Estimating The Effects of Non-discriminatory Trade Policy) mtitles(RTAs TRFFS GLBZN) b(3) se(3) scalars(N r2) star(+ 0.10 * .05 ** .01) staraux nogaps 
