********************************************************************************
****************  CHAPTER 2 - GENERAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

********************  EXERCISE 1: REMOVING SPECIFIC BORDER  ******************** 

* This exercise applies the methods developed by Anderson et al. (2015) in 
* order to investigates the potential effects of removing a specific border,
* while preserving the effects of geography.

* Data source: The database reports bilateral trade, including international and
*              intra-national trade, at the aggregated manufacturing level for 
*              69 countries for the period 1986-2006, provided by Thomas Zylkin,
*              based on UN COMTRADE, CEPII TradeProd and UN UNIDO INDSTAT 
*              databases. Standard gravity variables such as distance and 
*              continuous borders are taken from the CEPII GeoDist database.             


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
	set type double, permanently
	
* Set directory path, where "$input" refers to the path of the main folder 
* "Practical Guide to Gravity"	
	cd "$input/Chapter2"	
		
* Close and create log	
	capture log close
	log using "Exercises/1_RemovingSpecificBorder/Results/RemovingSpecificBorder.log", text replace

* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the esttab command if necessary
	* findit esttab
	
	
************************* OPEN AND MANAGE THE DATABASE *************************
* Open the database according to the Stata version you are using
	use "Datasets/Chapter2Exercise1.dta", clear

* Consider data only for the year == 2006
		keep if year == 2006

* Create the log of distance variable
		generate ln_DIST = ln(DIST)
	
* Create the international border dummy variable
		generate INTL_BRDR = 1
			replace INTL_BRDR = 0 if exporter == importer
			
* Create aggregate output
		bysort exporter: egen Y = sum(trade)

* Create aggregate expenditure
		bysort importer: egen E = sum(trade)

* Chose a country for reference group: GERMANY
* The country code of the reference country is set to "ZZZ" so that the exporter
* and exporter fixed effects of the reference country are always the last ones
* created
		gen E_R_BLN = E if importer == "DEU"
			replace exporter = "ZZZ" if exporter == "DEU"
			replace importer = "ZZZ" if importer == "DEU"
		egen E_R = mean(E_R_BLN)
		
* Create exporter fixed effects
		quietly tabulate exporter, gen(EXPORTER_FE)

* Create importer fixed effects
		quietly tabulate importer, gen(IMPORTER_FE)

* Set the number of exporter fixed effects variables
		quietly ds EXPORTER_FE*
		global N = `: word count `r(varlist)'' 
		global N_1 = $N - 1
		

***************************** EXERCISE 1 PART (i) ******************************
* Select two countries (A and B) available in the sample of the data file 
* "Chapter2Exercise1.dta", which share a common border and assume that the 
* "direct/partial equilibrium" effect of the border between country A and 
* country B is equal to the average border effect in the world by simulating
* the removal of the border between country A and country B only,while 
* preserving all other borders in the world in place. Discuss the results 
* in terms of effects on trade, real GDP, and effects on consumers and producers 
* in each country.

* The two countries considered here are Canada and the USA.		
* Generate a new variable for the border between countries A and B
		gen INTL_BRDR_AB = 0
			replace INTL_BRDR_AB = 1 if (exporter == "CAN" & importer == "USA") | (importer == "CAN" & exporter == "USA") 
			
* Save data 
	save "Datasets/1_RemovingSpecificBorder.dta", replace
	

************************* GENERAL EQUILIBRIUM ANALYSIS *************************
* Step I: Solve the baseline gravity model

	* Step I.a. Obtain estimates of trade costs and trade elasticities baseline 
	*			indexes
		* Estimate the gravity model in the "baseline" scenario with the PPML estimator:
		ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 ln_DIST CNTG INTL_BRDR, iter(30) noconst
			predict tradehat_BLN, mu
	
	* Step I.b. Construct baseline indexes	
		* Based on the estimated exporter and importer fixed effects, create
		* the actual set of fixed effects
			forvalues i = 1 (1) $N_1 {
				quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
				quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
			}
			
		* Create the exporter and importer fixed effects for the country of 
		* reference (Germany)
			quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
			quietly replace IMPORTER_FE$N = IMPORTER_FE$N * exp(0)
			
		* Create the variables stacking all the non-zero exporter and importer 
		* fixed effects, respectively		
			egen exp_pi_BLN = rowtotal(EXPORTER_FE1-EXPORTER_FE$N )
			egen exp_chi_BLN = rowtotal(IMPORTER_FE1-IMPORTER_FE$N ) 

		* Compute the variable of bilateral trade costs	
			generate tij_BLN = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR)

		* Compute the outward and inward multilateral resistances using the 
		* additive property of the PPML estimator that links the exporter and  
		* importer fixed effects with their respective multilateral resistances
		* taking into account the normalisation imposed
			generate OMR_BLN = Y * E_R / exp_pi_BLN
			generate IMR_BLN = E / (exp_chi_BLN * E_R)	
			
		* Compute the estimated level of international trade in the baseline for
		* the given level of ouptput and expenditures			
			generate tempXi_BLN = tradehat_BLN if exporter != importer
				bysort exporter: egen Xi_BLN = sum(tempXi_BLN)
					drop tempXi_BLN
			generate Y_BLN = Y
			generate E_BLN = E
	
* Given that the same procedure applies independently of the counterfactual,		
* two functions that implements the steps III and IV of the iterative procedure 
* is created and called upon to avoid duplicating the same commands three times
* (for 3 sub-question).			

program GEPPML_stepIII
	* The argument of the function is the additional name of the file to save 
	* the results in order to distinguish between sub-questions
	args file
	
 * Step III: Solve the counterfactual model

	* Step III.a.: Obtain conditional general equilibrium effects
	
	* (i):	Estimate the gravity model by imposing the constraints associated 
	* 		with the counterfactual scenario. The constraint is defined  
	* 		separately by taking the log of the counterfactual bilateral trade 
	* 		costs. The parameter of thisexpression will be constrainted to be 
	*		equal to 1 in the ppml estimator	
	
		* Specify the constraint in log
			generate ln_tij_CFL = log(tij_CFL)	
		
		* Re-create the exporters and imports fixed effects
				drop EXPORTER_FE* IMPORTER_FE*
			quietly tabulate exporter, generate(EXPORTER_FE)
			quietly tabulate importer, generate(IMPORTER_FE)

		* Estimate the constrained gravity model and generate predicted trade
		* value
		ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 , iter(30) noconst offset(ln_tij_CFL)
			predict tradehat_CDL, mu
	
	* (ii):	Construct conditional general equilibrium multilateral resistances
	
		* Based on the estimated exporter and importer fixed effects, create
		* the actual set of counterfactual fixed effects	
			forvalues i = 1(1)$N_1 {
				quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
				quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
			}
		
		* Create the exporter and importer fixed effects for the country of 
		* reference (Germany)
			quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
			quietly replace IMPORTER_FE$N = IMPORTER_FE$N * exp(0)
			
		* Create the variables stacking all the non-zero exporter and importer 
		* fixed effects, respectively		
			egen exp_pi_CDL = rowtotal( EXPORTER_FE1-EXPORTER_FE$N )
			egen exp_chi_CDL = rowtotal( IMPORTER_FE1-IMPORTER_FE$N )
			
		* Compute the outward and inward multilateral resistances 				
			generate OMR_CDL = Y * E_R / exp_pi_CDL
			generate IMR_CDL = E / (exp_chi_CDL * E_R)
			
		* Compute the estimated level of conditional general equilibrium 
		* international trade for the given level of ouptput and expenditures		
			generate tempXi_CDL = tradehat_CDL if exporter != importer
				bysort exporter: egen Xi_CDL = sum(tempXi_CDL)
					drop tempXi_CDL

					
	* Step III.b: Obtain full endowment general equilibrium effects

		* Create the iterative procedure by specifying the initial variables, 
		* where s = 0 stands for the baseline (BLN) value and s = 1 stands for  
		* the conditional general equilibrium (CD) value
		
			* The constant elasticity of substitutin is taken from the literature
			scalar sigma = 7
		
			* The parameter phi links the value of output with expenditures
			generate  phi = E/Y if exporter == importer
			
			* Compute the change in bilateral trade costs resulting from the 
			* counterfactual
			generate change_tij = tij_CFL / tij_BLN	

			* Re-specify the variables in the baseline and conditional scenarios
				* Output 
				generate Y_0 = Y
				generate Y_1 = Y
				
				* Expenditures, including with respect to the reference country   
				generate E_0 = E
				generate E_R_0 = E_R
				generate E_1 = E
				generate E_R_1 = E_R			
			
				* Predicted level of trade 
				generate tradehat_1 = tradehat_CDL
			
		* (i)	Allow for endogenous factory-gate prices
	
			* Re-specify the factory-gate prices under the baseline and 
			* conditional scenarios				
			generate exp_pi_0 = exp_pi_BLN
			generate tempexp_pi_ii_0 = exp_pi_0 if exporter == importer
				bysort importer: egen exp_pi_j_0 = mean(tempexp_pi_ii_0)
			generate exp_pi_1 = exp_pi_CDL
			generate tempexp_pi_ii_1 = exp_pi_1 if exporter == importer
				bysort importer: egen exp_pi_j_1 = mean(tempexp_pi_ii_1)
				drop tempexp_pi_ii_*
			generate exp_chi_0 = exp_chi_BLN	
			generate exp_chi_1 = exp_chi_CDL	
			
			* Compute the first order change in factory-gate prices	in the 
			* baseline and conditional scenarios
			generate change_pricei_0 = 0				
			generate change_pricei_1 = ((exp_pi_1 / exp_pi_0) / (E_R_1 / E_R_0))^(1/(1-sigma))
			generate change_pricej_1 = ((exp_pi_j_1 / exp_pi_j_0) / (E_R_1 / E_R_0))^(1/(1-sigma))
		
			* Re-specify the outward and inward multilateral resistances in the
			* baseline and conditional scenarios
			generate OMR_FULL_0 = Y_0 * E_R_0 / exp_pi_0
			generate IMR_FULL_0 = E_0 / (exp_chi_0 * E_R_0)		
			generate IMR_FULL_1 = E_1 / (exp_chi_1 * E_R_1)
			generate OMR_FULL_1 = Y_1 * E_R_1 / exp_pi_1
			
		* Compute initial change in outward and multilateral resitances, which 
		* are set to zero		
			generate change_IMR_FULL_1 = exp(0)		
			generate change_OMR_FULL_1 = exp(0)
		

	****************************************************************************
	******************** Start of the Iterative Procedure  *********************
	
	* Set the criteria of convergence, namely that either the standard errors or
	* maximum of the difference between two iterations of the factory-gate 
	* prices are smaller than 0.01, where s is the number of iterations	
		local s = 3	
		local sd_dif_change_pi = 1
		local max_dif_change_pi = 1
	while (`sd_dif_change_pi' > 0.01) | (`max_dif_change_pi' > 0.01) {
		local s_1 = `s' - 1
		local s_2 = `s' - 2
		local s_3 = `s' - 3
		
		* (ii)	Allow for endogenous income, expenditures and trade	
			generate trade_`s_1' =  tradehat_`s_2' * change_pricei_`s_2' * change_pricej_`s_2' / (change_OMR_FULL_`s_2'*change_IMR_FULL_`s_2')

		* (iii)	Estimation of the structural gravity model
				drop EXPORTER_FE* IMPORTER_FE*
				quietly tabulate exporter, generate (EXPORTER_FE)
				quietly tabulate importer, generate (IMPORTER_FE)
			capture ppml trade_`s_1' EXPORTER_FE* IMPORTER_FE*, offset(ln_tij_CFL) noconst iter(30) 
				predict tradehat_`s_1', mu
					
			* Update output & expenditure			
				bysort exporter: egen Y_`s_1' = total(tradehat_`s_1')
				quietly generate tempE_`s_1' = phi * Y_`s_1' if exporter == importer
					bysort importer: egen E_`s_1' = mean(tempE_`s_1')
				quietly generate tempE_R_`s_1' = E_`s_1' if importer == "ZZZ"
					egen E_R_`s_1' = mean(tempE_R_`s_1')
				
			* Update factory-gate prices 
				forvalues i = 1(1)$N_1 {
					quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
					quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
				}
				quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
				egen exp_pi_`s_1' = rowtotal(EXPORTER_FE1-EXPORTER_FE$N ) 
				quietly generate tempvar1 = exp_pi_`s_1' if exporter == importer
					bysort importer: egen exp_pi_j_`s_1' = mean(tempvar1) 		
					
			* Update multilateral resistances
				generate change_pricei_`s_1' = ((exp_pi_`s_1' / exp_pi_`s_2') / (E_R_`s_1' / E_R_`s_2'))^(1/(1-sigma))
				generate change_pricej_`s_1' = ((exp_pi_j_`s_1' / exp_pi_j_`s_2') / (E_R_`s_1' / E_R_`s_2'))^(1/(1-sigma))
				generate OMR_FULL_`s_1' = (Y_`s_1' * E_R_`s_1') / exp_pi_`s_1' 
					generate change_OMR_FULL_`s_1' = OMR_FULL_`s_1' / OMR_FULL_`s_2'					
				egen exp_chi_`s_1' = rowtotal(IMPORTER_FE1-IMPORTER_FE$N )	
				generate IMR_FULL_`s_1' = E_`s_1' / (exp_chi_`s_1' * E_R_`s_1')
					generate change_IMR_FULL_`s_1' = IMR_FULL_`s_1' / IMR_FULL_`s_2'
				
			* Iteration until the change in factory-gate prices converges to zero
				generate dif_change_pi_`s_1' = change_pricei_`s_2' - change_pricei_`s_3'
					display "************************* iteration number " `s_2' " *************************"
						summarize dif_change_pi_`s_1', format
					display "**********************************************************************"
					display " "
						local sd_dif_change_pi = r(sd)
						local max_dif_change_pi = abs(r(max))	
						
			local s = `s' + 1
			drop temp* 
	}
	
	********************* End of the Iterative Procedure  **********************
	****************************************************************************
		
		* (iv)	Construction of the "full endowment general equilibrium" 
		*		effects indexes
			* Use the result of the latest iteration S
			local S = `s' - 2
	
		* Compute the full endowment general equilibrium of factory-gate price
			generate change_pricei_FULL = ((exp_pi_`S' / exp_pi_0) / (E_R_`S' / E_R_0))^(1/(1-sigma))		

		* Compute the full endowment general equilibrium of the value output
			generate Y_FULL = change_pricei_FULL  * Y_BLN

		* Compute the full endowment general equilibrium of the value of 
		* aggregate expenditures
			generate tempE_FULL = phi * Y_FULL if exporter == importer
				bysort importer: egen E_FULL = mean(tempE_FULL)
					drop tempE_FULL
			
		* Compute the full endowment general equilibrium of the outward and 
		* inward multilateral resistances 
			generate OMR_FULL = Y_FULL * E_R_`S' / exp_pi_`S'
			generate IMR_FULL = E_`S' / (exp_chi_`S' * E_R_`S')	
			
		* Compute the full endowment general equilibrium of the value of 
		* bilateral trade 
			generate X_FULL = (Y_FULL * E_FULL * tij_CFL) /(IMR_FULL * OMR_FULL)			
		
		* Compute the full endowment general equilibrium of the value of 
		* total international trade 
			generate tempXi_FULL = X_FULL if exporter != importer
				bysort exporter: egen Xi_FULL = sum(tempXi_FULL)
					drop tempXi_FULL
					
	* Save the conditional and general equilibrium effects results		
	save "Exercises\1_RemovingSpecificBorder\Results\FULLGE_`file'.dta", replace
end

program GEPPML_stepIV
	* The argument of the function is the additional name of the file to save 
	* the results in order to distinguish between sub-questions
	args file
	
* Step IV: Collect, construct, and report indexes of interest
	use "Exercises\1_RemovingSpecificBorder\Results\FULLGE_`file'.dta", clear
		collapse(mean) OMR_FULL OMR_CDL OMR_BLN change_pricei_FULL Xi_* Y_BLN Y_FULL, by(exporter)
			rename exporter country
			replace country = "DEU" if country == "ZZZ"
			sort country
		
		* Percent change in full endowment general equilibrium of factory-gate prices
			generate change_price_FULL = (change_pricei_FULL - 1) / 1 * 100
			
		* Percent change in full endowment general equilibirum of outward multilateral resistances
			generate change_OMR_CDL = (OMR_CDL^(1/(1-sigma)) - OMR_BLN^(1/(1-sigma))) / OMR_BLN^(1/(1-sigma)) * 100
		
		* Percent change in full endowment general equilibrium of outward multilateral resistances			
			generate change_OMR_FULL = (OMR_FULL^(1/(1-sigma)) - OMR_BLN^(1/(1-sigma))) / OMR_BLN^(1/(1-sigma)) * 100

		* Percent change in conditional general equilibrium of bilateral trade
			generate change_Xi_CDL = (Xi_CDL - Xi_BLN) / Xi_BLN * 100	
			
		* Percent change in full endowment general equilibrium of bilateral trade		
			generate change_Xi_FULL = (Xi_FULL - Xi_BLN) / Xi_BLN * 100
	save "Exercises\1_RemovingSpecificBorder\Results\FULL_PROD_`file'.dta", replace


	* Construct the percentage changes on import/consumption side
	use "Exercises\1_RemovingSpecificBorder\Results\FULLGE_`file'.dta", clear
		collapse(mean) IMR_FULL IMR_CDL IMR_BLN, by(importer)
			rename importer country
			replace country = "DEU" if country == "ZZZ"
			sort country		

		* Conditional general equilibrium of inward multilateral resistances
			generate change_IMR_CDL = (IMR_CDL^(1/(1-sigma)) - IMR_BLN^(1/(1-sigma))) / IMR_BLN^(1/(1-sigma)) * 100
			
		* Full endowment general equilibrium of inward multilateral resistances
			generate change_IMR_FULL = (IMR_FULL^(1/(1-sigma)) - IMR_BLN^(1/(1-sigma))) / IMR_BLN^(1/(1-sigma)) * 100
	save "Exercises\1_RemovingSpecificBorder\Results\FULL_CONS_`file'.dta", replace

	* Merge the general equilibrium results from the production and consumption
	* sides
	use "Exercises\1_RemovingSpecificBorder\Results\FULL_PROD_`file'.dta", clear
		joinby country using "Exercises\1_RemovingSpecificBorder\Results\FULL_CONS_`file'.dta"
		
		* Full endowment general equilibrium of real GDP
			generate rGDP_BLN = Y_BLN / (IMR_BLN ^(1 / (1 -sigma)))
			generate rGDP_FULL = Y_FULL / (IMR_FULL ^(1 / (1 -sigma)))
				generate change_rGDP_FULL = (rGDP_FULL - rGDP_BLN) / rGDP_BLN * 100
			
		* Keep indexes of interest	
			keep country change_Xi_CDL change_Xi_FULL change_price_FULL change_IMR_FULL change_rGDP_FULL Y_BLN
			order country change_Xi_CDL change_Xi_FULL change_price_FULL change_IMR_FULL change_rGDP_FULL Y_BLN
				
	* Export the results in Excel
		export excel using "Exercises\1_RemovingSpecificBorder\Results\FULL_`file'.xls", firstrow(variables) replace
end

* Step II: Define a conterfactual scenario
	* The counterfactual scenario consists in removing the international border
	* between countries A and B by constraining the variable of international 
	* borders to be zero for the border between countries A and B,and assuming 
	* the effects of the other international borders and the geographic 
	* variables (DIST and CNTG) remain the same.
		* Constructing the counterfactual bilateral trade costs	by imposing the
		* constraints associated with the counterfactual scenario
			generate INTL_BRDR_CFL = INTL_BRDR
				replace INTL_BRDR_CFL = 0 if (exporter == "CAN" & importer == "USA") | (importer == "CAN" & exporter == "USA") 
			generate tij_CFL = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR_CFL)
	
* Step III: Solve the counterfactual model
	GEPPML_stepIII "part_A"

* Step IV: Collect, construct, and report indexes of interest
	GEPPML_stepIV "part_A"

	
***************************** EXERCISE 1 PART (ii) *****************************
* Given the specific relationship between countries A and B, assume that the 
* differential partial border effects between country A and country B are 
* different from the average border effect. Are the expectations for a lower or
* larger border between countries A and B confirmed? What are the volume effects
* of the border? 

	use "Datasets/1_RemovingSpecificBorder.dta", clear
	
		* Distinguish between the average border effect and the border effect
		* between countries A and B
		replace INTL_BRDR = 0 if INTL_BRDR_AB == 1

		* Estimate the gravity model in the "baseline" scenario with the PPML estimator:
		ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 ln_DIST CNTG INTL_BRDR INTL_BRDR_AB, iter(30) noconst
			* Save the estimation results to be used instead of re-estimating the same equation three times
			estimates store BLN
	

**************************** EXERCISE 1 PART (iii) *****************************
* Use the new border estimates from part B to obtain the general equilibrium 
* effects of the removal of the border between countries A and B. Discuss the 
* results and compare them with those from part A.

************************* GENERAL EQUILIBRIUM ANALYSIS *************************

* Step I: Solve the baseline gravity model

	* Step I.a. Obtain estimates of trade costs and trade elasticities baseline 
	*			indexes
		* Estimate the gravity model in the "baseline" scenario with the PPML estimator:
		* ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 ln_DIST CNTG INTL_BRDR INTL_BRDR_AB, iter(30) noconst
		* Alternatively recall the "baseline" results of the gravity model obtained above 
			estimate restore BLN
			predict tradehat_BLN, mu
	
	* Step I.b. Construct baseline indexes	
		* Based on the estimated exporter and importer fixed effects, create
		* the actual set of fixed effects
			forvalues i = 1 (1) $N_1 {
				quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
				quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
			}
			
		* Create the exporter and importer fixed effects for the country of 
		* reference (Germany)
			quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
			quietly replace IMPORTER_FE$N = IMPORTER_FE$N * exp(0)
			
		* Create the variables stacking all the non-zero exporter and importer 
		* fixed effects, respectively		
			egen exp_pi_BLN = rowtotal(EXPORTER_FE1-EXPORTER_FE$N )
			egen exp_chi_BLN = rowtotal(IMPORTER_FE1-IMPORTER_FE$N ) 

		* Compute the variable of bilateral trade costs	
			generate tij_BLN = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR + _b[INTL_BRDR_AB]*INTL_BRDR_AB)

		* Compute the outward and inward multilateral resistances using the 
		* additive property of the PPML estimator that links the exporter and  
		* importer fixed effects with their respective multilateral resistances
		* taking into account the normalisation imposed
			generate OMR_BLN = Y * E_R / exp_pi_BLN
			generate IMR_BLN = E / (exp_chi_BLN * E_R)	
			
		* Compute the estimated level of international trade in the baseline for
		* the given level of ouptput and expenditures			
			generate tempXi_BLN = tradehat_BLN if exporter != importer
				bysort exporter: egen Xi_BLN = sum(tempXi_BLN)
					drop tempXi_BLN
			generate Y_BLN = Y
			generate E_BLN = E

* Step II: Define a conterfactual scenario
	* The counterfactual scenario consists in removing the international border
	* between countries A and B by constraining the parameter associated with 
	* the variable representing the border between countries A and B 
	* (INTL_BRDR_AB) to be zero and assuming the effects of the international
	* borders and the geographic variables (DIST and CNTG) remain the same.
	
		* Constructing the counterfactual bilateral trade costs	by imposing the
		* constraints associated with the counterfactual scenario
			generate INTL_BRDR_CFL = INTL_BRDR
				replace INTL_BRDR_CFL = 0 if (exporter == "CAN" & importer == "USA") | (importer == "CAN" & exporter == "USA") 
			generate tij_CFL = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR_CFL)
	
* Step III: Solve the counterfactual model
	GEPPML_stepIII "part_BC"

* Step IV: Collect, construct, and report indexes of interest
	GEPPML_stepIV "part_BC"


***************************** EXERCISE 1 PART (iv) *****************************
* Some politicians believe that their country will benefit more if trading 
* partners remove the impediments for this country’s exports, while the country
* in question preserves its borders on imports from abroad. Use the partial 
* estimates from part B to simulate a unilateral removal of the border for 
* exports from country A to country B. Discuss the results relative to the 
* estimates from part C.
************************* GENERAL EQUILIBRIUM ANALYSIS *************************

	use "Datasets/1_RemovingSpecificBorder.dta", clear	
		* Distinguish between the average border effect and the border effect
		* between countries A and B
		replace INTL_BRDR = 0 if INTL_BRDR_AB == 1
	
* Step I: Solve the baseline gravity model

	* Step I.a. Obtain estimates of trade costs and trade elasticities baseline 
	*			indexes
		* Estimate the gravity model in the "baseline" scenario with the PPML estimator:
		* ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 ln_DIST CNTG INTL_BRDR INTL_BRDR_AB, iter(30) noconst
		* Alternatively recall the "baseline" results of the gravity model obtained above 
			estimate restore BLN
			predict tradehat_BLN, mu
	
	* Step I.b. Construct baseline indexes	
		* Based on the estimated exporter and importer fixed effects, create
		* the actual set of fixed effects
			forvalues i = 1 (1) $N_1 {
				quietly replace EXPORTER_FE`i' = EXPORTER_FE`i' * exp(_b[EXPORTER_FE`i'])
				quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
			}
			
		* Create the exporter and importer fixed effects for the country of 
		* reference (Germany)
			quietly replace EXPORTER_FE$N = EXPORTER_FE$N * exp(_b[EXPORTER_FE$N ])
			quietly replace IMPORTER_FE$N = IMPORTER_FE$N * exp(0)
			
		* Create the variables stacking all the non-zero exporter and importer 
		* fixed effects, respectively		
			egen exp_pi_BLN = rowtotal(EXPORTER_FE1-EXPORTER_FE$N )
			egen exp_chi_BLN = rowtotal(IMPORTER_FE1-IMPORTER_FE$N ) 

		* Compute the variable of bilateral trade costs	
			generate tij_BLN = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR + _b[INTL_BRDR_AB]*INTL_BRDR_AB)

		* Compute the outward and inward multilateral resistances using the 
		* additive property of the PPML estimator that links the exporter and  
		* importer fixed effects with their respective multilateral resistances
		* taking into account the normalisation imposed
			generate OMR_BLN = Y * E_R / exp_pi_BLN
			generate IMR_BLN = E / (exp_chi_BLN * E_R)	
			
		* Compute the estimated level of international trade in the baseline for
		* the given level of ouptput and expenditures			
			generate tempXi_BLN = tradehat_BLN if exporter != importer
				bysort exporter: egen Xi_BLN = sum(tempXi_BLN)
					drop tempXi_BLN
			generate Y_BLN = Y
			generate E_BLN = E

* Step II: Define a conterfactual scenario
	* The counterfactual scenario consists in removing the international border
	* for exports from country A to country B by constraining the variable
	* representing the border between countries A and B (INTL_BRDR_AB) to be 
	* zero for the country pair A and B and assuming the effects of the 
	* bilateral border (for country pair B and A), the other international
	* borders and the geographic variables (DIST and CNTG) remain the same.
	
		* Constructing the counterfactual bilateral trade costs	by imposing the
		* constraints associated with the counterfactual scenario		
			generate INTL_BRDR_AB_CFL = INTL_BRDR_AB
				replace INTL_BRDR_AB_CFL = 0 if exporter == "CAN" & importer == "USA"
			generate tij_CFL = exp(_b[ln_DIST]*ln_DIST + _b[CNTG]*CNTG + _b[INTL_BRDR]*INTL_BRDR + _b[INTL_BRDR_AB]*INTL_BRDR_AB_CFL)

* Step III: Solve the counterfactual model
	GEPPML_stepIII "part_D"

* Step IV: Collect, construct, and report indexes of interest
	GEPPML_stepIV "part_D"

********************************************************************************	
