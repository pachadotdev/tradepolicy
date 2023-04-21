********************************************************************************
****************  CHAPTER 2 - GENERAL EQUILIBRIUM TRADE POLICY  **************** 
****************		      ANALYSIS WITH STRUCTURAL GRAVITY 	**************** 
********************************************************************************

*************  APPLICATION 2: IMPACT OF REGIONAL TRADE AGREEMENTS  ************* 

* This application applies the methods developed by Anderson et al. (2015) in 
* order to investigates the potential effects of removing the North American
* Free Trade Agreement (NAFTA).

* Data source: The database reports bilateral trade, including international and
*              intra-national trade, at the aggregated manufacturing level for 
*              69 countries for the period 1986-2006, provided by Thomas Zylkin,
*              based on UN COMTRADE, CEPII TradeProd and UN UNIDO INDSTAT 
*              databases. Information on RTAs come from Mario Larch's Regional
*              Trade Agreements Database (http://www.ewf.uni-bayreuth.de/en/
*              research/RTA-data/index.html). Standard gravity variables such 
*              as distance, continuous borders, and common language, are taken
*              from the CEPII GeoDist database.


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
	log using "Applications/2_RTAsEffects/Results/RTAsEffects.log", text replace

* Install or update the ppml command if necessary	
	* ssc install ppml

* Install or update the esttab command if necessary
	* findit esttab
	
	
************************* OPEN AND MANAGE THE DATABASE *************************
* Open the database according to the Stata version you are using
	use "Datasets/Chapter2Application2.dta", clear

* Consider panel data with 4 year interval (1986, 1990, ..., 2006)
		keep if year == 1986 | year == 1990 | year == 1994 | year == 1998 | year == 2002 | year == 2006 
		
* Create the log of distance variable
		generate ln_DIST = ln(DIST)
	
* Create aggregate output
		bysort exporter year: egen Y = sum(trade)

* Create aggregate expenditure
		bysort importer year: egen E = sum(trade)

* Chose a country for reference group: GERMANY
* The country code of the reference country is set to "ZZZ" so that the exporter
* and exporter fixed effects of the reference country are always the last ones
* created
		gen E_R_BLN = E if importer == "DEU"
			replace exporter = "ZZZ" if exporter == "DEU"
			replace importer = "ZZZ" if importer == "DEU"
		bysort year: egen E_R = mean(E_R_BLN)

* Create exporter time fixed effects
		egen exp_time = group(exporter year)
		quietly tabulate exp_time, gen(EXPORTER_TIME_FE)

* Create importer time fixed effects
		egen imp_time = group(importer year)
		quietly tabulate imp_time, gen(IMPORTER_TIME_FE)

* Rearrange so that country pairs (e.g. NER-PAN, MWI-MAC, NPL-MWI, PAN-MWI,
* NPL-CMR), which will be dropped due to no trade, are last	
		bysort pair_id: egen X = sum(trade)
		quietly summarize pair_id
		replace pair_id = pair_id + r(max) + 1 if X == 0 | X == .
		drop X

* Rearrange so that the last country pair is the one for internal trade
		quietly sum pair_id
		replace pair_id = r(max) + 1 if exporter == importer
		quietly tabulate pair_id, gen(PAIR_FE)		
 
* Set additional exogenous parameters
* Set additional exogenous parameters
		quietly ds EXPORTER_TIME_FE*
		global NT = `: word count `r(varlist)'' 
		global NT_1 = $NT - 1
		
		quietly tabulate year, gen(TIME_FE)		
		quietly ds TIME_FE*
		global Nyr = `: word count `r(varlist)''
		global NT_yr = $NT - $Nyr
		
		quietly ds PAIR_FE*
		global NTij = `: word count `r(varlist)'' 
		global NTij_1 = $NTij - 1
		global NTij_8 = $NTij - 8

* Save data
	save "Datasets/2_RTAsEffects.dta", replace

	
************************* GENERAL EQUILIBRIUM ANALYSIS *************************

* Step I: Solve the baseline gravity model

	* Step I.a. Obtain estimates of trade costs and trade elasticities baseline 
	*			indexes
	
		* Implementation of Anderson and Yotov (2016) two-stage procedure to 
		* construct the full matrix of trade costs, including when there is no
		* trade or zero trade
	
			* Stage 1: Obtain the estimates of pair fixed effects and the effects of RTAs
			ppml trade PAIR_FE1-PAIR_FE$NTij_1 EXPORTER_TIME_FE* IMPORTER_TIME_FE1-IMPORTER_TIME_FE$NT_yr RTA, iter(30) noconst
				estimate store gravity_panel
				* Note that the nlcom is particularly slow.
				* nlcom (exp(_b[RTA])-1) * 100
				scalar RTA_est = _b[RTA]		
	
				* Construct the trade costs from the pair fixed effects
					forvalues ijt = 1(1)$NTij_8{
						qui replace PAIR_FE`ijt' = PAIR_FE`ijt' * _b[PAIR_FE`ijt']
					}
					
					egen gamma_ij = rowtotal(PAIR_FE1-PAIR_FE$NTij )
						replace gamma_ij = . if gamma_ij == 1 & exporter != importer
						replace gamma_ij = 0 if gamma_ij == 1 & exporter == importer
					generate tij_bar = exp(gamma_ij)
					generate tij_bln = exp(gamma_ij + RTA_est*RTA)	
					
					
			* Stage 2: Regress the estimates of pair fixed effects on gravity variables and country fixed effects
				* Perform the regression for the baseline year
					keep if year == 1994
					
				* Specify the dependent variable as the estimates of pair fixed
				* effects
					generate tij = exp(gamma_ij)
					
				* Create the exporters and importers fixed effects	
					quietly tabulate exporter, gen(EXPORTER_FE)
					quietly tabulate importer, gen(IMPORTER_FE)
					
				* Estimate the standard gravity model 
				ppml tij EXPORTER_FE* IMPORTER_FE* ln_DIST CNTG LANG CLNY if exporter != importer, cluster(pair_id)
					estimates store gravity_est
					
				* Create the predicted values 	
					predict tij_noRTA, mu
						replace tij_noRTA = 1 if exporter == importer

				* Replace the missing trade costs with predictions from the
				* standard gravity regression
					replace tij_bar = tij_noRTA if tij_bar == . 
					replace tij_bln = tij_bar * exp(RTA_est*RTA) if tij_bln == .
				
				* Specify the complete set of bilateral trade costs in log to
				* be used as a constraint in the PPML estimation of the 
				* structural gravity model
					generate ln_tij_bln = log(tij_bln)	
					
		* Set the number of exporter fixed effects variables
		quietly ds EXPORTER_FE*
		global N = `: word count `r(varlist)'' 
		global N_1 = $N - 1	
	
		* Estimate the gravity model in the "baseline" scenario with the PPML
		* estimator constrained with the complete set of bilateral trade costs
		ppml trade EXPORTER_FE* IMPORTER_FE1-IMPORTER_FE$N_1 , iter(30) noconst offset(ln_tij_bln)
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

		* Compute the variable of bilateral trade costs, i.e. the fitted trade
		* value by omitting the exporter and importer fixed effects		
			generate tij_BLN = tij_bln			

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
	* The counterfactual scenario consists in removing the impact of the NAFTA
	* by re-specifying the RTA variable with zeros for the country pairs 
	* associated with the NAFTA
		generate RTA_NO_NAFTA = RTA
			replace RTA_NO_NAFTA=0 if (exporter == "CAN" & importer == "USA") | (exporter == "CAN" & importer == "MEX") | (exporter == "MEX" & importer == "USA") | (exporter == "MEX" & importer == "CAN") | (exporter == "USA" & importer == "MEX") | (exporter == "USA" & importer == "CAN")

		* Constructing the counterfactual bilateral trade costs	by imposing the
		* constraints associated with the counterfactual scenario
			generate tij_CFL = tij_bar * exp(RTA_est * RTA_NO_NAFTA) 
			
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
			bysort year: generate phi = E/Y if exporter == importer
			
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
		*	forvalues i = 1 (1) $N_1 {
		*		quietly replace IMPORTER_FE`i' = IMPORTER_FE`i' * exp(_b[IMPORTER_FE`i'])
		*	}		
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
	save "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULLGE.dta", replace


* Step IV: Collect, construct, and report indexes of interest
	use "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULLGE.dta", clear
		collapse(mean) OMR_FULL OMR_CDL OMR_BLN change_pricei_FULL Xi_* Y_BLN Y_FULL, by(exporter)
			rename exporter country
			replace country = "DEU" if country == "ZZZ"
			sort country
		
		* Percent change in full endowment general equilibrium of factory-gate prices
			generate change_price_FULL = (1 - change_pricei_FULL) / 1 * 100
			
		* Percent change in full endowment general equilibirum of outward multilateral resistances
			generate change_OMR_CDL = (-OMR_CDL^(1/(1-sigma)) + OMR_BLN^(1/(1-sigma))) / OMR_CDL^(1/(1-sigma)) * 100
		
		* Percent change in full endowment general equilibrium of outward multilateral resistances			
			generate change_OMR_FULL = (-OMR_FULL^(1/(1-sigma)) + OMR_BLN^(1/(1-sigma))) / OMR_FULL^(1/(1-sigma)) * 100

		* Percent change in conditional general equilibrium of bilateral trade
			generate change_Xi_CDL = (-Xi_CDL + Xi_BLN) / Xi_CDL * 100	
			
		* Percent change in full endowment general equilibrium of bilateral trade		
			generate change_Xi_FULL = (-Xi_FULL + Xi_BLN) / Xi_FULL * 100
	save "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULL_PROD.dta", replace


	* Construct the percentage changes on import/consumption side
	use "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULLGE.dta", clear
		collapse(mean) IMR_FULL IMR_CDL IMR_BLN, by(importer)
			rename importer country
			replace country = "DEU" if country == "ZZZ"
			sort country		

		* Conditional general equilibrium of inward multilateral resistances
			generate change_IMR_CDL = (-IMR_CDL^(1/(1-sigma)) + IMR_BLN^(1/(1-sigma))) / IMR_CDL^(1/(1-sigma)) * 100
			
		* Full endowment general equilibrium of inward multilateral resistances
			generate change_IMR_FULL = (-IMR_FULL^(1/(1-sigma)) + IMR_BLN^(1/(1-sigma))) / IMR_FULL^(1/(1-sigma)) * 100
	save "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULL_CONS.dta", replace

	* Merge the general equilibrium results from the production and consumption
	* sides
	use "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULL_PROD.dta", clear
		joinby country using "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULL_CONS.dta"
		
		* Full endowment general equilibrium of real GDP
			generate rGDP_BLN = Y_BLN / (IMR_BLN ^(1 / (1 -sigma)))
			generate rGDP_FULL = Y_FULL / (IMR_FULL ^(1 / (1 -sigma)))
				generate change_rGDP_FULL = (-rGDP_FULL + rGDP_BLN) / rGDP_FULL * 100
			
		* Keep indexes of interest	
			keep country change_Xi_CDL change_Xi_FULL change_price_FULL change_IMR_FULL change_rGDP_FULL Y_BLN
			order country change_Xi_CDL change_Xi_FULL change_price_FULL change_IMR_FULL change_rGDP_FULL Y_BLN
				
	* Export the results in Excel
		export excel using "Applications/2_RTAsEffects/Results/2_RTAsEffects_FULL.xls", firstrow(variables) replace

		
********************************************************************************	
	
