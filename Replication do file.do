********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************
/*

Ryan P. Thombs, Andrew K. Jorgenson, and Brett Clark. "Reducing U.S. Military Spending Could Lead to Substantial Decreases in Energy Consumption." 

The code below corresponds to the analyses in the article and supplemental material. 


The code for the forecast is presented at the bottom of the do file. 

*/


********************************************************************************
********************************************************************************
********************************************************************************
********************************************************************************

*Load military_energy data 
	use "DATA FILE HERE"




*tsset data  
	tsset year  



*Generate Partial Sums
	gen dexp=d.lnexp
	gen p = dexp*(dexp>0) if dexp != .
	gen n = dexp*(dexp<0) if dexp != .
	gen xp = sum(p) if p !=. // positive sum
	gen xn = sum(n) if n !=. // negative sum






*Figure 1 
	twoway (tsline DOD_energy, xlabel(,labsize(medium)) ylabel(,labsize(medium)) yaxis(1) lwidth(thick) xtitle("Year", size(medium)) ///
	ytitle("Energy Consumption (Trillion BTU)", size(medium)) text(1390 1975 "Total DOD", size(small))) (tsline facility, yaxis(1) ///
	text(479 1975 "Facility", size(small))) (tsline VE, yaxis(1) text(951 1977 "Vehicle & Equipment", size(small))) ///
	(tsline milexp, ylabel(,labsize(medium) axis(2)) lpattern(dash) yaxis(2) ytitle("Military Expenditures (Millions)", /// 
	axis(2) size(medium)) lwidth(thick) text(245 1974 "Military Exp.", size(small)) legend(off)) ///

	
	

********************************************************************************
*																			   *
* Summary Statistics Reported in Supplemental Information Table 1. 			   *
*																			   *		
********************************************************************************					

	sum lnden lnVE lnf lnjf lnother lnDOD_nor lnjfper xp xn if year<2023





********************************************************************************
*																			   *
*			            Asymmetry Analysis									    *
*																			   *
********************************************************************************

*NARDL Analysis for Total Energy Consumption (Table 1 and Supplementary Table 1)



*NARDL Analysis for Total Energy (Table 1 and Supplementary Material Table 2, Column 1)
	reg d.lnden l.lnden d.xp l.xp d.xn l.xn if year<2023
	estimates store den
	estat sbcusum 
	estat bgodfrey, lags(1/3)
	estat durbina , lags(1/3) 

** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn]

** LR Effects
	mat lr = J(12,5,.)
	nlcom -_b[l.xp]/_b[l.lnden]
	mat lr[1,1] = r(table)[1,1]
	mat lr[1,2] = r(table)[2,1]
	mat lr[1,3] = r(table)[3,1]
	mat lr[1,4] = r(table)[5,1]
	mat lr[1,5] = r(table)[6,1]

	nlcom -_b[l.xn]/_b[l.lnden]
	mat lr[2,1] = r(table)[1,1]
	mat lr[2,2] = r(table)[2,1]
	mat lr[2,3] = r(table)[3,1]
	mat lr[2,4] = r(table)[5,1]
	mat lr[2,5] = r(table)[6,1]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnden]) = (-_b[l.xn]/_b[l.lnden]) 









*NARDL Analysis for Facility Energy Consumption (Table 1 and Supplementary Material Table 2, Column 2)
	reg d.lnf l.lnf d.xp l.xp d.xn l.xn ld.xn if year<2023
	estimates store f1
	estat sbcusum 
	estat bgodfrey, lags(1/3) 
	estat durbina , lags(1/3) 


** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn] 

** LR Effects
	nlcom -_b[l.xp]/_b[l.lnf]
	mat lr[3,1] = r(table)[1,1]
	mat lr[3,2] = r(table)[2,1]
	mat lr[3,3] = r(table)[3,1]
	mat lr[3,4] = r(table)[5,1]
	mat lr[3,5] = r(table)[6,1]

	nlcom -_b[l.xn]/_b[l.lnf]
	mat lr[4,1] = r(table)[1,1]
	mat lr[4,2] = r(table)[2,1]
	mat lr[4,3] = r(table)[3,1]
	mat lr[4,4] = r(table)[5,1]
	mat lr[4,5] = r(table)[6,1]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnf]) = (-_b[l.xn]/_b[l.lnf])


		


		
		
*NARDL Analysis for Vehicle and Equipment Energy Consumption (Table 1 and Supplementary Material Table 2, Column 3)
	reg d.lnVE l.lnVE d.xp l.xp d.xn l.xn if year<2023
	estimates store ve
	estat sbcusum 
	estat bgodfrey, lags(1/3) 
	estat durbina , lags(1/3) 

** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn] 

** LR Effects
	nlcom -_b[l.xp]/_b[l.lnVE]
	mat lr[5,1] = r(table)[1,1]
	mat lr[5,2] = r(table)[2,1]
	mat lr[5,3] = r(table)[3,1]
	mat lr[5,4] = r(table)[5,1]
	mat lr[5,5] = r(table)[6,1]

	nlcom -_b[l.xn]/_b[l.lnVE]
	mat lr[6,1] = r(table)[1,1]
	mat lr[6,2] = r(table)[2,1]
	mat lr[6,3] = r(table)[3,1]
	mat lr[6,4] = r(table)[5,1]
	mat lr[6,5] = r(table)[6,1]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnVE]) = (-_b[l.xn]/_b[l.lnVE]) 







*Is it primarily changes in jet fuel consumption? (Table 2 and Supplementary Material Table 3, Columns 1 and 2)

*Jet Fuel
	reg d.lnjf l.lnjf d.xp l.xp d.xn l.xn if year<2023	
	estimates store jf
	estat sbcusum 
	estat bgodfrey, lags(1/3)
	estat durbina , lags(1/3)

** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn] 

** LR Effects
	nlcom -_b[l.xp]/_b[l.lnjf]
	mat lr[7,1] = r(table)[1,1]
	mat lr[7,2] = r(table)[2,1]
	mat lr[7,3] = r(table)[3,1]
	mat lr[7,4] = r(table)[5,1]
	mat lr[7,5] = r(table)[6,1]

	nlcom -_b[l.xn]/_b[l.lnjf]
	mat lr[8,1] = r(table)[1,1]
	mat lr[8,2] = r(table)[2,1]
	mat lr[8,3] = r(table)[3,1]
	mat lr[8,4] = r(table)[5,1]
	mat lr[8,5] = r(table)[6,1]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnjf]) = (-_b[l.xn]/_b[l.lnjf]) 





*Other 
	reg d.lnother l.lnother d.xp l.xp d.xn l.xn ld.(lnother xp xn) l2d.(lnother xp xn) if year<2023
	estimates store other
	estat sbcusum 
	estat bgodfrey, lags(1/3) 
	estat durbina , lags(1/3) 

** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn] 

** LR Effects
	nlcom -_b[l.xp]/_b[l.lnother]
	mat lr[9,1] = r(table)[1,1]
	mat lr[9,2] = r(table)[2,1]
	mat lr[9,3] = r(table)[3,1]
	mat lr[9,4] = r(table)[5,1]
	mat lr[9,5] = r(table)[6,1]

	nlcom -_b[l.xn]/_b[l.lnother]
	mat lr[10,1] = r(table)[1,1]
	mat lr[10,2] = r(table)[2,1]
	mat lr[10,3] = r(table)[3,1]
	mat lr[10,4] = r(table)[5,1]
	mat lr[10,5] = r(table)[6,1]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnother]) = (-_b[l.xn]/_b[l.lnother]) 






*Test whether effects are different
	suest jf other


*Test whether SR effects are different (Supplementary Material Table 5)
	test _b[jf_mean:d.xp]=_b[other_mean:d.xp] 
	test _b[jf_mean:d.xn]=_b[other_mean:d.xn] 

*Test whether LR effects are different (Supplementary Material Table 5)
	testnl (-_b[jf_mean:l.xp]/_b[jf_mean:l.lnjf]) = (-_b[other_mean:l.xp]/_b[other_mean:l.lnother]) 
	testnl (-_b[jf_mean:l.xn]/_b[jf_mean:l.lnjf]) = (-_b[other_mean:l.xn]/_b[other_mean:l.lnother])






*Is effect different with renewables removed? (Supplementary Material Table 3, Column 3)
	reg d.lnDOD_nor l.lnDOD_nor d.xp l.xp d.xn l.xn if year<2023
	estat sbcusum 
	estat bgodfrey, lags(1/3)
	estat durbina , lags(1/3) 


** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn] // Symmetric 

** LR Effects
	nlcom -_b[l.xp]/_b[l.lnDOD_nor]
	mat lr[11,1] = r(table)[1,1]
	mat lr[11,2] = r(table)[2,1]
	mat lr[11,3] = r(table)[3,1]
	mat lr[11,4] = r(table)[5,1]
	mat lr[11,5] = r(table)[6,1]

	nlcom -_b[l.xn]/_b[l.lnDOD_nor]
	mat lr[12,1] = r(table)[1,1]
	mat lr[12,2] = r(table)[2,1]
	mat lr[12,3] = r(table)[3,1]
	mat lr[12,4] = r(table)[5,1]
	mat lr[12,5] = r(table)[6,1]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnDOD_nor]) = (-_b[l.xn]/_b[l.lnDOD_nor])  	






*Compare Total Energy to Fossil Fuels Only 
	reg d.(lnden xp xn) l.(lnden xp xn) if year<2023
	estimates store all
	reg d.(lnDOD_nor xp xn) l.(lnDOD_nor xp xn) if year<2023
	estimates store noren
	suest all noren

*Get SR & LR effects in easy to read format
	nlcom (SR_all_p:_b[all_mean:d.xp]) (SR_noren_p:_b[noren_mean:d.xp]) (LR_all_p:-_b[all_mean:l.xp]/_b[all_mean:l.lnden]) ///
	(LR_noren_p:-_b[noren_mean:l.xp]/_b[noren_mean:l.lnDOD_nor]) (SR_all_n:_b[all_mean:d.xn]) (SR_noren_n:_b[noren_mean:d.xn]) ///
	(LR_all_n:-_b[all_mean:l.xn]/_b[all_mean:l.lnden]) (LR_noren_n:-_b[noren_mean:l.xn]/_b[noren_mean:l.lnDOD_nor]) ///


*Test whether SR effects are different (Supplementary Material Table 5)
	test _b[all_mean:d.xp]=_b[noren_mean:d.xp]
	test _b[all_mean:d.xn]=_b[noren_mean:d.xn]

*Test whether LR effects are different (Supplementary Material Table 5)
	testnl (-_b[all_mean:l.xp]/_b[all_mean:l.lnden]) = (-_b[noren_mean:l.xp]/_b[noren_mean:l.lnDOD_nor])
	testnl (-_b[all_mean:l.xn]/_b[all_mean:l.lnden]) = (-_b[noren_mean:l.xn]/_b[noren_mean:l.lnDOD_nor])




	matrix colnames lr = b se z ll ul 
	matrix rownames lr = xp xn xp xn xp xn xp xn xp xn xp xn
	matlist lr 
	**z-score for Fxp[3,3] and Oxp[9,3] are negative, multiply them by -1
	mat lr[3,3] = lr[3,3]*-1
	mat lr[9,3] = lr[9,3]*-1






*% Jet Fuel Total Energy Consumption (Supplementary Information Table 3, Column 4)
	reg d.lnjfper l.lnjfper d.xp l.xp d.xn l.xn if year<2023
	estat bgodfrey, lags(1/3) 
	estat durbina , lags(1/3) 
	estat sbcusum 

** Test for SR Asymmetry 
	test _b[D.xp]=_b[D.xn] 

** LR Effects
	nlcom -_b[l.xp]/_b[l.lnjfper]
	nlcom -_b[l.xn]/_b[l.lnjfper]

** Test for LR Asymmetry 
	testnl (-_b[l.xp]/_b[l.lnjfper]) = (-_b[l.xn]/_b[l.lnjfper]) 













*Coefficient Plots

*Figure 2
	coefplot (den, msymbol(S) msize(large) mc("17 112 170") ciopt(lc("17 112 170") recast(rcap))) (f1, msize(large) mc("95 162 206") ///
	ciopt(lc("95 162 206") recast(rcap))) (ve, msymbol(D) msize(large) mc("252 125 11") ciopt(lc("252 125 11") recast(rcap))) ///
	(jf, msymbol(T) msize(large) mc("163 172 185") ciopt(lc("163 172 185") recast(rcap))) (other, msymbol(X) msize(large) ///
	mc("87 96 108") ciopt(lc("87 96 108") recast(rcap))), keep(D.xp D.xn) xline(0, lcolor(black)) xlabel(, labsize(medlarge)) /// 
	ciopts(recast(rcap)) plotlabels("Total" "Facility" "V&E" "Jet Fuel" "All Other") ///
	coeflabels(D.xp = "Military Exp.{sup:Increase}" D.xn = "Military Exp.{sup:Decrease}") /// 
	subtitle(, size(large) fcolor(none) lcolor(none)) bycoefs byopts(noiylabel noiytick title("Short-Run Effects")) ///
	legend(title("Models") cols(5) size(medlarge)) ///


	forval i= 1/ `=rowsof(lr)'{
		mat lr`i'= lr[`i', 1..5]
	}

*Figure 3
	coefplot (matrix(lr1[,1]), ci((lr1[,4] lr1[,5])) at(lr1[,3]) msymbol(S) msize(large) mc("17 112 170") ///
	ciopt(lc("17 112 170") recast(rcap))) (matrix(lr3[,1]), ci((lr3[,4] lr3[,5])) at(lr3[,3]) msymbol(O) ///
	msize(large) mc("95 162 206") ciopt(lc("95 162 206") recast(rcap))) (matrix(lr5[,1]), ci((lr5[,4] lr5[,5])) ///
	at(lr5[,3]) msymbol(D) msize(large) mc("252 125 11") ciopt(lc("252 125 11") recast(rcap))) (matrix(lr7[,1]), ci((lr7[,4] lr7[,5])) ///
	at(lr7[,3]) msymbol(T) msize(large) mc("163 172 185") ciopt(lc("163 172 185") recast(rcap))) (matrix(lr9[,1]), ci((lr9[,4] lr9[,5])) ///
	at(lr9[,3]) msymbol(X) msize(large) mc("87 96 108") ciopt(lc("87 96 108") ///
	recast(rcap))), bylabel(Military Exp.{sup:Increase}) || (matrix(lr2[,1]), ci((lr2[,4] lr2[,5])) ///
	at(lr2[,3]) msymbol(S) msize(large) mc("17 112 170") ciopt(lc("17 112 170") recast(rcap))) ///
	(matrix(lr4[,1]), ci((lr4[,4] lr4[,5])) at(lr4[,3]) msymbol(O) msize(large) mc("95 162 206") ///
	ciopt(lc("95 162 206") recast(rcap))) (matrix(lr6[,1]), ci((lr6[,4] lr6[,5])) at(lr6[,3]) msymbol(D) ///
	msize(large) mc("252 125 11") ciopt(lc("252 125 11") recast(rcap))) (matrix(lr8[,1]), ci((lr8[,4] lr8[,5])) ///
	at(lr8[,3]) msymbol(T) msize(large) mc("163 172 185") ciopt(lc("163 172 185") recast(rcap))) ///
	(matrix(lr10[,1]), ci((lr10[,4] lr10[,5])) at(lr10[,3]) msymbol(X) msize(large) mc("87 96 108") ciopt(lc("87 96 108") ///
	recast(rcap))), bylabel(Military Exp.{sup:Decrease}) horizontal plotlabels("Total" "Facility" "V&E" "Jet Fuel" "All Other") ///
	addplot(scatteri 1.4 -1 "Inconclusive", msymbol(i) || scatteri 1.10 -1 1.10 2 3.68 2 3.68 -1, recast(area) color(gray%25) ///
	lwidth(none)) legend(title("Models") cols(5) size(medlarge)) subtitle(, size(large) fcolor(none) lcolor(none)) ///
	byopts(title("Long-Run Effects")) ytitle("z-statistic", size(medlarge)) xlabel(, labsize(medlarge)) ylabel(, labsize(medlarge)) ///




	
	
	
	
	
	
	
	
	
********************************************************************************
*																			   *
* Unit Root Tests Reported in Supplemental Information Table 4. 			   *
*                                                                              *
********************************************************************************


*Unit Root Tests for Level of lnden

* Null: Unit root with drift 
	dfgls lnden
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls lnden, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss lnden, trend auto qs 

* Null: Mean stationary 
	kpss lnden, notrend auto qs






*Is lnden I(1)?

* Null: Unit root with drift 
	dfgls d.lnden 
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")


* Null: Unit root 
	dfgls d.lnden, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 


* Null: Trend stationary 
	kpss d.lnden, trend auto qs 

* Null: Mean stationary 
	kpss d.lnden, notrend auto qs









*Unit Root Tests for Level of lnf

* Null: Unit root with drift 
	dfgls lnf
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls lnf, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss lnf, trend auto qs 

* Null: Mean stationary 
	kpss lnf, notrend auto qs 




*Is lnf I(1)?

* Null: Unit root with drift 
	dfgls d.lnf 
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")


* Null: Unit root 
	dfgls d.lnf, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 


* Null: Trend stationary 
	kpss d.lnf, trend auto qs

* Null: Mean stationary 
	kpss d.lnf, notrend auto qs





*Unit Root Tests for Level of lnVE

* Null: Unit root with drift 
	dfgls lnVE
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls lnVE, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss lnVE, trend auto qs 

* Null: Mean stationary 
	kpss lnVE, notrend auto qs




*Is lnVE I(1)?

* Null: Unit root with drift 
	dfgls d.lnVE
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls d.lnVE, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss d.lnVE, trend auto qs 

* Null: Mean stationary 
	kpss d.lnVE, notrend auto qs





*Unit Root Tests for Level of lnjf

* Null: Unit root with drift 
	dfgls lnjf
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls lnjf, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss lnjf, trend auto qs 

* Null: Mean stationary 
	kpss lnjf, notrend auto qs






*Is lnjf I(1)?

* Null: Unit root with drift 
	dfgls d.lnjf 
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")


* Null: Unit root 
	dfgls d.lnjf, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 


* Null: Trend stationary 
	kpss d.lnjf, trend auto qs 

* Null: Mean stationary 
	kpss d.lnjf, notrend auto qs






*Unit root tests for jet fuel (% energy consumption)

* Null: Unit root with drift 
	dfgls lnjfper
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")


* Null: Unit root 
	dfgls lnjfper, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 


* Null: Trend stationary 
	kpss lnjfper, trend auto qs 

* Null: Mean stationary 
	kpss lnjfper, notrend auto qs





*Is lnjfper I(1)?

* Null: Unit root with drift 
	dfgls d.lnjfper 
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")


* Null: Unit root 
	dfgls d.lnjfper, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 


* Null: Trend stationary 
	kpss d.lnjfper, trend auto qs 

* Null: Mean stationary 
	kpss d.lnjfper, notrend auto qs







*Unit Root Tests for positive changes in military expenditures

* Null: Unit root with drift  
	dfgls xp
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls xp, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss xp, trend auto qs 

* Null: Mean stationary 
	kpss xp, notrend auto qs 



*Is xp I(1)?

* Null: Unit root with drift 
	dfgls d.xp
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls d.xp, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss d.xp, trend auto qs

* Null: Mean stationary 
	kpss d.xp, notrend auto qs




*Unit Root Tests for negative changes in military expenditures

* Null: Unit root with drift 
	dfgls xn
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls xn, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss xn, trend auto qs

* Null: Mean stationary 
	kpss xn, notrend auto qs




*Is xn I(1)?

* Null: Unit root with drift 
	dfgls d.xn
	scalar sic = r(maxlag) - r(siclag) + 1
	mat a = r(cvalues)[sic,1..5]
	mata:a = st_matrix("a")

* Null: Unit root 
	dfgls d.xn, notrend
	scalar sic = r(maxlag) - r(siclag) + 1
	mat b = r(cvalues)[sic,1..5]
	mata:b = st_matrix("b")

	mata: c = (a\b)
	mata: st_matrix("c",c)
	matrix colnames c = lag stat 1cv 5cv 10cv
	matlist c 

* Null: Trend stationary 
	kpss d.xn, trend auto qs

* Null: Mean stationary 
	kpss d.xn, notrend auto qs















*******************************************
*				Forecast                  *
*******************************************

	*ECM and ARDL produce same graph
	/*
	Note: Numbers will be slightly different from article because they are based
	on simulations
	*/
	
	
*% changes 
	sum milexp_ch if milexp_ch > 0, detail // 10th = 0.63%, 50th = 4.71%, 90th = 12.28%
	sum milexp_ch if milexp_ch < 0, detail // 10th = -0.52%, 50th = -2.31%, 90th = -6.59%
	sum milexp_ch, detail // Historical Median = 0.57

	insobs 10 
	replace year = 2023 in 49
	replace year = 2024 in 50
	replace year = 2025 in 51
	replace year = 2026 in 52
	replace year = 2027 in 53
	replace year = 2028 in 54
	replace year = 2029 in 55
	replace year = 2030 in 56
	replace year = 2031 in 57
	replace year = 2032 in 58


*Historical Median 
	sort year
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xp = L.xp + .0057 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _p_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _p_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_med 
	rename _p_up _p_up_med
	rename _p_dn _p_dn_med 
	
	gen p_med_exp = exp(p_med)*exp((`e(rmse)'^2)/2)
	gen p_up_exp = exp(_p_up_med)*exp((`e(rmse)'^2)/2)
	gen p_dn_exp = exp(_p_dn_med)*exp((`e(rmse)'^2)/2)
	
drop sd_lnden 
	

	
	

*Increase
	
	
*10th Percentile Increase
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xp = L.xp + .0063 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _p_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _p_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_I10 
	rename _p_up _p_up_I10
	rename _p_dn _p_dn_I10 
	
	gen p_I10_exp = exp(p_I10)*exp((`e(rmse)'^2)/2)
	gen p_I10U_exp = exp(_p_up_I10)*exp((`e(rmse)'^2)/2)
	gen p_I10D_exp = exp(_p_dn_I10)*exp((`e(rmse)'^2)/2)	
	
drop sd_lnden 
	
*Median Percentile Increase
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xp = L.xp + .0471 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _p_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _p_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_I50 
	rename _p_up _p_up_I50
	rename _p_dn _p_dn_I50 
	
	gen p_I50_exp = exp(p_I50)*exp((`e(rmse)'^2)/2)
	gen p_I50U_exp = exp(_p_up_I50)*exp((`e(rmse)'^2)/2)
	gen p_I50D_exp = exp(_p_dn_I50)*exp((`e(rmse)'^2)/2)	

drop sd_lnden 

*90th Percentile Increase
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xp = L.xp + .1228 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _p_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _p_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_I90 
	rename _p_up _p_up_I90
	rename _p_dn _p_dn_I90 
	
	gen p_I90_exp = exp(p_I90)*exp((`e(rmse)'^2)/2)
	gen p_I90U_exp = exp(_p_up_I90)*exp((`e(rmse)'^2)/2)
	gen p_I90D_exp = exp(_p_dn_I90)*exp((`e(rmse)'^2)/2)
	
	
	
	


	


		
		
drop sd_lnden 






* Decrease

*10th Percentile Decrease
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xn = L.xn - .0052 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _n_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _n_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_D10 
	rename _n_up _p_up_D10
	rename _n_dn _p_dn_D10 
	
	gen p_D10_exp = exp(p_D10)*exp((`e(rmse)'^2)/2)
	gen p_D10U_exp = exp(_p_up_D10)*exp((`e(rmse)'^2)/2)
	gen p_D10D_exp = exp(_p_dn_D10)*exp((`e(rmse)'^2)/2)	
	
drop sd_lnden 	
	
		
*50th Percentile Decrease
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xn = L.xn - .0231 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _n_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _n_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_D50 
	rename _n_up _p_up_D50
	rename _n_dn _p_dn_D50 
	
	gen p_D50_exp = exp(p_D50)*exp((`e(rmse)'^2)/2)
	gen p_D50U_exp = exp(_p_up_D50)*exp((`e(rmse)'^2)/2)
	gen p_D50D_exp = exp(_p_dn_D50)*exp((`e(rmse)'^2)/2)	
	
drop sd_lnden 		
		
*90th Percentile Decrease
	foreach var of varlist xp xn {
		replace `var' = L.`var' if year> 2022
	}
	replace xn = L.xn - .0659 if year>2022
	quietly reg lnden l.lnden xp l.xp xn l.xn
	estimates store reg 
	forecast create reg, replace 
	forecast estimates reg, predict(xb)
	forecast exogenous xp xn 
	forecast solve, begin(2023) end(2032) prefix(p_) simulate(betas errors, statistic(stddev, prefix(sd_)) reps(100)) actuals 
	gen _n_up = p_lnden + invnormal(0.975)*sd_lnden
	gen _n_dn = p_lnden + invnormal(0.025)*sd_lnden

	rename p_lnden p_D90 
	rename _n_up _p_up_D90
	rename _n_dn _p_dn_D90 
	
	gen p_D90_exp = exp(p_D90)*exp((`e(rmse)'^2)/2)
	gen p_D90U_exp = exp(_p_up_D90)*exp((`e(rmse)'^2)/2)
	gen p_D90D_exp = exp(_p_dn_D90)*exp((`e(rmse)'^2)/2)	
		
		

		
		
*Figure 4

	twoway (tsline DOD_energy if year<2023 & year>2009) (tsline p_med_exp if year>2021) ///
	(rarea p_up_exp p_dn_exp year if year>2009, fintensity(30) fcolor(%30) lwidth(none)) (tsline p_I10_exp if year>2021) ///
	(rarea p_I10U_exp p_I10D_exp year if year>2009, fintensity(30)  fcolor(%30) lwidth(none)) (tsline p_I50_exp if year>2021) ///
	(rarea p_I50U_exp p_I50D_exp year if year>2009, fintensity(30) fcolor(%30) lwidth(none)) (tsline p_I90_exp if year>2021) ///
	(rarea p_I90U_exp p_I90D_exp year if year>2009, fintensity(30) fcolor(%30) lwidth(none)) (tsline p_D10_exp if year>2021) ///
	(rarea p_D10U_exp p_D10D_exp year if year>2009, fintensity(30) fcolor(%30) lwidth(none)) (tsline p_D50_exp if year>2021) ///
	(rarea p_D50U_exp p_D50D_exp year if year>2009, fintensity(30) fcolor(%30) lwidth(none)) (tsline p_D90_exp if year>2021) ///
	(rarea p_D90U_exp p_D90D_exp year if year>2009, fintensity(30) fcolor(%30) lwidth(none)), ///
	legend(order(1 "Observed Energy Cons." 2 "Median Hist. Change (+0.57%)" - "Increase" 4 "+0.63% (10th Percentile)" ///
	6 "+4.71% (50th Percentile)" 8 "+12.28% (90th Percentile)" - "Decrease" 10 "-0.52% (10th Percentile)" 12 "-2.31% (50th Percentile)" ///
	14 "-6.59% (90th Percentile)") position(0) bplacement(swest) col(1) region(fcolor(gs15) lcolor(black))) ///
	xline(2023, lcolor(black)) ylabel(0 (200) 1000,labsize(medium)) xlabel(2010 (5) 2032,labsize(medium)) ///
	xtitle("Year", size(medium)) ytitle("Energy Consumption (Trillion BTU)", size(medium)) ///
	text(300 2022.5 "2023 (Start of Forecast)", orientation(vertical)) ///
				

		
