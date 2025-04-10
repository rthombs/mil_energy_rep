Ryan P. Thombs, Andrew K. Jorgenson, and Brett Clark. "Reducing U.S. Military Spending Could Lead to Substantial Decreases in Energy Consumption." 

The data file "mil_data_75_22" contains the data for military expenditures and DOD energy consumption. There are 21 variables.

year	      : year
DOD_energy    : DOD energy consumption
facility      : Facility energy consumption
VE            : Vehicle and equipment energy consumption
jetfuel	      : Jet fuel consumption
punch_ren     : Purchased renewable energy 
on_ren        : On-site renewable energy  
other_en      : Energy consumption other than jet fuel
milexp        : SIPRI military expenditures by country (constant 2021$)
lnden         : Natural logarithm of DOD_energy
lnexp         : Natural logarithm of milexp
lnf           : Natural logarithm of Facility_DOD
lnVE          : Natural logarithm of VE_DOD
lnjf	      : Natural logarithm of jetfuel 
lnother       : Natural logarithm of other_en
milexp_ch     : % change change in military expenditures ((milexp-l.milexp)/l.milexp)*100
tot_ren	      : purch_ren + on_ren (total renewable energy) 
DOD_minus_ren : DOD_energy - tot_ren (total fossil fuel energy) 
lnDOD_nor     : ln DOD_minus_ren
per_jf	      : % energy consumption from jet fuel
lnjfper	      : Natural logarithm of per_jf

There are 48 years in the analysis (1975-2022). 
