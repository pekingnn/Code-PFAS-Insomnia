clear all
use "data.dta"

//PFASs
local exposure "PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs"
foreach out of local exposure{
	gen `out'_ln = log(`out')/log(2)
}
save "data_insomnia.dta"

//Baseline
local exposure "age BMI"
foreach out of local exposure{
    bysort insom: sum `out'
	sktest `out'
	ranksum `out', by(insom)
}
local exposure "occupation education"
foreach out of local exposure{
    codebook `out'
    tabulate `out' insom, chi2 column expected
}

//Concentration
ssc install logout, replace
logout, save(percent0) excel replace: tabstat PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs, s(min p5 p25 p50 p75 p95 max) ,if insom==0
logout, save(mean0) excel replace: ameans PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs if insom==0
logout, save(percent1) excel replace: tabstat PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs, s(min p5 p25 p50 p75 p95 max) ,if insom==1
logout, save(mean1) excel replace: ameans PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs if insom==1

//Association
local exposure "PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs"
foreach out of local exposure{
quantiles `out'_ln, gen(`out'_Q2) n(2)
}

local exposure "PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs"
foreach out of local exposure{
quantiles `out'_ln, gen(`out'_Q4) n(4)
}

local exposure "PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs"
foreach out of local exposure{
bysort `out'_Q4:egen `out'_four=pctile(`out'_ln), p(50)
}

putexcel set "Association", sheet("Table3", replace) modify
putexcel A1=("pollutants") B1=("con") C1=("2q2") D1=("4q2") E1=("4q3") F1=("4q4") G1=("pvalue") 

local x=1
set more off

local exposure "PFHxA PFHpA PFOA PFNA PFDA PFUdA PFHxS PFHpS PFOS F53B_9Cl PFASs"
foreach out of local exposure{
	
	local x=`x'+1	
	putexcel A`x'=("`out'")
	
	local x=`x'+1
	
	logit insom c.`out'_ln ,or
	matrix a=r(table)
	local con=string(a[1,1],"%9.2f")+" ("+string(a[5,1],"%9.2f")+","+string(a[6,1],"%9.2f")+")" 
	
	logit insom ib1.`out'_Q2  ,or
	matrix b=r(table)
	local 2q2=string(b[1,2],"%9.2f")+" ("+string(b[5,2],"%9.2f")+","+string(b[6,2],"%9.2f")+")" 
	
	logit insom ib1.`out'_Q4  ,or
	matrix d=r(table)
	local 4q2=string(d[1,2],"%9.2f")+" ("+string(d[5,2],"%9.2f")+","+string(d[6,2],"%9.2f")+")" 
	local 4q3=string(d[1,3],"%9.2f")+" ("+string(d[5,3],"%9.2f")+","+string(d[6,3],"%9.2f")+")" 
	local 4q4=string(d[1,4],"%9.2f")+" ("+string(d[5,4],"%9.2f")+","+string(d[6,4],"%9.2f")+")" 		
	
	logit insom c.`out'_four ,or 
	local pvalue = string(e(p),"%9.3f")
	
	putexcel B`x'=("`con'") C`x'=("`2q2'") D`x'=("`4q2'")  E`x'=("`4q3'")  F`x'=("`4q4'") G`x'=("`pvalue'") 
 
 
	local x=`x'+1
	
	logit insom c.`out'_ln age BMI occupation education,or
	matrix a=r(table)
	local con=string(a[1,1],"%9.2f")+" ("+string(a[5,1],"%9.2f")+","+string(a[6,1],"%9.2f")+")" 
	
	logit insom ib1.`out'_Q2 age BMI occupation education,or
	matrix b=r(table)
	local 2q2=string(b[1,2],"%9.2f")+" ("+string(b[5,2],"%9.2f")+","+string(b[6,2],"%9.2f")+")" 
	
	logit insom ib1.`out'_Q4 age BMI occupation education,or
	matrix d=r(table)
	local 4q2=string(d[1,2],"%9.2f")+" ("+string(d[5,2],"%9.2f")+","+string(d[6,2],"%9.2f")+")" 
	local 4q3=string(d[1,3],"%9.2f")+" ("+string(d[5,3],"%9.2f")+","+string(d[6,3],"%9.2f")+")" 
	local 4q4=string(d[1,4],"%9.2f")+" ("+string(d[5,4],"%9.2f")+","+string(d[6,4],"%9.2f")+")" 		
	
	logit insom c.`out'_four age BMI occupation education,or 
	local pvalue = string(e(p),"%9.3f")
	
	putexcel B`x'=("`con'") C`x'=("`2q2'") D`x'=("`4q2'")  E`x'=("`4q3'")  F`x'=("`4q4'") G`x'=("`pvalue'")
 
}
