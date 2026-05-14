// simulating data for testing analysis methods
// based on Daniel et al. 2013
** Generating dataset 1 **
clear 
clear matrix 
set seed 10330 
set obs 2000 
gen u=uniform()<0.4 
gen a0=uniform()<0.5 
qui gen l=uniform()<0.25+0.3*a0-0.2*u-0.05*a0*u 
qui gen a1=uniform()<0.4+0.5*a0-0.3*l-0.4*a0*l 
qui gen y=2.5-0.5*a0-0.75*a1-u+0.2*a0*a1+0.2*invnormal(uniform()) 
save "/Users/darwin.delcastillofernandez/Documents/GitHub/financial_distress/data/sim_data/sim1.dta", replace

** Generating dataset 2 **
clear
clear matrix
set seed 12720
set obs 2000
gen u=uniform()<0.4 
gen a0=rnormal() 
gen l=runiform()<(exp(-1.2+0.5*a0-u)/(1+exp(-1.2+0.5*a0-u))) 
gen a1=0.2+0.6*a0-l+0.75*rnormal() 
gen y=0.2-0.12*a0-0.2*a1-0.5*u+0.93*rnormal()
save "/Users/darwin.delcastillofernandez/Documents/GitHub/financial_distress/data/sim_data/sim2.dta", replace

// dataset 3 contains T = 3, i.e. binary confounder anaemia (L) is measured at visits 0,1,2,3, treatment is also measured at those, Lt is affected by At-1, and At is affected by Lt and At-1.
** Generating dataset 3 **
clear
clear matrix
set seed 10330
set obs 2000
gen u=uniform()<0.4
qui gen l0=uniform()<exp(0.4-0.3*u)/(1+exp(0.4-0.3*u))
qui gen a0=uniform()<exp(0.65-0.5*l0)/(1+exp(0.65-0.5*l0)) 
qui gen l1=uniform()<exp(0.25+0.3*a0+0.2*l0-0.2*u-0.05*a0*l0)/ /// 
    (1+exp(0.25+0.3*a0+0.2*l0-0.2*u-0.05*a0*l0)) 
qui gen a1=uniform()<exp(0.4+0.5*a0-0.3*l1-0.4*a0*l1)/ /// 
    (1+exp(0.4+0.5*a0-0.3*l1-0.4*a0*l1)) 
qui gen l2=uniform()<exp(0.25+0.3*a1+0.2*l1-0.2*u-0.05*a1*l1)/ /// 
    (1+exp(0.25+0.3*a1+0.2*l1-0.2*u-0.05*a1*l1))
qui gen a2=uniform()<exp(0.4+0.5*a1-0.3*l2-0.4*a1*l2)/ /// 
    (1+exp(0.4+0.5*a1-0.3*l2-0.4*a1*l2)) 
qui gen l3=uniform()<exp(0.25+0.3*a2+0.2*l2-0.2*u-0.05*a2*l2)/ /// 
    (1+exp(0.25+0.3*a2+0.2*l2-0.2*u-0.05*a2*l2)) 
qui gen a3=uniform()<exp(0.4+0.5*a2-0.3*l3-0.4*a2*l3)/ /// 
    (1+exp(0.4+0.5*a2-0.3*l3-0.4*a2*l3)) 
qui gen y=2.5-0.5*(a0+a1+a2+a3)-u+0.2*invnormal(uniform()) 
save "/Users/darwin.delcastillofernandez/Documents/GitHub/financial_distress/data/sim_data/sim3.dta", replace
