webuse sysdsn1, clear

mlogit insure age male nonwhite i.site
est sto mlogit

gen indemnity = (insure == 1)
gen prepaid = (insure == 2)
gen uninsure = (insure == 3)

/*Calculate conditional choice probabilities using logit model*/

logit indemnity age male nonwhite i.site
predict p1, pr

logit prepaid age male nonwhite i.site
predict p2, pr

logit uninsure age male nonwhite i.site
predict p3, pr

/*Calculate log-odds ratio for dependent variable*/
gen lp2p1 = log(p2/p1)
gen lp3p1 = log(p3/p1)

pause off 

/*Iterate*/
forvalues i = 1/10 {

qui  {

/*Estimate logistic model for each outcome and predict score*/
noi reg lp2p1 age male nonwhite i.site 
predict score2, xb

noi reg lp3p1 age male nonwhite i.site
predict score3, xb

/*Calculate new choice probabilities*/
qui replace p1 = 1/(1 + exp(score2) + exp(score3))
qui replace p2 = exp(score2)/(1 + exp(score2) + exp(score3))
qui replace p3 = exp(score3)/(1 + exp(score2) + exp(score3))

/*Update log-odd ratio*/
qui replace lp2p1 = log(p2/p1)
qui replace lp3p1 = log(p3/p1)

drop score2 score3

pause

}
}

/*Compare to mlogit results*/
est replay mlogit

/*Note: Estimates do not improve upon iteration. I think this is because the true data
is only used once: in the initial conditional choice probability calculation. 
Also, we don't get standard errors on additional iterations because all
observations with the same values for covariates have same conditional choice 
probabilities*/
