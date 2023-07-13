
// Tarea 2 - Macroeconometría Aplicada
// Integrantes: Juan Andrés B. y Vicente Jaramillo

clear all

/*1.1*/

cd "/Users/vicentejaramillo/Downloads/MA/Tarea 3"
import excel "/Users/vicentejaramillo/Downloads/MA/Tarea 3/Datos Tarea 3 Covid - Google.xlsx", sheet("Datos MinSal") cellrange(A1:AK568) firstrow clear case(lower)
order metropolitana metropolitana_casos
drop aricayparinacota - mediamovilcasosrm
rename metropolitana metropolitana_pcr
label var metropolitana_pcr "PCRs RM"
drop  in 1/2
gen date = date(fecha, "MDY")
seq s, f(1) t(81) b(7)
collapse metropolitana_pcr metropolitana_casos date, by(s)
format date %td
save base, replace

import excel "/Users/vicentejaramillo/Downloads/MA/Tarea 3/Datos Tarea 3 Covid - Google.xlsx", sheet("GTCOVID") cellrange(A2:F82) firstrow case(lower) clear
rename totalhits GTCOVID
order GTCOVID
drop date - sintomas
gen date = mod(_n*7+22010,22576) 
format %td date

merge m:m date using base.dta, force
drop if _merge==2
drop _merge s
rename metropolitana_casos c
rename metropolitana_pcr p

tsset date, delta(7)

tw (tsline GTCOVID ,yaxis(1) lc(black)) (tsline p ,yaxis(2) lc(red))
tw (tsline GTCOVID ,yaxis(1) lc(black)) (tsline c ,yaxis(2) lc(red))
correlate GTCOVID p
correlate GTCOVID c


/*1.2*/

forv i=1/20 { 
qui matrix lag`i' = J(1,3,.)
qui mat colnames lag`i' = lags p-fuller p-pperron
qui matrix lag`i'[1,1] = `i'
qui dfuller p, lags(`i') 
qui matrix lag`i'[1,2] = r(p)
qui pperron p, lags(`i') 
qui matrix lag`i'[1,3] = r(pval)
}
forv i=2/20 { 
qui matrix lag1 = lag1\lag`i'
}
mat li lag1

/*1.3*/

gen y = d.p/l.p
gen x = d.c/l.c
gen dGT=d.GTCOVID/l.GTCOVID

forv i=1/20 { 
qui matrix lagg`i' = J(1,5,.)
qui mat colnames lagg`i' = lags p-fuller(y) p-pperron(y) p-fuller(x) p-pperron(x)
qui matrix lagg`i'[1,1] = `i'
qui dfuller y, lags(`i') 
qui matrix lagg`i'[1,2] = r(p)
qui pperron y, lags(`i') 
qui matrix lagg`i'[1,3] = r(pval)
qui dfuller x, lags(`i') 
qui matrix lagg`i'[1,4] = r(p)
qui pperron x, lags(`i') 
qui matrix lagg`i'[1,5] = r(pval)
}
forv i=2/20 { 
qui matrix lagg1 = lagg1\lagg`i'
}
mat li lagg1
dfuller y,lags(5)

newey y l.y, lag(4)
newey y l.y l.dGT l2.dGT, lag(4)
test

newey x l.y, lag(4)
newey x l.y l.dGT l2.dGT, lag(4)
test

/*1.4*/

gen m1y=.
gen m2y=.
gen m1x=.
gen m2x=.
forvalues T = 25/80{
	quietly newey y l.y if _n <= `T', lag(4) 
	quietly predict pm1y
	quietly replace m1y = pm1y if _n == `T'+1
	quietly drop pm1y
	
	quietly newey y l.y l.dGT l2.dGT if _n <= `T', lag(4) 
	quietly predict pm2y
	quietly replace m2y = pm2y if _n == `T'+1
	quietly drop pm2y
	
	quietly newey x l.y if _n <= `T', lag(4) 
	quietly predict pm1x
	quietly replace m1x = pm1x if _n == `T'+1
	quietly drop pm1x
	
	quietly newey x l.y l.dGT l2.dGT if _n <= `T', lag(4) 
	quietly predict pm2x
	quietly replace m2x = pm2x if _n == `T'+1
	quietly drop pm2x
}

tsline y m1y
tsline y m2y
tsline x m1x
tsline x m2x

gen m1ye= y-m1y
gen m2ye= y-m2y
gen m1xe= x-m1x
gen m2xe= x-m2x

tsline m1ye m2ye
tsline m1xe m2xe

newey m1ye, lag(2)
newey m2ye, lag(2)
newey m1xe, lag(2)
newey m2xe, lag(2)

/*1.5*/

gen m1ye2= (y-m1y)^2
gen m2ye2= (y-m2y)^2
gen m1xe2= (x-m1x)^2
gen m2xe2= (x-m2x)^2

gen ly=m1ye2-m2ye2
gen lx=m1xe2-m2xe2
reg ly
reg lx
ttest ly==0
ttest lx==0

/*2.1*/
clear all
import delimited "/Users/vicentejaramillo/Downloads/MA/Tarea 3/EURO STOXX 50.csv", clear
keep date close
generate dated = date(date,"MDY",2050)
format dated %td
seq d, f(22593) t(0)
format d %td
tsset d
gen r=d.close/l.close
label var r "Retorno"
tsline rclear all
import delimited "/Users/vicentejaramillo/Downloads/MA/Tarea 3/EURO STOXX 50.csv", clear
keep date close
generate dated = date(date,"MDY",2050)
format dated %td
seq d, f(22593) t(0)
format d %td
tsset d
gen r=d.close/l.close
label var r "Retorno"
tsline r



forv i=1/20 { 
qui matrix lagss`i' = J(1,4,.)
qui mat colnames lagss`i' = lags p-fuller p-pperron  Port-statistic
qui matrix lagss`i'[1,1] = `i'
qui dfuller r, lags(`i') 
qui matrix lagss`i'[1,2] = r(p)
qui pperron r, lags(`i') 
qui matrix lagss`i'[1,3] = r(pval)
qui wntestq r, lags(`i') 
qui matrix lagss`i'[1,4] = r(stat)
}
forv i=2/20 { 
qui matrix lagss1 = lagss1\lagss`i'
}
mat li lagss1
pperron r, lags(1)


corrgram r, lags(20)
ac r , lags(20) name(ac) nodraw
pac r , lags(20) name(pac) nodraw
graph combine ac pac


foreach a of numlist 0 3 5 16 18 {
foreach m of numlist  0 3 5 16 18{
if `a'!=16 & `a'!=18{
qui arima r, arima(`a',0,`m') 
qui estimates store ARIMA`a'0`m'
}
else if `a'==16 & `m'!=18{
qui arima r, arima(`a',0,`m') 
qui estimates store ARIMA`a'0`m'
}
else if `a'==18 & `m'!=18 & `m'!=16{
qui arima r, arima(`a',0,`m') 
qui estimates store ARIMA`a'0`m'
}
}
}

foreach a of numlist  3 5 16 18 {
foreach m of numlist  3 5 16 18{
qui arima r, ar(1/`a') ma(`m')
qui estimates store AR`a'ma`m'
qui arima r, ar(`a') ma(1/`m')
qui estimates store ar`a'MA`m'
qui arima r, ar(`a')
qui estimates store ar`a'
qui arima r, ma(`m')
qui estimates store ma`m'
}
}

foreach a of numlist 3 5 16 18{
foreach r of numlist  5 16 18{
foreach m of numlist 3 5 16 18 {
foreach e of numlist  5 16 18{
if `a'<`r' & `m'<`e'{
qui arima r, ar(1/`a' `r') ma(1/`m' `e')
qui estimates store AR`a'ar`r'MA`m'ma`e'
}
}
}
}
}
estimates stats *, n(9084) 

return list
di r(S)[1,1]
qui matrix est = J(97,6,.)
qui mat colnames est = N - ll(model) df AIC BIC
qui matrix est[1,1] = r(S)
mat li est
matsort est 6 "down"
mat li est

arima r, ar(1/5 16) ma(1/3 16)
estat aroots
predict re, res
ac re, lags(20) name(acre) nodraw
pac re, lags(20) name(pacre) nodraw
graph combine acre pacre
tw (sc re l.re) (lfit re l.re)
wntestq re, lags(16) /*residuos*/
wntestb re


*2.2
gen re2=re^2
ac re2, lags(20) name(acre2) nodraw
pac re2, lags(20) name(pacre2) nodraw
graph combine acre2 pacre2

newey re2 l(1/16).re2,lag(16)
test
*se rechaza nula (volatilidad homogenea), es decir, se apoya la prescencia de arch(q)*

*2.3
qui arch re,  arch(1)
qui estimates store z1
qui arch re,  arch(2)
qui estimates store z2
qui arch re, arch(1) g(1)
qui estimates store z3
qui arch re,  arch(1) g(1/2)
qui estimates store z4
qui arch re, arch(1) g(2)
qui estimates store z5
qui arch re,  arch(2) g(1)
qui estimates store z6
qui arch re, arch(2) g(1/2)
qui estimates store z7
qui arch re,  arch(2) g(2)
qui estimates store z8
qui arch re,   arch(1/2) g(1/2)
qui estimates store z9
estimates stats z*

qui arch re, arch(1) g(1/2)
predict r1, res
ac r1, lags(20) name(r1) nodraw
pac r1, lags(20) name(re1) nodraw
graph combine r1 re1

*2.4
predict re_var, var
gen r1v=r1/sqrt(re_var)
ac r1v, lags(20) name(acr1) nodraw
pac r1v, lags(20) name(pacr1) nodraw
graph combine acr1 pacr1
newey r1v l(1/16).r1,lag(16)
test

*2.5
line re_var dated

