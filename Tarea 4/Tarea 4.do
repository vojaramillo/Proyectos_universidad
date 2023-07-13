

*Integrantes: Juan Andres y Vicente Jaramillo


clear all
cd "/Users/vicentejaramillo/Downloads/MA/T4"
import excel "/Users/vicentejaramillo/Downloads/MA/t4/base.xlsx", sheet("Canasta") firstrow case(lower)
gen fecha=mofd(periodo)
format fecha %tm
drop periodo
drop i
tsset fecha
gen fepuchile=f.epuchile
drop in 249/284
gen mes = mod(_n-1,12) + 1
replace mes=f4.mes

/*no hace falta desestacionalizar*/
tab mes, gen(dmes)
newey dolar dmes2-dmes12,lag(6)
newey embi dmes2-dmes12,lag(6)
newey ipc dmes2-dmes12,lag(6)

replace cobre=ln(cobre)
replace dolar=ln(dolar)
replace epuchile=ln(epuchile)
replace epuglobal=ln(epuglobal)
replace embi=embi/10000
replace ipc=ipc/100 
replace fepuchile=ln(fepuchile)


local i=1
foreach v in cobre dolar epuchile epuglobal embi ipc imacec{  
qui matrix lag`i' = J(1,2,.)
qui mat colnames lag`i' = p-dfuller  p-pperron
qui mat rownames lag`i' = `v'
qui dfuller `v'
qui matrix lag`i'[1,1] = r(p)
qui pperron `v' 
qui matrix lag`i'[1,2] = r(pval)
local i= `i'+1
}
forv i=2/7 { 
qui matrix lag1 = lag1\lag`i'
}
mat li lag1

/*1.2*/
forv i=2/12{ 
varsoc  epuglobal cobre embi ipc dolar epuchile imacec, maxlag(`i') 
}


/*1.3*/
/*esto agrega las restricciones y tira los acs*/
foreach x in  1  3 4 5 6 7 8 2 { /*lags*/  /*dejar el 2 al final, para analizar las funciones de impuslo respuesta fevd*/
local i=1
forv n=1/`x' {
foreach v in dolar epuchile  embi ipc imacec{ 
constraint define `i' [cobre]L`n'.`v'=0
local i= `i'+1
constraint define `i' [epuglobal]L`n'.`v'=0
local i= `i'+1
}
constraint define `i' [epuglobal]L`n'.cobre=0 /*internacionales*/
local i= `i'+1
constraint define `i' [cobre]L`n'.epuglobal=0
local i= `i'+1
}
*constraint list
local i =`i'-1
var epuglobal cobre imacec ipc epuchile dolar embi, lags(1/`x')  constraint(1/`i')
varstable
predict res`x', res 
tsline res`x', name(ts`x', replace) title("`x' lags")  nodraw
*	wntestb res`x'
ac res`x', lags(40) name(a`x') nodraw 
pac res`x', lags(40) name(p`x') nodraw 
graph combine a`x' p`x', name(c`x', replace) title("`x' lags")
constraint drop _all
}
graph combine ts1 ts2 ts3 ts4 ts5 ts6 ts7 ts8
graph combine c1 c2 c3 c4 c5 c6 c7 c8
vargranger
*elegimos el con 2 lags


/*1.4*/
irf create q, step(24) bs rep(500) set(q, replace)
/*1.4b*/
irf graph oirf, impulse(epuchile) response(epuglobal cobre imacec ipc epuchile dolar embi) level(90) individual iname(irfr, replace)
/*1.4c*/
irf table fevd, noci
irf graph fevd, impulse(epuchile) response(epuglobal cobre imacec ipc epuchile dolar embi) level(90) name(InflationFEVD, replace)


/*1.6*/  /*esta es la 1.6, por orden para el correcto análisis de las funciones*/
irf graph oirf, impulse(epuglobal) response(epuglobal cobre imacec ipc epuchile dolar embi) level(90) individual iname(irfr, replace)

irf table fevd, noci
irf graph fevd, impulse(epuglobal) response(epuglobal cobre imacec ipc epuchile dolar embi) level(90) name(InflationFEVD, replace)

*al ciclo ec
irf graph oirf, impulse(epuglobal cobre imacec ipc epuchile dolar embi) response(imacec )
irf table fevd, noci
irf graph fevd, impulse(epuglobal cobre imacec ipc epuchile dolar embi) response(imacec ) level(90) name(InflationFEVD, replace)

/*1.5*/
foreach x in  2 { /*lags*/  /*dejar el 2 al final, para analizar las funciones de impuslo respuesta fevd*/
local i=1
forv n=1/`x' {
foreach v in dolar fepuchile  embi ipc imacec{ 
constraint define `i' [cobre]L`n'.`v'=0
local i= `i'+1
constraint define `i' [epuglobal]L`n'.`v'=0
local i= `i'+1
}
constraint define `i' [epuglobal]L`n'.cobre=0 /*internacionales*/
local i= `i'+1
constraint define `i' [cobre]L`n'.epuglobal=0
local i= `i'+1
}
*constraint list
local i =`i'-1
var epuglobal cobre imacec ipc fepuchile dolar embi, lags(1/`x')  constraint(1/`i')
varstable
predict resf`x', res 
tsline resf`x', name(tsf`x', replace) title("`x' lags")  nodraw
*	wntestb resf`x'
ac resf`x', lags(40) name(af`x') nodraw 
pac resf`x', lags(40) name(pf`x') nodraw 
graph combine af`x' pf`x', name(cf`x', replace) title("`x' lags")
constraint drop _all
}

irf create qq, step(24) bs rep(500) set(qq, replace)
irf graph oirf, impulse(fepuchile) response(epuglobal cobre imacec ipc fepuchile dolar embi) level(90) individual iname(oirf, replace)
graph combine oirf1 oirf2 oirf3 oirf4 
graph combine oirf5 oirf6 oirf7

irf table fevd, noci
irf graph fevd, impulse(fepuchile) response(epuglobal cobre imacec ipc fepuchile dolar embi) level(90) name(InflationFEVD, replace)








