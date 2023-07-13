
// Tarea 2 - Macroeconometría Aplicada
// Integrantes: Juan Bravo y Vicente Jaramillo


clear all
cd "/Users/vicentejaramillo/Downloads/MA/Tarea 2"
import excel "/Users/vicentejaramillo/Downloads/MA/Tarea 2/cobre.xlsx", firstrow
format %tdCCYY-NN-DD periodo
gen date = mofd(periodo)
format date %tm
drop periodo
tsset date

rename wti p
replace p=ln(p)
rename ipc i
replace cobre=ln(cobre)
label var cobre "Log-cobre"

/*1*/
tsline cobre
forv i=1/20 { 
qui matrix lag`i' = J(1,3,.)
qui mat colnames lag`i' = lags p-fuller p-pperron
qui matrix lag`i'[1,1] = `i'
qui dfuller cobre, lags(`i') 
qui matrix lag`i'[1,2] = r(p)
qui pperron cobre, lags(`i') 
qui matrix lag`i'[1,3] = r(pval)
}
forv i=2/20 { 
qui matrix lag1 = lag1\lag`i'
}
mat li lag1 /*respuesta 1*/

/*2*/
gen n=_n
gen nn=n^2
gen nnn=n^3
qui reg cobre n nn nnn
qui predict xb
tsline xb cobre
predict c, res 
tsline c
label var c "Log-Cobre sin tendencia"
forv i=1/20 { 
qui matrix lags`i' = J(1,3,.)
qui mat colnames lags`i' = lags p-fuller p-pperron
qui matrix lags`i'[1,1] = `i'
qui dfuller c, lags(`i') 
qui matrix lags`i'[1,2] = r(p)
qui pperron c, lags(`i') 
qui matrix lags`i'[1,3] = r(pval)
}
forv i=2/20 { 
qui matrix lags1 = lags1\lags`i'
}
mat li lags1/*respuesta 2*/

/*3*/
tsfilter hp hc = cobre, smooth(129000)
label var hc "Cobre con filtro HP"
forv i=1/20 { 
qui matrix lagss`i' = J(1,4,.)
qui mat colnames lagss`i' = lags p-fuller p-pperron  Port-statistic
qui matrix lagss`i'[1,1] = `i'
qui dfuller hc, lags(`i') 
qui matrix lagss`i'[1,2] = r(p)
qui pperron hc, lags(`i') 
qui matrix lagss`i'[1,3] = r(pval)
qui wntestq hc, lags(`i') 
qui matrix lagss`i'[1,4] = r(stat)
}
forv i=2/20 { 
qui matrix lagss1 = lagss1\lagss`i'
}
mat li lagss1/*respuesta 3*/
tsline hc
*pperron hc


/*4*/
qui tsline c, name(c) nodraw
qui tsline hc, name(hc) nodraw
qui tsline c hc, name(chc) nodraw
graph combine c hc chc
correlate c hc

/*5*/
mat li lags1 /*estacionariedad*/
wntestq hc

/*estacionalidad*/
gen mes = mod(_n-1,12) + 1
gen trim1=1 if mes==1 | mes==2 | mes==3 
replace trim1=0 if trim1==.
gen trim2=1 if mes==4 | mes==5 | mes==6 
replace trim2=0 if trim2==.
gen trim3=1 if mes==7 | mes==8 | mes==9 
replace trim3=0 if trim3==.
gen trim4=1 if mes==10 | mes==11 | mes==12
replace trim4=0 if trim4==.

tab mes, gen(month)

newey hc t*, lag(2)
outreg2 using qqqqqqq, excel append  ctitle(estaionalidad trimestral) 
newey hc month*, lag(2)
outreg2 using qqqqqqq, excel append  ctitle(estaionalidad mensual) 


corrgram hc, lags(20)
ac hc , lags(20) name(ac) nodraw
pac hc , lags(20) name(pac) nodraw
graph combine ac pac


foreach a of numlist 0 1 2 11 12 {
foreach m of numlist  0 1 2 3 4 5 6 7 8 9 10 18{
if `a'!=2 & `a'!=11{
qui arima hc, arima(`a',0,`m')          
qui estimates store ARIMA`a'0`m'
}
else if `a'==2  & `m'!=7{
qui arima hc, arima(`a',0,`m')          
qui estimates store ARIMA`a'0`m'
}
else if `a'==11  & `m'!=10{
qui arima hc, arima(`a',0,`m')          
qui estimates store ARIMA`a'0`m'
}
}
}
estimates stats _all 



arima hc , arima(3,0,0) /*3er rezago no significativo*/ 
outreg2 using qqwq, excel append  ctitle(arima(3,0,0)) 
arima hc , arima(2,0,0) 
outreg2 using qqwq, excel append  ctitle(arima(2,0,0))
forv i=2/20 { // mejor AR es el AR(2)
varsoc hc, maxlag(`i')
} 
estat aroots
test
predict rhc, res
tsline rhc, ttitle(Residuos Log-Cobre-HC Post-estimación ARIMA(2,0,0))
newey hc l(1/2).hc, lag(2)
reg hc l(1/2).hc
estat bgodfrey

wntestq rhc, lags(2) /*residuos*/
wntestb rhc
corrgram rhc, lags(20)
drop _est_ARIMA000 - _est_ARIMA1207


/*6*/
newey i l.i l(0/11).hc , lag(11)
outreg2 using qqwqw, excel append  ctitle(Pregunta 6) 
test hc l.hc l2.hc l3.hc l4.hc l5.hc l6.hc l7.hc l8.hc l9.hc l10.hc l11.hc 
label var hc "Cu"
coefplot, keep(*hc) xline(0) msymbol(d) ci vert


/*7*/
tsfilter hp hp = p, smooth(129000)
forv i=1/20 { 
qui matrix lagz`i' = J(1,4,.)
qui mat colnames lagz`i' = lags p-fuller p-pperron  Port-statistic
qui matrix lagz`i'[1,1] = `i'
qui dfuller hc, lags(`i') 
qui matrix lagz`i'[1,2] = r(p)
qui pperron hc, lags(`i') 
qui matrix lagz`i'[1,3] = r(pval)
qui wntestq hc, lags(`i') 
qui matrix lagz`i'[1,4] = r(stat)
}
forv i=2/20 { 
qui matrix lagz1 = lagz1\lagz`i'
}
mat li lagz1
newey i l.i l(0/11).hp , lag(11)
outreg2 using qqwqww, excel append  ctitle(Pregunta 7) 
test hp l.hp l2.hp l3.hp l4.hp l5.hp l6.hp l7.hp l8.hp l9.hp l10.hp l11.hp 
label var hp "wti"
coefplot, keep(*hc) xline(0) msymbol(d) ci vert


/*2.1*/
gen tc =cobre-hc
label var tc "Tendencia del cobre"
tsline tc

tsfilter hp hcc = cobre, smooth(999999999999999)
gen tcc =cobre-hcc
label var tcc "Tendencia del cobre con lambda infinito"
tsline tcc cobre

 



