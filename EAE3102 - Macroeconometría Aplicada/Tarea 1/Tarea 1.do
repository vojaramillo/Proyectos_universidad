

// Tarea 1 - Macroeconometría Aplicada
// Integrantes: Juan Bravo y Vicente Jaramillo


// 1.- y 2.- 

clear all

//Arreglo de base de datos y formatos
import excel "/Users/vicentejaramillo/Downloads/MA/Tarea 1/imacec_chile.xlsx", sheet("Canasta") firstrow

*Fuente: ajuste propio, basado en lo visto en la ayudantía 1.

format %tdCCYY-NN-DD Mes
gen date = mofd(Mes)
format date %tm
drop Mes
tsset date
generate log_imacec = log(Imacec)
gen y_t = log(Imacec) - log(l12.Imacec)


//Búsqueda del mejor modelo: candidatos.

summarize y_t
gen y_t_mean = r(mean)
tsline y_t y_t_mean

wntestq y_t
pperron y_t 

corrgram y_t, lags(20)

*Tenemos distintos candidatos, posible AR(1). Fuente del código: Ayudantía 3.
ac y_t, lags(30)		 
pac y_t, lags(30)

forvalue i=1/15{
	if `i'<6 {
		arima y_t, arima(`i',0,0)
	}
	else if `i'>=6 & `i'< 11{
		local j = `i' - 5
		arima y_t, arima(0,0,`j')
	}
	else if `i'>=11 {
		local j = `i' - 10
		arima y_t, arima(`j',0,`j')
	}
	estimates store m`i'
}
estimates stats _all //AR(1) tendría buen BIC
varsoc y_t, maxlag(5) //AR(1) es el mejor de los AR


/*
arima y_t, arima(0,0,8)
estat aroots
drop residualsar2
predict residualsar2, r 
tsline residualsar2
wntestq residualsar2, lags(8) 

*/

//Aplicamos 

arima y_t, arima(1,0,0)
estat aroots
predict residualsar1, r 
tsline residualsar1
wntestq residualsar1, lags(1) 

reg y_t L1.y_t
estat bgodfrey, lags (2)
estat bgodfrey


//3.- Ahora quitamos la fecha de la pandemia: fuente código para borrar filas: https://www.statalist.org/forums/forum/general-stata-discussion/general/1415282-drop-rows

gen n = _n
sort n
drop if n >= 291

summarize y_t
gen y_t_mean = r(mean)
tsline y_t y_t_mean

wntestq y_t
pperron y_t 

corrgram y_t, lags(20)

ac y_t, lags(30)		 
pac y_t, lags(30)

// Tenemos distintos candidatos, pero evaluaremos BIC. Fuente del código: Ayudantía 3

forvalue i=1/15{
	if `i'<6 {
		arima y_t, arima(`i',0,0)
	}
	else if `i'>=6 & `i'< 11{
		local j = `i' - 5
		arima y_t, arima(0,0,`j')
	}
	else if `i'>=11 {
		local j = `i' - 10
		arima y_t, arima(`j',0,`j')
	}
	estimates store m`i'
}

estimates stats _all

arima y_t, arima(3,0,3)
estat aroots //vemos que se cumple estacionariedad al no haber raices unitarias.
drop residualsar2
predict residualsar2, r 
tsline residualsar2
wntestq residualsar2, lags(1) //no se rechaza hipótesis nula de que sigue ruido blanco. 


// 4.- se importa una base modificada con las fechas que calzan con las fechas del precio del cobre e imacec

clear all
import excel "/Users/vicentejaramillo/Downloads/MA/Tarea 1/imacecycobre_chile.xlsx", sheet("Canasta") firstrow clear


format %tdCCYY-NN-DD mes
gen date = mofd(mes)
format date %tm
drop mes
tsset date

gen y_t = log(imacec) - log(l12.imacec)

reg y_t L0.xt
estat ic
reg y_t L1.xt // este es el que tiene menor BIC, por lo tanto el mejor modelo según los criterios de información. 
estat ic
reg y_t L2.xt
estat ic
reg y_t L3.xt
estat ic
reg y_t L4.xt
estat ic
reg y_t L5.xt
estat ic
reg y_t L6.xt
estat ic
reg y_t L7.xt
estat ic
reg y_t L8.xt
estat ic

/*

clear all

mport excel "/Users/vicentejaramillo/Downloads/MA/Tarea 1/imacecycobre_chile.xlsx", sheet("Canasta") firstrow clear


format %tdCCYY-NN-DD mes
gen date = mofd(mes)
format date %tm
drop mes
tsset date

gen y_t = log(imacec) - log(l12.imacec)

gen n = _n
sort n
drop if n < 13

reg y_t L0.xt
estat ic
reg y_t L1.xt // este es el que tiene menor BIC, por lo tanto el mejor modelo según los criterios de información. 
estat ic
reg y_t L2.xt
estat ic
reg y_t L3.xt
estat ic
reg y_t L4.xt
estat ic
reg y_t L5.xt
estat ic
reg y_t L6.xt
estat ic
reg y_t L7.xt
estat ic
reg y_t L8.xt
estat ic

*/ 

// pregunta 6

clear all

import excel "/Users/vicentejaramillo/Downloads/MA/Tarea 1/imacecnominero_cobre.xlsx", sheet("Canasta") firstrow clear
format %tdCCYY-NN-DD mes
gen date = mofd(mes)
format date %tm
drop mes
tsset date

gen y_t2 = log(imacec_no_minero) - log(l12.imacec_no_minero)
rename cobre xt

reg y_t2 L0.xt
estat ic
reg y_t2 L1.xt // mejor modelo porrque tiene menor BIC, por lo tanto el mejor modelo según los criterios de información. 
estat ic
reg y_t2 L2.xt
estat ic
reg y_t2 L3.xt
estat ic
reg y_t2 L4.xt
estat ic
reg y_t2 L5.xt
estat ic
reg y_t2 L6.xt
estat ic
reg y_t2 L7.xt
estat ic
reg y_t2 L8.xt
estat ic


