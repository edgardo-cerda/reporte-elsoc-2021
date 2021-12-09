
////////////////////////////////////////////////////////////////////////////////
///////////////////////// Base ELSOC formato WIDE //////////////////////////////
////////////////////////////////////////////////////////////////////////////////

/*Ajustes preliminares*/
cls
clear all
set more off, perm

// Modificar global directorio a carpeta de dropbox propia para correr codigo //

/*Abre base de datos: CASEN 2017*/
use "..\..\..\..\2_Bases_de_Datos\10_Combinacion_de_Olas_ELSOC\0E_Bases_de_Datos_Resultantes_2016_2021\ELSOC_Long_2016_2021_v1.00_Stata14.dta" , clear
run "egp_ajustado_solis_boado.do"

/////////////////////
//////// EGP ////////
/////////////////////

// Principal sostenedor del hogar //
gen principal_sostenedor = (m19 == 1) if m19 != .

// ISCO08 to ISCO88 //
iscogen ciuo88 = isco88(ciuo08_m03), from(isco08)
replace ciuo88 = ciuo88_m03 if ola == 1

iscogen ciuo88_ps = isco88(ciuo08_m22), from(isco08)
replace ciuo88_ps = ciuo88_m22 if ola == 1

// Trabajadores que supervisa //
recode m06 (-777 -888 -999 = .), gen(nsupervisa)
recode m25 (-777 -888 -999 = .), gen(nsupervisa_ps)

* Imputacion de numero de personas supervisadas para empleadores: 
replace m06 = 1 if m07 == 4 & m06 == 0 & m05 == 1
replace m06 = 3 if m07 == 4 & m06 == 0 & m05 == 2
replace m06 = 7 if m07 == 4 & m06 == 0 & m05 == 3
replace m06 = 23 if m07 == 4 & m06 == 0 & m05 >= 4 & m05 != .

replace m25 = 1 if m26 == 4 & m25 == 0 & m24 == 1
replace m25 = 3 if m26 == 4 & m25 == 0 & m24 == 2
replace m25 = 7 if m26 == 4 & m25 == 0 & m24 == 3
replace m25 = 23 if m26 == 4 & m25 == 0 & m24 >= 4 & m24 != .

*Auto-empleo
recode m07 (4/5=1)(.=.)(else=0), gen(selfemp)
recode m26 (4/5=1)(.=.)(else=0), gen(selfemp_ps)

*** EGP 11 para encuestado ***
EGP_ajustado egp11 , isco(ciuo88) selfemp(selfemp) nsupervisa(nsupervisa) from(isco88)

*** EGP 11 para principal sostenedor del hogar ***
EGP_ajustado egp11_ps_aux , isco(ciuo88_ps) selfemp(selfemp_ps) nsupervisa(nsupervisa_ps) from(isco88)

gen egp11_ps = egp11 if principal_sostenedor == 1
replace egp11_ps = egp11_ps_aux if principal_sostenedor == 0

label define legp11 ///
1 "I. Gestión Alto" ///
2 "II. Gestión Bajo" ///
3 "IIIa. Rutina No-Manual Alto" ///
4 "IIIb. Rutina No-Manual Bajo" ///
5 "IVa. Pequeño propietario con empleados" ///
6 "IVb. Trabajadores independientes" ///
7 "V. Técnicos de nivel bajo, supervisores de trabajadores manuales" ///
8 "VI. Trabajadores calificados" ///
9 "VIIa. Trabajadores manuales semi y no calificados" ///
10 "VIIb. Agricultores y otros trabajadores en producción primaria" ///
11 "IVc. Campesinos y pequeños propietarios agrícolas, otros trabajadores por cuenta propia en el sector primario" 
label values egp11* legp11

label var egp11 "Clases egp11"
label var egp11_ps "Clases egp11 (principal sostenedor)"

*** EGP 07 para encuestado ***
recode egp11 (1=1)(2=2)(3 4=3)(5 6=4)(7=5)(8=6)(9 10 11=7) , gen(egp07)

*** EGP 07 para principal sostenedor del hogar ***
recode egp11_ps (1=1)(2=2)(3 4=3)(5 6=4)(7=5)(8=6)(9 10 11=7) , gen(egp07_ps)

label define legp07 ///
1 "I. Clase alta" ///
2 "II. Gestión bajo" ///
3 "IIIa+IIIb. Rutina No-Manual" ///
4 "IVa+IVb. Pequeños propietarios e independientes" ///
5 "V. Técnicos y supervisores" ///
6 "VI. Trabajadores calificados" ///
7 "VIIa+VIIb+IVc. Clase operarios"
label values egp07* legp07

label var egp07 "Clases egp07"
label var egp07_ps "Clases egp07 (principal sostenedor)"

keep idencuesta ola egp11 egp07 egp11_ps egp07_ps

save "elsoc_egp.dta", replace
