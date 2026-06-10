/*=============================================================================*
				PONTIFICIA UNIVERSIDAD CATÓLICA DEL ECUADOR 
							FACULTAD DE ECONOMÍA
					INSTITUTO DE INVESTIGACIONES ECONÓMICAS 
*==============================================================================*
** Fuente: Encuesta Nacional de Empleo, Desempleo y Subempleo (ENEMDU)
*==============================================================================*
** Elaboración:

Carolina Sánchez
Alejandra Aguirre 
Alexis Guayasamín 
Camila Rodríguez
Carlos González
*==============================================================================*
******************* INDICADORES MERCADO LABORAL Y SEGURIDAD  *******************
*=============================================================================*/


clear all

use "merged_21.sav"
*==============================================================================*

cd "C:\Users\aiguayasamin\Documents\merged_anual"
global  enemdu_anual "18 19 21 22"
foreach  a of global enemdu_anual {

		  
use "merged_20`a'.dta",clear
	  
cap drop bdd_fecha 
gen      bdd_fecha = "20`a'"
lab var  bdd_fecha   "20`a'"
order    bdd_fecha
	
*==============================================================================*
*********** CONSTRUCCIÓN DE VARIABLES Y LIMPIEZA BASE DE DATOS *****************
*==============================================================================*


*==============================================================================*
** Variable autoidentificacion étnica **
*==============================================================================*
	    gen etnia = .
	replace etnia = 1 if p15 == 6 | p15 == 7
	replace etnia = 2 if p15 == 2 | p15 == 3 | p15 == 4
	replace etnia = 3 if p15 == 1
	replace etnia = 4 if p15 == 5 | p15 == 8
	  label define label_etnia 1"Mestizo-blanco" 2"Afroecuatoriano" 3"Indigena" 4"Montuvio"
	  label values etnia label_etnia
*==============================================================================*
** Variable grupos etario **
*==============================================================================*
	
	keep if p03>=15 //Poblacion en Edad de Trabajar (PET)
	    gen edad = .
	replace edad = 1 if p03>=15 & p03<=29
	replace edad = 2 if p03>=30 & p03<=64 
	replace edad = 3 if p03>=65
	  label define label_edad 1"Joven" 2"Adulto" 3"Adulto Mayor" 
	  label values edad label_edad

*==============================================================================*
***************************   DATOS MERCADO LABORAL   **************************
*==============================================================================*

*==============================================================================*
** 01. POBLACIÓN EN EDAD PARA TRABAJAR **
*==============================================================================*
		gen pet = (p03 >= 15) if  !missing(condact)
  label var pet "`a' Poblacion en Edad de Trabajar"
  
		tabout p02 pet [aw=fexp] using laboral_anual.xls, cells(col) dpcomma f(2)
	    tabout etnia pet [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2)
		tabout area pet [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2) 
		tabout edad pet [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2) 

*==============================================================================*
** 01. POBLACIÓN ECONOMICAMENTE ACTIVA **
*==============================================================================*	
		gen pea = (condact>=1 & condact <= 8) if  !missing(condact)
  label var pea "`a' Poblacion Economicamente Activa"
 
	    tabout p02  pea [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia pea [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2) 
		tabout area  pea [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2) 
		tabout edad  pea [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2) 
*==============================================================================*
** 02. POBLACIÓN ECONÓMICAMENTE INACTIVA **
*==============================================================================*
		gen pei = (condact == 9) if  !missing(condact)
  label var pei "`a' Poblacion Economicamente Inactiva"
    
	    tabout p02 pei [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia pei [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area pei [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad pei [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 04. TASA DE PARTICIPACIoN GLOBAL **
*==============================================================================*
		gen tpg = pea/pet
  label var tpg "`a' Tasa de Participacion Global"
  
 	    tabout p02 tpg [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia tpg [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area tpg [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad tpg [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 05. POBLACIÓN CON EMPLEO **
*==============================================================================*
		gen emp = 0 if pea == 1
	replace emp = 1 if pea == 1 & p20 == 1
	replace emp = 1 if pea == 1 & p20 == 2 & p21 <= 11
	replace emp = 1 if pea == 1 & p20 == 2 & p21 == 12 & p22 == 1
  label var emp "`a' Poblacion con empleo"
    
 	    tabout p02 emp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia emp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area emp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad emp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 06. TASA DE EMPLEO ADECUADO **
*==============================================================================*
	** Numerador **
		gen t_adec=1 if condact==1
	**Denominador**
	replace t_adec=0 if (condact==2 | condact==3 |condact==4| condact==5 | condact==6 | condact==7 | condact==8)
  label var t_adec "`a' Empleo Adecuado"
    
 	    tabout p02 t_adec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia t_adec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area t_adec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad t_adec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)

*==============================================================================*
** 07. TASA DE EMPLEO INADECUADO **
*==============================================================================*
	** Numerador **
		gen inadec = .
    replace inadec = 1 if (condact==2 | condact==3 | condact==4 |condact==5)
	**Denominador**
    replace inadec = 0 if (condact==1 | condact==6 | condact==7 | condact==8)
  label var inadec "`a' Empleo Inadecuado"
  
 	    tabout p02 inadec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia inadec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area inadec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad inadec [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 08. SUBEMPLEO **
*==============================================================================*
		gen subemp = 1 if (condact==2 | condact==3 )
	**Denominador**
    replace subemp = 0 if (condact==1 | condact==7 | condact==8 | condact==4 | condact==5 |condact==6 )
  label var subemp "`a' Subempleo"
  
 	    tabout p02 subemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia subemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area subemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad subemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 09. DESEMPLEO **
*==============================================================================*
		* Desempleo abierto
	    gen des_ab = 0 if pea == 1
	replace des_ab = 1 if pea == 1 & p20 == 2 & p21 == 12 & p22 == 2 & p32 <= 10
  label var des_ab "`a' Desempleo Abierto"
		
	    * Desempleo oculto
		gen des_oc =0 if pea==1
    replace des_oc =1 if pea==1 & p20 ==2 & p21 ==12 & p22==2 & p32 ==11 & p34 <=7 & p35==1
  label var des_oc "`a' Desempleo Oculto"
  
	    * Desempleo
	   egen desemp = rowtotal(des_ab des_oc), missing
  label var desemp "`a' Poblacion sin Empleo"
   
 	    tabout p02 desemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia desemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area desemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad desemp [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 10. TRABAJO NO REMUNERADO **
*==============================================================================*
		gen tnr = .
	replace tnr = 1 if inrange(p42,7,9)
	replace tnr = 1 if inrange(p54,7,9)
  label var tnr "`a' Trabajo No Remunerado"
   
 	    tabout p02 tnr [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2)
	    tabout etnia tnr [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2)
		tabout area tnr [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2)
		tabout edad tnr [aw=fexp] using laboral_anual.xls, append cells(col) dpcomma f(2)

*==============================================================================*
** 11. EMPLEO EN EL SECTOR FORMAL **
*==============================================================================*
		* Ocupados en el Sector Formal
		gen ocu_f = 0 if pea == 1
    replace ocu_f = 1 if secemp == 1
		** Numerador **
		gen formal = 0 if condact > 0 & condact < 7
		**Denominador**
    replace formal = 1 if ocu_f == 1 & condact > 0 & condact < 7
  label var formal "`a' Tasa de ocupacion en el sector formal"

 
 	    tabout p02 formal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia formal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area formal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad formal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)

*==============================================================================*
** 12. EMPLEO EN EL SECTOR INFORMAL **
*==============================================================================*
		* Ocupados en el Sector Informal
		gen ocu_inf = 0 if pea == 1
	replace ocu_inf = 1 if secemp == 2
		** Numerador **
		gen informal = 0 if condact > 0 & condact < 7 
		**Denominador**
    replace informal=1 if ocu_inf == 1 & condact > 0 & condact < 7
  label var informal "`a' Tasa de ocupacion en el sector informal"
  
 	    tabout p02 informal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia informal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area informal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad informal [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 13. EMPLEO EN EL SECTOR INFORMAL AGRÍrowA **
*==============================================================================*
		* Ocupados en el Sector Informal
		generat ocu_infa = 0 if pea == 1
		replace ocu_infa = 1 if secemp==2 & rama==1
		** Numerador **
		gen informala = 0 if condact>0 & condact<7 & p03 >= 15 
		**Denominador**
		replace informala=1 if ocu_infa==1 & condact>0 & condact<7
      label var informala "`a' Empleo Informal Agrícola"
  
 	    tabout p02 informala [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia informala [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area informala [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad informala [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 14. EMPLEO EN EL SECTOR INFORMAL NO AGRÍrowA **
*==============================================================================*
		* Ocupados en el Sector Informal
		gen ocu_infna=0 if pea ==1
	replace ocu_infna=1 if secemp==2 & rama!=1
		** Numerador **
		gen informalna=0 if condact>0 & condact<7 & p03 >= 15 
		**Denominador**
    replace informalna=1 if ocu_infna==1 & condact>0 & condact<7 & p03 >= 15
  label var informalna "`a' Empleo Informal NO Agrícola"
  
 	    tabout p02 informalna [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia informalna [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area informalna [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad informalna [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 15. TASA DE EMPLEO DOMÉSTICO **
*==============================================================================*
		* Ocupados en Empleo doméstico
	    gen ocu_dom = 0 if pea ==1
	replace ocu_dom = 1 if secemp==3
		** Numerador **
		gen domest = 0 if condact>0 & condact < 7 & p03 >= 15
		**Denominador**
	replace domest = 1 if ocu_dom == 1 & condact > 0 & condact<7 & p03 >= 15
  label var domest "`a' Empleo Doméstico"
  
 	    tabout p02 domest [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia domest [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area domest [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad domest [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 16. POR RAMA DE ACTIVIDAD **
*==============================================================================*      
		tabout p02 rama [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia rama [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area rama [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad rama [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 18. DESEMPLEO JUVENIL **
*==============================================================================*
		gen desemp_juv = 1 if (condact==7 | condact==8 )& (p03>=18 & p03<=29)
		**Denominador**
	replace desemp_juv = 0 if (condact==1 | condact==2 | condact==3 | condact==4 | condact==5 |condact==6 ) & (p03>=18 & p03<=29)
  label var desemp_juv "`a' Tasa de Desempleo Juvenil" 
  
 	    tabout p02 desemp_juv [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia desemp_juv [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area desemp_juv [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad desemp_juv [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 21. SEGURIDAD SOCIAL **
*==============================================================================*  
gen segsoc = .
replace segsoc = 1 if p05a == 1 | p05a == 2 | p05a == 3 | p05a == 4
replace segsoc = 1 if p05b == 1 | p05b == 2 | p05b == 3 | p05b == 4
recode segsoc . = 2
lab def l_segsoc 1"Si" 2"No"
lab val segsoc l_segsoc
		
		tabout p02 segsoc [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
	    tabout etnia segsoc [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout area segsoc [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)
		tabout edad segsoc [aw=fexp] using laboral_anual.xls, append cells(row) dpcomma f(2)

*==============================================================================*
** 19. INGRESO LABORAL PROMEDIO **
*==============================================================================*
	replace ingrl = . if (ingrl == 0 | ingrl == 999999)

tabout p02   [aw=fexp] using laboral_anual.xls, append c(mean p24) dpcomma f(2) sum
tabout etnia [aw=fexp] using laboral_anual.xls, append c(mean p24) dpcomma f(2) sum
tabout area  [aw=fexp] using laboral_anual.xls, append c(mean p24) dpcomma f(2) sum
tabout edad  [aw=fexp] using laboral_anual.xls, append c(mean p24) dpcomma f(2) sum
   
*==============================================================================*
** 20. HORAS PROMEDIO DE TRABAJO SEMANAL **
*==============================================================================*

tabout p02   [aw=fexp] using laboral_anual.xls, append c(mean ingrl) dpcomma f(2) sum
tabout etnia [aw=fexp] using laboral_anual.xls, append c(mean ingrl) dpcomma f(2) sum
tabout area  [aw=fexp] using laboral_anual.xls, append c(mean ingrl) dpcomma f(2) sum
tabout edad  [aw=fexp] using laboral_anual.xls, append c(mean ingrl) dpcomma f(2) sum
  	
				
			}
*
*==============================================================================*




cd "C:\Users\aiguayasamin\Documents\INEC\ENEMDU\BD_12_SPSS"
use "200912_merged.dta", clear

import spss using "enemdu_persona_2022_12.sav", clear


cd "C:\Users\Mateo\Desktop\Code\Stata\Enemdu"
ssc install tabout, replace
global enemdu_dic "24"
foreach  a of global enemdu_dic {
			  
import spss using "enemdu_persona_2024_12.sav", clear
 
*==============================================================================*
*********** CONSTRUCCIÓN DE VARIABLES Y LIMPIEZA BASE DE DATOS *****************
*==============================================================================*

*==============================================================================*
** Variable autoidentificacion étnica **
*==============================================================================*
svyset upm [w=fexp], strata(estrato) vce(linearized) singleunit(certainty)
import spss using "enemdu_persona_2024_12.sav", clear
	   
	   gen etnia = .
	replace etnia = 1 if p15 == 6 | p15 == 7
	replace etnia = 2 if p15 == 2 | p15 == 3 | p15 == 4
	replace etnia = 3 if p15 == 1
	replace etnia = 4 if p15 == 5 /*| p15 == 8*/
	  label define label_etnia 1"Mestizo-blanco" 2"Afroecuatoriano" 3"Indigena" 4"Montuvio"
	  label values etnia label_etnia
	  
*==============================================================================*
** 21. SEGURIDAD SOCIAL ** *necesaria antes de cambiar el p03*
*==============================================================================*  
 	    	    gen segsoc = .
	replace segsoc = 1 if p05a == 1 | p05a == 2 | p05a == 3 | p05a == 4
	replace segsoc = 1 if p05b == 1 | p05b == 2 | p05b == 3 | p05b == 4
	 recode segsoc . = 2
	    lab def l_segsoc 1"Si" 2"No"
	    lab val segsoc l_segsoc
		gen edad3 = .
replace edad3= 1 if  p03>=0 & p03<=5
replace edad3 = 2 if p03>=6 & p03<=11
replace edad3 = 3 if p03>=12 & p03<=17
replace edad3= 4 if p03>=18 & p03<=29
replace edad3 = 5 if p03>=30 & p03<=64
replace edad3 = 6 if        p03 >= 65
label define label_edad3 1"Infante/a" 2"Niño/a" 3"Adolescente" 4"Joven adulto" 5"Adulto/a" 6"Tercera edad"
label values edad3 label_edad3

		
		tabout  segsoc [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout p02 segsoc [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia segsoc [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area segsoc [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)		
		tabout edad3 segsoc [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
  
*==============================================================================*
** Variable grupos etario Y  **
*==============================================================================*
		
	keep if p03>=15 //Poblacion en Edad de Trabajar (PET)
	    gen edad = .
	replace edad = 1 if p03>=15 & p03<=29
	replace edad = 2 if p03>=30 & p03<=64 
	replace edad = 3 if p03>=65
	  label define label_edad 1"Joven" 2"Adulto" 3"Adulto Mayor" 
	  label values edad label_edad

*==============================================================================*
***************************   DATOS MERCADO LABORAL   **************************
*==============================================================================*

*==============================================================================*
** 01. POBLACIÓN EN EDAD PARA TRABAJAR  **
*==============================================================================*
		gen pet = (p03 >= 15) if  !missing(condact)
  label var pet "`a' Poblacion en Edad de Trabajar"
  
		tabout p02 pet [aw=fexp] using laboral_dic.xls, cells(col) dpcomma f(2)
	    tabout etnia pet [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
		tabout area pet [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2) 
		tabout edad pet [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2) 

*==============================================================================*
** 01. POBLACIÓN ECONOMICAMENTE ACTIVA **
*==============================================================================*	
		gen pea = (condact>=1 & condact <= 8) if  !missing(condact)
  label var pea "`a' Poblacion Economicamente Activa"
 
 tab etnia pea [aw=fexp], nof row
 
	    tabout p02 pea [aw=fexp]   using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia pea [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2) 
		tabout area pea [aw=fexp]  using laboral_dic.xls, append cells(col) dpcomma f(2) 
		tabout edad pea [aw=fexp]  using laboral_dic.xls, append cells(col) dpcomma f(2) 
		
*==============================================================================*
** 02. POBLACIÓN ECONÓMICAMENTE INACTIVA **
*==============================================================================*
		gen pei = (condact == 9) if  !missing(condact)
  label var pei "`a' Poblacion Economicamente Inactiva"
    
	    tabout p02 pei [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia pei [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
		tabout area pei [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
		tabout edad pei [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
*==============================================================================*
** 04. TASA DE PARTICIPACIoN GLOBAL **
*==============================================================================*
		gen tpg = pea/pet
  label var tpg "`a' Tasa de Participacion Global"
  
 	    tabout p02 tpg [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia tpg [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area tpg [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad tpg [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 05. POBLACIÓN CON EMPLEO **
*==============================================================================*
		gen emp = 0 if pea == 1
	replace emp = 1 if pea == 1 & p20 == 1
	replace emp = 1 if pea == 1 & p20 == 2 & p21 <= 11
	replace emp = 1 if pea == 1 & p20 == 2 & p21 == 12 & p22 == 1
  label var emp "`a' Poblacion con empleo"
    
 	    tabout p02 emp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia emp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area emp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad emp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 06. TASA DE EMPLEO ADECUADO **
*==============================================================================*
	** Numerador **
		gen t_adec=1 if condact==1
	**Denominador**
	replace t_adec=0 if (condact==2 | condact==3 |condact==4| condact==5 | condact==6 | condact==7 | condact==8)
  label var t_adec "`a' Empleo Adecuado"
    
 	    tabout p02 t_adec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia t_adec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area t_adec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad t_adec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)

*==============================================================================*
** 07. TASA DE EMPLEO INADECUADO **
*==============================================================================*
	** Numerador **
		gen inadec = .
    replace inadec = 1 if (condact==2 | condact==3 | condact==4 |condact==5)
	**Denominador**
    replace inadec = 0 if (condact==1 | condact==6 | condact==7 | condact==8)
  label var inadec "Empleo Inadecuado"
  
 	    tabout p02 inadec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia inadec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area inadec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad inadec [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 08. SUBEMPLEO **
*==============================================================================*
		gen subemp = 1 if (condact==2 | condact==3 )
	**Denominador**
    replace subemp = 0 if (condact==1 | condact==7 | condact==8 | condact==4 | condact==5 |condact==6 )
  label var subemp "Subempleo"
  
 	    tabout p02 subemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia subemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area subemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad subemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 09. DESEMPLEO **
*==============================================================================*
		* Desempleo abierto
	    gen des_ab = 0 if pea == 1
	replace des_ab = 1 if pea == 1 & p20 == 2 & p21 == 12 & p22 == 2 & p32 <= 10
  label var des_ab "Desempleo Abierto"
		
	    * Desempleo oculto
		gen des_oc =0 if pea==1
    replace des_oc =1 if pea==1 & p20 ==2 & p21 ==12 & p22==2 & p32 ==11 & p34 <=7 & p35==1
  label var des_oc "Desempleo Oculto"
  
	    * Desempleo
	   egen desemp = rowtotal(des_ab des_oc), missing
  label var desemp "Poblacion sin Empleo"
   
 	    tabout p02 desemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia desemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area desemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad desemp [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 10. TRABAJO NO REMUNERADO **
*==============================================================================*
**Numerador**
gen tnr=1 if (condact==5 )& (p03>=15)
**Denominador**
replace tnr=0 if (condact==1 | condact==7 | condact==8 | condact==4 | condact==2 |condact==6 |condact==3 ) & (p03>=15)
label var tnr "Trabajo no remunerado"
   
 	    tabout p02 tnr [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
	    tabout etnia tnr [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
		tabout area tnr [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
		tabout edad tnr [aw=fexp] using laboral_dic.xls, append cells(col) dpcomma f(2)
*==============================================================================*
** 11. EMPLEO EN EL SECTOR FORMAL **
*==============================================================================*
		* Ocupados en el Sector Formal
		gen ocu_f = 0 if pea == 1
    replace ocu_f = 1 if secemp == 1
		** Numerador **
		gen formal = 0 if condact > 0 & condact < 7
		**Denominador**
    replace formal = 1 if ocu_f == 1 & condact > 0 & condact < 7
  label var formal "Tasa de ocupacion en el sector formal"

 	    tabout p02 formal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia formal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area formal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad formal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 12. EMPLEO EN EL SECTOR INFORMAL **
*==============================================================================*
		* Ocupados en el Sector Informal
		gen ocu_inf = 0 if pea == 1
	replace ocu_inf = 1 if secemp == 2
		** Numerador **
		gen informal = 0 if condact > 0 & condact < 7 
		**Denominador**
    replace informal=1 if ocu_inf == 1 & condact > 0 & condact < 7
  label var informal "Tasa de ocupacion en el sector informal"
  
 	    tabout p02 informal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia informal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area informal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad informal [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)	
*==============================================================================*
** 13. EMPLEO EN EL SECTOR INFORMAL AGRÍrowA **
*==============================================================================*
		* Ocupados en el Sector Informal
		generat ocu_infa = 0 if pea == 1
		replace ocu_infa = 1 if secemp==2 & rama==1
		** Numerador **
		gen informala = 0 if condact>0 & condact<7 & p03 >= 15 
		**Denominador**
		replace informala=1 if ocu_infa==1 & condact>0 & condact<7
      label var informala "Empleo Informal Agrícola"
  
 	    tabout p02 informala [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia informala [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area informala [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad informala [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 14. EMPLEO EN EL SECTOR INFORMAL NO AGRÍrowA **
*==============================================================================*
		* Ocupados en el Sector Informal
		gen ocu_infna=0 if pea ==1
	replace ocu_infna=1 if secemp==2 & rama!=1
		** Numerador **
		gen informalna=0 if condact>0 & condact<7 & p03 >= 15 
		**Denominador**
    replace informalna=1 if ocu_infna==1 & condact>0 & condact<7 & p03 >= 15
  label var informalna "Empleo Informal NO Agrírowa"
  
 	    tabout p02 informalna [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia informalna [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area informalna [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad informalna [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
*==============================================================================*
** 15. TASA DE EMPLEO DOMÉSTICO **
*==============================================================================*
		* Ocupados en Empleo doméstico
	    gen ocu_dom = 0 if pea ==1
	replace ocu_dom = 1 if secemp==3
		** Numerador **
		gen domest = 0 if condact>0 & condact < 7 & p03 >= 15
		**Denominador**
	replace domest = 1 if ocu_dom == 1 & condact > 0 & condact<7 & p03 >= 15
  label var domest "Empleo Doméstico"
  
 	    tabout p02 domest [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia domest [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area domest [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad domest [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 16. POR RAMA DE ACTIVIDAD **
*==============================================================================*      
		tabout p02 rama [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia rama [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area rama [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad rama [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		
*==============================================================================*
** 18. DESEMPLEO JUVENIL **
*==============================================================================*
		gen desemp_juv = 1 if (condact==7 | condact==8 )& (p03>=18 & p03<=29)
		**Denominador**
	replace desemp_juv = 0 if (condact==1 | condact==2 | condact==3 | condact==4 | condact==5 |condact==6 ) & (p03>=18 & p03<=29)
  label var desemp_juv "Tasa de Desempleo Juvenil" 
  
 	    tabout p02 desemp_juv [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
	    tabout etnia desemp_juv [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout area desemp_juv [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		tabout edad desemp_juv [aw=fexp] using laboral_dic.xls, append cells(row) dpcomma f(2)
		
		
*==============================================================================*
** 19. INGRESO LABORAL PROMEDIO **
*==============================================================================*
	replace ingrl = . if (ingrl == 0 | ingrl == 999999)

tabout p02   [aw=fexp] using laboral_dic.xls, append c(mean p24) dpcomma f(2) sum
tabout etnia [aw=fexp] using laboral_dic.xls, append c(mean p24) dpcomma f(2) sum
tabout area  [aw=fexp] using laboral_dic.xls, append c(mean p24) dpcomma f(2) sum
tabout edad  [aw=fexp] using laboral_dic.xls, append c(mean p24) dpcomma f(2) sum
   
*==============================================================================*
** 20. HORAS PROMEDIO DE TRABAJO SEMANAL **
*==============================================================================*

tabout p02   [aw=fexp] using laboral_dic.xls, append c(mean ingrl) dpcomma f(2) sum
tabout etnia [aw=fexp] using laboral_dic.xls, append c(mean ingrl) dpcomma f(2) sum
tabout area  [aw=fexp] using laboral_dic.xls, append c(mean ingrl) dpcomma f(2) sum
tabout edad  [aw=fexp] using laboral_dic.xls, append c(mean ingrl) dpcomma f(2) sum
  	
				
			}
*
*==============================================================================*
svyset upm [w=fexp], strata(estrato) vce(linearized) singleunit(certainty)
svy: proportion segsoc, over (edad2)
estat cv

import spss using "enemdu_persona_201912.sav", clear
*edad para seguridad social*
		gen edad3 = .
replace edad3= 1 if  p03>=0 & p03<=5
replace edad3 = 2 if p03>=6 & p03<=11
replace edad3 = 3 if p03>=12 & p03<=17
replace edad3= 4 if p03>=18 & p03<=29
replace edad3 = 5 if p03>=30 & p03<=64
replace edad3 = 6 if        p03 >= 65
label define label_edad3 1"Infante/a" 2"Niño/a" 3"Adolescente" 4"Joven adulto" 5"Adulto/a" 6"Tercera edad"
label values edad3 label_edad3

    gen segsoc = .
	replace segsoc = 1 if p05a == 1 | p05a == 2 | p05a == 3 | p05a == 4
	replace segsoc = 1 if p05b == 1 | p05b == 2 | p05b == 3 | p05b == 4
	 recode segsoc . = 2
	    lab def l_segsoc 1"Si" 2"No"
	    lab val segsoc l_segsoc
tab edad3 segsoc [aw=fexp], nof row 

svyset upm [w=fexp], strata(estrato) vce(linearized) singleunit(certainty)
svy: proportion segsoc, over (edad3)
estat cv