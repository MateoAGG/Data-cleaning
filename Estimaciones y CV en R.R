# ==========================================================================
#==================Pontificia Universidad Catolica del ECUADOR=============
#==========================================================================
# Tabulados de Datos-SocioEconomicos
#             -Mercado laboral
#             -Seguridad Social
#             -Pobreza y Desigualdad
#             -Vivienda
#             -Educacion
# Elaboracion: 
#
#
#
# 1) Working directory -------------------------------------------------------

rm(list = ls())
setwd("C:/Users/magarciagu/OneDrive - Pontificia Universidad Católica del Ecuador/IIE/Data/Enemdu/Anual")
dir()

# 2) Packages ----------------------------------------------------------------

packages <- c("dplyr",
              "tidyverse",
              "survey",
              "haven",
              "tables",
              "sjlabelled",
              "openxlsx",
              "janitor",
              "purrr",
              "srvyr",
              "Hmisc"
              )
for(pkg in packages){
  if(!requireNamespace(pkg, quietly = T)){
    install.packages(pkg)
  }
  library(pkg, character.only = T)
  
}
rm(pkg)
rm(packages)
# 3) Data --------------------------------------------------------------------

dt_24 <- read_dta("base_unida_2024_anual.dta")
dt_23 <- read_dta("base_unida_2023_anual.dta")
dt_22 <- read_dta("base_unida_2022_anual.dta")
dt_21 <- read_dta("base_unida_2021_anual.dta")
dt_19 <- read_dta("base_unida_2019_anual.dta")
dt_18 <- read_dta("base_unida_2018_anual.dta")



# 4) Variables y metadata ------------------------------------------

get_label(dt_24) # Sirve para ver el Nombre de las Variables
get_labels(dt_24) # Sirve para las etiquetas de CADA variable
get_labels(dt_24$pobreza) # Sirve para ver las etiquetas de cada variable individual

# 5) Selección de variables -------------------------------------------------

indicadores <- c("p02", 
                 "area", 
                 "etnia",
                 "edad",
                 "edad2",
                 "prov")

variables_edu <- c("analfabeto", 
                   "nbasica", 
                   "bachillerato", 
                   "nsuperior", 
                   "nini")
variables_labor <- c("pet","pea","pei","tpg","emp","t_adec", 
                     "inadec","subemp","desemp","tnr", 
                     "formal","informal","informala","informalna", 
                     "domest","rama1","grupo1","desemp_juv",
                     "segsoc","ingrl","p24")

variables_poverty <- c("pobreza", 
                       "epobreza", 
                       "TPM", 
                       "TPEM", 
                       "A",
                       "IPM")

variables_vivi <- c("agua", 
                    "luz", 
                    "excretas", 
                    "basura", 
                    "hacinamiento",
                    "def_hab",
                    "tenecia",
                    "tipo",
                    "via_acc")

dt_24$p02 <- factor(dt_24$p02,
                    levels = c(1,2),
                    labels = c("Hombre","Mujer"))
dt_24$p02 <- set_label(dt_24$p02, "Sexo")

get_labels(dt_24$p02)

dt_24$area


### Etnia

dt_24 <- dt_24 %>%
  mutate(etnia = case_when(
    p15 %in% c(6,7) ~ 1,
    p15 %in% c(2,3,4) ~ 2,
    p15 == 1 ~ 3,
    p15 %in% c(5,8) ~ 4,
    TRUE ~ NA_real_
  ))
dt_24$etnia <- add_labels(dt_24$etnia, labels = c("Mestizos y Blancos" = 1,
                                                  "Afroecuatorianos" = 2,
                                                  "Indigena"= 3,
                                                  "Montuvio"=4))
dt_24$etnia <- set_label(dt_24$etnia, label = "Etnia de la Persona")
dt_24$etnia <- factor(dt_24$etnia,
                      levels = c(1:4),
                      labels = c("Mestizos y Blancos", 
                                 "Afroecuatorianos", 
                                 "Indígena", 
                                 "Montuvio"))


### Edad

dt_24 <- dt_24 %>%
  mutate(edad = case_when(
    p03 >= 12 & p03 <= 17 ~ 1,
    p03 >= 18 & p03 <= 29 ~ 2,
    p03 >= 30 & p03 <= 64 ~ 3,
    p03 >= 65             ~ 4,
    TRUE                  ~ NA_real_
  ))

dt_24$edad <- factor(dt_24$edad,
                     levels = c(1:4),
                     labels = c("Adolescentes", 
                                "Jovenes Adultos", 
                                "Adulto", 
                                "Tercera edad"))
dt_24$edad <- set_label(dt_24$edad, "Grupos de edad1")

# Edad2 (Más desagregado)

dt_24 <- dt_24 %>%
  mutate(edad2 = case_when(
    p03 >= 0 & p03 <= 5 ~ 1,
    p03 >= 6 & p03 <= 11 ~ 2,
    p03 >= 12 & p03 <= 17 ~ 3,
    p03 >= 18 & p03 <= 29 ~ 4,
    p03 >= 30 & p03 <= 64 ~ 5,
    p03 >= 65             ~ 6,
    TRUE                  ~ NA_real_
  ))

dt_24$edad2 <- factor(dt_24$edad2,
                     levels = c(1:6),
                     labels = c("Infantes",
                                "Niños",
                                "Adolescentes", 
                                "Jovenes Adultos", 
                                "Adulto", 
                                "Tercera edad"))
dt_24$edad2 <- set_label(dt_24$edad2, "Grupos de edad2")


# 6) Mercado laboral ----------------------------------------------------------

dt_24 <- dt_24 %>%
  mutate(
    # 01. Población en Edad de Trabajar
    pet = case_when(
      !is.na(condact) & p03 >= 15 ~ 1,
      !is.na(condact) & p03 < 15  ~ 0,
      TRUE                         ~ NA_real_
    ),
    # 02. Población Económicamente Activa
    pea = case_when(
      !is.na(condact) & condact >= 1 & condact <= 8 ~ 1,
      !is.na(condact) & condact == 9                ~ 0,  # excluye inactivos
      TRUE                                           ~ NA_real_
    ),
    # 03. Población Económicamente Inactiva
    pei = case_when(
      !is.na(condact) & condact == 9 ~ 1,
      !is.na(condact) & condact %in% 1:8 ~ 0,
      TRUE                              ~ NA_real_
    ),
    # 04. Tasa de Participación Global
    tpg = if_else(!is.na(pet) & pet > 0, pea / pet, NA_real_),
    # 05. Población con Empleo
    emp = case_when(
      pea == 1 & p20 == 1                                ~ 1,
      pea == 1 & p20 == 2 & p21 <= 11                    ~ 1,
      pea == 1 & p20 == 2 & p21 == 12 & p22 == 1         ~ 1,
      pea == 1                                           ~ 0,
      TRUE                                               ~ NA_real_
    ),
    # 06. Empleo Adecuado
    t_adec = case_when(
      condact == 1              ~ 1,
      condact %in% 2:8          ~ 0,
      TRUE                      ~ NA_real_
    ),
    # 07. Empleo Inadecuado
    inadec = case_when(
      condact %in% 2:5          ~ 1,
      condact %in% c(1, 6:8)    ~ 0,
      TRUE                      ~ NA_real_
    ),
    # 08. Subempleo
    subemp = case_when(
      condact %in% c(2, 3)      ~ 1,
      condact %in% c(1, 4:8)    ~ 0,
      TRUE                      ~ NA_real_
    ),
    # 09a. Desempleo Abierto
    des_ab = case_when(
      pea == 1 & p20 == 2 & p21 == 12 & p22 == 2 & p32 <= 10 ~ 1,
      pea == 1                                               ~ 0,
      TRUE                                                   ~ NA_real_
    ),
    # 09b. Desempleo Oculto
    des_oc = case_when(
      pea == 1 & p20 == 2 & p21 == 12 & p22 == 2 & p32 == 11 & p34 <= 7 & p35 == 1 ~ 1,
      pea == 1                                                                    ~ 0,
      TRUE                                                                        ~ NA_real_
    ),
    # 09c. Total desempleo
    desemp = rowSums(cbind(des_ab, des_oc), na.rm = TRUE),
    # 10. Trabajo No Remunerado
    tnr = case_when(
      p42 %in% 7:9 | p54 %in% 7:9 ~ 1,
      TRUE                        ~ NA_real_
    ),
    # 11. Empleo en Sector Formal
    ocu_f = case_when(
      pea == 1 & secemp == 1 ~ 1,
      pea == 1               ~ 0,
      TRUE                   ~ NA_real_
    ),
    formal = case_when(
      ocu_f == 1 & condact > 0 & condact < 7 ~ 1,
      condact > 0 & condact < 7              ~ 0,
      TRUE                                   ~ NA_real_
    ),
    # 12. Empleo en Sector Informal
    ocu_inf = case_when(
      pea == 1 & secemp == 2 ~ 1,
      pea == 1               ~ 0,
      TRUE                   ~ NA_real_
    ),
    informal = case_when(
      ocu_inf == 1 & condact > 0 & condact < 7 ~ 1,
      condact > 0 & condact < 7                ~ 0,
      TRUE                                     ~ NA_real_
    ),
    # 13. Empleo Informal Agrícola
    ocu_infa = case_when(
      pea == 1 & secemp == 2 & rama1 == 1 ~ 1,
      pea == 1                            ~ 0,
      TRUE                                ~ NA_real_
    ),
    informala = case_when(
      ocu_infa == 1 & condact > 0 & condact < 7 & p03 >= 15 ~ 1,
      condact > 0 & condact < 7 & p03 >= 15                 ~ 0,
      TRUE                                                  ~ NA_real_
    ),
    
    # 14. Empleo Informal No Agrícola
    ocu_infna = case_when(
      pea == 1 & secemp == 2 & rama1 != 1 ~ 1,
      pea == 1                            ~ 0,
      TRUE                                ~ NA_real_
    ),
    informalna = case_when(
      ocu_infna == 1 & condact > 0 & condact < 7 & p03 >= 15 ~ 1,
      condact > 0 & condact < 7 & p03 >= 15                   ~ 0,
      TRUE                                                    ~ NA_real_
    ),
    
    # 15. Empleo Doméstico
    ocu_dom = case_when(
      pea == 1 & secemp == 3 ~ 1,
      pea == 1               ~ 0,
      TRUE                   ~ NA_real_
    ),
    domest = case_when(
      ocu_dom == 1 & condact > 0 & condact < 7 & p03 >= 15 ~ 1,
      condact > 0 & condact < 7 & p03 >= 15                 ~ 0,
      TRUE                                                  ~ NA_real_
    ),
    
    # 16. Desempleo Juvenil
    desemp_juv = case_when(
      condact %in% 7:8 & p03 >= 18 & p03 <= 29 ~ 1,
      condact %in% 1:6 & p03 >= 18 & p03 <= 29 ~ 0,
      TRUE                                     ~ NA_real_
    ),
    
    # 17. Seguridad Social
    segsoc = case_when(
      p05a %in% 1:4 | p05b %in% 1:4 ~ 1,
      TRUE                          ~ 2
    ),
    
    # 18. Ingreso Laboral Promedio (extremos a NA)
    ingrl = if_else(ingrl == 0 | ingrl == 999999, NA_real_, ingrl)
  )

dt_24$pet <- set_label(dt_24$pet,
                       "Poblacion en edad para trabjar")
dt_24$pea <- set_label(dt_24$pea,
                       "Poblacion economicamente activa")
dt_24$pei <- set_label(dt_24$pei,
                       "Poblacion economicamente inactiva")
dt_24$tpg <- set_label(dt_24$tpg,
                       "Tasa de participacion global")
dt_24$emp <- set_label(dt_24$emp,
                       "Poblacion con empleo")
dt_24$t_adec <- set_label(dt_24$t_adec,
                          "Tasa de empleo adecuado")
dt_24$inadec <- set_label(dt_24$inadec,
                          "Tasa de empleo inadecuado")
dt_24$subemp <- set_label(dt_24$subemp,
                          "Subempleo")
dt_24$desemp <- set_label(dt_24$desemp,
                          "Desempleo")
dt_24$desemp_juv <- set_label(dt_24$desemp_juv,
                              "Desempleo juvenil")
dt_24$tnr <- set_label(dt_24$tnr,
                       "Trabajo no remunerado")
dt_24$formal <- set_label(dt_24$formal,
                          "Empleo en el sector formal")
dt_24$informal <- set_label(dt_24$informal,
                            "Empleo en el sector informal")
dt_24$informala <- set_label(dt_24$informala,
                             "Empleo en el sector informal agricola")
dt_24$informalna <- set_label(dt_24$informalna,
                              "Empleo en el sector informal no agricola")
dt_24$domest <- set_label(dt_24$domest,
                          "Tasa de empleo domestico")
dt_24$segsoc <- set_label(dt_24$segsoc,
                          "Seguridad Social")



# 7) Educación ---------------------------------------------------------------

### Analfabetismo

dt_24 <- dt_24 %>%
  mutate(analfabeto = case_when(
    p03 >= 15 & p11 == 2 ~ 1,
    p03 >= 15            ~ 0,
    TRUE                 ~ NA_real_
  ))

dt_24$analfabeto <- add_labels(
  dt_24$analfabeto,
  labels = c(
    "Sabe leer y escribir"         = 0,
    "No sabe leer ni escribir"     = 1
  ))
dt_24$analfabeto <-  set_label(dt_24$analfabeto,
                               "Condicion de analfabetismo")
dt_24$analfabeto <- as_factor(dt_24$analfabeto)

### Anios de Escolaridad

dt_24 <- dt_24 %>%
  mutate(anosins = case_when(
    p03 >= 24 & p03 <= 98 & p10a == 1                              ~ 0,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 0                  ~ 0,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 1                  ~ 2,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 2                  ~ 4,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 3                  ~ 6,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 4                  ~ 7,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 5                  ~ 8,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 6                  ~ 9,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 7                  ~ 10,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 8                  ~ 11,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 9                  ~ 12,
    p03 >= 24 & p03 <= 98 & p10a == 2 & p10b == 10                 ~ 13,
    p03 >= 24 & p03 <= 98 & p10a == 3                              ~ 1,
    p03 >= 24 & p03 <= 98 & p10a == 4                              ~ 1 + p10b,
    p03 >= 24 & p03 <= 98 & p10a == 5                              ~ p10b,
    p03 >= 24 & p03 <= 98 & p10a == 6                              ~ 7 + p10b,
    p03 >= 24 & p03 <= 98 & p10a == 7                              ~ 10 + p10b,
    p03 >= 24 & p03 <= 98 & p10a %in% c(8, 9)                      ~ 13 + p10b,
    p03 >= 24 & p03 <= 98 & p10a == 10                             ~ 18 + p10b,
    TRUE ~ NA_real_  # para los que no cumplen ninguna condición
  ))

dt_24$anosins <- set_label(dt_24$anosins,
                           "Años de Escolaridad")

### Tasa Neta de Asistencia de Eduación Basica

dt_24 <- dt_24 %>%
  mutate(
    nbasica = case_when(
      # Numerador: asisten y están en los niveles básicos esperados
      p03 >= 5 & p03 <= 14 & p07 == 1 &
        (
          p10a == 4 | p10a == 5 | (p10a == 6 & p10b >= 3 & p10b <= 5)
        ) ~ 1,
      # Denominador: edad dentro del rango, pero no cumple el criterio del numerador
      p03 >= 5 & p03 <= 14 ~ 0,
      
      # Otros casos fuera de rango → NA
      TRUE ~ NA_real_
    ))
dt_24$nbasica <- set_label(dt_24$nbasica, 
                           "Tasa neta de Asistencia de Eduacion Basica")

### Tasa Neta de Asistencia Bachillerato

dt_24 <- dt_24 %>%
  mutate(
    bachillerato = case_when(
      # Numerador: asiste y cumple los niveles educativos definidos
      p03 >= 15 & p03 <= 17 & p07 == 1 & (
        (p10a == 5 & p10b == 10) |
          (p10a == 6 & p10b >= 3 & p10b <= 6) |
          (p10a == 7 & p10b >= 0 & p10b <= 3) |
          p10a %in% c(8, 9)
      ) ~ 1,
      
      # Denominador: edad entre 15 y 17
      p03 >= 15 & p03 <= 17 ~ 0,
      
      # Otros casos → NA
      TRUE ~ NA_real_
    )
  )

# Agregamos la etiqueta de variable
dt_24$bachillerato <- set_label(dt_24$bachillerato, "Tasa de Asistencia neta bachillerato")

### Tasa de Asistencia a la Educacion Superior

dt_24 <- dt_24 %>%
  mutate(
    nsuperior = case_when(
      # Numerador: tiene entre 18 y 24, asiste y cursa superior
      p03 >= 18 & p03 <= 24 & p07 == 1 & p10a %in% c(8, 9, 10) ~ 1,
      
      # Denominador: tiene entre 18 y 24 pero no cumple condiciones del numerador
      p03 >= 18 & p03 <= 24 ~ 0,
      
      # Resto: fuera de la edad objetivo
      TRUE ~ NA_real_
    )
  )

# Etiqueta de variable
dt_24$nsuperior <- set_label(dt_24$nsuperior, "Tasa neta de asistencia a educación superior")

### Jovenes que no estudian ni trabajan

dt_24 <- dt_24 %>%
  # Creamos la copia protegida de empleo
  mutate(
    empleo_edu = ifelse(is.na(empleo) & p03 >= 15, 0, empleo),  # reemplaza missings con 0 si tiene 15+
    
    # Definimos la variable NiNi
    nini = case_when(
      edad2 == 4 ~ 0,  # todos los jóvenes inicialmente como no-NiNi
      edad2 == 4 & empleo_edu == 0 & p07 == 2 ~ 1,  # condición NiNi
      TRUE ~ NA_real_
    )
  )

# Etiquetamos la variable nini
dt_24$nini <- add_labels(dt_24$nini,
                         labels = c("No NiNi" = 0, "NiNi" = 1))
dt_24$nini <- set_label(dt_24$nini, "Jóvenes que no estudian ni trabajan")

# 9) Vivienda -----------------------------------------------------------------


# 0) Precalculamos hsize de forma limpia y numérica
dt_24 <- dt_24 %>%
  mutate(
    hsize = as.numeric(ave(as.numeric(p01), id_hogar, FUN = function(x) sum(!is.na(x))))
  )

# 1) Ahora el mutate completo
dt_24 <- dt_24 %>%
  mutate(
    # 01. Sin servicio de agua por red pública
    agua = ifelse(is.na(vi10), NA, vi10 != 1),
    
    # 02. Sin acceso a energía eléctrica pública
    luz = ifelse(!is.na(vi12), vi12 %in% 2:4, NA),
    
    # 03. Sin saneamiento de excretas
    excretas = case_when(
      is.na(vi09) | is.na(area) ~ NA,
      area == 1 & vi09 %in% 2:5 ~ TRUE,
      area == 2 & vi09 %in% 3:5 ~ TRUE,
      TRUE ~ FALSE
    ),
    
    # 04. Sin servicio municipal de recolección de basura
    basura = case_when(
      is.na(vi13)       ~ NA,
      vi13 %in% c(1,3,4,5) ~ TRUE,
      TRUE              ~ FALSE
    ),
    
    # 05. Hacinamiento
    vi07_num = as.numeric(vi07),
    vi07_num = if_else(vi07_num == 99, NA_real_, vi07_num),
    vi07_num = if_else(vi07_num == 0, 1, vi07_num),
    hacinamiento = case_when(
      is.na(vi07_num)       ~ NA_integer_,
      (hsize / vi07_num) > 3 ~ 1L,
      TRUE                  ~ 0L
    ),
    vi07_num = NULL,  # eliminamos auxiliar
    
    # 06. Déficit habitacional
    techo = case_when(
      vi03a == 1 & vi03b %in% 1:2 ~ 1L,
      vi03b == 1 & vi03a %in% 2:4 ~ 1L,
      vi03a == 1 & vi03b == 3     ~ 2L,
      vi03b == 2 & vi03a %in% 2:4 ~ 2L,
      vi03b == 3 & vi03a %in% 2:4 ~ 3L,
      vi03a %in% c(5,6)           ~ 3L,
      TRUE                        ~ NA_integer_
    ),
    pared = case_when(
      (vi05a == 1 & vi05b %in% 1:2) |
        (vi05a == 2 & vi05b == 1)    ~ 1L,
      (vi05a == 1 & vi05b == 3) |
        (vi05a == 2 & vi05b == 2) |
        (vi05a %in% 3:5 & vi05b %in% 1:2) ~ 2L,
      (vi05b == 3 & vi05a %in% 2:5) |
        (vi05a %in% c(6,7))         ~ 3L,
      TRUE                        ~ NA_integer_
    ),
    piso = case_when(
      vi04a <= 3 & vi04b %in% 1:2 ~ 1L,
      vi04b == 1 & vi04a %in% 4:5 ~ 1L,
      (vi04a <= 3 & vi04b == 3) |
        (vi04b == 2 & vi04a %in% 4:5) |
        (vi04a == 6 & vi04b == 1)    ~ 2L,
      (vi04b == 3 & vi04a %in% 4:6) |
        (vi04a == 6 & vi04b == 2) |
        (vi04a %in% 7:8)            ~ 3L,
      TRUE                       ~ NA_integer_
    ),
    tipviv = case_when(
      techo == 1 & pared == 1 & piso %in% 1:3 ~ 1L,
      techo == 1 & pared == 2 & piso == 1    ~ 1L,
      techo == 1 & pared == 2 & piso %in% 2:3 ~ 2L,
      techo == 1 & pared == 3 & piso %in% 1:2 ~ 2L,
      techo == 2 & pared == 1 & piso %in% 1:2 ~ 2L,
      techo == 2 & pared == 2 & piso %in% 1:2 ~ 2L,
      techo == 3 & pared == 1 & piso %in% 1:2 ~ 2L,
      techo == 3 & pared == 2 & piso == 1    ~ 2L,
      techo == 1 & pared == 3 & piso == 3    ~ 3L,
      techo == 2 & pared == 1 & piso == 3    ~ 3L,
      techo == 2 & pared == 2 & piso == 3    ~ 3L,
      techo == 2 & pared == 3 & piso %in% 1:3 ~ 3L,
      techo == 3 & pared == 1 & piso == 3    ~ 3L,
      techo == 3 & pared == 2 & piso %in% 1:3 ~ 3L,
      techo == 3 & pared == 3 & piso %in% 1:3 ~ 3L,
      TRUE                                  ~ NA_integer_
    ),
    def_hab = case_when(
      tipviv %in% c(2,3) ~ 1L,
      tipviv == 1       ~ 0L,
      TRUE              ~ NA_integer_
    ),
    
    # 07. Tenencia de la vivienda
    tenencia = case_when(
      is.na(vi14) ~ NA_integer_,
      vi14 != 4   ~ 1L,
      TRUE        ~ 0L
    ),
    
    # 08. Tipo de vivienda
    tipo = case_when(
      vi02 %in% 5:7 ~ 4L,
      TRUE          ~ as.integer(vi02)
    ),
    
    # 09. Vía de acceso principal
    via_acc = case_when(
      vi01 == 3       ~ 2L,
      vi01 %in% 5:6   ~ 4L,
      TRUE            ~ as.integer(vi01)
    ),
    via_acc = case_when(
      via_acc == 4    ~ 3L,
      TRUE            ~ via_acc
    )
  )

dt_24$agua <- set_label(dt_24$agua,
                   "Servicio de agua por red publica")

dt_24$luz <- set_label(dt_24$luz,
                        "Energia electrica")

dt_24$excretas <- set_label(dt_24$excretas,
                        "Sin saneamiento de excretas")

dt_24$basura <- set_label(dt_24$basura,
                            "Sin servicios de rec basura")

dt_24$hacinamiento <- set_label(dt_24$hacinamiento,
                            "Hacinamiento")

dt_24$def_hab <- set_label(dt_24$def_hab,
                            "Def Habitacional")

dt_24$tenencia <- set_label(dt_24$tenencia,
                            "Tenecia")

dt_24$tipo <- set_label(dt_24$tipo,
                            "Tipo de vivienda")

dt_24$via_acc <- set_label(dt_24$via_acc,
                            "Vias de acceso")

# 8) Pobreza -----------------------------------------------------------------

# DIMENSION 1: EDUCACIÓN

dt_24 <- dt_24 %>%
  mutate(
    asist_basica = case_when(
      p03 >= 5 & p03 <= 14 & p07 == 1 &
        (p10a %in% 1:3 |
           (p10a == 4 & p10b %in% 0:6) |
           (p10a == 5 & p10b %in% 0:9) |
           (p10a == 6 & p10b %in% 0:2)) ~ 1,
      p03 >= 5 & p03 <= 14 ~ 0,
      TRUE ~ NA_real_
    ),
    asist_bach = case_when(
      p03 >= 15 & p03 <= 17 & p07 == 1 &
        ((p10a == 5 & p10b == 10) |
           (p10a == 7 & p10b %in% 0:2) |
           (p10a == 6 & p10b %in% 3:5)) ~ 1,
      p03 >= 15 & p03 <= 17 ~ 0,
      TRUE ~ NA_real_
    ),
    dim1_ind1 = case_when(
      (asist_basica == 1 | asist_bach == 1 | p10a >= 8) & (p03 >= 5 & p03 <= 17) ~ 0,
      p10a < 8 & (p03 >= 5 & p03 <= 17) ~ 1,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    dim1_ind2 = case_when(
      p03 >= 18 & p03 <= 29 & p07 == 2 &
        ((p10a == 7 & p10b >= 3) |
           (p10a == 6 & p10b >= 6) |
           (p10a %in% 8:9)) & p09 == 3 ~ 1,
      p03 >= 18 & p03 <= 29 & p07 %in% 1:2 &
        ((p10a == 7 & p10b >= 3) |
           (p10a == 6 & p10b >= 6) |
           p10a %in% 8:10) ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    escol = case_when(
      p10a == 1 & p10b == 0 ~ 0, p10a == 2 & p10b == 0 ~ 0,
      p10a == 2 & p10b == 1 ~ 2, p10a == 2 & p10b == 2 ~ 4,
      p10a == 2 & p10b == 3 ~ 6, p10a == 2 & p10b == 4 ~ 7,
      p10a == 3 ~ 1, p10a == 4 ~ 1 + p10b, p10a == 5 ~ p10b,
      p10a == 6 ~ 7 + p10b, p10a == 7 ~ 10 + p10b,
      p10a == 8 ~ 13 + p10b, p10a == 9 ~ 13 + p10b,
      p10a == 10 ~ 18 + p10b,
      TRUE ~ 0
    ),
    dim1_ind3 = case_when(
      p03 >= 18 & p03 <= 64 & escol < 10 & p07 == 2 ~ 1,
      p03 >= 18 & p03 <= 64 ~ 0,
      TRUE ~ NA_real_
    )
  )


# DIMENSION 2: TRABAJO Y SEGURIDAD SOCIAL

dt_24 <- dt_24 %>%
  mutate(across(starts_with("p51"), ~na_if(., 999))) %>%
  mutate(hh = rowSums(select(., starts_with("p51")), na.rm = TRUE)) %>%
  mutate(
    horas = case_when(
      pea == 1 & p20 == 2 & p21 == 12 & p22 == 1 ~ hh,
      pea == 1 & p20 == 1 ~ p24,
      pea == 1 & p20 == 2 & p21 <= 11 ~ p24,
      empleo == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    dim2_ind1 = case_when(
      (p20 == 1 | p21 %in% 1:11 | p22 == 1) & (p03 >= 5 & p03 <= 14) ~ 1,
      condact %in% 2:6 & (p03 >= 15 & p03 <= 17) ~ 1,
      condact == 1 & (p03 >= 15 & p03 <= 17) & (p07 == 2 | horas > 30) ~ 1,
      p03 >= 5 & p03 <= 17 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    dim2_ind2 = case_when(
      p03 < 18 | p03 == 99 ~ NA_real_,
      is.na(p20) & is.na(p21) & is.na(p22) & is.na(p32) & is.na(p34) & is.na(p35) ~ NA_real_,
      condact %in% 2:8 & (p03 >= 18 & p03 <= 98) ~ 1,
      p03 >= 18 & p03 <= 98 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%
  mutate(
    dim2_ind3 = case_when(
      pet == 0 | (p03 >= 0 & p03 <= 14) ~ NA_real_,
      pet == 1 & p77 == 1 ~ 0,
      p03 >= 65 & p72a == 2 & p75 == 1 ~ 0,
      (desemp == 1 | pei == 1) & p03 >= 65 & p72a == 2 ~ 1,
      empleo == 1 & p03 >= 65 & p72a == 1 ~ 0,
      empleo == 1 & (p05a %in% 5:10) & (p05b %in% 5:10) ~ 1,
      TRUE ~ 0
    )
  )


# DIMENSION 3: SALUD, AGUA Y ALIMENTACIÓN

dt_24 <- dt_24 %>%
  mutate(
    dim3_ind1 = epobreza,
    dim3_ind2 = if_else(is.na(vi10), NA_real_, as.numeric(vi10 != 1))
  )


# DIMENSION 4: HABITAT, VIVIENDA Y AMBIENTE SANO

dt_24 <- dt_24 %>%
  mutate(vi07 = if_else(vi07 == 0, 1, vi07)) %>%
  group_by(id_hogar) %>%
  mutate(hsize = n()) %>%
  ungroup() %>%
  mutate(dim4_ind1 = if_else((hsize / vi07) > 3, 1, 0)) %>%
  mutate(
    techo = case_when(
      (vi03a == 1 & vi03b %in% 1:2) | (vi03b == 1 & vi03a %in% 2:4) ~ 1,
      (vi03a == 1 & vi03b == 3) | (vi03b == 2 & vi03a %in% 2:4) ~ 2,
      (vi03b == 3 & vi03a %in% 2:4) | vi03a %in% 5:6 ~ 3
    ),
    pared = case_when(
      (vi05a == 1 & vi05b %in% 1:2) | (vi05a == 2 & vi05b == 1) ~ 1,
      (vi05a == 1 & vi05b == 3) | (vi05a == 2 & vi05b == 2) | (vi05a %in% 3:5 & vi05b %in% 1:2) ~ 2,
      (vi05b == 3 & vi05a %in% 2:5) | vi05a %in% 6:7 ~ 3
    ),
    piso = case_when(
      (vi04a <= 3 & vi04b %in% 1:2) | (vi04b == 1 & vi04a %in% 4:5) ~ 1,
      (vi04a <= 3 & vi04b == 3) | (vi04b == 2 & vi04a %in% 4:5) | (vi04a == 6 & vi04b == 1) ~ 2,
      (vi04b == 3 & vi04a %in% 4:6) | (vi04a == 6 & vi04b == 2) | vi04a %in% 7:8 ~ 3
    )
  ) %>%
  mutate(
    tipviv = case_when(
      (techo == 1 & pared == 1 & piso %in% 1:3) | (techo == 1 & pared == 2 & piso == 1) ~ 1,
      (techo == 1 & pared == 2 & piso %in% 2:3) | (techo == 1 & pared == 3 & piso %in% 1:2) |
        (techo == 2 & pared == 1 & piso %in% 1:2) | (techo == 2 & pared == 2 & piso %in% 1:2) |
        (techo == 3 & pared == 1 & piso %in% 1:2) | (techo == 3 & pared == 2 & piso == 1) ~ 2,
      (techo == 1 & pared == 3 & piso == 3) | (techo == 2 & pared == 1 & piso == 3) |
        (techo == 2 & pared == 2 & piso == 3) | (techo == 2 & pared == 3 & piso %in% 1:3) |
        (techo == 3 & pared == 1 & piso == 3) | (techo == 3 & pared == 2 & piso %in% 1:3) |
        (techo == 3 & pared == 3 & piso %in% 1:3) ~ 3
    )
  ) %>%
  set_labels(tipviv, labels = c("Aceptables" = 1, "Recuperables" = 2, "Irrecuperables" = 3)) %>%
  mutate(
    dim4_ind2 = if_else(tipviv %in% 2:3, 1, if_else(tipviv == 1, 0, NA_real_)),
    dim4_ind3 = case_when(
      is.na(vi09) | is.na(area) ~ NA_real_,
      (area == 1 & vi09 %in% 2:5) | (area == 2 & vi09 %in% 3:5) ~ 1,
      TRUE ~ 0
    ),
    dim4_ind4 = if_else(is.na(vi13), NA_real_, as.numeric(vi13 %in% c(1, 3, 4, 5)))
  )


# Identificación y agregación de privaciones

dim_vars <- names(dt_24)[grep("^dim\\d_ind\\d$", names(dt_24))]

dt_24 <- dt_24 %>%
  rename_with(~ paste0(., "_p"), all_of(dim_vars)) %>%
  group_by(id_hogar) %>%
  mutate(across(ends_with("_p"), ~max(., na.rm = TRUE), .names = "{.col}_h")) %>%
  ungroup() %>%
  rename_with(~ sub("_p_h$", "_h", .), ends_with("_p_h")) %>%
  mutate(across(ends_with("_h"), ~replace(., is.infinite(.), NA))) %>%
  mutate(across(ends_with("_h"), ~coalesce(., 0)))


# Tratamiento de valores extremos: Missing data

dt_24 <- dt_24 %>%
  mutate(
    miss_dim1_ind1 = if_else(p03 %in% 5:17, rowSums(is.na(select(., p07, p10a))), NA_real_),
    miss_dim1_ind2 = if_else(p03 %in% 18:29, rowSums(is.na(select(., p07, p10a))), NA_real_),
    miss_dim1_ind3 = if_else(p03 %in% 18:64, rowSums(is.na(select(., p07, escol))), NA_real_),
    miss_dim2_ind1 = case_when(
      p03 %in% 5:14 ~ as.numeric(is.na(p20) & is.na(p21) & is.na(p22)),
      p03 %in% 15:17 ~ as.numeric(is.na(condact) | (condact == 1 & (is.na(p07) | is.na(horas)))),
      TRUE ~ NA_real_
    ),
    miss_dim2_ind2 = if_else(p03 %in% 18:98, as.numeric(is.na(condact) | (is.na(p20) & is.na(p21) & is.na(p22) & is.na(p32) & is.na(p34) & is.na(p35))), NA_real_),
    miss_dim2_ind3 = if_else(p03 %in% 15:98, rowSums(is.na(select(., p72a, p75, p77, p05a, p05b))), NA_real_),
    miss_dim3_ind1 = as.numeric(is.na(epobreza)),
    miss_dim3_ind2 = as.numeric(is.na(vi10)),
    miss_dim4_ind1 = rowSums(is.na(select(., hsize, vi07))),
    miss_dim4_ind2 = rowSums(is.na(select(., vi03a, vi03b, vi04a, vi04b, vi05a, vi05b))),
    miss_dim4_ind3 = as.numeric(is.na(vi09)),
    miss_dim4_ind4 = as.numeric(is.na(vi13))
  ) %>%
  mutate(miss_total = rowSums(select(., starts_with("miss_dim")), na.rm = TRUE)) %>%
  mutate(miss_total = if_else(miss_total > 0, 1, 0)) %>%
  filter(p03 != 99 | is.na(p03)) %>%
  group_by(id_hogar) %>%
  mutate(miss_total = max(miss_total, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(miss_total != 1)


# Estructura de ponderación y cálculo final

pesos <- c(1/4*1/3, 1/4*1/3, 1/4*1/3, 1/4*1/3, 1/4*1/3, 1/4*1/3, 
           1/4*1/2, 1/4*1/2, 1/4*1/4, 1/4*1/4, 1/4*1/4, 1/4*1/4)

dim_h_vars <- sort(names(dt_24)[grep("^dim\\d_ind\\d_h$", names(dt_24))])

df_wp <- dt_24[dim_h_vars] * rep(pesos, each = nrow(dt_24))
names(df_wp) <- paste0("wp_", dim_h_vars)

dt_24 <- dt_24 %>%
  bind_cols(df_wp) %>%
  mutate(
    ci = rowSums(select(., starts_with("wp_")), na.rm = TRUE)
  ) %>%
  mutate(
    TPM  = as.numeric(ci >= 4/12),
    TPEM = as.numeric(ci >= 6/12),
    A    = if_else(TPM == 1, ci, NA_real_),
    IPM  = if_else(TPM == 1, ci, 0)
  )

# 10) Diseño Muestral ---------------------------------------------------------

design <- svydesign(ids = ~upm,
                    strata = ~estrato,
                    weights = ~fexp,
                    data = dt_24,
                    nest = TRUE)

design_srvyr <- dt_24 %>%
  as_survey_design(
    ids = upm,
    strata = estrato,
    weights = fexp,
    nest = TRUE
  )

# 11) How to Use Tabulations of the sampling design (optional) ----------------------------------

dt_24 %>%
  tabyl(analfabeto, weights = fexp, show_na = F) %>%
  mutate(
    Frecuencia = n,
    Porcentaje = round(n / sum(n) * 100, 2),
    Cum        = round(cumsum(Porcentaje), 2)
  ) %>%
  select(analfabeto, Frecuencia, Porcentaje, Cum) %>%
  mutate(
    Porcentaje = paste0(Porcentaje, "%"),
    Cum        = paste0(Cum, "%")
  )

tab_w <- svytable(~analfabeto, design)

# 3) Porcentajes y acumulado
pct   <- prop.table(tab_w) * 100     # porcentaje de cada categoría
cum   <- cumsum(pct)                 # porcentaje acumulado

# 4) Armar el data.frame con dos decimales
res <- data.frame(
  analfabeto = names(tab_w),
  Frecuencia = round(as.numeric(tab_w), 2),
  Porcentaje = round(as.numeric(pct), 2),
  Cum        = round(as.numeric(cum), 2),
  stringsAsFactors = FALSE
)

# 5) Añadir fila Total
total <- data.frame(
  analfabeto = "Total",
  Frecuencia = sum(res$Frecuencia),
  Porcentaje = round(sum(res$Frecuencia) / sum(res$Frecuencia) * 100, 2),
  Cum        = 100,
  stringsAsFactors = FALSE
)

resultado <- bind_rows(res, total)

# 6) (Opcional) Formatear porcentaje y acumulado con “%”
resultado <- resultado %>%
  mutate(
    Porcentaje = paste0(sprintf("%.2f", Porcentaje), "%"),
    Cum        = paste0(sprintf("%.2f", Cum), "%")
  )

print(resultado)





# 12) Estimates and Coefficients of Variation ---------------------------------

a <- design_srvyr %>%
  group_by(edad2) %>%
  summarise(
     TPM= survey_mean(TPM, na.rm = T, vartype = "cv")
  ) %>%
  mutate(
    TPM = format(round(TPM * 100, 25), nsmall = 2),
    TPM_cv = format(round(TPM_cv * 100, 25), nsmall = 2)
  )

for (ind in indicadores) {
for (edu in variables_edu) {
  design_srvyr %>%
    group_by(ind) %>%
    summarise(
      edu = survey_mean(edu, na.rm = T, vartype = "cv")
    ) %>%
    mutate(
      edu = format(round(edu * 100, 10), nsmall = 2),
      edu_cv = format(round(edu * 100, 10), nsmall = 2)
    )
}
}





###
horas_sexo <- svyby(~TPM, ~p02, design, svymean, na.rm = TRUE, vartype = "cv")
horas_sexo
###

# 13) Write in excel (En producción)) ----------------------------------------------------------

library(openxlsx)

wb <- createWorkbook()
addWorksheet(wb, "Indicadores")

## 1) Estilos
title_style    <- createStyle(fontSize = 14, halign = "CENTER", textDecoration = "bold",
                              fgFill = "#002060", fontColour = "white")
header_azul    <- createStyle(textDecoration = "bold", halign = "CENTER",
                              fgFill = "#002060", fontColour = "white",
                              border = "TopBottomLeftRight")
subheader_style<- createStyle(textDecoration = "bold", fgFill = "#D9E1F2", 
                              halign = "LEFT", border = "TopBottomLeftRight")
border_style   <- createStyle(border = "TopBottomLeftRight", halign = "CENTER")
nota1          <- createStyle(fontSize = 8, fontColour = "#444444")
nota2          <- createStyle(fontSize = 8, fontColour = "#FF0000")

## 2) TÍTULO combinado en filas 1–2, columnas 1–8
writeData(wb, "Indicadores", "Indicadores educación 2018-2024", startRow = 1, startCol = 1)
mergeCells(wb, "Indicadores", cols = 1:8, rows = 1:2)
addStyle(wb, "Indicadores", title_style, rows = 1, cols = 1:8, gridExpand = TRUE)

## 3) Encabezado “AÑO” + años 2018–2024 en fila 4, columnas 2–8
writeData(wb, "Indicadores", "AÑO", startRow = 4, startCol = 2)
mergeCells(wb, "Indicadores", cols = 2, rows = 4)
writeData(wb, "Indicadores", as.character(2018:2024), startRow = 4, startCol = 3)
addStyle(wb, "Indicadores", header_azul, rows = 4, cols = 2:8, gridExpand = TRUE)

## 4) “Nacional” en fila 5, col 2 (subencabezado)
writeData(wb, "Indicadores", "Nacional", startRow = 5, startCol = 2)
addStyle(wb, "Indicadores", subheader_style, rows = 5, cols = 2, gridExpand = TRUE)

## 5) Estructura de filas en columna A (fila 6 en adelante)
categorias <- c(
  "Género", 
  "  Hombre", 
  "  Mujer", 
  "Área", 
  "  Urbano", 
  "  Rural", 
  "Autoidentificación", 
  "  Mestizos/as y Blancos/as", 
  "  Afroecuatorinos/as", 
  "  Indígenas", 
  "  Montuvios/as", 
  "Grupo de edad", 
  "  Jóvenes adultos", 
  "  Adultos", 
  "  Tercera edad"
)
writeData(wb, "Indicadores", categorias, startRow = 6, startCol = 1)
# Aplicar estilo de subheader a las categorías “padre” (sin sangría):
padres <- c(6, 9, 12)  # filas donde están “Género” (6), “Área” (9), “Autoidentificación” (12) y “Grupo de edad” (16) 
# (ajusta índices según tu lista)
addStyle(wb, "Indicadores", subheader_style, rows = c(6, 9, 12, 16), cols = 1, gridExpand = TRUE)

## 6) DATOS (valores numéricos) desde fila 6, columna 3 en adelante (para cada categoría y año)
# Supongamos que tienes un data.frame llamado df_valores con 15 filas y 7 columnas (años).
# df_valores[i, j] corresponde al valor de la categoría i en el año j (2018 + j - 1).
# Por ejemplo:
# df_valores <- data.frame(
#   `2018` = c(10.05, 10.17, 9.95, 11.27, 7.22, 10.56, 9.39, 6.47, 7.03, 12.62, 10.34, 6.34),
#   `2019` = c(10.08, 10.17, 10.00, 11.27, 7.30, 10.61, 9.64, 6.55, 7.21, 12.76, 10.37, 6.42),
#   ...
# )
#
# Para este ejemplo usaremos números al azar:
set.seed(123)
df_valores <- round(matrix(runif(length(categorias) * 7, 6, 14), nrow = length(categorias), ncol = 7), 2)

writeData(wb, "Indicadores", df_valores, startRow = 6, startCol = 3, colNames = FALSE)

## 7) Bordes alrededor de toda la zona de datos (incluyendo títulos y filas de categorías)
addStyle(wb, "Indicadores", border_style, rows = 4:(5 + nrow(df_valores)), cols = 1:8, gridExpand = TRUE)

## 8) Notas al pie en las filas 22–23 (ajusta si tus datos ocupan más o menos filas)
writeData(wb, "Indicadores", "*Los datos han sido calculados en base a las ENEMDU anual de cada año", 
          startRow = 22, startCol = 1)
addStyle(wb, "Indicadores", nota1, rows = 22, cols = 1)

writeData(wb, "Indicadores", "**Coeficiente de Variación superior al 15% por lo tanto se debe utilizar con precaución.", 
          startRow = 23, startCol = 1)
addStyle(wb, "Indicadores", nota2, rows = 23, cols = 1)

## 9) Ajustar ancho de columnas (opcional, para que los textos no queden recortados)
setColWidths(wb, "Indicadores", cols = 1, widths = 25)  # columna A más ancha para subcategorías
setColWidths(wb, "Indicadores", cols = 2, widths = 15)  # columna B
setColWidths(wb, "Indicadores", cols = 3:8, widths = 12) # columnas de años

## 10) Guardar archivo
saveWorkbook(wb, "indicadores_final.xlsx", overwrite = TRUE)












collapse_pea <- svyby(
  formula = ~pea,
  by = ~etnia,
  design = design,
  FUN = svymean,
  na.rm = TRUE,
  vartype = c("se", "ci")
)

print(collapse_pea)

collapse_peatotal <- design_srvyr %>%
  group_by(etnia) %>%
  summarise(
    prop_pea = survey_mean(bachillerato, vartype = c("ci", "se")),
    total_pea = survey_total(bachillerato, vartype = "se")
  )

print(collapse_peatotal)





resultados <- list()

for (i in indicadores) {
  for (v in variables_labor) {
    
    tmp <- design_srvyr %>%
      group_by(!! sym(i)) %>%
      summarise(
        # genera columnas "v" y "v_cv"
        !! v := survey_mean(.data[[v]], vartype = "cv", na.rm = FALSE),
        .groups = "drop"
      ) %>%
      mutate(
        # multiplicamos siempre *100 y redondeamos a 8 decimales
        !! v             := format(round(.data[[v]] * 100, 8),     nsmall = 8),
        !! paste0(v, "_cv") := format(round(.data[[paste0(v, "_cv")]], 8), nsmall = 8)
      )
    
    # guardamos con nombre "indicador_variable"
    resultados[[ paste(i, v, sep = "_") ]] <- tmp
  }
}

# Ejemplo: ver el resultado para etnia–pet
resultados[["etnia_rama1"]]




get_labels(dt_24$pobreza)
