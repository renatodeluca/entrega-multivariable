#Amaya Barbet, Renato De Luca y Tomás Gonzalez
#entrega grupal multivariable nº1

# Carga de paquetes --------
library(haven)
library(survey)
library(ggplot2)
library(psych)
library(effectsize)
library(parameters)
library(dplyr)
library(modeest)
library(jtools)

# cargar datos ----------------------------------
datos <- read_sav("input/data/original/base-de-datos---enusc-2024.sav")
options(scipen = 999)

# filtrar informante Kish ----
base_kish <- datos %>% filter(Kish == 1)

# seleccionar variables -----
proc_datos <- dplyr::select(base_kish,
                            Conglomerado, VarStrat, Fact_Pers_Reg,
                            P_INSEG_LUGARES_1, P_INSEG_LUGARES_2, P_INSEG_LUGARES_3, P_INSEG_LUGARES_4,
                            P_INSEG_LUGARES_5, P_INSEG_LUGARES_6, P_INSEG_LUGARES_7, P_INSEG_LUGARES_8,
                            P_INSEG_LUGARES_9, P_INSEG_LUGARES_10, P_INSEG_LUGARES_11, P_INSEG_LUGARES_12,
                            P_INSEG_LUGARES_13, P_INSEG_LUGARES_14, P_INSEG_LUGARES_15, P_INSEG_LUGARES_16,
                            P_INSEG_OSCURO_1, P_INSEG_DIA_1, P_INSEG_OSCURO_2, P_INSEG_DIA_2,
                            P_DESORDENES_1, P_DESORDENES_2, P_DESORDENES_3, P_DESORDENES_4,
                            P_DESORDENES_5, P_DESORDENES_6, P_DESORDENES_7, P_DESORDENES_8,
                            P_INCIVILIDADES_1, P_INCIVILIDADES_2, P_INCIVILIDADES_3,
                            P_INCIVILIDADES_4, P_INCIVILIDADES_5, P_INCIVILIDADES_6, P_INCIVILIDADES_7,
                            rph_idgen, rph_disc_a, rph_disc_b, rph_disc_c, rph_disc_d,
                            rph_disc_f, rph_nivel, PRESENCIA_TRAFICO, P_FUENTE_INFO_PAIS_1)

# recodificar missings ----------------
proc_datos <- proc_datos %>%
  mutate(across(starts_with("P_"),
                ~ ifelse(. %in% c(77, 85, 88, 99), NA, .)),
         PRESENCIA_TRAFICO = ifelse(PRESENCIA_TRAFICO %in% c(88, 99), NA, PRESENCIA_TRAFICO),
         rph_idgen = ifelse(rph_idgen %in% c(88, 96, 99), NA, rph_idgen),
         rph_nivel = ifelse(rph_nivel %in% c(0,88, 99), NA, rph_nivel))

# crear escalas ----------------
cols <- c(paste0("P_INSEG_LUGARES_", 1:16),
          "P_INSEG_OSCURO_1", "P_INSEG_DIA_1",
          "P_INSEG_OSCURO_2", "P_INSEG_DIA_2")

proc_datos <- proc_datos %>%
  mutate(
    escala1_n = rowSums(!is.na(across(all_of(cols)))),
    escala1_valida = escala1_n >= ceiling(length(cols) / 3),
    escala1 = if_else(
      escala1_valida,
      rowMeans(across(all_of(cols)), na.rm = TRUE),
      NA_real_
    ),
    escala2 = rowMeans(select(., starts_with("P_DESORDENES_") |
                                starts_with("P_INCIVILIDADES_")), na.rm = FALSE),
    escala_vulfis = rowMeans(select(., rph_disc_a, rph_disc_b, rph_disc_c,
                                    rph_disc_d, rph_disc_f), na.rm = FALSE))

colSums(is.na(proc_datos))
table(proc_datos$escala1_valida)


#limpiar datos para análisis --------
proc_datos <- proc_datos %>%
  select(-escala1_n, -escala1_valida) %>%
  filter(
    !is.na(escala1),
    !is.na(escala2),
    !is.na(escala_vulfis),
    !is.na(rph_idgen),
    !is.na(rph_nivel),
    !is.na(PRESENCIA_TRAFICO),
    !is.na(P_FUENTE_INFO_PAIS_1)
  )

nrow(proc_datos)

# submuestra 2500 casos --------
set.seed(123)
muestra <- proc_datos %>% sample_n(2500)
nrow(muestra)


#ver alpha de las escalas-------

# alpha escala 1 (20 ítems)
alpha(muestra %>% select(all_of(cols)))

# alpha escala 2 (15 ítems)
alpha(muestra %>% select(starts_with("P_DESORDENES_") | 
                           starts_with("P_INCIVILIDADES_")))
#alpha escala vulnerabilidad (5 ítems)
alpha(muestra %>% select(rph_disc_a, rph_disc_b, rph_disc_c, rph_disc_d, rph_disc_f))

# recodificaciones sobre la submuestra --------
muestra <- muestra %>%
  mutate(
    escala1_norm = (4 - escala1) / 3,
    escala2_norm = (escala2 - 1) / 4,
    indice_inseg = ((escala1_norm + escala2_norm) / 2) * 100,
    fuente_personal = ifelse(P_FUENTE_INFO_PAIS_1 == 1, 1, 0),
    vulnerable = ifelse(escala_vulfis > 1, 1, 0),
    rph_idgen = factor(rph_idgen,
                       levels = c(2, 1, 3),
                       labels = c("Mujer", "Hombre", "Trans")),
    rph_nivel = factor(rph_nivel,
                       levels = c(3, 1, 2),
                       labels = c("Superior", "Basica", "Media")))

# declarar diseño complejo --------
options(survey.lonely.psu = "adjust")
diseno_enusc <- svydesign(
  id      = ~Conglomerado,
  strata  = ~VarStrat,
  weights = ~Fact_Pers_Reg,
  data    = muestra,
  nest    = TRUE)

# descriptivos e histograma --------
# variable dependiente
# continuas
  #VARIABLE DEPENDIENTE
summary(muestra$indice_inseg)
sd(muestra$indice_inseg)
  #PRESENCIA TRAFICO
summary(muestra$PRESENCIA_TRAFICO)
sd(muestra$PRESENCIA_TRAFICO)

# categóricas
table(muestra$rph_idgen)
prop.table(table(muestra$rph_idgen)) * 100

table(muestra$rph_nivel)
prop.table(table(muestra$rph_nivel)) * 100

# dicotómicas
prop.table(table(muestra$vulnerable)) * 100
prop.table(table(muestra$fuente_personal)) * 100
# histograma --------
svyhist(~indice_inseg, diseno_enusc,
        breaks = 20,
        col = "#fcba03",
        main = "Índice de percepción de inseguridad",
        xlab = "Puntaje")

png("output/graphs/hist_indice_inseg.png")
svyhist(~indice_inseg, diseno_enusc,
        breaks = 20,
        col = "#fcba03",
        main = "Índice de percepción de inseguridad",
        xlab = "Puntaje")
dev.off()

# bivariados --------
modelo_genero <- svyglm(indice_inseg ~ rph_idgen, design = diseno_enusc)
summary(modelo_genero)

modelo_vulfis <- svyglm(indice_inseg ~ vulnerable, design = diseno_enusc)
summary(modelo_vulfis)

modelo_educ <- svyglm(indice_inseg ~ rph_nivel, design = diseno_enusc)
summary(modelo_educ)

modelo_fuente <- svyglm(indice_inseg ~ fuente_personal, design = diseno_enusc)
summary(modelo_fuente)

svyby(~indice_inseg, ~fuente_personal, diseno_enusc, svymean)

svycor(~indice_inseg + PRESENCIA_TRAFICO, diseno_enusc, sig.stats = TRUE)

#guardar base
saveRDS(muestra, "input/data/proc/muestra_procesada.rds")
