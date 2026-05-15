#Amaya Barbet, Renato De Luca y Tomás Gonzalez
#entrega grupal multivariable nº1

# Carga de paquetes --------
install.packages("lm.beta")
library(haven)
library(survey)
library(ggplot2)
library(psych)
library(effectsize)
library(parameters)
library(dplyr)
library(modeest)
library(jtools)
library(lm.beta)
# cargar datos ----------------------------------
datos <- read_sav("/input/data/original/base-de-datos---enusc-2024.sav")

options(scipen = 999)

# filtrar informante Kish ----
base_kish <- datos %>% filter(Kish == 1)

# seleccionar variables de interés --------
proc_datos <- dplyr::select(base_kish,
                            Conglomerado, VarStrat, Fact_Pers_Reg,
                            P_INSEG_LUGARES_1, P_INSEG_LUGARES_2, P_INSEG_LUGARES_3, P_INSEG_LUGARES_4,
                            P_INSEG_LUGARES_5, P_INSEG_LUGARES_7, P_INSEG_LUGARES_8,
                            P_INSEG_LUGARES_10, P_INSEG_LUGARES_11, P_INSEG_LUGARES_12,
                            P_INSEG_LUGARES_13, P_INSEG_LUGARES_14, P_INSEG_LUGARES_16,
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
         rph_nivel = ifelse(rph_nivel %in% c(0, 88, 99), NA, rph_nivel))

# crear escalas ----------------
proc_datos <- proc_datos %>%
  mutate(
    escala1 = rowMeans(select(., starts_with("P_INSEG")), na.rm = FALSE),
    escala2 = rowMeans(select(., starts_with("P_DESORDENES_") |
                                starts_with("P_INCIVILIDADES_")), na.rm = FALSE),
    escala_vulfis = rowMeans(select(., rph_disc_a, rph_disc_b, rph_disc_c,
                                    rph_disc_d, rph_disc_f), na.rm = FALSE))

# eliminar missings --------
proc_datos <- na.omit(proc_datos)
nrow(proc_datos) # 3570 casos

# submuestra aleatoria --------
set.seed(123)
muestra <- proc_datos %>% sample_n(3000)
nrow(muestra) # 3000 casos

# alpha de las escalas --------
alpha(muestra %>% select(starts_with("P_INSEG")))
alpha(muestra %>% select(starts_with("P_DESORDENES_") |
                           starts_with("P_INCIVILIDADES_")))
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

# descriptivos --------
# variable dependiente (continua)
summary(muestra$indice_inseg)
sd(muestra$indice_inseg)

# exposición a drogas (ordinal)
summary(muestra$PRESENCIA_TRAFICO)
sd(muestra$PRESENCIA_TRAFICO)

# género (categórica nominal)
table(muestra$rph_idgen)
prop.table(table(muestra$rph_idgen)) * 100

# nivel educacional (categórica ordinal)
table(muestra$rph_nivel)
prop.table(table(muestra$rph_nivel)) * 100

# vulnerabilidad física (dicotómica)
table(muestra$vulnerable)
prop.table(table(muestra$vulnerable)) * 100

# fuente de información (dicotómica)
table(muestra$fuente_personal)
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

# guardar base --------
saveRDS(muestra, "input/data/proc/muestra_procesada.rds")

# Regresión lineal múltiple
# Modelo 1
# (Género y Discapacidad/Vulnerabilidad física)
m1 <- svyglm(indice_inseg ~ rph_idgen + rph_disc_a + rph_nivel,
             design = diseno_enusc)

# Modelo 2
# (Presencia de tráfico y Nivel educacional)
m2 <- svyglm(indice_inseg ~ PRESENCIA_TRAFICO+ P_FUENTE_INFO_PAIS_1, 
             design = diseno_enusc)

# Modelo 3:Todas las variables 
m3 <- svyglm(indice_inseg ~ rph_idgen + rph_disc_a + PRESENCIA_TRAFICO + 
               rph_nivel + P_FUENTE_INFO_PAIS_1, 
             design = diseno_enusc)

summary(lm.beta(m1))

summary(lm.beta(m2))
summary(lm.beta(m3))


# R estandarizado y b std 

summ(m1, scale = TRUE, transform.response = TRUE, digits = 3)
summ(m2, scale = TRUE, transform.response = TRUE, digits = 3)
summ(m3, scale = TRUE, transform.response = TRUE, digits = 3)
# R de pearson para modelo 3 
sqrt(0.294)

