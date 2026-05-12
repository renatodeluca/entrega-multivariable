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
# cargar datos ----------------------------------
datos <- read_sav("input/base-de-datos---enusc-2024.sav")
options(scipen = 999)
# filtrar informante Kish ----
base_kish <- datos %>% filter(Kish == 1)

# seleccionar variables -----
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
         rph_nivel = ifelse(rph_nivel %in% c(88, 99), NA, rph_nivel))

# crear escalas ----------------
proc_datos <- proc_datos %>%
  mutate(
    escala1 = rowMeans(select(., starts_with("P_INSEG")), na.rm = FALSE),
    escala2 = rowMeans(select(., starts_with("P_DESORDENES") | 
                                starts_with("P_INCIVILIDADES")), na.rm = FALSE),
    escala_vulfis = rowMeans(select(., rph_disc_a, rph_disc_b, rph_disc_c,
                                    rph_disc_d, rph_disc_f), na.rm = FALSE))

colSums(is.na(proc_datos))

nrow(proc_datos)  # filas antes
proc_datos <- na.omit(proc_datos)
nrow(proc_datos)  # filas después


#declarar diseño complejo --------
options(survey.lonely.psu = "adjust")
diseno_enusc <- svydesign(
  id      = ~Conglomerado,
  strata  = ~VarStrat,
  weights = ~Fact_Pers_Reg,
  data    = muestra,
  nest    = TRUE)

# crear índice y recodificaciones -----
muestra <- muestra %>%
  mutate(
    escala1_norm  = (4 - escala1) / (4 - 1),
    escala2_norm  = (escala2 - 1) / (5 - 1),
    indice_inseg  = ((escala1_norm + escala2_norm) / 2) * 100,
    fuente_personal = ifelse(P_FUENTE_INFO_PAIS_1 == 1, 1, 0),
    vulnerable = ifelse(escala_vulfis > 1, 1, 0),
    rph_idgen = factor(rph_idgen,
                       levels = c(2, 1, 3),
                       labels = c("Mujer", "Hombre", "Trans")),
    rph_nivel = factor(rph_nivel,
                       levels = c(3, 1, 2),
                       labels = c("Superior", "Basica", "Media")))


#descriptivos e histograma---------
describe(muestra$indice_inseg) #descriptivos variable dependiente

mfv(muestra$indice_inseg) #moda variable dependiente

#histograma
hist(muestra$indice_inseg,
     breaks = 20,
     col = "#fcba03",
     main = "Índice de percepción de inseguridad",
     xlab = "Puntaje")

#exportar histograma a imagen png
png("output/indice_inseg.png")
hist(
  muestra$indice_inseg,
  breaks = 20,
  col = "#fcba03",
  main = "Índice de percepción de inseguridad",
  xlab = "Puntaje")
dev.off()
# bivariados ------------

# género
modelo_genero <- lm(indice_inseg ~ rph_idgen, data = muestra)
summary(modelo_genero)

standardize_parameters(modelo_genero)

#vulnerabilidad física
modelo_vulfis <- lm(indice_inseg ~ vulnerable, data = muestra)
summary(modelo_vulfis)
standardize_parameters(modelo_vulfis)

#nivel educacional (ref = Superior)
modelo_educ <- lm(indice_inseg ~ rph_nivel, data = muestra)
summary(modelo_educ)
standardize_parameters(modelo_educ)

#fuente de información
t.test(indice_inseg ~ fuente_personal, data = muestra %>% filter(!is.na(fuente_personal)))
muestra %>%
  filter(!is.na(fuente_personal)) %>%
  group_by(fuente_personal) %>%
  summarise(media = mean(indice_inseg, na.rm = TRUE),
            sd = sd(indice_inseg, na.rm = TRUE),
            n = n())
cohens_d(indice_inseg ~ fuente_personal, data = muestra %>% filter(!is.na(fuente_personal)))
# Exposición a drogas
cor.test(muestra$PRESENCIA_TRAFICO, muestra$indice_inseg, method = "pearson")


#guardar base de datos —-----------------
write_sav(muestra, "Grupo_Iota.sav")

