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
                            P_INSEG_LUGARES_5, P_INSEG_LUGARES_6, P_INSEG_LUGARES_7, P_INSEG_LUGARES_8,P_INSEG_LUGARES_9, 
                            P_INSEG_LUGARES_10, P_INSEG_LUGARES_11, P_INSEG_LUGARES_12,
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
         rph_nivel = ifelse(rph_nivel %in% c(88, 99), NA, rph_nivel))

# crear escalas ----------------
cols <- paste0("P_INSEG_LUGARES_", 1:16)
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

proc_datos <- proc_datos %>%
  select(-escala1_n, -escala1_valida) %>%   # sacar columnas auxiliares
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

#submuestra 2500 casos
set.seed(123) # para reproducibilidad
muestra <- proc_datos %>% sample_n(2500)
nrow(muestra)

#recodificaciones sobre la submuestra
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

# declaar el diseño complejo con los datos ya limpios
options(survey.lonely.psu = "adjust")
diseno_enusc <- svydesign(
  id      = ~Conglomerado,
  strata  = ~VarStrat,
  weights = ~Fact_Pers_Reg,
  data    = muestra,
  nest    = TRUE)


#descriptivos e histograma---------
      #descriptivos 
         #variable dependiente
svymean(~indice_inseg, diseno_enusc)
svyquantile(~indice_inseg, diseno_enusc, quantiles = c(0.25, 0.5, 0.75))
sqrt(svyvar(~indice_inseg, diseno_enusc))
         #variables independientes
          # Género (categórica nominal)
svytable(~rph_idgen, diseno_enusc)
prop.table(svytable(~rph_idgen, diseno_enusc))

# Nivel educacional (categórica ordinal)
svytable(~rph_nivel, diseno_enusc)
prop.table(svytable(~rph_nivel, diseno_enusc))

# Vulnerabilidad física (dicotómica)
svymean(~vulnerable, diseno_enusc)
svytable(~vulnerable, diseno_enusc)

# Fuente de información (dicotómica)
svymean(~fuente_personal, diseno_enusc)
svytable(~fuente_personal, diseno_enusc)

# Exposición a drogas (ordinal)
svytable(~PRESENCIA_TRAFICO, diseno_enusc)
prop.table(svytable(~PRESENCIA_TRAFICO, diseno_enusc))
svymean(~PRESENCIA_TRAFICO, diseno_enusc)
sqrt(svyvar(~PRESENCIA_TRAFICO, diseno_enusc))












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

