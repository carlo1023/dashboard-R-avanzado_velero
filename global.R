# global.R ---------------------------------------------------------------------
# Description: Este script carga toda la información y paquetes necesarios
# para ejecutar el dashboard.
# Created by -------------------------------------------------------------------
# Name: CIM Data Team
# Created on: 2024-01-29
# Editorial --------------------------------------------------------------------
# Section for editorial changes or notes
# ______________________________________________________________________________

# Requerimientos ---------------------------------------------------------------
source("requirements.R")

if (!file.exists("data/registro_civil.csv")) {
  source("preprocesamiento.R")
}

registro_civil <- import("data/registro_civil.csv")
rnve <- import("data/RNVE.csv")


# CÁLCULOS ----------------------------------------------------------------

reg_civil <- registro_civil %>% 
  filter(year(fecha_nac) < year(today())) 
rnve1 <- rnve %>% 
  filter(fecha_vac <= today())

listado_anos <- rnve1 %>%
  mutate(ano = lubridate::year(fecha_vac)) %>%
  pull(ano)

#table(listado_anos)

dosis <- rnve1 %>% 
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  group_by(ano, dosis) %>% 
  tally(name = "total_dosis")

pop_LT1_rn <- reg_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano) %>% 
  tally()


pop_LT1_rn <- pop_LT1_rn %>%
  mutate(ano = ano + 1)

cobertura <- dosis %>% 
  left_join(., pop_LT1_rn, by = c("ano")) %>% 
  mutate(cobertura = total_dosis / n  * 100)

vacunados_rnve <- rnve %>% 
  filter(dosis == "Primera") %>% 
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  group_by(ano, municipio_res_mad) %>% 
  tally(name = "vacunados_primera")

#head(vacunados_rnve)

pop_municipio <- registro_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano, municipio_res_mad) %>% 
  tally(name = "poblacion") %>% 
  mutate(ano = ano + 1)

#head(pop_municipio)


cobertura_municipio <- vacunados_rnve %>% 
  left_join(., pop_municipio, by = c("ano", "municipio_res_mad")) %>% 
  mutate(cobertura = vacunados_primera / poblacion  * 100)

#head(cobertura_municipio)

susceptibles_municipio <- cobertura_municipio %>%
  mutate(falla_primaria = 0.05) %>% 
  mutate(no_vacunados = poblacion - vacunados_primera) %>% 
  mutate(susceptibles = ceiling(no_vacunados + (vacunados_primera * falla_primaria)))

#head(susceptibles_municipio)
