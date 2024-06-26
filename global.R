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
#nnn

archivos <- file.info("data/rnve.csv")

fecha_creacion <- archivos$mtime

fecha_creacion <- format(fecha_creacion, "%Y-%m-%d")

fecha_creacion <- fecha_creacion[complete.cases(fecha_creacion)]

Sys.Date() > na.omit(fecha_creacion)

if (Sys.Date() > na.omit(fecha_creacion)) {
  source("preprocesamiento.R")
}

registro_civil <- import("data/registro_civil.csv")
rnve <- import("data/rnve.csv")

#source("intersection_shp_BD_GEO_velero.R")
source("GEO_velero.R")

# CÁLCULOS ----------------------------------------------------------------

registro_civil <- registro_civil %>%
  filter(year(fecha_nac) <= year(today()) - 1)
rnve <- rnve %>% filter(fecha_vac <= today())

listado_anos <- rnve %>%
  mutate(ano = lubridate::year(fecha_vac)) %>%
  pull(ano)

dosis <- rnve %>% 
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  group_by(ano, dosis) %>% 
  tally(name = "total_dosis")
head(dosis)

pop_LT1_rn <- registro_civil %>% 
mutate(ano = lubridate::year(fecha_nac)) %>% 
group_by(ano) %>% 
tally()

pop_LT1_rn <- pop_LT1_rn %>%
mutate(ano = ano + 1)
cobertura <- dosis %>%
left_join(., pop_LT1_rn, by = c("ano")) %>% 
mutate(cobertura = total_dosis / n  * 100)

rc <- registro_civil %>% 
select(ID, nombre, apellido, fecha_nac)

rnve_2 <- rnve %>% 
  select(ID, nombre, apellido, fecha_nac, dosis, fecha_vac)

rc_rnve <- rc %>% 
left_join(rnve_2, by = c("ID", "nombre", "apellido", "fecha_nac")) %>%
pivot_wider(
id_cols = c(ID, nombre, apellido, fecha_nac),
names_from = "dosis",
values_from = "fecha_vac") %>% 
select(-`NA`) %>% 
mutate(Completo = if_all(c(5,7), ~ !is.na(.x))) %>% 
mutate(ano_nac = year(fecha_nac)) %>% 
group_by(ano_nac, Completo)%>% 
tally() %>% 
pivot_wider(id_cols = c(ano_nac),
            names_from = "Completo",
            values_from = "n") %>% 
  rename(si = `TRUE`,
         no = `FALSE`) %>% 
  mutate(cobertura_esq_compelto = (si/(si+no))*100)

dosis_mensual <- rnve %>% 
  mutate(ano = year(fecha_vac), mes = month(fecha_vac)) %>% 
  group_by(ano, mes, dosis) %>% 
  tally(name = "n_dosis") %>% 
  mutate(fecha = lubridate::ym(
    stringr::str_glue("{ano}-{mes}")
  ))


dosis_semanal <- rnve %>% 
mutate(ano = year(fecha_vac), semana_epi = lubridate::epiweek(fecha_vac)) %>% 
group_by(ano, semana_epi, dosis) %>% 
tally(name = "n_dosis") %>% 
mutate(fecha = aweek::get_date(
    week = semana_epi, year = ano, day = 1, start = "Monday"))

ggplot(
  dosis_mensual,
  aes(x = fecha, y = n_dosis, fill = dosis)
) +
  geom_bar(stat = "identity", position = "stack")


### Semanal --------------------------------------------------------------------
ggplot(
  dosis_semanal,
  aes(x = fecha, y = n_dosis, fill = dosis)
) +
  geom_bar(stat = "identity", position = "stack")


cobertura_mensual_1 <- dosis_mensual %>% 
  left_join(pop_LT1_rn, by = "ano") %>% 
  group_by(ano, mes, dosis) %>% 
  mutate(cobertura_mes = round(n_dosis / n * 100, 2))

cobertura_mensual <- cobertura_mensual_1 %>% 
  group_by(ano, dosis) %>% 
  mutate(cobertura = cumsum(cobertura_mes))

para_graficar <- cobertura_mensual %>%
  filter(dosis != "Campaña") %>% 
  filter(ano <= 2023)


ggplot(
  para_graficar,
  aes(x = fecha, fill = dosis)
) +
  labs(y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
  geom_bar(aes(y = n_dosis), stat = "identity", position = "stack") +
  geom_line(aes(y = cobertura * 100, linetype = dosis), linewidth = 1) +
  scale_y_continuous(
    limits = c(0, 10e3),
    sec.axis = sec_axis( trans= ~./100, name = "Cobertura (%)")
  ) +
  facet_wrap(~ ano, scales = "free_x") +
  theme_classic() +
  theme(text = element_text(size = 16))

cobertura_acumulada_mensual <- cobertura_mensual %>% 
  select(ano, mes, fecha, dosis, cobertura)

prueba_1 <- cobertura_acumulada_mensual %>% 
  group_by(ano, dosis) %>% 
  arrange(mes) %>% 
  mutate(cobertura_mes = cobertura - lag(cobertura))

prueba_2 <- cobertura_acumulada_mensual %>% 
  group_by(ano, dosis) %>% 
  arrange(mes) %>% 
  mutate(cobertura_mes = case_when(
    is.na(lag(cobertura)) ~ cobertura,
    TRUE ~ cobertura - lag(cobertura)))

vacunados_rnve <- rnve %>% 
  filter(dosis == "Primera") %>% 
  mutate(ano = lubridate::year(fecha_vac)) %>% 
  group_by(ano, municipio_res_mad) %>% 
  tally(name = "vacunados_primera")
head(vacunados_rnve)

pop_municipio <- registro_civil %>% 
  mutate(ano = lubridate::year(fecha_nac)) %>% 
  group_by(ano, municipio_res_mad) %>% 
  tally(name = "poblacion") %>% 
  mutate(ano = ano + 1)

cobertura_municipio <- vacunados_rnve %>% 
  left_join(., pop_municipio, by = c("ano", "municipio_res_mad")) %>% 
  mutate(cobertura = vacunados_primera / poblacion  * 100)


susceptibles_municipio <- cobertura_municipio %>%
  mutate(falla_primaria = 0.05) %>% 
  mutate(no_vacunados = poblacion - vacunados_primera) %>% 
  mutate(susceptibles = no_vacunados + (vacunados_primera * falla_primaria))
head(susceptibles_municipio)

susceptibles <- cobertura %>% 
  filter(ano <= 2023) %>% 
  filter(dosis == "Primera") %>% 
  mutate(falla_primaria = 0.05) %>% 
  mutate(no_vacunados = n - total_dosis) %>% 
  mutate(susceptibles = no_vacunados + (total_dosis * falla_primaria)) %>% 
  ungroup() %>% 
  arrange(ano) %>% 
  mutate(susceptibles_acumulado = cumsum(susceptibles))


ggplot(susceptibles, aes(x = ano)) +
  labs(
    title = "Susceptibles acumulados en los últimos 5 años",
    x = "Año",
    y = "Susceptibles Acumulados"
  ) +
  geom_bar(aes(y = cobertura * 400), position = "dodge", stat = "identity", fill = "#094775") +
  geom_line(aes(y = susceptibles_acumulado), colour = "#ff671f", linewidth = 1) +
  scale_y_continuous(
    limits = c(0, 40e3),
    sec.axis = sec_axis( trans= ~./400, name = "Cobertura (%)")) +
  scale_x_continuous(breaks = seq(2018, 2023, 1)) +
  theme_classic() +
  theme(text = element_text(size = 16))

fecha_campana <- as.Date("2024-03-04", "%Y-%m-%d")
fecha_campana
fecha_edad_minima <- fecha_campana %m-% months(13)
fecha_edad_minima
fecha_edad_maxima <- fecha_campana %m-% months(12 * 5) - 1
fecha_edad_maxima

pop_campana_adm1 <- registro_civil %>% 
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  group_by(departamento_res_mad) %>% 
  tally(name = "poblacion")

campana_departamento <- rnve %>% 
  filter(dosis == "Campaña") %>% 
  tidyr::separate(
    municipio_res_mad,
    c("municipio", "departamento_res_mad"),
    sep = "-"
  ) %>% 
  mutate(departamento_res_mad = trimws(departamento_res_mad)) %>% 
  group_by(fecha_vac, departamento_res_mad) %>% 
  summarise(
    vacunados = n()
  ) %>%
  left_join(., pop_campana_adm1, by = "departamento_res_mad") %>% 
  mutate(cobertura = vacunados / poblacion * 100) %>% 
  group_by(departamento_res_mad) %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
head(campana_departamento)

pop_campana_nacional <- sum(pop_campana_adm1$poblacion)
pop_campana_nacional

campana_nacional <- rnve %>% 
  filter(dosis == "Campaña") %>% 
  group_by(fecha_vac) %>% 
  summarise(
    vacunados = n()
  ) %>%
  mutate(cobertura = vacunados / pop_campana_nacional * 100) %>% 
  ungroup %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
head(campana_nacional)

ggplot(
  campana_nacional,
  aes(x = fecha_vac)
) +
  labs(x = "Fecha", y = "Dosis", fill = "Dosis", linetype = "Cobertura") +
  geom_bar(aes(y = vacunados), stat = "identity", position = "stack") +
  geom_line(aes(y = cobertura_acumulada * 50), linewidth = 1) +
  scale_y_continuous(
    limits = c(0, 7500),
    sec.axis = sec_axis( trans= ~./50, name = "Cobertura (%)")
  ) +
  theme_classic() +
  theme(text = element_text(size = 16))

#  Cálculo de coberturas avance por departamento ####
fecha_campana <- as.Date("2024-03-04", "%Y-%m-%d") #se le da el formato
fecha_campana

fecha_edad_minima <- fecha_campana %m-% months(12) # %m-%: es una función de lubridate, para restar fechas en meses
fecha_edad_minima

fecha_edad_maxima <- fecha_campana %m-% months(12 * 5) - 1 # cuando resto días puedo dejar el valor directo que en este caso seria el -1
fecha_edad_maxima

pop_campana_adm1 <- registro_civil %>% 
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  group_by(departamento_res_mad) %>% 
  tally(name = "poblacion")
head(pop_campana_adm1)

campana_departamento <- rnve %>% 
  filter(dosis == "Campaña") %>%
  tidyr::separate(
    municipio_res_mad,
    c("municipio", "departamento_res_mad"),
    sep = "-") %>%
  mutate(departamento_res_mad = trimws(departamento_res_mad)) %>%
  group_by(fecha_vac, departamento_res_mad) %>% 
  summarise(
    vacunados = n()
  ) %>%
  mutate(cobertura = vacunados / pop_campana_adm1$poblacion * 100) %>% 
    group_by(departamento_res_mad) %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
head(campana_departamento)

# Calculo cobertura de avance Nacional####
pop_campana_nacional <- registro_civil%>% 
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  tally(name = "poblacion") %>% 
  pull(poblacion)
pop_campana_nacional

campana_nacional <- rnve %>% 
  filter(dosis == "Campaña") %>% 
  group_by(fecha_vac) %>% 
  summarise(
  vacunados = n()
  ) %>%
  mutate(cobertura = vacunados / pop_campana_nacional * 100) %>% 
  ungroup %>% 
  arrange(fecha_vac) %>% 
  mutate(cobertura_acumulada = cumsum(cobertura))
head(campana_nacional)

# GRÁFICO AVANCE NACIONAL

ggplot(
  campana_nacional,
  aes(x = fecha_vac)
) +
  # Nombramos los ejes y la leyenda
  labs(x = "Fecha", y = "Dosis", fill = "Dosis", linetype = "Cobertura", title = "Avance de cobertura a nivel nacional") +
  # Mostramos el numero de dosis aplicadas cada mes
  geom_bar(aes(y = vacunados, fill = "Dosis"), stat = "identity", position = "stack") +
  scale_fill_manual(values = "red") +
  # Mostramos la cobertura acumulada para cada mes.
  # NOTA: Los datos de n_dosis alcanzan aprox 7,500, mientras que las 
  #       coberturas son entre 0 y 100. Por lo tanto, se multiplica el valor de 
  #       coberturas por 75 para igualar los dos ejes.
  geom_line(aes(y = cobertura_acumulada * 75), linewidth = 1) +
  # Modificamos el eje vertical
  scale_y_continuous(
    # Ajustamos los limites entre 0 y 1,000 dosis
    limits = c(0, 7500),
    # Agregamos un segundo eje horizontal
    # NOTA: Aplicamos un factor de conversión de 75 para que el eje de cobertura
    #       alcance 100% cuando el número de dosis alcance 7,500 dosis.
    sec.axis = sec_axis( trans= ~./75, name = "Cobertura (%)")
  ) +
  # Mejoramos la visualización
  theme_classic() +
  theme(text = element_text(size = 16))

#CAPA DE PUNTOS ####

rc1 <- registro_civil %>% 
  select(ID, nombre, apellido, sexo, fecha_nac, longitude, latitude, departamento_res_mad, municipio_res_mad, cod_municipio, edad_madre)

rnve_3 <- rnve %>% 
  select(ID, nombre, apellido, sexo, fecha_nac, longitude, latitude, municipio_res_mad, pais_res_mad, edad_madre, dosis, fecha_vac)


rc1_rnve3 <- rc1 %>% 
  left_join(rnve_3, by = c("ID", "nombre", "apellido", "fecha_nac", "longitude", "latitude", "municipio_res_mad")) %>%
  pivot_wider(id_cols = c(ID, nombre, apellido, fecha_nac, longitude, latitude, municipio_res_mad,departamento_res_mad),
              names_from = "dosis",
              values_from = "fecha_vac") %>% 
  select(-`NA`) %>% 
  mutate(Completo = if_all(7:8, ~ !is.na(.x))) %>% 
  mutate(ano_nac = year(fecha_nac))

fecha_edad_minima <- fecha_campana %m-% months(12)
fecha_edad_maxima <- fecha_campana %m-% months(12 * 5) - 1 

rc1_rnv_nuevo <-rc1_rnve3 %>% 
  filter(is.na(Campaña)) %>% 
  filter(fecha_nac <= fecha_edad_minima) %>% 
  filter(fecha_nac >= fecha_edad_maxima) %>% 
  select(fecha_nac, longitude,latitude, municipio_res_mad, departamento_res_mad)

rc1_rnv_nuevo <- rc1_rnv_nuevo %>% 
  mutate(edad = round(as.numeric(Sys.Date() -rc1_rnv_nuevo$fecha_nac)/365,0))

shp <- st_read("data/Anterior_URYMixed/URY_ADM2_Anterior.shp")

puntos1 <- rc1_rnv_nuevo

# Crear objetos sf transformando la informacion de latitud (y) y longitud (x)
puntos_sf1 <- st_as_sf(puntos1, # base 
                       coords = c("longitude", "latitude"), # Columnas 
                       crs = st_crs(shp))



