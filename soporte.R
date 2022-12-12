# Librerías ----
pacman::p_load(tidyverse, funModeling, googlesheets4, gargle, gt, 
               extrafont, scales, ggalt, kableExtra, wordcloud, networkD3,
               data.table, ggeconodist)

# Datos --------

## 2022 ----

# Encuesta
kiwi22 <- read_sheet("1BiP8KPGOZzg_BRMJzrvk5tn81_YoIhWMZpagX0OYNmQ") %>% 
  janitor::clean_names()


# Tipo de cambio 2022
tc <- read_sheet("1S1q2VT6yRLQl7Y-jvVHoy3A9yYh9FQQAH2YPfAV14OU") %>% 
  select(pais, tipo_cambio)



# Configuraciones generales ----

options(scipen = 999)   # Modifica la visualización de los ejes numérico a valores nominales

loadfonts(quiet = TRUE) # Permite cargar en R otros tipos de fuentes.

# Estilo limpio sin líneas de fondo
estilo <- theme(panel.grid = element_blank(),
                plot.background = element_rect(fill = "#FBFCFC"),
                panel.background = element_blank(),
                text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia verticales en gris claro
estilov <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.x = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

# Estilo limpio con líneas de referencia horizontales en gris claro
estiloh <- theme(panel.grid = element_blank(),
                 plot.background = element_rect(fill = "#FBFCFC"),
                 panel.background = element_blank(),
                 panel.grid.major.y = element_line(color = "#AEB6BF"),
                 text = element_text(family = "Roboto"))

genero <- c("#8624F5", "#1FC3AA", "#FFD129", "#75838F") #Violeta - Verde - Amarillo - Gris
genero3 <- c("#8624F5","#FFD129", "#1FC3AA")

colores <-  c("#8624F5", "#1FC3AA")

azul <- "#344D7E"
verde <-  "#4A9FC7"
rosa1 <- "#B95192"
rosa2 <- "#EE5777"
naranja <- "#FF764C"
amarillo <- "#FFA600"
gris <- "#75838F"
lila <- "#755395"
rojo <- "#943126"

col4 <- c(azul, lila, rosa1, rosa2)
col5 <- c(azul, lila, rosa1, rosa2, naranja)
col6 <- c(azul, lila, rosa1, rosa2, naranja, amarillo)

# Creo un objeto con un texto que se va a repetir mucho a lo largo del análisis
fuente <- "Fuente: Encuesta KIWI de Sueldos de RRHH para Latam 2022"

# Creo objetos para formatear las etiquetas numéricas de los ejes x e y
eje_x_n <- scale_x_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))

eje_y_n <- scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))


# Limpieza de datos 2022 ----

# Añade columna de id
kiwi22$id <- rep(1:nrow(kiwi22))

# Eliminar preguntas
kiwi22 <- kiwi22 %>% 
  select(id, everything(),
         -queres_contestar_mas_preguntas_30,
         -queres_contestar_mas_preguntas_45,
         -queres_contestar_mas_preguntas_56,
         -comentarios)

# Renombrar columnas
kiwi22 <- kiwi22 %>% 
  rename(fecha = marca_temporal,
         genero = identidad_de_genero,
         nivel_formacion = maximo_nivel_de_formacion,
         carrera_grado = que_carrera_de_grado_estudiaste,
         tipo_universidad = en_que_tipo_de_universidad_estudiaste_tu_carrera_de_grado,
         pais = pais_en_el_que_trabajas,
         provincia = provincia_donde_trabajas,
         rubro = rubro_de_la_empresa,
         dotacion = cuantos_empleados_tiene_la_empresa,
         origen_capital = origen_del_capital,
         dotacion_rh = cuantas_personas_integran_el_area_de_rrhh,
         puesto = en_que_puesto_trabajas,
         tipo_contratacion = tipo_de_contratacion,
         jornada = como_es_tu_jornada_laboral,
         funcion = cual_es_tu_funcion_principal_en_rrhh,
         personas_a_cargo = cuantas_personas_tenes_a_cargo_pone_0_si_no_tenes_gente_a_cargo,
         anios_empresa = hace_cuantos_anos_trabajas_en_la_empresa_donde_estas_0_para_menos_de_un_ano,
         anios_puesto = hace_cuantos_anos_estas_en_tu_puesto_actual_0_para_menos_de_un_ano,
         anios_rh = cuantos_anos_de_experiencia_tenes_en_rrhh,
         sueldo_bruto = cual_es_tu_remuneracion_bruta_mensual_en_tu_moneda_local_antes_de_impuestos_y_deducciones,
         beneficios = que_beneficios_tenes,
         bono = recibis_bonos,
         bono_frecuencia = frecuencia_del_bono,
         ajuste = tuviste_ajustes_por_inflacion_en_2022,
         ajuste_porcentaje = cual_fue_el_porcentaje_de_aumento_acumulado_que_tuviste_en_2022,
         ajuste_mes = mes_del_ultimo_ajuste,
         otros_proyectos = trabajas_en_proyectos_independientes_ademas_de_tu_empleo,
         partidos_oficina = van_a_ver_los_partidos_de_la_copa_del_mundo_de_futbol_en_la_oficina,
         agenda = se_permite_bloquear_la_agenda_durante_los_partidos_de_tu_seleccion_en_el_mundial,
         idioma_exigencia = te_exigieron_saber_un_idioma_extranjero_ingles_portugues_etc_para_entrar_a_trabajar_en_tu_empresa,
         idioma_porcentaje = que_porcentaje_del_tiempo_usas_el_idioma_extranjero_en_tu_puesto_actual,
         satisfaccion = que_tan_satisfecho_estas_con_tu_empresa,
         busqueda = estas_buscando_trabajo,
         desarrollo = que_tan_de_acuerdo_estas_con_la_siguiente_frase_tengo_un_plan_de_desarrollo_claro_dentro_de_la_organizacion,
         anecdota = cual_es_la_anecdota_mas_graciosa_o_bizarra_que_te_haya_sucedido_trabajando_en_rrhh,
         influencia_rh = en_tu_empresa_sentis_que_rrhh_tiene_un_rol_importante_dentro_de_la_toma_de_decisiones_de_la_empresa,
         colegio = recursos_humanos_deberia_tener_un_colegio_profesional_como_abogacia_por_ejemplo,
         colegio_fundamento = fundamenta_tu_respuesta_a_la_pregunta_anterior,
         comunidad = participas_en_alguna_comunidad_de_rrhh,
         comunidades = en_cuales,
         tarea_odiosa = cual_es_la_tarea_que_mas_odias_trabajando_en_rrhh_actual_o_pasada,
         diversidad_sexual = te_identificas_como_lgbtiq_lesbiana_gay_bisexual_transexual_otra_minoria_sexual,
         discapacidad = tenes_alguna_discapacidad,
         libertad_ser = en_tu_empresa_puedes_ser_como_realmente_eres_por_ej_expresar_abiertamente_tu_personalidad_tu_identidad_de_genero_orientacion_sexual_etc,
         diversidad_management = que_porcentaje_aproximado_del_management_de_tu_empresa_son_mujeres_entiendase_posiciones_de_jefatura_de_gerencia_o_de_direccion,
         contrata_senior = en_lo_que_va_del_ano_han_contratado_en_tu_empresa_a_personas_mayores_de_50_anos,
         contrata_discapacidad = en_lo_que_va_del_ano_han_contratado_en_tu_empresa_a_personas_con_discapacidad,
         lenguaje_inclusivo = es_importante_incorporar_el_lenguaje_inclusivo_en_la_organizacion,
         linea_segura = en_tu_organizacion_existe_una_linea_segura_o_politicas_definidas_para_actuar_frente_a_situaciones_de_acoso_o_discriminacion,
         sufrio_acoso = sufriste_alguna_situacion_de_acoso_abuso_o_de_discriminacion_en_algun_trabajo,
         machismo = sentis_que_tu_entorno_laboral_es_machista,
         modalidad = en_que_modalidad_estas_trabajando_actualmente,
         dias_teletrabajo = cantidad_de_dias_de_teletrabajo,
         satisfaccion_retorno = que_tan_satisfecho_estas_con_la_decision_de_retornar_a_la_oficina,
         estres = del_1_al_10_que_tan_estresado_o_estresada_te_sentis_1_poco_estres_10_mucho_estres,
         comparacion = en_comparacion_con_el_ano_pasado_como_te_sentis,
         motivo = cual_es_el_principal_motivo_por_el_que_sentis_estres_en_el_trabajo,
         registro_fiscal = como_estas_registrado_a_fiscalmente,
         anios_freelance = hace_cuantos_anos_trabajas_como_freelance_0_para_menos_de_un_ano,
         exporta = exportas_tus_servicios,
         medio_pago_exterior = si_exportas_servicios_a_traves_de_que_medios_de_pago_recibis_los_pagos_del_exterior,
         cuotas = aceptas_pagos_en_cuotas,
         colaboracion_freelance = trabajas_con_otros_freelancers_de_tu_mismo_rubro,
         servicio_busqueda = tu_servicio_principal_esta_relacionado_con_busqueda_y_seleccion,
         busqueda_it = te_dedicas_principalmente_a_realizar_busquedas_de_it_tecnologia,
         entrevista_ingles = haces_entrevistas_en_ingles,
         coeficiente = cual_es_el_coeficiente_que_cobras_por_tus_servicios,
         base_coeficiente = el_coeficiente_lo_calculas_sobre,
         garantia = ofreces_garantia,
         servicio_principal = cual_es_el_servicio_principal_que_brindas_si_brindas_mas_de_un_servicio_elegi_el_que_mas_ingresos_genere,
         negociacion = te_cuesta_mucho_definir_el_precio_con_nuevos_clientes,
         valor_hora = cual_es_el_valor_hora_promedio_que_ofreces_moneda_local)

# Base de Empleados en relación de dependencia
rh22 <- kiwi22 %>% 
  filter(trabajo == "Relación de Dependencia") %>% 
  filter(!funcion %in% c("-", "CPO", "No estoy en RRHH", "CPO"),
         !is.na(funcion)) %>% 
  select(id:motivo)

# Limpiar columnas en formato lista 
rh22 <- rh22 %>%  
  mutate(dotacion_rh = as.numeric(unlist(dotacion_rh)),
         anios_empresa = as.numeric(unlist(anios_empresa)),
         anios_rh = as.numeric(unlist(anios_rh)),
         ajuste_porcentaje = as.numeric(unlist(ajuste_porcentaje))) 

## Modificaciones manuales
rh22$anecdota[[19]] <- "0"
rh22$anecdota[[174]] <- "0"
rh22 <- unnest(data = rh22, cols = anecdota, keep_empty = TRUE)

rh22$comunidades[[19]] <- "0"

rh22 <- unnest(data = rh22, cols = comunidades, keep_empty = T)

## Añade datos de tipo de cambio a dolar ----
rh22 <- rh22 %>% 
  left_join(tc, by ="pais") %>%
  mutate(multiplicador = if_else(tipo_contratacion == "Part time", 1.5, 1),
         sueldo_ft = sueldo_bruto * multiplicador,    # Hace la equivalencia de un sueldo part time a full time
         sueldo_dolar = sueldo_ft/tipo_cambio,  # Convierto los sueldos a dólares
         cuenta = 1)

# Guarda csv rh22
write_delim(rh22, file = "data/rh_2022.csv",
            delim = ";")

# Creación de dataframe Freelancers -----

freelo22 <- kiwi22 %>% 
  filter(trabajo == "Freelance")

# Eliminar columnas vacias
freelo22 <- freelo22 %>% 
  select(where (~ !all(is.na(.x))), # Elimina columnas con valores NA
         -trabajo, 
         -dotacion_rh,
         -anios_empresa,
         -anios_rh,
         -ajuste_porcentaje,
         -anecdota,
         -comunidades) 

# Convertir las columnas tipo lista
freelo22 <- freelo22 %>% 
  mutate(anios_freelance = as.numeric(unlist(anios_freelance)))

freelo22$coeficiente[[17]] <- as.numeric(freelo22$coeficiente[[17]])
freelo22$coeficiente[[18]] <- as.numeric(freelo22$coeficiente[[18]])
freelo22$coeficiente[[21]] <- as.numeric(freelo22$coeficiente[[21]])

freelo22 <- unnest(data = freelo22, cols = coeficiente, keep_empty = TRUE)

# Guardar los datos en un archivo
write_delim(freelo22, file = "data/freelancers_2022.csv", delim = ";")

# Guardar archivo original
kiwi22$anecdota[[20]] <- as.character(kiwi22$anecdota[[20]])
kiwi22$anecdota[[191]] <- as.character(kiwi22$anecdota[[191]])

kiwi22 <- unnest(data = kiwi22, cols = anecdota, keep_empty = TRUE)

kiwi22 %>% select(id, anecdota) %>% filter(!is.na(anecdota)) %>% print(n = Inf)

# Borrar anecdotas polémicas 
kiwi22$anecdota[63] <- NA
kiwi22$anecdota[217] <- NA

write_delim(kiwi22, file = "data/kiwi_2022.csv", delim = ";")
