# TALLER PRISMA + BIBLIOSHINY | Version para estudiantes | 07-10-2026 | revision 2
# Sirve para cualquier tema. Los conteos se calculan con tus archivos.
#
# ANTES DE LA CLASE (una sola vez, en la consola):
# install.packages(c("rlang", "bibliometrix"), repos = "https://cloud.r-project.org")
# Despues: Session > Restart R.
#
# EN CLASE:
# 1. Abre este archivo en RStudio y pulsa Source (arriba del editor).
# 2. Elige primero Scopus CSV y despues WoS TXT. Conserva los originales.
# 3. Comprueba conteos y revisa la tabla auditoria.
# 4. Si la revision es correcta, ejecuta en consola: confirmar_duplicados()
# 5. Ejecuta: biblioshiny()
# 6. Data > Import or Load > Load bibliometrix > corpus_sin_duplicados.RData.
#
# Si hay un duplicado mal resuelto, consulta al docente antes de confirmar.
# confirmar_duplicados() acepta IDs reales en ids_restaurar e ids_retirar.
# Ejemplo de forma (NO es un ID para copiar): ids_restaurar = c("ID_REAL").
#
# Las funciones principales se usaron en la prueba manual del docente.
# Este archivo completo no se ha ejecutado en el entorno de elaboracion,
# que no dispone de R. El docente debe probarlo con sus exportaciones.
# Documentacion: https://bibliometrix.org/vignettes/Data-Importing-and-Converting.html
# https://stat.ethz.ch/CRAN/web/packages/bibliometrix/refman/bibliometrix.html

# PREPARAR E IMPORTAR
Sys.setenv(VROOM_CONNECTION_SIZE = "10485760")
library(bibliometrix)
if (!"remove.duplicates" %in% names(formals(convert2df))) {
  stop("Actualiza bibliometrix y reinicia R antes de ejecutar este archivo.")
}

message("PRIMER ARCHIVO: selecciona el CSV ORIGINAL de Scopus con todos los metadatos.")
archivo_scopus <- file.choose()
if (tolower(tools::file_ext(archivo_scopus)) != "csv") {
  stop("Scopus requiere la exportacion CSV original. Vuelve a pulsar Source.")
}
S <- convert2df(archivo_scopus, dbsource = "scopus", format = "csv", remove.duplicates = FALSE)
message("Registros importados de Scopus: ", nrow(S))

message("SEGUNDO ARCHIVO: selecciona WoS Core Collection, TEXTO PLANO TXT.")
archivo_wos <- file.choose()
message("Archivo seleccionado para WoS: ", basename(archivo_wos))
# Comprobar el CONTENIDO: una extension distinta no implica otro formato.
# Excel binario (XLS) y archivos ZIP, incluido XLSX, no son WoS Plain Text.
cabecera_wos <- readBin(archivo_wos, what = "raw", n = 8L)
es_excel_binario <- identical(cabecera_wos,
                            as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0xa1, 0xb1, 0x1a, 0xe1)))
es_zip <- length(cabecera_wos) >= 4L &&
  identical(cabecera_wos[1:4], as.raw(c(0x50, 0x4b, 0x03, 0x04)))
if (es_excel_binario || es_zip) {
  stop("El archivo seleccionado es Excel o ZIP. Reexporta WoS en Plain Text, Full Record and Cited References. Cambiar el nombre no convierte el formato.")
}
inicio_wos <- readLines(archivo_wos, n = 200L, warn = FALSE)
if (!any(grepl("^PT ", inicio_wos, useBytes = TRUE))) {
  stop("El contenido no presenta etiquetas PT de WoS Plain Text. Comprueba el archivo elegido y reexporta en Plain Text si hace falta.")
}
if (tolower(tools::file_ext(archivo_wos)) != "txt") {
  message("El nombre no termina en TXT, pero el contenido tiene etiquetas WoS: se importara como texto plano.")
}
W <- convert2df(archivo_wos, dbsource = "wos", format = "plaintext", remove.duplicates = FALSE)
message("Registros importados de WoS: ", nrow(W))
stopifnot(is.data.frame(S), is.data.frame(W), nrow(S) > 0, nrow(W) > 0)
stopifnot(all(c("TI", "AU", "PY", "SO") %in% names(S)),
          all(c("TI", "AU", "PY", "SO") %in% names(W)))

campo <- function(datos, nombre) {
  if (nombre %in% names(datos)) as.character(datos[[nombre]]) else rep(NA_character_, nrow(datos))
}
normalizar_doi <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- sub("^https?://(dx\\.)?doi\\.org/", "", x)
  x <- trimws(sub("^doi\\s*:\\s*", "", x, perl = TRUE))
  x[is.na(x) | x %in% c("", "na", "n/a")] <- NA_character_
  x
}
S$ID_ORIGEN <- sprintf("S%04d", seq_len(nrow(S)))
W$ID_ORIGEN <- sprintf("W%04d", seq_len(nrow(W)))
S$FUENTE_TALLER <- "Scopus"
W$FUENTE_TALLER <- "WoS"
S$DI_ORIGINAL <- campo(S, "DI")
W$DI_ORIGINAL <- campo(W, "DI")
S$DI <- normalizar_doi(S$DI_ORIGINAL)
W$DI <- normalizar_doi(W$DI_ORIGINAL)

# INTEGRAR Y PREPARAR LA REVISION HUMANA
M_todas <- mergeDbSources(S, W, remove.duplicated = FALSE, verbose = FALSE)
M_auto <- mergeDbSources(S, W, remove.duplicated = TRUE, verbose = TRUE)
stopifnot(all(c("ID_ORIGEN", "FUENTE_TALLER") %in% names(M_todas)),
          all(c("ID_ORIGEN", "FUENTE_TALLER") %in% names(M_auto)),
          !anyNA(M_todas$ID_ORIGEN), !anyNA(M_auto$ID_ORIGEN))
stopifnot(nrow(M_todas) == nrow(S) + nrow(W), !anyDuplicated(M_todas$ID_ORIGEN),
          nrow(M_auto) > 0, all(M_auto$ID_ORIGEN %in% M_todas$ID_ORIGEN))
campos <- c("ID_ORIGEN", "FUENTE_TALLER", "TI", "AU", "PY", "SO", "DI_ORIGINAL", "DI", "AB")
auditoria <- as.data.frame(setNames(lapply(campos, function(x) campo(M_todas, x)), campos))
auditoria$DECISION_AUTOMATICA <- ifelse(
  auditoria$ID_ORIGEN %in% M_auto$ID_ORIGEN, "Conservar", "Retirar")
auditoria <- auditoria[order(auditoria$TI, auditoria$PY), ]
conteos_automaticos <- data.frame(
  Etapa = c("Scopus", "WoS", "Total identificado", "Retirados automaticamente", "Restantes provisionales"),
  Registros = c(nrow(S), nrow(W), nrow(M_todas), nrow(M_todas) - nrow(M_auto), nrow(M_auto)))
print(conteos_automaticos, row.names = FALSE)
carpeta_salida <- tempfile("Taller_PRISMA_", tmpdir = dirname(archivo_scopus))
if (!dir.create(carpeta_salida)) stop("No se pudo crear la carpeta de salida junto al CSV de Scopus.")
write.csv2(auditoria, file.path(carpeta_salida, "auditoria_automatica.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
save(S, W, M_todas, M_auto, auditoria, archivo_scopus, archivo_wos,
     file = file.path(carpeta_salida, "respaldo_importaciones.RData"))

# Esta funcion guarda las salidas SOLO cuando confirmas la revision de duplicados.
# Una nueva confirmacion crea otra carpeta y conserva las salidas anteriores.
confirmar_duplicados <- function(ids_restaurar = character(0), ids_retirar = character(0)) {
  ids_restaurar <- unique(ids_restaurar)
  ids_retirar <- unique(ids_retirar)
  if (!all(ids_restaurar %in% setdiff(M_todas$ID_ORIGEN, M_auto$ID_ORIGEN)) ||
      !all(ids_retirar %in% M_auto$ID_ORIGEN)) stop("Comprueba los ID_ORIGEN que deseas corregir.")
  ids <- union(setdiff(M_auto$ID_ORIGEN, ids_retirar), ids_restaurar)
  nueva_base <- M_todas[M_todas$ID_ORIGEN %in% ids, , drop = FALSE]
  if (nrow(nueva_base) == 0) stop("La base revisada no puede quedar vacia.")
  doi <- normalizar_doi(campo(nueva_base, "DI"))
  if (anyDuplicated(doi[!is.na(doi)])) stop("Quedan DOI repetidos. Revisa las correcciones antes de guardar.")
  class(nueva_base) <- unique(c("bibliometrixDB", class(nueva_base)))
  M <<- nueva_base
  M_identificado <<- nueva_base
  carpeta_base <<- tempfile("Base_revisada_", tmpdir = carpeta_salida)
  if (!dir.create(carpeta_base)) stop("No se pudo crear la carpeta de la base revisada.")
  auditoria_final <- auditoria
  auditoria_final$DECISION_VERIFICADA <- ifelse(
    auditoria_final$ID_ORIGEN %in% M$ID_ORIGEN, "Conservar", "Retirar_duplicado")
  conteos <- data.frame(
    Etapa = c("Scopus", "WoS", "Total identificado", "Duplicados eliminados tras revision", "Registros antes del cribado"),
    Registros = c(nrow(S), nrow(W), nrow(M_todas), nrow(M_todas) - nrow(M), nrow(M)))
  columnas <- c("ID_ORIGEN", "FUENTE_TALLER", "TI", "AU", "PY", "SO", "DI", "AB")
  cribado <- as.data.frame(setNames(lapply(columnas, function(x) campo(M, x)), columnas))
  cribado$DECISION_TITULO_RESUMEN <- ""
  cribado$MOTIVO_TITULO_RESUMEN <- ""
  cribado$RECUPERACION_TEXTO <- ""
  cribado$DECISION_TEXTO_COMPLETO <- ""
  cribado$MOTIVO_EXCLUSION_TEXTO_COMPLETO <- ""
  cribado$REVISOR <- ""
  cribado$FECHA <- ""
  cribado$OBSERVACIONES <- ""
  guardar_base <- function(M, nombre) save(M, file = file.path(carpeta_base, nombre))
  guardar_base(M, "corpus_sin_duplicados.RData")
  guardar_base(S, "Scopus_separada.RData")
  guardar_base(W, "WoS_separada.RData")
  write.csv2(auditoria_final, file.path(carpeta_base, "auditoria_duplicados_revisados.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
  write.csv2(conteos, file.path(carpeta_base, "conteos_PRISMA_identificacion.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
  write.csv2(cribado, file.path(carpeta_base, "registro_cribado.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
  save(S, W, M_todas, M_auto, M, M_identificado, auditoria_final, conteos, cribado,
       archivo_scopus, archivo_wos, file = file.path(carpeta_base, "respaldo_taller.RData"))
  writeLines(capture.output(sessionInfo()), file.path(carpeta_base, "versiones_R_y_paquetes.txt"))
  print(conteos, row.names = FALSE)
  message("Archivos guardados en: ", normalizePath(carpeta_base))
  message("Biblioshiny: Load bibliometrix > corpus_sin_duplicados.RData.")
  message("Completa registro_cribado.csv para decidir pertinencia y evaluar textos completos.")
  if ("CR_raw" %in% names(M)) {
    message("Para cocitacion, acoplamiento y citas locales usa una base separada, con tus criterios de inclusion.")
  }
  invisible(M)
}

message("Comprueba los conteos frente a tus exportaciones y revisa auditoria.")
message("Cuando termines la revision, ejecuta en consola: confirmar_duplicados()")
message("Despues ejecuta: biblioshiny()")
if (interactive()) View(auditoria)
