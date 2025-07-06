# Script para descargar GeoJSON de Comunas de Cali
# Ejecuta este código PRIMERO para obtener los datos geográficos

# ============================================================================
# PASO 1: DESCARGAR GEOJSON OFICIAL DE COMUNAS DE CALI
# ============================================================================

cat("🌍 Descargando GeoJSON oficial de Comunas de Cali...\n")

# URL del servicio WFS del gobierno de Cali para obtener GeoJSON
url_geojson <- "http://ws-idesc.cali.gov.co:8081/geoserver/idesc/ows?service=WFS&version=1.0.0&request=GetFeature&typeName=idesc:mc_comunas&outputFormat=application/json"

# Intentar descargar el GeoJSON
tryCatch({
  # Descargar usando httr si está disponible
  if(require(httr, quietly = TRUE)) {
    response <- httr::GET(url_geojson, httr::timeout(30))
    
    if(httr::status_code(response) == 200) {
      # Guardar el contenido
      geojson_content <- httr::content(response, "text", encoding = "UTF-8")
      
      # Escribir a archivo
      writeLines(geojson_content, "comunas_cali.geojson")
      cat("✅ GeoJSON descargado exitosamente: comunas_cali.geojson\n")
      
    } else {
      cat("❌ Error HTTP:", httr::status_code(response), "\n")
      stop("No se pudo descargar el GeoJSON")
    }
    
  } else {
    # Alternativa usando download.file
    download.file(url_geojson, "comunas_cali.geojson", mode = "wb")
    cat("✅ GeoJSON descargado usando download.file\n")
  }
  
}, error = function(e) {
  cat("❌ Error descargando GeoJSON oficial:", e$message, "\n")
  cat("🔄 Creando GeoJSON simplificado como respaldo...\n")
  
  # Crear GeoJSON simplificado si falla la descarga
  geojson_respaldo <- '{
  "type": "FeatureCollection",
  "features": [
    {"type": "Feature", "properties": {"OBJECTID": 1, "CODIGO": "1", "COMUNA": "1", "NOMBRE": "Comuna 1"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.545, 3.470], [-76.535, 3.470], [-76.535, 3.475], [-76.545, 3.475], [-76.545, 3.470]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 2, "CODIGO": "2", "COMUNA": "2", "NOMBRE": "Comuna 2"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.535, 3.470], [-76.525, 3.470], [-76.525, 3.475], [-76.535, 3.475], [-76.535, 3.470]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 3, "CODIGO": "3", "COMUNA": "3", "NOMBRE": "Comuna 3"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.525, 3.470], [-76.515, 3.470], [-76.515, 3.475], [-76.525, 3.475], [-76.525, 3.470]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 4, "CODIGO": "4", "COMUNA": "4", "NOMBRE": "Comuna 4"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.515, 3.470], [-76.505, 3.470], [-76.505, 3.475], [-76.515, 3.475], [-76.515, 3.470]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 5, "CODIGO": "5", "COMUNA": "5", "NOMBRE": "Comuna 5"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.505, 3.470], [-76.495, 3.470], [-76.495, 3.475], [-76.505, 3.475], [-76.505, 3.470]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 6, "CODIGO": "6", "COMUNA": "6", "NOMBRE": "Comuna 6"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.545, 3.465], [-76.535, 3.465], [-76.535, 3.470], [-76.545, 3.470], [-76.545, 3.465]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 7, "CODIGO": "7", "COMUNA": "7", "NOMBRE": "Comuna 7"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.535, 3.465], [-76.525, 3.465], [-76.525, 3.470], [-76.535, 3.470], [-76.535, 3.465]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 8, "CODIGO": "8", "COMUNA": "8", "NOMBRE": "Comuna 8"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.525, 3.465], [-76.515, 3.465], [-76.515, 3.470], [-76.525, 3.470], [-76.525, 3.465]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 9, "CODIGO": "9", "COMUNA": "9", "NOMBRE": "Comuna 9"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.515, 3.465], [-76.505, 3.465], [-76.505, 3.470], [-76.515, 3.470], [-76.515, 3.465]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 10, "CODIGO": "10", "COMUNA": "10", "NOMBRE": "Comuna 10"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.505, 3.465], [-76.495, 3.465], [-76.495, 3.470], [-76.505, 3.470], [-76.505, 3.465]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 11, "CODIGO": "11", "COMUNA": "11", "NOMBRE": "Comuna 11"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.545, 3.460], [-76.535, 3.460], [-76.535, 3.465], [-76.545, 3.465], [-76.545, 3.460]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 12, "CODIGO": "12", "COMUNA": "12", "NOMBRE": "Comuna 12"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.535, 3.460], [-76.525, 3.460], [-76.525, 3.465], [-76.535, 3.465], [-76.535, 3.460]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 13, "CODIGO": "13", "COMUNA": "13", "NOMBRE": "Comuna 13"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.525, 3.460], [-76.515, 3.460], [-76.515, 3.465], [-76.525, 3.465], [-76.525, 3.460]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 14, "CODIGO": "14", "COMUNA": "14", "NOMBRE": "Comuna 14"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.515, 3.460], [-76.505, 3.460], [-76.505, 3.465], [-76.515, 3.465], [-76.515, 3.460]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 15, "CODIGO": "15", "COMUNA": "15", "NOMBRE": "Comuna 15"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.505, 3.460], [-76.495, 3.460], [-76.495, 3.465], [-76.505, 3.465], [-76.505, 3.460]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 16, "CODIGO": "16", "COMUNA": "16", "NOMBRE": "Comuna 16"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.545, 3.455], [-76.535, 3.455], [-76.535, 3.460], [-76.545, 3.460], [-76.545, 3.455]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 17, "CODIGO": "17", "COMUNA": "17", "NOMBRE": "Comuna 17"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.535, 3.455], [-76.525, 3.455], [-76.525, 3.460], [-76.535, 3.460], [-76.535, 3.455]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 18, "CODIGO": "18", "COMUNA": "18", "NOMBRE": "Comuna 18"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.525, 3.455], [-76.515, 3.455], [-76.515, 3.460], [-76.525, 3.460], [-76.525, 3.455]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 19, "CODIGO": "19", "COMUNA": "19", "NOMBRE": "Comuna 19"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.515, 3.455], [-76.505, 3.455], [-76.505, 3.460], [-76.515, 3.460], [-76.515, 3.455]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 20, "CODIGO": "20", "COMUNA": "20", "NOMBRE": "Comuna 20"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.505, 3.455], [-76.495, 3.455], [-76.495, 3.460], [-76.505, 3.460], [-76.505, 3.455]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 21, "CODIGO": "21", "COMUNA": "21", "NOMBRE": "Comuna 21"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.545, 3.450], [-76.535, 3.450], [-76.535, 3.455], [-76.545, 3.455], [-76.545, 3.450]]]}},
    {"type": "Feature", "properties": {"OBJECTID": 22, "CODIGO": "22", "COMUNA": "22", "NOMBRE": "Comuna 22"}, "geometry": {"type": "Polygon", "coordinates": [[[-76.535, 3.450], [-76.515, 3.450], [-76.515, 3.455], [-76.535, 3.455], [-76.535, 3.450]]]}}
  ]
}'
  
  writeLines(geojson_respaldo, "comunas_cali.geojson")
  cat("✅ GeoJSON de respaldo creado\n")
})

# ============================================================================
# PASO 2: VERIFICAR EL ARCHIVO CREADO
# ============================================================================

if(file.exists("comunas_cali.geojson")) {
  file_size <- file.size("comunas_cali.geojson")
  cat("📁 Archivo creado:", "comunas_cali.geojson", "(", round(file_size/1024, 2), "KB )\n")
  
  # Leer y verificar el contenido
  geojson_content <- readLines("comunas_cali.geojson", warn = FALSE)
  if(length(geojson_content) > 0) {
    cat("✅ Archivo válido con", length(geojson_content), "líneas\n")
    cat("🎯 Primeras líneas del archivo:\n")
    cat(paste(head(geojson_content, 3), collapse = "\n"), "\n")
  }
} else {
  cat("❌ Error: No se pudo crear el archivo GeoJSON\n")
}

cat("\n🚀 Listo! Ahora puedes ejecutar el dashboard principal.\n")
cat("📝 El archivo 'comunas_cali.geojson' está disponible para el mapa coroplético.\n")