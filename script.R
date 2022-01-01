get_data <- function(url) {
  a <- httr::GET(url)
  text <- httr::content(a, as = "text", encoding = "UTF-8")
  
  ct <- V8::v8()
  ct$eval(text)
  data <- ct$get("data")
  
  dir.create(file.path("data", data[["empresa"]]), showWarnings = FALSE, recursive = TRUE)
  
  date <- as.POSIXct(paste0(Sys.Date(), " ", data$ultimaActualizacion))
  if (Sys.time() - date < 0) {
    date <- as.POSIXct(paste0(Sys.Date() - 1, " ", data$ultimaActualizacion))
  }
  
  hash <- digest::digest(data)
  last_hash <- data$empresa
  if (file.exists(last_hash)) {
    last_hash <- readLines(last_hash)
  }
  
  if (hash != last_hash) {
    file <- file.path("data", data[["empresa"]], paste0(format(date, "%Y-%M-%dT%H:%M:%S"), ".json"))
    jsonlite::write_json(data, file, pretty= TRUE, auto_unbox = TRUE)
    writeLines(hash, data[["empresa"]])
    gert::git_add(c(file, data[["empresa"]]))
  }
  
  
  
}
urls <- c(edesur = "https://www.enre.gov.ar/paginacorte/js/data_EDS.js",
          edenor = "https://www.enre.gov.ar/paginacorte/js/data_EDN.js")

lapply(urls, get_data)

gert::git_commit(message = "Agrega datos (automático)")
gert::git_push()
