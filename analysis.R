renv::activate(profile = "analysis")

library(ggplot2)

files <- list.files(c("data/EDESUR",
                      "data/EDENOR"), full.names  = TRUE)
dates <-  as.POSIXct(
  basename(files), 
  format = "%Y-%m-%dT%H:%M:%S")


yesterday <- (as.numeric(Sys.time()) - as.numeric(dates))/3600 < 24
files <- files[yesterday]

as_number <- function(x) {
  as.numeric(gsub("\\.", "", x))
}
parse_file <- function(file) {
  a <- jsonlite::read_json(file)
  
  data.frame(
    fecha = as.POSIXct(
      basename(file), 
      format = "%Y-%m-%dT%H:%M:%S"),
    empresa = a$empresa,
    sin_suministro = as_number(a$totalUsuariosSinSuministro), 
    con_suministro = as_number(a$totalUsuariosConSuministro)
  )
  
} 

data <- do.call(rbind, lapply(files, parse_file))


g <- ggplot(data, aes(fecha, sin_suministro)) + 
  geom_step(aes(color = empresa)) +
  theme_minimal() +
  scale_x_datetime(NULL) +
  scale_y_continuous(NULL) +
  scale_color_brewer(NULL, palette = "Dark2") +
  labs(title = "# de clientes sin servicio en las úlimtas 24 hs.")
  
dir.create("figures", showWarnings = FALSE)
ggsave(file.path("figures", "plot_24hs.png"), width = 800, height = 400, units = "px", dpi = 72, bg = "white")
  
  
  # data %>% 
  #   copy() %>% 
  #   .[, sum(sin_suministro), by = fecha] %>% 
  #   .[order(fecha)] %>% 
  #   .[, usuarios_tiempo := cumsum(c(as.numeric(diff(fecha)), 0)/60*V1)] %>% 
  #   ggplot(aes(fecha, usuarios_tiempo)) +
  #   geom_line()
  
