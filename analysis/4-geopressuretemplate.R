# See https://raphaelnussbaumer.com/GeoPressureManual/geopressuretemplate-workflow.html

library(GeoPressureR)

config_list <- yaml::yaml.load_file("config.yml", eval.expr = FALSE)
config_list <- yaml::read_yaml("config.yml", eval.expr = FALSE)
config_tibble <- tibble::as_tibble(config_list)

id <- "14TZ"
geopressuretemplate_tag(id)
geopressuretemplate_graph(id)
geopressuretemplate_pressurepath(id)



geopressuretemplate(id)

## Run workflow for all tags

list_id <- tail(names(yaml::yaml.load_file("config.yml", eval.expr = FALSE)), -1)
geopressuretemplate_config(list_id[[2]])

list_id <- c("14ES", "14FF", "14GD", "14GF", "14GG", "14GH", "14GS", "14GW", "14HA", "14HB", "14HC", "14HT", "14TS", "14TF", "14TZ", "16BR", "16ER", "16ET", "16FB", "16FI", "16FT", "16GA", "16HB", "16HL", "16HM", "18HA", "18IC", "18IQ", "18LD", "18LX", "18LY", "18LZ")
list_id2 <- c("14ES", "14FF", "14GD", "14GF", "14GG", "14GH", "14GS", "14GW", "14HA", "14HB", "14HC", "14HT", "14TS", "14TF", "16BR", "16ER", "16ET", "16FB", "16FI", "16FT", "16GA", "16HB", "16HL", "18HA", "18IC", "18IQ", "18LD", "18LX", "18LY", "18LZ")

job::job({
  for (id in list_id){
    cli::cli_h1("Run tag for {id}")
    geopressuretemplate_tag(id)
    geopressuretemplate_graph(id)
  }
})

job::job({geopressuretemplate_tag(id)})


# Add wind
for (id in list_id){
  cli::cli_h1("Run tag_download_wind for {id}")
  load(glue::glue("./data/interim/{id}.RData"))
  a<-tag_download_wind(tag)
}

# Run graph
for (id in list_id){
  cli::cli_h1("Run graph for {id}")
  geopressuretemplate_graph(id)
}

# Run pressurepath
for (id in list_id){
  cli::cli_h1("Run pressurepath for {id}")
  geopressuretemplate_pressurepath(id)
}
plot_path(path_most_likely)
