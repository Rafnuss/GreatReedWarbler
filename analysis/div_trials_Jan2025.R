
library("GeoPressureR")
load("/Users/yannrime/Library/CloudStorage/OneDrive-Personnel/postdoc/GreatReedWarbler/GreatReedWarbler-main/data/interim/14TZ.RData")


abs(edge_most_likely$gs)
id <- "14TZ"

graph <- graph_create(tag,
                      thr_likelihood = 0.90,
                      thr_gs = 80, # km/h
                      quiet = TRUE
)

geopressureviz(id)

graph <- graph_set_movement(graph,
                            method = "gamma",
                            shape = 7,
                            scale = 5,
                            low_speed_fix = 15,
                            zero_speed_ratio = 2,
)
plot_graph_movement(graph)

path_most_likely <- graph_most_likely(graph, quiet = TRUE)

plot_path(path_most_likely)

marginal <- graph_marginal(graph, quiet = TRUE)

plot(marginal, path = path_most_likely, plot_leaflet = TRUE)


path_simulation <- graph_simulation(graph,
                                    nj = 10, # Number of simulation
                                    quiet = TRUE
)
plot_path(path_simulation, plot_leaflet = FALSE)
plot_pressurepath(pressurepath_most_likely)


geoprpressurepath_most_likelygeopressureviz(
  tag,
  path = path_most_likely) # start from the existing drawn path
