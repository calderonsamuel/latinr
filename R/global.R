library(DiagrammeR)

add_starter_node <- function(graph, label) {
    add_node(
        graph = graph, 
        label = stringr::str_wrap(label, 12), 
        node_aes = node_aes(shape = "circle", 
                            fontsize = 5, 
                            fillcolor = "seagreen", 
                            fontcolor = "white")
    )
}

add_custom_node <- function(graph, label, type = "A") {
    node_fillcolor <- switch (type,
                              A = "azure",
                              B = "cornsilk",
                              C = "aliceblue",
                              D = "lavender",
                              E = "mistyrose",
                              `F` = "palegreen"
    )
    
    add_node(
        graph = graph, 
        label = stringr::str_wrap(label, 12), 
        node_aes = node_aes(shape = "rectangle", 
                            fontsize = 5,
                            fillcolor = node_fillcolor,
                            fontcolor = "black"), 
        type = type
    )
}

add_custom_edge <- function(graph, from, to) {
    add_edge(
        graph = graph,
        from = stringr::str_wrap(from, 12),
        to = stringr::str_wrap(to, 12), 
    )
}

custom_render_graph <- function(graph) render_graph(graph, width = 650)

graph_quien_gasta <- create_graph(attr_theme = "lr") %>% 
    add_starter_node(label = "Consulta base") %>% 
    add_custom_node(label = "Nivel de gobierno") %>% 
    add_custom_node(label = "Sector") %>% 
    add_custom_node(label = "Gob local o mancomunidad") %>% 
    add_custom_node(label = "Pliego") %>% 
    add_custom_node(label = "Unidad ejecutora") %>% 
    add_custom_node(label = "Departamento") %>% 
    add_custom_node(label = "Mancomunidad") %>% 
    add_custom_node(label = "Provincia") %>% 
    add_custom_node(label = "Municipalidad") %>% 
    add_custom_edge(from = "Consulta base", to = "Nivel de gobierno") %>%
    add_custom_edge(from = "Nivel de gobierno", to = "Sector") %>%
    add_custom_edge(from = "Nivel de gobierno", to = "Gob local o mancomunidad") %>%
    add_custom_edge(from = "Sector", to = "Pliego") %>% 
    add_custom_edge(from = "Pliego", to = "Unidad ejecutora") %>% 
    add_custom_edge(from = "Gob local o mancomunidad", to = "Departamento") %>% 
    add_custom_edge(from = "Gob local o mancomunidad", to = "Mancomunidad") %>% 
    add_custom_edge(from = "Departamento", to = "Provincia") %>% 
    add_custom_edge(from = "Departamento", to = "Municipalidad") %>% 
    add_custom_edge(from = "Provincia", to = "Municipalidad") %>% 
    custom_render_graph()