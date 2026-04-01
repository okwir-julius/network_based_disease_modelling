
# author ------------------------------------------------------------------
# okwir julius
# disease modelling class

# introduction ------------------------------------------------------------
# Network-Based Disease Modelling for East Africa refugee movements
# Data Source: UNHCR refugee flows 
# Nodes = countries and Edges = refugee movements 

# --- 1. load packages -------------------------------------------------------
pacman::p_load(
  tidyverse,          # data wrangling and ggplot2
  igraph,             # network construction and network metrics computation
  tidygraph,          # tidy wrapper around igraph (tbl_graph objects)
  ggraph,             # network visualisation, ggplot2 style
  janitor,            # clean data
  rnaturalearth,      # country boundary shapefiles for map backgrounds
  rnaturalearthdata,  # supporting data for rnaturalearth
  sf                  # handle spatial objects
)


# --- 2. load and prepare data -----------------------------------------------
data <- read_csv("persons_of_concern.csv")

# clean column names and select desired columns
data <- data %>% 
  clean_names() %>% 
  select(
    year, country_of_origin, country_of_asylum, refugees, asylum_seekers
  )

# african countries - try this
african_countries <- c(
  "Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi",
  "Cabo Verde","Cameroon","Central African Republic","Chad","Comoros",
  "Democratic Republic of the Congo","Republic of the Congo",
  "Côte d'Ivoire","Djibouti","Egypt","Equatorial Guinea","Eritrea",
  "Eswatini","Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau",
  "Kenya","Lesotho","Liberia","Libya","Madagascar","Malawi","Mali",
  "Mauritania","Mauritius","Morocco","Mozambique","Namibia","Niger",
  "Nigeria","Rwanda","São Tomé and Príncipe","Senegal","Seychelles",
  "Sierra Leone","Somalia","South Africa","South Sudan","Sudan",
  "Tanzania","Togo","Tunisia","Uganda","Zambia","Zimbabwe"
)

# east african countries
ea_countries <- c(
  "Burundi", "Comoros", "Djibouti", "Eritrea", "Ethiopia", "Kenya",
  "Madagascar", "Malawi", "Mauritius", "Mozambique", "Rwanda",
  "Seychelles", "Somalia", "South Sudan", "Tanzania", "Uganda",
  "Zambia", "Zimbabwe"
)

#---3. build an edge list for east africa --------------------------------------
# what is an edge list by definition?
# An "edge" in our network represents refugee movement between two countries
# The weight = total number of refugees that moved
# Keep only movements where refugees > 0 and origin != asylum (no self-loops)
edges <- data %>%
  select(-asylum_seekers) %>% 
  filter(country_of_origin %in% east_africa, 
         country_of_asylum %in% east_africa,
         country_of_origin != country_of_asylum,   # remove within-country rows
         year == 2025,
         refugees > 0
  ) %>%
  select(
    from   = country_of_origin,   # edge source  (origin country)
    to     = country_of_asylum,   # edge target  (asylum country)
    weight = refugees             # edge weight  (number of refugees)
  )

# Node - number of unique countries either in from or to column
nodes <- tibble(name = unique(c(edges$from, edges$to)))

glimpse(edges)   # 70 rows - movements
glimpse(nodes)   # 14 rows - Unique countries


#--- 4a - undirected network ------------------------------------------
# Research question: Which countries are connected at all?
# An undirected graph treats movement as a mutual connection and simply shows if
# two countries are connected. It is not concerned with the direction of the connection

# Build an undirected tbl_graph (tidygraph wraps igraph).
# directed = FALSE 
tg_undirected <- tbl_graph(
  nodes    = nodes,
  edges    = edges,
  directed = FALSE
)

set.seed(123)   # fix layout seed so the plot is reproducible

ggraph(tg_undirected) +  

  # Draw edges as straight lines, width shows the refugee flow size
  geom_edge_link(
    aes(width = weight),
    colour = "steelblue",
    alpha  = 0.5            # transparency for overlapping edges
  ) +

  # Map edge width to a readable range
  scale_edge_width_continuous(
    name   = "refugees",
    range  = c(0.3, 3),
    labels = scales::comma
  ) +

  # Draw nodes; size encodes how many connections each country has
  geom_node_point(
    colour      = "#4E9EBD",
    show.legend = TRUE
  ) +

  # Label each node with the country name
  geom_node_text(
    aes(label = name),
    size   = 2,
    repel  = TRUE,    
    colour = "grey20"
  ) +

  labs(
    title    = "Undirected network graph for EA refugee movement in 2025",
  ) +

  theme_graph(base_family = "sans") +   
  theme(
    legend.position = "right"
  )


#--- 4b - directed network ------------------------------------------
# Research question: In what direction is the refugee movement?
# A directed graph adds arrows showing the direction of movement.
# arrows point FROM origin country TO asylum country.
# Edge width shows how many refugees travel on each route

# Build a directed tbl_graph (tidygraph wraps igraph).
# directed = TRUE 
tg_directed <- tbl_graph(
  nodes    = nodes,
  edges    = edges,
  directed = TRUE
)

set.seed(123)
ggraph(tg_directed) +
  # Draw curved arcs instead of straight lines to separates A->B and B->A 
  # so both arrows are visible
  geom_edge_fan(
    aes(width = weight),
    colour  = "steelblue",
    alpha   = 0.5,
    arrow   = arrow(length = unit(1, "mm"), type = "closed"),
    end_cap = circle(2, "mm")    # gap before node so arrowhead is not hidden
  ) +
  # Map edge width to a readable range
  scale_edge_width_continuous(
    name   = "Refugees",
    range  = c(0.3, 3),
    labels = scales::comma
  ) +
  # Draw nodes
  geom_node_point(
    colour      = "steelblue",
    show.legend = TRUE
  ) +
  # Label each node with the country name
  geom_node_text(
    aes(label = name),
    size   = 2,
    repel  = TRUE,
    colour = "grey20"
  ) +
  labs(
    title = "Directed network graph for EA refugee movement in 2025",
  ) +
  theme_graph(base_family = "sans") +
  theme(
    legend.position = "right"
  )

#---4c -- undirected network layed over a map-------------------------------
# Nodes are placed at each country's true geographic centroid using
# shapefiles, giving spatial context to the connections.

# ne_countries() returns sf polygons, "medium" scale suits regional maps
world  <- ne_countries(scale = "medium", returnclass = "sf")
ea_map <- world %>%
  filter(name_long %in% ea_countries |
           name    %in% ea_countries |
           admin   %in% ea_countries)

# 4c.1 Compute geographic centroids for node placement 
# st_centroid() returns the geographic midpoint of each country polygon
centroids <- ea_map %>%
  st_centroid() %>%           # one centroid point per country
  st_coordinates() %>%        # extract numeric lon (X) and lat (Y)
  as_tibble() %>%
  mutate(name = ea_map$name_long) %>%
  rename(lon = X, lat = Y) %>%
  # Harmonise names that differ between UNHCR data and Natural Earth
  mutate(name = case_when(
    name == "United Republic of Tanzania" ~ "Tanzania",
    TRUE ~ name
  ))

# 4c.2 Build geo-located node table and tbl_graph 
# Add lon/lat coordinates to the node list for use as manual layout positions
nodes_geo <- nodes %>%
  left_join(centroids, by = "name")

tg_undirected_geo <- tbl_graph(
  nodes    = nodes_geo,
  edges    = edges,
  directed = FALSE
)

# 4c. Plot 
# layout = "manual" tells ggraph to use the x/y coordinates we supply
ggraph(tg_undirected_geo, layout = "manual",
       x = nodes_geo$lon, y = nodes_geo$lat) +

  # Basemap layer - draw country polygons behind the network
  geom_sf(data = ea_map, inherit.aes = FALSE,
          fill = "#F5F0E8", colour = "white", linewidth = 0.3) +

  # Undirected edges as straight lines scaled by refugee count
  geom_edge_link(
    aes(width = weight),
    colour = "steelblue",
    alpha  = 0.5
  ) +

  scale_edge_width_continuous(
    name   = "Refugees",
    range  = c(0.3, 3),
    labels = scales::comma
  ) +

  # Nodes placed at country centroids
  geom_node_point(size = 2, colour = "steelblue") +

  # Labels nudged upward slightly to avoid sitting on the node dot
  geom_node_text(
    aes(label = name),
    size    = 2.8,
    nudge_y = 0.9,
    colour  = "grey20"
  ) +

  # coord_sf constrains the view to East Africa's bounding box
  coord_sf(xlim = c(25, 55), ylim = c(-25, 18)) +

  labs(
    title    = "Undirected Network of EA Refugee Flows (2025)",
  ) +

  theme_void(base_size = 11) +
  theme(
    legend.position = "right"
  )


# 4d. directed network layered over map --------------------------------------
# Same geographic layout as Section 4c but with directional arrows 
tg_directed_geo <- tbl_graph(
  nodes    = nodes_geo,
  edges    = edges,
  directed = TRUE
)

ggraph(tg_directed_geo, layout = "manual",
       x = nodes_geo$lon, y = nodes_geo$lat) +
  # Basemap layer - draw country polygons behind the network
  geom_sf(data = ea_map, inherit.aes = FALSE,
          fill = "#F5F0E8", colour = "white", linewidth = 0.3) +
  # Directed edges as curved arcs; fan separates A->B and B->A on the map
  geom_edge_fan(
    aes(width = weight),
    colour  = "steelblue",
    alpha   = 0.5,
    arrow   = arrow(length = unit(2, "mm"), type = "closed"),
    end_cap = circle(2, "mm")
  ) +
  scale_edge_width_continuous(
    name   = "Refugees",
    range  = c(0.3, 3),
    labels = scales::comma
  ) +
  # Nodes placed at country centroids
  geom_node_point(size = 2, colour = "steelblue") +
  
  # Labels nudged upward slightly to avoid sitting on the node dot
  geom_node_text(
    aes(label = name),
    size    = 2.8,
    nudge_y = 0.9,
    colour  = "grey20"
  ) +
  # coord_sf constrains the view to East Africa's bounding box
  coord_sf(xlim = c(25, 55), ylim = c(-25, 18)) +
  labs(
    title = "Directed Network of EA Refugee movements (2025)",
  ) +
  theme_void(base_size = 11) +
  theme(
    legend.position = "right"
  )


# 5. Some key network properties ---------------------------------------------------
#.  a. Network density - the proportion of all possible connections 
#.  that actually exist in a network
#   b. In-degree  - number of countries sending refugees TO another country
#   c. Out-degree - number of countries a country sends refugees TO
#   d. Centrality measures:
#       i. betweenness centrality - how often a country acts as a bridge on the 
#           shortest path between two countries
#      ii. closeness centrality - how close a country is to all other countries

# 5a. Network density -----------------------------------------------------
# for directed networks
# Density = actual edges / maximum possible directed edges
#         = E / (N x (N - 1))
# Range: 0 (disconnected) to 1 (fully connected)
# 0.00–0.10	Very sparse 
# 0.10–0.30	Moderately connected
# 0.30–0.50	Highly connected
# 0.50+		Very dense 
# Higher density means disease has more pathways between populations.

n_density <- edge_density(g_dir)

n_density


# 5b. In-degree
# High in-degree = country receives refugees from many different origins
# Disease modelling interpretation: high importation risk from many sources
# g_dir <- as.igraph(tg_directed)
g_dir <- graph_from_data_frame(edges, directed = TRUE, vertices = nodes)

in_deg <- degree(g_dir, mode = "in") %>%
  enframe(name = "country", value = "in_degree") %>%
  arrange(desc(in_degree))

# 5c. Out-degree 
# High out-degree = country sends refugees to many different destinations.
# Disease modelling interpretation: potential to seed outbreaks in many places.
out_deg <- degree(g_dir, mode = "out") %>%
  enframe(name = "country", value = "out_degree") %>%
  arrange(desc(out_degree))


#  Summary table
degree_summary <- in_deg %>%
  full_join(out_deg, by = "country") %>%
  mutate(
    total_degree = in_degree + out_degree,   # overall node connectivity
    role = case_when(
      in_degree > out_degree ~ "Net receiver (asylum)",
      out_degree > in_degree ~ "Net sender (origin)",
      TRUE                   ~ "Balanced"
    )
  ) %>%
  arrange(desc(total_degree))

degree_summary

# Bar chart of in-degree vs out-degree per country 
degree_summary %>%
  pivot_longer(
    cols      = c(in_degree, out_degree),
    names_to  = "type",
    values_to = "degree"
  ) %>%
  mutate(
    type = recode(
      type,
      "in_degree"  = "In-degree  (asylum country)",
      "out_degree" = "Out-degree (origin country)"
    ),
    country = fct_reorder(country, total_degree)   # most connected at top
  ) %>%
  ggplot(aes(x = degree, y = country, fill = type)) +
  geom_col(position = "dodge") +
  scale_fill_manual(
    values = c(
      "In-degree  (asylum country)"  = "#4E9EBD",
      "Out-degree (origin country)"  = "#E07B54"
    ),
    name = NULL
  ) +
  labs(
    title    = "In-Degree vs Out-Degree - EA Refugee Network (2025)",
    x        = "Degree",
    y        = NULL,
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom"
  )


# 5d. Betweenness centrality
# Measures how often a country acts as a bridge on the shortest path between 
# two countries
# High betweenness = a critical "bridge" country in the network
# Disease modelling interpretation: removing or intervening at high-betweenness countries
# would most disrupt disease spread between regions

betweenness_c <- betweenness(g_dir, directed = TRUE, normalized = TRUE) %>%
  # normalized = TRUE scales scores to 0-1 so values are comparable across
  # networks of different sizes
  enframe(name = "country", value = "betweenness") %>%
  arrange(desc(betweenness))

# 5d. Closeness centrality
# Measures how close a country is to all other countries in the network
# High closeness = short average path length to every other country
# Disease interpretation: high closeness countries spread disease fastest
# because they are few steps away from all other populations

closeness_c <- closeness(g_dir, mode = "out", normalized = TRUE) %>%
  # mode = "out" measures closeness via outgoing paths (spreading direction)
  # normalized = TRUE scales to 0-1 for interpretability
  enframe(name = "country", value = "closeness") %>%
  arrange(desc(closeness))

# update summary table
network_properties <- degree_summary %>%
  left_join(betweenness_c, by = "country") %>%
  left_join(closeness_c,   by = "country") %>%
  arrange(desc(betweenness))








