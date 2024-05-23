## Visualizing authorship networks: code for this blog post:
## https://mathewkiang.com/2018/06/17/my-collaboration-network/
library(scholar) # devtools::install_github("jkeirstead/scholar")
library(visNetwork)
library(tidyverse)

## Constants
MIN_TIME <- 60 * 5
MAX_TIME <- 60 * 30
PROJ_WEIGHT <- .8
NODE_WEIGHT <- 1
my_scholar_id <- "hiua2E8AAAAJ"

## If we've already pulled from Google Scholar, don't do it again. The sleep
## timer makes this an unreasonably long process.
if (!file.exists("./scholar_pulls.rda")) {
  ## Pull publication history of myself
  mvk_df <- get_publications(my_scholar_id)
  
  ## Drop the Opioid Trends preprint so we're not double-counting
  mvk_df <- mvk_df %>% 
    filter(pubid != "-yGd096yOn8C") %>% 
    mutate_at(vars(one_of("title", "author", "journal", 
                          "number", "cid", "pubid")), 
              as.character)
  
  ## Fix UTF-8 encoding -- once here and once later.
  # mvk_df <- mvk_df %>% 
  #   mutate(author = gsub("é|\xe9", "e", author))
  
  ## The summary df does not have a full author list so we need that. Set a
  ## generous sleep timer to stay below rate limit. It seems like one 
  ## pull every 15 to 30 minutes is fairly safe.
  authors <- NULL
  pubid   <- NULL
  for (p in mvk_df$pubid) {
    print(p)
    authors <- c(authors, get_complete_authors(my_scholar_id, p))
    pubid   <- c(pubid, p)
    Sys.sleep(runif(1, min = MIN_TIME, max = MAX_TIME))
  }
  
  ## Save it so we don't have to pull again.
  save(authors, pubid, mvk_df, file = "./scholar_pulls.rda")
}

## Now munging data
load("./scholar_pulls.rda")

## Split out every author into their own column 
full_authors <- tibble(pubid, f_authors = authors)
full_authors <- full_authors %>% 
  ## Fix encoding
  mutate(f_authors = gsub("é|\xe9", "e", f_authors)) %>% 
  ## Fix Rob's middle initial
  mutate(f_authors = gsub("W Moeller", "M Moeller", f_authors)) %>% 
  ## Fix Jen's middle initial
  mutate(f_authors = gsub("Jennifer Hayes", "Jennifer E Hayes", 
                          f_authors)) %>% 
  separate(f_authors, into = sprintf("author_%02i", 1:30), 
           remove = FALSE, sep = ", ", fill = "right")

## Remove all the completely empty columns
full_authors <- full_authors[, colSums(!is.na(full_authors)) > 0]

## Merge back to original dataframe
mvk_df <- mvk_df %>% 
  left_join(full_authors)

## Get a list of unique co-authors for the node list
distinct_authors <- unique(
  unlist(strsplit(full_authors$f_authors, ", ", fixed = TRUE))
)
distinct_authors <- distinct_authors[distinct_authors != "Mathew V Kiang"] 

## Now let's make a node list
nodes <- bind_rows(
  mvk_df %>% 
    select(title) %>% 
    mutate(value = PROJ_WEIGHT, 
           group = "paper", 
           id = title), 
  tibble(
    title = distinct_authors, 
    value = NODE_WEIGHT, 
    group = "coauthor", 
    id = title
  )
)

## Add some github side projects
nodes <- nodes %>% 
  add_case(title = "narcan", 
           value = PROJ_WEIGHT, 
           group = "software", 
           id = title) %>% 
  add_case(title = "metabeiwe", 
           value = PROJ_WEIGHT, 
           group  = "software", 
           id = title) %>% 
  add_case(title = "beiwe_data_sample", 
           value = PROJ_WEIGHT, 
           group  = "data", 
           id = title)

## Now the edge list -- reshape our merged dataframe into an edgelist 
## going from paper to co-author
edges <- mvk_df %>% 
  select(title, starts_with("author_")) %>% 
  gather(key = "author", value = "from", starts_with("author_")) %>% 
  filter(!is.na(from), from != "Mathew V Kiang") %>% 
  select(to = title, from)

## Add cases for the non-papers
edges <- edges %>% 
  add_case(to = "narcan", from = "Monica J Alexander") %>% 
  add_case(to = "beiwe_data_sample", from = "Jukka-Pekka Onnela") %>% 
  add_case(to = "beiwe_data_sample", from = "Jeanette Lorme")

visNetwork(nodes, edges) %>% 
  visLayout(randomSeed = 123456) %>% 
  visInteraction(zoomView = FALSE, dragView = FALSE) %>% 
  visGroups(groupname = "coauthor", color = "#3288bd") %>% 
  visGroups(groupname = "paper", color = "#d53e4f") %>% 
  visGroups(groupname = "data", color = "#abdda4") %>%
  visGroups(groupname = "software", color = "#fdae61") %>% 
  visNodes(scaling = list(min = 15, max = 25)) %>% 
  visEdges(width = 3, color = "grey") %>% 
  visSave("./author_network.html")

# library(dplyr)
# library(stringr)
# library(scholar)
# library(igraph)
# library(ggraph)
# library(see)
# library(tidygraph)
# 
# 
# # Functions --------------------
# find_coauthors <- function(id) {
#   df <- scholar:::list_coauthors(id, Inf)
#   df$id <- scholar:::grab_id(df$coauthors_url)
#   df[c("author", "coauthors", "id")]
# }
# 
# 
# list_coauthors <- function(df, sleep = 0, silent = FALSE) {
#   data <- data.frame()
#   
#   for (i in 1:nrow(df)) {
#     if (silent == FALSE) {
#       cat(paste0(round(i / nrow(df) * 100, 2), "%\n"))
#     }
#     
#     Sys.sleep(runif(1, 1, sleep))
#     
#     if (!df$coauthors[i] %in% unique(df$author)) {
#       data <- rbind(data, find_coauthors(df$id[i]))
#     }
#   }
#   
#   data
# }
# 
# 
# get_coauthors <- function(id = "hiua2E8AAAAJ", n_deep = 1, sleep = 3, silent = FALSE) {
#   stopifnot(is.numeric(n_deep), length(n_deep) >= 1, n_deep != 0)
#   
#   df <- find_coauthors(id)
#   if (n_deep > 0) {
#     for (level in 1:n_deep) {
#       cat(paste0("level ", level, ":\n"))
#       df <- rbind(df, list_coauthors(df, sleep = sleep, silent = silent))
#     }
#   }
#   
#   df$author <- stringr::str_to_title(df$author)
#   df$coauthors <- stringr::str_to_title(df$coauthors)
#   df <- df[!df$author %in% c("Sort By Citations", "Sort By Year", "Sort By Title"), ]
#   df <- df[!df$coauthors %in% c("Sort By Citations", "Sort By Year", "Sort By Title"), ]
#   df[c("author", "coauthors")]
# }
# 
# 
# 
# create_graph <- function(data) {
#   data |>
#     tidygraph::as_tbl_graph(directed = FALSE) |>
#     dplyr::filter(name != "") |>
#     dplyr::mutate(
#       closeness = tidygraph::centrality_closeness(normalized = TRUE),
#       degree = tidygraph::centrality_degree(normalized = TRUE)
#     ) |>
#     tidygraph::activate(edges) |>
#     dplyr::mutate(
#       importance = tidygraph::centrality_edge_betweenness(),
#       group = as.factor(from)
#     ) |>
#     tidygraph::activate(nodes) |>
#     dplyr::filter(!name %in% c("Sort By Citations", "Sort By Year", "Sort By Title")) |>
#     dplyr::mutate(
#       name = stringr::str_remove(name, ",.*"),
#       # Other groupings: group_edge_betweenness(), group_walktrap(), group_spinglass(), group_louvain()
#       group = as.factor(tidygraph::group_walktrap())
#     ) |>
#     as.list()
# }
# 
# 
# 
# # Get data --------------------------------
# 
# # Scrap data from google scholar
# data <- get_coauthors("hiua2E8AAAAJ", n_deep = 2, sleep = 15)
# 
# # Save data so that it can be re-used
# data |>
#   filter(!coauthors %in% c("About Scholar", "Search Help")) |>
#   filter(!author %in% c("About Scholar", "Search Help")) |>
#   write.csv("data/data_network.csv", row.names = FALSE)
# 
# 
# 
# # Process data ------------------------------------------------------------
# data <- read.csv("data/data_network.csv")
# 
# # Prune
# # 1. Find direct relations of DM
# data1 <- data[(data$author == "Ethan M. Mccormick" | data$coauthors == "Ethan M. Mccormick"), ]
# firstlevel <- unique(c(data1$author, data1$coauthors))
# 
# # 2. Find direct relations of these first-level co-authors
# data2 <- data[(data$author %in% firstlevel | data$coauthors %in% firstlevel), ]
# secondlevel <- unique(c(data2$author, data2$coauthors))
# 
# # 3. Find whether these have also coauthors in the list to link them between them
# data3 <- data[(data$author %in% secondlevel & data$coauthors %in% secondlevel) |
#                 (data$author %in% secondlevel & data$coauthors %in% secondlevel), ]
# 
# 
# # Make plot --------------------------------
# 
# # Plot
# data_graph <- create_graph(data=data3)
# 
# p <- tidygraph::tbl_graph(nodes = data_graph$nodes, edges = data_graph$edges, directed = FALSE) |>
#   ggraph::ggraph(layout = "nicely") + # fr, kk, nicely, lgl, graphopt, dh
#   ggraph::geom_edge_arc(aes(alpha = importance), show.legend = FALSE, strength = 0.1) +
#   ggraph::geom_node_point(aes(size = degree, colour = group), show.legend = FALSE) +
#   ggraph::geom_node_text(aes(label = name, size = degree), repel = TRUE, check_overlap = TRUE, show.legend = FALSE, max.overlaps = 20) +
#   # ggraph::geom_node_label(aes(label = name, size = degree), repel = TRUE, show.legend = FALSE) +
#   ggraph::theme_graph() +
#   scale_size_continuous(range = c(2, 6)) +
#   scale_edge_alpha_continuous(range = c(0.1, 0.8)) +
#   # scale_color_viridis_d() +
#   see::scale_color_material_d(palette = "rainbow", reverse = TRUE)
# 
# # Show plot
# p
# # Save
# ggsave("img/collaboration_network.png", p, dpi = 500, width = 10, height = 10)