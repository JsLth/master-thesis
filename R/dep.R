library(tidyverse)
library(stringr)
library(network)
library(visNetwork)

files <- dir("R", full.names = TRUE) %>%
  setdiff("R/dep.R")

nodes <- tibble(id = seq_along(files), label = str_remove_all(files, "R\\/"))
nodes_list <- as.list(nodes$id) %>%
  setNames(nodes$label)

edges <- map_dfr(files, function(f) {
  code <- readLines(f)
  deps <- code[str_detect(code, fixed("source("))] %>%
    str_remove_all("source|\\(|\\)|R\\/|\"")
  f <- str_remove(f, "R\\/")
  
  tibble(from = unlist(nodes_list[deps]), to = unlist(nodes_list[f]))
}) %>%
  na.omit()

intr <- c("main.R", "collect_tweets.R", "geocode.R", "read_context_bulk.R")
nodes$group <- "Auxiliary"
nodes$group[nodes$label %in% intr] <- "Workflow"

tow <- edges %>%
  group_by(to) %>%
  summarise(n = n())
fromw <- edges %>%
  group_by(from) %>%
  summarise(n = n())
nodes <- nodes %>%
  left_join(tow, by = c("id" = "to")) %>%
  left_join(fromw, by = c("id" = "from")) %>%
  replace_na(list(n.x = 0, n.y = 0)) %>%
  mutate(value = (n.x + n.y) / 10) %>%
  dplyr::select(id, label, group, value)

nodes[nodes$group %in% "interactive", "color.background"] <- "#FD9898"
nodes[nodes$group %in% "interactive", "color.border"] <- "#AF6969"

net <- visNetwork(nodes, edges, width = "50%") %>%
  visNodes(scaling = list(min = 10, max = 30)) %>%
  visEdges(arrows = "to") %>%
  visGroups(groupname = "Workflow", color = list(background = "#FD9898", border = "#AF6969")) %>%
  visGroups(groupname = "Auxiliary") %>%
  visLegend(
    addEdges = data.frame(label = "Sourced by")
  ) %>%
  visLayout(randomSeed = rseed) %>%
  visSave(file = "plots/network.html")
