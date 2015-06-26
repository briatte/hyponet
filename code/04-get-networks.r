#
# 4. plot all networks in a sequence
#

library(dplyr)
library(readr)
library(stringr)

library(animation)
library(ggplot2)
library(RColorBrewer)

library(GGally)
library(igraph)
library(intergraph)
library(network)

dir.create("model", showWarnings = FALSE)

e = read_csv("data/edges.csv")

# simplified date

e$t = substr(e$t, 1, 4)
e$t[ e$t == "2008" ] = "2009" # lone blog post

# simplified edges (blogs)

e$ii = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", e$i) %>%
  str_trim %>%
  tolower
e$jj = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", e$j) %>%
  str_trim %>%
  tolower

e = filter(e, ii != jj)

# group_by(e, t) %>%
#   summarise(edges = n(), blogs_i = n_distinct(ii), blogs_j = n_distinct(jj)) %>%
#   knitr::kable(.)

#
# COMPLETE NETWORK
#

# weighted edge list
t = select(e, ii, jj, w) %>%
  group_by(ii, jj) %>%
  summarise(w = sum(1 / w), c = n())

# directed network, no self-loops
n = network(t[, 1:2 ], directed = TRUE, loops = FALSE)

# weight ties by inverse number of hyperlinks in blog post
network::set.edge.attribute(n, "weight", t$w)
network::set.edge.attribute(n, "count", t$c)

# Walktrap community detection
oc = walktrap.community(asIgraph(n))
n %v% "oc" = paste0("g", str_pad(membership(oc), width = 3, pad = "0"))

n %n% "year" = table(e$t)
n %n% "modularity" = modularity(oc)
n %n% "communities" = n_distinct(n %v% "oc")

t = select(e, i, j)
t = unique(c(t$i, t$j)) %>%
  gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", .)

n %n% "files" = t

colors = sort(table(n %v% "oc"), decreasing = TRUE)
x = ifelse(length(colors) > 7, 8, length(colors))
colors[ 1:x ] = brewer.pal(x, "Set1")
colors[ nchar(colors) < 7 ] = "#AAAAAA"

# nodes sized by (unweighted) degree
ggnet(n, size = 0, node.group = n %v% "oc") +
  geom_text(aes(label = network.vertex.names(n), color = n %v% "oc",
                size = cut(igraph::degree(asIgraph(n)),
                           c(0, 10, 20, 40, 80)))) +
  geom_text(aes(label = network.vertex.names(n),
                size = cut(igraph::degree(asIgraph(n)),
                           c(0, 10, 20, 40, 80))),
            color = "black", alpha = .5) +
  scale_color_manual("", values = colors) +
  guides(color = FALSE, size = FALSE) +
  ggtitle(paste("Hypothesesosphère 2009-2015:", network.size(n), "blogs\n"))

ggsave("hyponet.png", width = 7, height = 7)
ggsave("hyponet.pdf", width = 7, height = 7)

#
# YEAR-SPECIFIC NETWORKS
#

l = list()

for(i in as.character(2009:2015)) {

  # weighted edge list
  t = e[ e$t == i, ] %>%
    select(ii, jj, w) %>%
    group_by(ii, jj) %>%
    summarise(w = sum(1 / w), c = n())

  # directed network, no self-loops
  n = network(t[, 1:2 ], directed = TRUE, loops = FALSE)

  # weight ties by inverse number of hyperlinks in blog post
  network::set.edge.attribute(n, "weight", t$w)
  network::set.edge.attribute(n, "count", t$c)

  # Walktrap community detection
  oc = walktrap.community(asIgraph(n))
  n %v% "oc" = paste0("g", str_pad(membership(oc), width = 3, pad = "0"))

  n %n% "year" = i
  n %n% "modularity" = modularity(oc)
  n %n% "communities" = n_distinct(n %v% "oc")

  t = e[ e$t == i, c("i", "j") ]
  t = unique(c(t$i, t$j)) %>%
    gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", .)

  n %n% "files" = t

  cat("Network for year", i, ":", length(t), "files,",
      network.size(n), "nodes,", network.edgecount(n), "ties",
      n %n% "communities", "communities\n")

  l[[ i ]] = n

}

save(l, file = "model/networks.rda")

# network dimensions

# data_frame(
#   network = names(l),
#   nodes = sapply(l, network.size),
#   edges = sapply(l, network.edgecount),
#   density = sapply(l, network.density),
#   communities = sapply(l, function(x) x %n% "communities"),
#   modularity = sapply(l, function(x) x %n% "modularity")
# ) %>%
#   knitr::kable(., digits = 2)

# network animation

saveGIF({
  for(n in l) {

    colors = sort(table(n %v% "oc"), decreasing = TRUE)
    x = ifelse(length(colors) > 7, 8, length(colors))
    colors[ 1:x ] = brewer.pal(x, "Set1")
    colors[ nchar(colors) < 7 ] = "#AAAAAA"

    g = ggnet(n, size = 0, label = TRUE, label.size = 4,
              node.group = n %v% "oc", segment.color = "grey25") +
      geom_text(aes(label = network.vertex.names(n)),
                color = "black", alpha = .5, size = 4) +
      scale_color_manual("", values = colors) +
      guides(color = FALSE) +
      ggtitle(paste("Hypothesesosphère", n %n% "year",
                    ":", network.size(n), "blogs\n"))

    print(g)

  }
}, movie.name = "hyponet.gif", interval = 2, ani.width = 800, ani.height = 800)

#
# YEAR-MONTH NETWORKS
#

# reload edge list
e = read_csv("data/edges.csv")

e$i = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", e$i) %>%
  str_trim %>%
  tolower
e$j = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", e$j) %>%
  str_trim %>%
  tolower

# remove self-loops
e = filter(e, i != j)

# year-month
e$t = substr(e$t, 1, 7)

t = unique(e[, 2:3 ])
t$uid = paste(t$i, t$j)

n = graph.edgelist(as.matrix(t[, 1:2 ]), directed = TRUE)
E(n)$uid = t$uid
l = layout.fruchterman.reingold(n)

V(n)$frame.color = NA
V(n)$color = NA
V(n)$label = ""
V(n)$size = 2
E(n)$arrow.mode = 1
E(n)$arrow.size = .3
E(n)$color = NA

ramp = colorRampPalette(c("grey50", "tomato"))(50)

saveGIF({
  for(i in unique(e$t)) {

    w = with(e[ e$t == i, ], unique(c(i, j)))
    m = with(e[ e$t == i, ], paste(i, j))

    V(n)$size = V(n)$size + as.numeric(V(n)$name %in% w)
    V(n)$color = ifelse(V(n)$name %in% w | !is.na(V(n)$color), ramp[ V(n)$size ], NA)
    V(n)$frame.color = ifelse(V(n)$name %in% w | !is.na(V(n)$frame.color), "grey75", NA)
    E(n)$color = ifelse(E(n)$uid %in% m | !is.na(E(n)$color), "grey50", NA)

    plot(n, layout = l, main = paste(i, ":", sum(!is.na(V(n)$color)), "blogs",
                                     sum(!is.na(E(n)$color)), "links"))

  }
}, movie.name = "hypodyn.gif", interval = .5, ani.width = 800, ani.height = 800)
