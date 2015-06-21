#
# 4. plot all networks in a sequence
#

library(dplyr)
library(readr)

library(animation)
library(ggplot2)
library(RColorBrewer)

library(GGally)
library(igraph)
library(network)

e = read_csv("data/edges_hypotheses.csv")

e$t = substr(e$t, 1, 4)
e$i = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", tolower(e$i))
e$j = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", tolower(e$j))

group_by(e, t) %>%
  summarise(edges = n(), blogs_i = n_distinct(i), blogs_j = n_distinct(j))

# complete network (too large for optimal community detection, so using Waltrap)

n = network(e[, c("i", "j") ])

network::set.edge.attribute(n, "w", 1 / e$w)
n %v% "oc" = paste0("i", membership(walktrap.community(intergraph::asIgraph(n),
                                                       weights = n %e% "w")))

colors = sort(table(n %v% "oc"), decreasing = TRUE)
x = ifelse(length(colors) > 7, 8, length(colors))
colors[ 1:x ] = brewer.pal(x, "Set1")
colors[ nchar(colors) < 7 ] = "#AAAAAA"

ggnet(n, size = 0, node.group = n %v% "oc") +
  geom_text(aes(label = network.vertex.names(n), color = n %v% "oc",
                size = cut(igraph::degree(intergraph::asIgraph(n)),
                           c(0, 10, 20, 40, 80)))) +
  geom_text(aes(label = network.vertex.names(n),
                size = cut(igraph::degree(intergraph::asIgraph(n)),
                           c(0, 10, 20, 40, 80))),
            color = "black", alpha = .5) +
  scale_color_manual("", values = colors) +
  guides(color = FALSE, size = FALSE) +
  ggtitle(paste("Hypothesesosphère 2009-2015:", network.size(n), "blogs\n"))

ggsave("hyponet.png", width = 7, height = 7)
ggsave("hyponet.pdf", width = 7, height = 7)

# year-specific networks (small enough to use optimal community detection)

saveGIF({
  for(i in as.character(2009:2015)) {

    n = network(e[ e$t == i, c("i", "j") ], directed = TRUE)

    network::set.edge.attribute(n, "w", 1 / e[ e$t == i, "w" ])
    n %v% "oc" = letters[ membership(optimal.community(intergraph::asIgraph(n),
                                                       weights = n %e% "w")) ]

    colors = sort(table(n %v% "oc"), decreasing = TRUE)
    x = ifelse(length(colors) > 7, 8, length(colors))
    colors[ 1:x ] = brewer.pal(x, "Set1")
    colors[ nchar(colors) < 7 ] = "#AAAAAA"

    g = ggnet(n, size = 0, label = TRUE, label.size = 4, node.group = n %v% "oc", segment.color = "grey25") +
      geom_text(aes(label = network.vertex.names(n)), color = "black", alpha = .5, size = 4) +
      scale_color_manual("", values = colors) +
      guides(color = FALSE) +
      ggtitle(paste("Hypothesesosphère", i, ":", network.size(n), "blogs\n"))

    print(g)

  }
}, movie.name = "hyponet.gif", interval = 2, ani.width = 800, ani.height = 800)
