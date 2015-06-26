library(dplyr)
library(readr)
library(lubridate)
library(stringr)

library(network)
library(networkDynamic)
library(ndtv)

e = read_csv("data/edges.csv")

e$i = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", e$i) %>%
  str_trim %>%
  tolower
e$j = gsub("https?://([a-z0-9-]+).hypotheses.org/(.*)", "\\1", e$j) %>%
  str_trim %>%
  tolower

# remove self-loops
e = filter(e, i != j)

# year-quarter
e$q = paste0(year(e$t), "-", quarter(e$t))

# base network
fn = network(unique(e[, 2:3 ]), directed = TRUE, loops = FALSE)

yq = list()

for(i in unique(e$q)) {

  # weighted edge list
  t = e[ e$q == i, ] %>%
    select(i, j, w) %>%
    group_by(i, j) %>%
    summarise(w = sum(1 / w), c = n()) %>%
    data.frame

  # directed network, no self-loops
  n = network(t[, 1:2 ], directed = TRUE, loops = FALSE)

  # weight ties by inverse number of hyperlinks in blog post
  network::set.edge.attribute(n, "weight", t$w)
  network::set.edge.attribute(n, "count", t$c)

  n %n% "t" = i

  cat("Network for period", i, ":",
      network.size(n), "nodes,", network.edgecount(n), "ties\n")

  yq[[ i ]] = n

}

# max(sapply(yq, function(x) max(degree(x))))
ramp = colorRampPalette(c("grey50", "tomato"))(25)

nd = networkDynamic(network.list = yq, vertex.pid = "vertex.names",
                    base.net = fn)

head(as.data.frame(nd))
tail(as.data.frame(nd))

compute.animation(nd, animation.mode = "kamadakawai",
                  slice.par = list(start = 0, end = 25, interval = 1,
                                   aggregate.dur = 1, rule = "any"))

render.d3movie(nd, displaylabels = TRUE, usearrows = TRUE, edge.col = "grey50",
               main = paste0("Links between Hypotheses.org blogs, ",
                             "2009-Q1 to 2015-Q2"),
               filename = "hyponet.html",
               label = function(slice) { slice %v% "vertex.names" },
               label.col = function(slice) { ramp[ degree(slice) ] },
               vertex.border = function(slice) { ramp[ degree(slice) ] },
               vertex.col = function(slice) { ramp[ degree(slice) ] },
               vertex.cex = function(slice) { degree(slice)^(1/2) },
               vertex.tooltip = function(slice) {
                 paste0("During the observed period, ",
                        "the blog <a href='http://", slice %v% "vertex.names",
                        ".hypotheses.org/' target='_blank'>",
                        slice %v% "vertex.names", "</a> ",
                        "linked to <strong>", degree(slice, cmode = "outdegree"),
                        "</strong> blog(s) and was linked to by <strong>",
                        degree(slice, cmode = "indegree"), "</strong> blog(s).")
               })
