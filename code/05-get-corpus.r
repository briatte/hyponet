#
# 5. get a structured corpus for the network
#

library(dplyr)
library(readr)
library(rvest)

library(ggplot2)
library(network)

library(stringr)
library(SnowballC) # stemming
library(tm)        # cleaning

e = read_csv("data/edges.csv")

e = unique(c(e$i, e$j))
f = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", e)

k = e[ !file.exists(f) ]

# allow for a few blog posts to be unavailable
while(length(k) > 50) {

  cat("Downloading", sprintf("%4.0f", length(k)), "articles\n")

  for(j in sample(k, ifelse(length(k) > 500, 500, length(k)))) {

    fn = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", j)

    if(!file.exists(fn))
      try(download.file(j, fn, quiet = TRUE), silent = TRUE)

    if(!file.exists(fn) | !file.info(fn)$size)
      cat("failed to download", j, "\n")

    # be nice with Hypothèses
    Sys.sleep(.5)

  }

  # remove empty files
  null = !file.info(list.files("html", full.names = TRUE))$size
  null = file.remove(list.files("html", full.names = TRUE)[ null ])

  if(length(null))
    cat("Removed", length(null), "empty files\n")

  k = e[ !file.exists(f) ]

}

load("model/networks.rda")

text = data_frame()

for(n in l) {

  cat("Building corpus for", n %n% "year")

  for(j in unique(n %v% "oc")) {

    # files for all nodes (both senders and receivers)
    ff = n %n% "files"

    # subset to the optimal community under scrutiny
    oc = network.vertex.names(n)[ n %v% "oc" == j ]
    oc = paste0(oc, collapse = "|")

    ff = ff[ grepl(paste0("^html/(", oc, ")"), ff) ]

    tags = c()

    for(i in ff[ file.exists(ff) ]) {

      h = html(i)

      t = html_nodes(h, xpath = "//meta[@property = 'dc:subject']") %>%
        html_attr("content")

      if(length(t) > 0)
        tags = c(tags, tolower(t))

    }

    # remove prefix numbers
    tags = gsub("^\\d+(-|\\.)? ", "", tags)

    # remove punctuation
    tags = gsub("[[:punct:]]", " ", tags)

    # trim whitespace
    tags = str_trim(gsub("\\s+", " ", tags))

    # keyword exceptions
    tags = tags[ !tags %in% c("", "non classé") ]

    if(length(tags))
      text = rbind(text, data_frame(year = n %n% "year", oc = j,
                                    text = paste0(tags, collapse = " . ")))

  }

  cat(":", sum(text$year == n %n% "year"), "articles\n")

}

text$oc = paste0("i", sprintf("%03.0f", 1:nrow(text)))
write_csv(text, "data/corpus.csv")
