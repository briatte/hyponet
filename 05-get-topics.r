#
# 4. get a structured corpus and model them via a structural topic model
#

library(readr)
library(rvest)

library(SnowballC) # stemming
library(tm)        # cleaning
library(stm)       # modeling

e = read_csv("data/edges_hypotheses.csv")

e = unique(c(e$i, e$j))
f = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", e)

k = e[ !file.exists(f) ]

while(length(k) > 0) {

  cat("Downloading", sprintf("%4.0f", length(k)), "articles\n")

  for(j in sample(k, ifelse(length(k) > 100, 100, length(k)))) {

    f = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", j)

    if(!file.exists(f))
      try(download.file(j, f, quiet = TRUE), silent = TRUE)

    if(!file.exists(f) | !file.info(f)$size)
      cat("failed to download", j, "\n")

    # be nice with Hypothèses
    Sys.sleep(.5)

  }

  # remove empty files
  null = !file.info(list.files("html", full.names = TRUE))$size
  null = file.remove(list.files("html", full.names = TRUE)[ null ])

  if(length(null))
    cat("Removed", length(null), "empty files\n")

  # select 25% of full sample
  k = e[ !file.exists(f) ]

}

text = data_frame()

for(n in l) {

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

      stopifnot(length(t) > 0)
      tags = c(tags, t)

    }

    text = rbind(text, data_frame(year = n %n% "year",
                                  community = paste0("y", n %n% "year", j),
                                  text = paste0(tolower(tags), collapse = " . ")))

  }

}

# # column 1: year
# # column 2: optimal community name
# # column 3: optimal community text (keywords)
# glimpse(text)

txt = textProcessor(text$text, metadata = text[, 1:2 ], language = "fr")
txt = prepDocuments(txt$documents, txt$vocab, txt$meta)

stm = stm(txt$documents, txt$vocab, K = 20,
          prevalence =~ community + year, max.em.its = 75,
          data = txt$meta, seed = 3007)
