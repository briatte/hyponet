#
# 4. get a structured corpus and model them via a structural topic model
#

library(readr)
library(rvest)

library(SnowballC) # stemming
library(tm)        # cleaning
library(stm)       # modeling

library(stmBrowser)
library(stmCorrViz)

e = read_csv("data/edges_hypotheses.csv")

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

load("data/networks.rda")

text = data_frame()

for(n in l) {

  cat("Building corpus for", n %n% "year", "...\n")

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
        tags = c(tags, t)

    }

    text = rbind(text, data_frame(year = n %n% "year",
                                  community = paste0("y", n %n% "year", j),
                                  text = paste0(tolower(tags), collapse = " . ")))

  }

}

# # column 1: year (2009 to 2015)
# # column 2: optimal community year-name (unique)
# # column 3: optimal community text (keywords)
# glimpse(text)

txt = textProcessor(text$text, metadata = text, language = "fr")
txt = prepDocuments(txt$documents, txt$vocab, txt$meta)

stm = stm(txt$documents, txt$vocab, K = 20,
          prevalence =~ community + year, max.em.its = 100,
          data = txt$meta, seed = 3007)

save(text, txt, stm, file = "data/topics_test.rda")

stmCorrViz(stm, "corrviz.html")
stmBrowser(stm, data = txt$meta, covariates = "year", text = "text")
