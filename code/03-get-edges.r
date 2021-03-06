#
# 3. find network ties in the blogs
#

library(dplyr)
library(readr)
library(rvest)
library(stringr)

a = read_csv("data/posts.csv")

# select well-formed article addresses
b = with(a, paste0(blog, slug))
b = b[ grepl("^http://[a-z0-9]+.hypotheses.org/\\d+$", b) ]

# list of file names
f = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", b)
f = f[ file.exists(f) ]
f = sample(f[ file.info(f)$size > 0 ])

e = data_frame() # edge list

for(i in rev(f)) {

  h = html(i)

  base = html_nodes(h, xpath = "//link[@rel = 'dc:identifier']") %>%
    html_attr("href")

  date = html_nodes(h, xpath = "//meta[@property = 'dcterms:created']") %>%
    html_attr("content")

  #   # title
  #   html_nodes(h, xpath = "//meta[@property = 'dc:title']") %>%
  #     html_attr("content")

  #   # author
  #   html_nodes(h, xpath = "//meta[@property = 'dc:creator']") %>%
  #     html_attr("content")

  text = html_nodes(h, "div.post")

  if(!length(text))
    text = html_nodes(h, "div.entry-content")

  # stopifnot(length(text) > 0)
  if(!length(text))
    cat(i, ": empty\n")

  urls = html_nodes(text, "a") %>% html_attr("href") %>% na.omit
  urls = urls[ !grepl(gsub("http://(.*)/\\d+", "\\1", base), urls) ]

  # links to Hypothèses articles
  hypo = urls[ str_detect(urls, "\\w{2,}.hypotheses\\.org/\\d+") ]
  hypo = unique(gsub("(.*)#(.*)", "\\1", hypo))

  stopifnot(!is.na(hypo))

  if(length(hypo))
    e = rbind(e, data_frame(t = date, i = base, j = hypo, w = length(hypo)))

  if(!which(f == i) %% 500)
    cat(sprintf("%5.0f", which(f == i)), "posts left,",
        sprintf("%5.0f", nrow(e)), "edges\n")

}

# corrections
e$i = gsub("^https://", "http://", e$i)
e$j = gsub("^https://", "http://", e$j)
e$i = gsub("^//", "http://", e$i)
e$j = gsub("^//", "http://", e$j)
e$i = gsub("/$", "", e$i)
e$j = gsub("/$", "", e$j)

e = filter(e, grepl("/\\d+$", i), str_count(i, "/") == 3,
           grepl("/\\d+$", j), str_count(j, "/") == 3)

write_csv(arrange(e, t), "data/edges.csv")
