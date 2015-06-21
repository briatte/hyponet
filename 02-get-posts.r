#
# 2. get a large random sample of all blog posts
#

library(readr)

dir.create("html", showWarnings = FALSE)

a = read_csv("data/posts.csv")

# select well-formed article addresses
b = with(a, paste0(blog, slug))
b = b[ grepl("^http://[a-z0-9]+.hypotheses.org/\\d+$", b) ]

# fraction of full sample to download
s = .25

k = sample(b, s * nrow(a) - length(list.files("html")))

while(length(k) > 0) {

  cat("Downloading", length(k), "articles\n")

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
  k = sample(b, s * nrow(a) - length(list.files("html")))

}
