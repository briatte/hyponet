#
# 2. get a large random sample of all blog posts
#

library(dplyr)
library(readr)

dir.create("html", showWarnings = FALSE)

a = read_csv("data/posts.csv") %>%
  unique

# build full post URLs
a = with(a, paste0(blog, slug))

# select only well-formed
a = a[ grepl("^http://[a-z0-9]+.hypotheses.org/\\d+$", a) ]

# fraction of full sample to download
s = 1/4

# list of file names
l = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", a)
names(l) = a

# select fraction of full sample
k = round(s * length(a)) - sum(file.exists(l))

# sample that many missing files
k = sample(a[ !file.exists(l) ], k)

while(length(k) > 0) {

  cat(date(), ": downloading", sprintf("%4.0f", length(k)), "articles\n")

  for(j in sample(k, ifelse(length(k) > 500, 500, length(k)))) {

    f = l[ j ]

    if(!file.exists(f))
      try(download.file(j, f, quiet = TRUE), silent = TRUE)

    if(!file.exists(f) | !file.info(f)$size)
      cat(".. failed to download", j, "\n")

    # be nice with Hypothèses
    Sys.sleep(1)

  }

  # remove empty files
  null = !file.info(list.files("html", full.names = TRUE))$size
  null = file.remove(list.files("html", full.names = TRUE)[ null ])

  if(length(null))
    cat(".. removed", length(null), "empty file(s)\n")

  # select fraction of full sample
  k = round(s * length(a)) - sum(file.exists(l))

  # sample that many missing files
  k = sample(a[ !file.exists(l) ], k)

}
