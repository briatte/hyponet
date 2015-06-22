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
s = 1/4

# list of file names
l = gsub("http://(.*)\\.hypotheses\\.org/(.*)", "html/\\1.\\2.html", b)
names(l) = b

# select fraction of full sample
k = sample(b, s * nrow(a) - sum(file.exists(l)))

while(length(k) > 0) {

  cat(date(), ": downloading", sprintf("%4.0f", length(k)), "articles\n")

  for(j in sample(k, ifelse(length(k) > 100, 100, length(k)))) {

    f = l[ j ]

    if(!file.exists(f))
      try(download.file(j, f, quiet = TRUE), silent = TRUE)

    if(!file.exists(f) | !file.info(f)$size)
      cat("failed to download", j, "\n")

    # be nice with Hypothèses
    Sys.sleep(1)

  }

  # remove empty files
  null = !file.info(list.files("html", full.names = TRUE))$size
  null = file.remove(list.files("html", full.names = TRUE)[ null ])

  if(length(null))
    cat("Removed", length(null), "empty files\n")

  # select fraction of full sample
  k = sample(b, s * nrow(a) - sum(file.exists(l)))

}
