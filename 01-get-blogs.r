#
# 1. get a list of blogs and a list of their posts
#

library(dplyr)
library(lubridate)
library(rvest)
library(readr)
library(XML)

dir.create("data", showWarnings = FALSE)

blogs = "data/blogs.csv"
posts = "data/posts.csv"

if(!file.exists(blogs)) {

  l = data_frame()

  # http://www.openedition.org/catalogue-notebooks (currently 1071 blogs)
  for(i in 1:11) {

    cat("Scraping page", i)

    h = html(paste0("http://www.openedition.org/catalogue-notebooks",
                    "?limit=100&p=", i))

    a = html_nodes(h, ".oe-CatalogueArticlesBlock-article")

    # creation date (sometimes missing)
    date = sapply(a, xpathSApply, "p[2]", xmlValue)
    date[ !sapply(date, length) ] = NA
    date = gsub("Au catalogue depuis le ", "", date) %>%
      parse_date_time("%d %m %Y", locale = "fr_FR") %>%
      as.Date

    a = data_frame(oeid = html_nodes(a, "a:contains('Notice')") %>%
                     html_attr("href"),
                   blog = html_nodes(a, "a:contains('Site')") %>%
                     html_attr("href"),
                   name = html_text(html_nodes(a, "h2 a")),
                   desc = html_text(html_node(a, "p")),
                   date)

    l = rbind(l, a)

    cat(":", nrow(l), "blogs\n")

  }

  l$blog = gsub("(/)?$", "/", l$blog)
  write_csv(l, blogs)

}

l = read_csv(blogs)

# start with existing file
if(file.exists(posts)) {

  a = read_csv(posts)

} else {

  a = data_frame()

}

# start with existing file
if(file.exists(links)) {

  b = read_csv(links)

} else {

  b = data_frame()

}

left = sample(l$blog[ !l$blog %in% a$blog ])

# note: blogs that do not allow to paginate through articles are skipped, e.g.
# criminocorpus.hypotheses.org

for(i in rev(left)) {

  cat("Blog:", i, "\n")

  j = 1
  m = data_frame()

  h = try(html(paste0(i, "page/", j)), silent = TRUE)

  while(!"try-error" %in% class(h) & j < 500) {

    cat("Scraping", paste0(i, "page/", j))

    # find blog entries and titles
    slug = html_nodes(h, "#content h1 a") %>% html_attr("href")
    name = html_nodes(h, "#content h1 a") %>% html_text()

    # if empty, try alternative class
    if(!length(slug)) {

      slug = html_nodes(h, "#content h2 a") %>% html_attr("href")
      name = html_nodes(h, "#content h2 a") %>% html_text()

    }

    # if still empty, try another alternative class
    if(!length(slug)) {

      slug = html_nodes(h, "#mainwrapper h1 a") %>% html_attr("href")
      name = html_nodes(h, "#mainwrapper h1 a") %>% html_text()

    }

    # if still empty, interpret as end of pagination
    if(!length(slug)) {

      cat(": empty page\n")
      break

    }

    # comments = html_nodes(h, "#content .comments-link a") %>% html_text()
    # author = html_nodes(h, xpath = "//a[@rel='author']") %>% html_text()

    m = rbind(m, data_frame(blog = i, slug = gsub(i, "", slug), name))
    cat(":", nrow(m), "articles\n")

    j = j + 1
    h = try(html(paste0(i, "page/", j)), silent = TRUE)

  }

  a = unique(rbind(a, m))

  if(nrow(a) > 0)
    write_csv(a, posts)

  cat(which(left == i) - 1, "blogs left\n")

}
