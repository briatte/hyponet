#
# 6. model the corpus via a structural topic model
#

library(dplyr)
library(readr)

library(ggplot2)
library(RColorBrewer)

library(GGally)
library(igraph)
library(intergraph)
library(network)

library(stm)

# library(stmBrowser)
# library(stmCorrViz)

text = read_csv("data/corpus.csv")

txt = textProcessor(text$text, metadata = text, language = "fr")
txt = prepDocuments(txt$documents, txt$vocab, txt$meta)

# find_k = selectModel(txt$documents, txt$vocab, data = txt$meta,
#                      prevalence = ~ year + oc, K = 10, runs = 20)

# find_k = searchK(txt$documents, txt$vocab, data = txt$meta,
#                  prevalence = ~ year + oc, K = seq(10, 100, 10))

stm = stm(txt$documents, txt$vocab, data = txt$meta,
          K = 20, prevalence = ~ year + oc,
          max.em.its = 100, seed = 3007)

save(text, txt, stm, file = "model/topics.rda")

# stmCorrViz(stm, "corrviz.html")
# stmBrowser(stm, data = txt$meta, covariates = "year", text = "text")

# topic expected proportions

lab = labelTopics(stm, topics = 1:20, n = 3, frexweight = .5)[[ "prob" ]] %>%
  apply(., 1, function(x) paste(x[ nchar(x) > 0 ], collapse = ", ")) %>%
  data_frame(lab = ., topics = 1:20, freq = colMeans(stm$theta[ , topics ]))

qplot(data = lab, y = reorder(topics, freq), yend = reorder(topics, freq),
      xend = 0, x = freq, geom = "segment") +
  geom_text(aes(x = freq + 0.005, label = lab, hjust = 0)) +
  xlim(0, max(lab$freq) * 2) +
  labs(y = "Topic\n", x = "\nExpected proportion") +
  theme_bw(14) +
  theme(panel.grid = element_blank())

ggsave("hyponet_topics_proportions.png", width = 9, height = 7)
ggsave("hyponet_topics_proportions.pdf", width = 9, height = 7)

# topic correlation network

tcn = topicCorr(stm)
tcn = network(tcn$posadj, directed = FALSE)
network.vertex.names(tcn) = gsub(", ", "\n", lab$lab)

tcn %v% "oc" = as.character(membership(optimal.community(asIgraph(tcn))))

colors = sort(table(tcn %v% "oc"), decreasing = TRUE)
x = ifelse(length(colors) > 7, 8, length(colors))
colors[ 1:x ] = brewer.pal(x, "Set1")
colors[ nchar(colors) < 7 ] = "#AAAAAA"

ggnet(tcn, size = 0, node.group = tcn %v% "oc") +
  geom_text(aes(label = network.vertex.names(tcn), color = tcn %v% "oc",
                size = igraph::degree(asIgraph(tcn)))) +
  geom_text(aes(label = network.vertex.names(tcn),
                size = igraph::degree(asIgraph(tcn))),
            color = "black", alpha = .5) +
  scale_size_area("", max_size = 6) +
  scale_color_manual("", values = colors) +
  guides(color = FALSE, size = FALSE) +
  ggtitle(paste("Hypothesesosphère 2009-2015:", network.size(tcn), "thèmes\n"))

ggsave("hyponet_topics_network.png", width = 7, height = 7)
ggsave("hyponet_topics_network.pdf", width = 7, height = 7)
