#
# 6. model the corpus via a structural topic model
#

library(dplyr)
library(readr)

library(stm)

# library(stmBrowser)
# library(stmCorrViz)

text = read_csv("data/corpus.csv")

txt = textProcessor(text$text, metadata = text, language = "fr")
txt = prepDocuments(txt$documents, txt$vocab, txt$meta)

stm = stm(txt$documents, txt$vocab, data = txt$meta,
          K = 20, prevalence = ~ year + oc,
          max.em.its = 100, seed = 3007)

save(text, txt, stm, file = "model/topics.rda")

# stmCorrViz(stm, "corrviz.html")
# stmBrowser(stm, data = txt$meta, covariates = "year", text = "text")

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

ggsave("hypotopics.png", width = 9, height = 7)
