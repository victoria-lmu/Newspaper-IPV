###############################
### Inter Coder Reliability ###
###############################

library(tidycomm)

source("setup.R")

IPV_articles <- read_excel("IPV_articles_ICR.xlsx")

IPV_articles[IPV_articles == "NA"] <- NA

# 1. Rename columns for consistency and convert to factor ----
names(IPV_articles) <- c("title", "author", "source", "keywords", "article_type",
                     "extra_infos", "incident_in_title",
                     "ipv_in_text", "ipv_type", "gender_victim",
                     "blame_perp", "blame_victim", "ethn_victim", "ethn_perp",
                     "migr_victim",  "migr_perp", "struct_context", "struct_problem",
                     "episodic_framing", "attack_quarrel", "murder_tragedy",
                     "gender_author", "language_active")
IPV_articles[-c(1:6)] <- lapply(IPV_articles[-c(1:6)], factor)
str(IPV_articles)

articles <- IPV_articles %>%
  select("title", "blame_perp", "blame_victim", "migr_victim",  "migr_perp", "struct_context", "struct_problem",
         "episodic_framing", "attack_quarrel", "murder_tragedy", "language_active")

articles[] <- lapply(articles, function(x) {
  if (is.factor(x)) as.numeric(as.character(x)) else x
})

articles_nas <- articles %>%
  mutate(across(everything(), ~replace(., is.na(.), 3)))

str(articles_nas)

str(articles)

# # Eine ID pro Artikel-Codierung hinzufügen (1 und 2) und Breit pivotieren
# articles_wide <- articles %>%
#   group_by(title) %>%
#   mutate(coder_id = row_number()) %>%
#   ungroup() %>%
#   pivot_wider(names_from = coder_id, values_from = c("blame_perp", "blame_victim", "migr_victim",  "migr_perp", "struct_context", "struct_problem",
#                                                      "episodic_framing", "attack_quarrel", "murder_tragedy", "language_active"))


articles_long_nas <- articles_nas %>%
  group_by(title) %>%
  mutate(coder_id = row_number()) %>%
  ungroup()

articles_long <- articles %>%
  group_by(title) %>%
  mutate(coder_id = row_number()) %>%
  ungroup()

ICR_nas <- test_icr(data = articles_long_nas,
         unit = title,
         coder = coder_id, 
         na.omit = TRUE)

ICR <- test_icr(data = articles_long,
                    unit = title,
                    coder = coder_id, 
                    na.omit = TRUE)

print(ICR_nas)

print(ICR)
