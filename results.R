source("setup.R")

articles <- read_excel("IPV_articles_final.xlsx")

articles[articles == "NA"] <- NA

# 1. Rename columns for consistency and convert to factor ----
names(articles) <- c("title", "author", "source", "keywords", "article_type",
                     "extra_infos", "incident_not_in_title", "ipv_not_in_title",
                     "ipv_not_in_text", "ipv_type", "gender_victim",
                     "blame_perp", "blame_victim", "ethn_victim", "ethn_perp",
                     "migr_victim",  "migr_perp",
                     "struct_context_no", "struct_problem_no",
                     "episodic_framing", "attack_quarrel", "murder_tragedy",
                     "gender_author", "language_passive")
articles[-c(1:6)] <- lapply(articles[-c(1:6)], factor)
str(articles)
# for binary variables, value 1 refers to "No", value 2 to "Yes"
# if question not applicable to a case, use "NA"

articles <- articles %>%
  mutate(
    ethn_migr_victim = case_when(
      ethn_victim == 1 & (migr_victim == 1 | is.na(migr_victim)) ~ 1,
      ethn_victim == 2 & migr_victim == 2 ~ 2,
      ethn_victim == 2 & migr_victim == 1 ~ 3,
      TRUE ~ NA_real_
    ), 
    ethn_migr_perp = case_when(
      ethn_perp == 1 & (migr_perp == 1 | is.na(migr_perp)) ~ 1,
      ethn_perp == 2 & migr_perp == 2 ~ 2,
      ethn_perp == 2 & migr_perp == 1 ~ 3,
      TRUE ~ NA_real_
    )) %>%
  select(!c(ethn_perp, ethn_victim, migr_perp, migr_victim))
  #   ethn_migr_victim = factor(ethn_migr_victim,
  #                     levels = c(1, 2, 3),
  #                     labels = c("No ethnicity mentioned",
  #                                "Migration background",
  #                                "No migration background"))
  # )


