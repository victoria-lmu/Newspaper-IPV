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


# murder_tragedy, gender_author

table(articles$gender_author, articles$murder_tragedy)/rowSums(table(articles$gender_author, articles$murder_tragedy))

# attack_quarrel, gender_author

table(articles$gender_author, articles$attack_quarrel)/rowSums(table(articles$gender_author, articles$attack_quarrel))

table(articles$gender_author, articles$language_passive)/rowSums(table(articles$gender_author, articles$language_passive))


ipv_text_gender <- table(articles$gender_author, articles$ipv_not_in_text)/rowSums(table(articles$gender_author, articles$attack_quarrel))


print(xtable(ipv_text_gender), include.rownames = FALSE)


q2_faceted <- articles %>%
  select(gender_author,
         incident_not_in_title, ipv_not_in_title, ipv_not_in_text,
         blame_victim, blame_perp,
         attack_quarrel, murder_tragedy, language_passive) %>%
  pivot_longer(cols = -gender_author, names_to = "variable", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("language_passive",
                                                "blame_perp",
                                                "blame_victim",
                                                "incident_not_in_title",
                                                "ipv_not_in_title",
                                                "ipv_not_in_text",
                                                "murder_tragedy",
                                                "attack_quarrel")))
ggplot(q2_faceted, aes(x = variable, fill = factor(value))) +
  geom_bar(position = "fill", width = 0.8) +
  coord_flip() +
  # order so that left-leaning newspapers in left column, right in right
  facet_wrap(~forcats::fct_relevel(gender_author, "Female", "Male", "Both", "AI", "Press Agency", "No Author found")) + 
  labs(y = "Proportion",
       title = "Linguistic Framing by gender") +
  scale_fill_manual(values = c("lightblue", "lightblue4", "gray90"),
                    labels = c("False", "True")) +
  scale_x_discrete(labels = c(
    "murder_tragedy" = "Murder as 'tragedy'",
    "language_passive" = "Passive Language",
    "ipv_not_in_title" = "IPV not in title",
    "ipv_not_in_text" = "IPV not in text",
    "incident_not_in_title" = "Incident not in title",
    "blame_victim" = "Victim is blamed",
    "blame_perp" = "Perpetrator's behaviour justified\nwith psychological state",
    "attack_quarrel" = "Assault/Attack as 'fight'/'quarrel'")) +
  theme(plot.title = element_blank(),
        strip.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
