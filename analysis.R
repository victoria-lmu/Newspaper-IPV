
source("setup.R")

articles <- read_excel("IPV_articles_final.xlsx")

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



# 2. Analysis ----

#### 1) Description ----
questions_fac <- sapply(articles, is.factor)[-c(10, 11, 16, 17, 23)]
  # without ipv_type, gender_author, gender_victim, migr_victim, migr_perp for now
articles_fac <- gather(articles, variable, value, names(articles[-c(10, 11, 16, 17, 23)][questions_fac == 1]))
ggplot(articles_fac, aes(x = value)) +
  geom_bar(aes()) +
  facet_wrap(facets = ~variable) +
  labs(title = "Absolute Counts for Categorical Variables")

ggplot(articles_fac, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill",  width = 0.8) +
  coord_flip() +
  labs(y = "Proportion",
       title = "Description for Binarised Questions") +
  scale_fill_manual(values = c( "darkseagreen3","darkseagreen4", "gray90"),
                    labels = c("False", "True", "NA")) +
  scale_x_discrete(labels = rev(c("IPV not addressed as\n structural problem",
                     "Similar past incidents not mentioned",
                     "Murder as 'tragedy'",
                     #"Victim is a migrant",
                     #"Perpetrator is a migrant",
                     "Passive language",
                     "IPV not in title",
                     "IPV not in text",
                     "Incident not in title",
                     "Mention of victim's ethnicity",
                     "Mention of perpetrator's ethnicity",
                     "Incident framed in episodic way",
                     "Victim is blamed",
                     "Perpetrator's behaviour justified\n with psychological state",
                     "Assault/Attack as 'fight'/'quarrel'"))) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
ggsave("description.png", path = "./plots")


##### Fließtext descriptions for source, ethnicity & migr ----

# source
table(articles$source)  # more than half from Bild


# ethn_victim
table(articles$ethn_victim)/nrow(articles)  # only ~22% (28 articles) gave info on victim's ethnicity
# ethn_victim | source
table(articles$source, articles$ethn_victim)/rowSums(table(articles$source, articles$ethn_victim))
  # FAZ & SZ mentioned victim's ethnicity most

# migr_victim | ethn_victim: If ethnicity given, migrant yes/no?
table(articles$ethn_victim, articles$migr_victim)/rowSums(table(articles$ethn_victim, articles$migr_victim))
  # if ethnicity of victim given (in 28 articles), 79% were migrants
# grouped by source:
table(articles[articles$source == "SZ", "ethn_victim", drop = T], articles[articles$source == "SZ", "migr_victim", drop = T])
  # SZ: if ethnicity of victim given (4 articles), all of them were migrants
table(articles[articles$source == "Spiegel", "ethn_victim", drop = T], articles[articles$source == "Spiegel", "migr_victim", drop = T])
  # Spiegel: if ethnicity given (3 articles), in 2 of them victim was migrant
table(articles[articles$source == "FAZ", "ethn_victim", drop = T], articles[articles$source == "FAZ", "migr_victim", drop = T])
  # FAZ: if ethnicity given (6 articles), in 5 of them victim was migrant
table(articles[articles$source == "Bild", "ethn_victim", drop = T], articles[articles$source == "Bild", "migr_victim", drop = T])
  # Bild: if ethnicity given (15 articles), in 11 of them the victim was migrant



# ethn_perp
table(articles$ethn_perp)/nrow(articles)  # ~42% (54 articles) gave info on perpetrator's ethnicity -> almost 2x as much as for victim ethnicity
# ethn_perp | source
table(articles$source, articles$ethn_perp)/rowSums(table(articles$source, articles$ethn_perp))
  # FAZ & SZ mentioned perpetrator's ethnicity most

# migr_perp | ethn_perp: among perpetrators with known ethnicity, how many were migrants?
table(articles$ethn_perp, articles$migr_perp)/rowSums(table(articles$ethn_perp, articles$migr_perp))
  # if their ethnicity was given (second row), they were migrants in 72% of these cases
# grouped by source:
table(articles[articles$source == "SZ", "ethn_perp", drop = T], articles[articles$source == "SZ", "migr_perp", drop = T])
  # SZ: if ethnicity of perp given (7 articles), all of them were migrants
table(articles[articles$source == "Spiegel", "ethn_perp", drop = T], articles[articles$source == "Spiegel", "migr_perp", drop = T])
  # Spiegel: if ethnicity given (5 articles), in 2 of them the perp was migrant
table(articles[articles$source == "FAZ", "ethn_perp", drop = T], articles[articles$source == "FAZ", "migr_perp", drop = T])
  # FAZ: if ethnicity given (10 articles), in 6 of them the perp was migrant
table(articles[articles$source == "Bild", "ethn_perp", drop = T], articles[articles$source == "Bild", "migr_perp", drop = T])
  # Bild: if ethnicity given (32 articles), in 24 of them the perp was migrant



# migr_perp: around half did not give info on ethnicity, so also no info on migration history
# language_passive | migr_perp
table(articles$migr_perp, articles$language_passive)/rowSums(table(articles$migr_perp, articles$language_passive))
  # use of active language in 66% of cases if migrant, compared to 62% if not migrant
# struct_context_no | migr_perp
table(articles$migr_perp, articles$struct_context_no)/rowSums(table(articles$migr_perp, articles$struct_context_no))
  # among cases with migrant offender, 10 PP more articles with no structural context compared to cases with non-migrant offender
  # -> "surprising": means with migrant offender, more individualistic discourse; could have expected more structural/societal
# struct_problem_no | migr_perp
table(articles$migr_perp, articles$struct_problem_no)/rowSums(table(articles$migr_perp, articles$struct_problem_no))
  # about as many articles without structural discourse among migrant and non-migrant offenders
# blame_perp | migr_perp
table(articles$migr_perp, articles$blame_perp)/rowSums(table(articles$migr_perp, articles$blame_perp))
  # about as many for migrant and non-migrant


##### Fließtext descriptions for ipv_type, gender_author, gender_victim bc multi-class ----

# ipv_type: physical (1), psychological (2) or both (3)
table(articles$ipv_type)/nrow(articles)  # no articles with only psychological, 84% on physical -> compare with PKS!

# gender_author
table(articles$gender_author)/nrow(articles) # most male (38%), then female (23%), then AI (20%)

# gender_victim: female (1), male (2), unknown (3), both (4)
table(articles$gender_victim)/nrow(articles) # most are female (93%)
# language_passive | gender_victim: how was language given the victim's gender?
table(articles$gender_victim, articles$language_passive)
# among women (first row) and men (second row), most articles used active language
# contrary for cases where both female and male victims but high uncertainty bc only 3 articles
# blame_victim | gender_victim: how often was victim blamed given their gender?
table(articles$gender_victim, articles$blame_victim)
# only victim blaming in case of female victims (but limited representativeness bc very few on male)



#### 2) Linguistic Framing ----
q2 <- articles %>%
  dplyr::select(incident_not_in_title, ipv_not_in_title, ipv_not_in_text,
                blame_victim, blame_perp,
                attack_quarrel, murder_tragedy, language_passive) %>%
  gather(variable, value)
ggplot(q2, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill",  width = 0.8) +
  coord_flip() +
  labs(y = "Proportion",
       title = "Linguistic Framing") +
  scale_fill_manual(values = c("lightblue", "lightblue4", "gray90"),
                    labels = c("False", "True")) +
  scale_x_discrete(labels = c("murder_tragedy" = "Murder as 'tragedy'",
                              "language_passive" = "Passive Language",
                              "ipv_not_in_title" = "IPV not in title",
                              "ipv_not_in_text" = "IPV not in text",
                              "incident_not_in_title" = "Incident not in title",
                              "blame_victim" = "Victim is blamed",
                              "blame_perp" = "Perpetrator's behaviour justified\n with psychological state",
                              "attack_quarrel" = "Assault/Attack as 'fight'/'quarrel'")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
ggsave("ling_framing.png", path = "./plots", width = 8)

# attack_quarrel
table(articles$attack_quarrel)/nrow(articles) # ~7.7% circumscribed attack as quarrel/argument/disagreement
# blame_perp
table(articles$attack_quarrel)/nrow(articles) # ~10% justified perpetrator's actions with psychological state
# blame_victim
table(articles$blame_victim) # 2.3% with victim blaming (perceived in 3 articles)
# incident_not_in_title
table(articles$incident_not_in_title)/nrow(articles)
  # ~ 13% used titles that article is about a crime ("Der Flug einer Bierflasche"/
  # Marilyn Manson wird nicht angeklagt/Was wäre los, wenn Monsieur Macron das gemacht hätte? )
# ipv_not_in_title
table(articles$ipv_not_in_title)/nrow(articles)
  # almost 50% did not make clear in the title that article is about IPV (by using "IPV" or "Mann tötet Ex-Freundin")
# ipv_not_in_text
  # only ~30% used the explicit term IPV or DV -> 70% rest used milder terminology
# language_passive
table(articles$language_passive) # ~1/3 used passive language
# murder_tragedy
table(articles$murder_tragedy)/nrow(articles) # ~4.6% circumscribed murder as tragedy/relationship drama/drama

# keywords: IPV/DV or milder vocab
table(articles$keywords)
# among words we used to search for IPV articles, no cases with explicit term "IPV"
length(grep("Häusliche Gewalt", articles$keywords))/nrow(articles)  # but 12% that used "Häusliche Gewalt" as proxy
# proportion of cases that were femicides
length(grep("Femizid", articles$keywords))/nrow(articles)  # more than 50%

# by source
q2_faceted <- articles %>%
  select(source,
         incident_not_in_title, ipv_not_in_title, ipv_not_in_text,
         blame_victim, blame_perp,
         attack_quarrel, murder_tragedy, language_passive) %>%
  pivot_longer(cols = -source, names_to = "variable", values_to = "value")
ggplot(q2_faceted, aes(x = variable, fill = factor(value))) +
  geom_bar(position = "fill", width = 0.8) +
  coord_flip() +
  # order so that left-leaning newspapers in left column, right in right
  facet_wrap(~forcats::fct_relevel(source, "SZ", "FAZ", "Spiegel", "Bild")) + 
  labs(y = "Proportion",
       title = "Linguistic Framing by Source") +
  scale_fill_manual(values = c("lightblue", "lightblue4", "gray90"),
                    labels = c("False", "True")) +
  scale_x_discrete(labels = c(
    "murder_tragedy" = "Murder as tragedy",
    "language_passive" = "Passive Language",
    "ipv_not_in_title" = "IPV not in title",
    "ipv_not_in_text" = "IPV not in text",
    "incident_not_in_title" = "Incident not in title",
    "blame_victim" = "Victim is blamed",
    "blame_perp" = "Perpetrator's behaviour justified\nwith psychological state",
    "attack_quarrel" = "Assault/Attack as 'fight'/'quarrel'")) +
  theme(plot.title = element_text(hjust = 0.5, size = 12),
        strip.text = element_text(size = 12),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 9),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
ggsave("ling_framing_faceted.png", path = "./plots", width = 9, height = 5)

# victim blaming only perceived for Bild articles!



#### 3) Contextualisation ----
q3 <- articles %>%
  dplyr::select(struct_context_no, struct_problem_no, episodic_framing) %>%
  gather(variable, value)
ggplot(q3, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill", width = 0.8) +
  labs(y = "Proportion",
       title = "Missing Contextualisation") +
  scale_fill_brewer(palette = 7,
                    labels = c("False", "True")) +
  scale_x_discrete(labels = c(
    "episodic_framing" = "Incident framed in episodic way",
    "struct_context_no" = "Similar past incidents not mentioned",
    "struct_problem_no" = "  IPV not addressed as structural problem")) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        legend.title = element_blank(),
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
ggsave("context.png", path = "./plots", width = 9.5)
# episodic_framing:  ~ 2/3 described incident as single, isolated (no history of incidents within relationship was given)
# struct_context_no: ~77% did not embed the incident in broader context, i.e. did not mention other similar cases
# struct_problem_no: ~10% described incident as structural issue, rest without any discourse on the problem


# by source

# 3 variables -> 3 shades of orange
# within each variable: light = False, dark = True
colour_map <- c(
  "episodic_framing_1" = "#f5deb3",
  "episodic_framing_2"  = "#ffae42",
  "struct_context_no_1" = "#edc9af",
  "struct_context_no_2"  = "#e25822",
  "struct_problem_no_1" = "#ffcba4",
  "struct_problem_no_2"  = "#fd8d3c")

q3_faceted <- articles %>%
  select(source,
         struct_context_no, struct_problem_no, episodic_framing) %>%
  pivot_longer(cols = -source, names_to = "variable", values_to = "value") %>%
  mutate(fill_key = paste(variable, value, sep = "_"))  # combined mapping key (all combos of var + 1/2)

ggplot(q3_faceted, aes(x = variable, fill = fill_key)) +
  geom_bar(position = "fill", width = 0.8) +
  facet_grid(~fct_relevel(source, "SZ", "FAZ", "Spiegel", "Bild")) +
  labs(y = "Proportion",
       title = "Missing Contextualisation by Source") +
  scale_fill_manual(values = colour_map,
                    labels = c("episodic_framing_1" = "No episodic framing",
                               "episodic_framing_2" = "Episodic framing",
                               "struct_context_no_1" = "Similar past incidents mentioned",
                               "struct_context_no_2" = "No similar past incidents mentioned",
                               "struct_problem_no_1" = "IPV addressed as structural problem",
                               "struct_problem_no_2" = "IPV not addressed as structural problem")) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 12),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
ggsave("context_faceted_source.png", path = "./plots")


# by gender_author
q3_faceted2 <- articles %>%
  select(gender_author,
         struct_context_no, struct_problem_no, episodic_framing) %>%
  pivot_longer(cols = -gender_author, names_to = "variable", values_to = "value")  %>%
  mutate(fill_key = paste(variable, value, sep = "_"))  # combined mapping key
ggplot(q3_faceted2, aes(x = variable, fill = fill_key)) +
  geom_bar(position = "fill", width = 0.8) +
  facet_grid(~ gender_author,
             labeller = as_labeller(c("1" = "Female",
                                      "2" = "Male",
                                      "3" = "Both",
                                      "4" = "AI",
                                      "5" = "Press Agency",
                                      "NA" = "Unknown"))) +
  labs(y = "Proportion",
       title = "Missing Contextualisation by Author's Gender") +
  scale_fill_manual(values = colour_map,
                    labels = c("episodic_framing_1" = "No episodic framing",
                               "episodic_framing_2" = "Episodic framing",
                               "struct_context_no_1" = "Similar past incidents mentioned",
                               "struct_context_no_2" = "No similar past incidents mentioned",
                               "struct_problem_no_1" = "IPV addressed as structural problem",
                               "struct_problem_no_2" = "IPV not addressed as structural problem")) +
  theme(plot.title = element_text(hjust = 0.5),
        strip.text = element_text(size = 9),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        panel.grid.major = element_line(colour = "lightgrey"),
        panel.grid.minor = element_line(colour = "lightgrey", linetype = "dashed"))
ggsave("context_faceted_author.png", path = "./plots")



