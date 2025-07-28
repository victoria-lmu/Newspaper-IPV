
source("setup.R")

articles <- read_excel("IPV_articles_final.xlsx")

# 1. Rename columns for consistency and convert to factor ----
names(articles) <- c("title", "author", "source", "keywords", "article_type",
                     "extra_infos", "incident_in_title", "ipv_in_title",
                     "ipv_in_text", "ipv_type", "gender_victim",
                     "blame_perp", "blame_victim", "ethn_victim", "ethn_perp",
                     "migr_victim",  "migr_perp", "struct_context", "struct_problem",
                     "episodic_framing", "attack_quarrel", "murder_tragedy",
                     "gender_author", "language_active")
articles[-c(1:6)] <- lapply(articles[-c(1:6)], factor)
str(articles)
# for binary variables, value 1 refers to "No", value 2 to "Yes"
# if question not applicable to a case, use "NA"



# 2. Analysis ----

#### 1) Downplaying ----
q1 <- articles %>%
  dplyr::select(blame_victim, blame_perp, attack_quarrel, murder_tragedy, language_active) %>%
  gather(variable, value)
ggplot(q1, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion") +
  scale_fill_brewer(palette = 3)
# attack_quarrel
table(articles$attack_quarrel)/nrow(articles) # ~7.7% circumscribed attack as quarrel/argument/disagreement
# blame_perp
table(articles$attack_quarrel)/nrow(articles) # ~10% justified perpetrator's actions with psychological state
# blame_victim
table(articles$blame_victim) # 2.3% with victim blaming (perceived in 3 articles)
# language_active
table(articles$language_active) # ~2/3 used active language and 1/3 passive
# murder_tragedy
table(articles$murder_tragedy)/nrow(articles) # ~4.6% circumscribed murder as tragedy/relationship drama/drama

# keywords: IPV/DV or milder vocab
table(articles$keywords)
# among words we used to search for IPV articles, no cases with explicit term "IPV"
length(grep("Häusliche Gewalt", articles$keywords))/nrow(articles)  # but 12% that used "Häusliche Gewalt" as proxy
# proportion of cases that were femicides
length(grep("Femizid", articles$keywords))/nrow(articles)  # more than 50%



#### 2) Awareness raised to incident as structural issue ----
q2 <- articles %>%
  dplyr::select(ipv_in_text, struct_context, struct_problem, episodic_framing) %>%
  gather(variable, value)
ggplot(q2, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion") +
  scale_fill_brewer(palette = 3)
# episodic_framing:  ~ 2/3 described incident as single, isolated (no history of incidents within relationship was given)
# ipv_in_text: only ~30% used the explicit term IPV or DV -> rest used milder terminology
# struct_context: ~23% embedded the incident in broader context, i.e. mentioned other similar cases
# struct_problem: ~10% described incident as structural issue, rest without any discourse on the problem



#### 3) Description ----
questions_fac <- sapply(articles, is.factor)
articles_fac <- gather(articles, variable, value, names(articles[questions_fac == 1]))
ggplot(articles_fac, aes(x = value)) +
  geom_bar(aes()) +
  facet_wrap(facets = ~variable) +
  labs(title = "Absolute counts for categories")

ggplot(articles_fac, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(y = "Proportion",
       title = "Proportions of categories") +
  theme(axis.text.x = element_text(angle = 90)) +
  scale_fill_brewer(palette = 3)


# ipv_type: physical (1), psychological (2) or both (3)
table(articles$ipv_type)/nrow(articles)  # no articles with only psychological, 84% on physical

# gender_author
table(articles$gender_author)/nrow(articles) # most male (38%), then female (23%), then AI (20%)

# gender_victim: female (1), male (2), unknown (3), both (4)
table(articles$gender_victim)/nrow(articles) # most are female (93%)
# language_active | gender_victim: how was language given the victim's gender?
table(articles$gender_victim, articles$language_active)
  # among women (first row) and men (second row), most articles used active language
  # contrary for cases where both female and male victims but high uncertainty bc only 3 articles
# blame_victim | gender_victim: how often was victim blamed given their gender?
table(articles$gender_victim, articles$blame_victim)
  # only victim blaming in case of female victims (but only limited representativeness bc very few on male)

# ethn_victim
table(articles$ethn_victim)/nrow(articles)  # only ~20% gave info on victim's ethnicity

# ethn_perp
table(articles$ethn_perp)/nrow(articles)  # ~40% gave info on perpetrator's ethnicity
# migr_perp | ethn_perp: among perpetrators with known ethnicity, how many were migrants?
table(articles$ethn_perp, articles$migr_perp)/rowSums(table(articles$ethn_perp, articles$migr_perp))
  # if their ethnicity was known (second row), they were migrants in 72% of these cases

# migr_perp: around half did not give info on ethnicity, so also no info on migration history
# language_active | migr_perp
table(articles$migr_perp, articles$language_active)/rowSums(table(articles$migr_perp, articles$language_active))
  # use of active language in 66% of cases if migration history, compared to 62% if not migrant
# struct_context | migr_perp
table(articles$migr_perp, articles$struct_context)/rowSums(table(articles$migr_perp, articles$struct_context))
  # among cases with non-migrant offender, ~30% were placed in structural context
# struct_problem | migr_perp
table(articles$migr_perp, articles$struct_problem)
  # about as many articles with structural discourse among migrant and non-migrant offenders
# blame_perp | migr_perp
table(articles$migr_perp, articles$blame_perp)/rowSums(table(articles$migr_perp, articles$blame_perp))
  # used childhood/mental health to justify crime in 8% of cases for non-migrant, in 10% for migrant

# source
table(articles$source)  # more than half from Bild
# ipv_in_text | source: for each newspaper, how often explicit use of term "IPV"/"DV"?
table(articles$source, articles$ipv_in_text)
  # most balanced for SZ, least for Bild
# language_active | source: for each newspaper, how often used passive/active language?
table(articles$source, articles$language_active)/rowSums(table(articles$source, articles$language_active))
  # Spiegel used passive language (=1) the most, Bild the least
# ethn_victim | source
table(articles$source, articles$ethn_victim)/rowSums(table(articles$source, articles$ethn_victim))
  # proportionally, FAZ mentioned victim's ethnicity most
# ethn_perp | source
table(articles$source, articles$ethn_perp)/rowSums(table(articles$source, articles$ethn_perp))
  # FAZ, SZ mentioned perpetrator's ethnicity most
# blame_victim | source
table(articles$source, articles$blame_victim)  
  # victim blaming only perceived for Bild articles

      