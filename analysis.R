
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
questions_fac <- sapply(articles, is.factor)[-c(10, 11, 23)]  # without ipv_type, gender_author, gender_victim for now
articles_fac <- gather(articles, variable, value, names(articles[-c(10, 11, 23)][questions_fac == 1]))
ggplot(articles_fac, aes(x = value)) +
  geom_bar(aes()) +
  facet_wrap(facets = ~variable) +
  labs(title = "Absolute Counts for Categorical Variables")

ggplot(articles_fac, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(y = "Proportion",
       title = "Description for Binarised Questions") +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank()) +
  scale_fill_brewer(palette = 2, direction = 1,
                    labels = c("False", "True", "NA"))

# ethn_victim
table(articles$ethn_victim)/nrow(articles)  # only ~22% gave info on victim's ethnicity

# ethn_perp
table(articles$ethn_perp)/nrow(articles)  # ~42% gave info on perpetrator's ethnicity
# migr_perp | ethn_perp: among perpetrators with known ethnicity, how many were migrants?
table(articles$ethn_perp, articles$migr_perp)/rowSums(table(articles$ethn_perp, articles$migr_perp))
  # if their ethnicity was known (second row), they were migrants in 72% of these cases

# migr_perp: around half did not give info on ethnicity, so also no info on migration history
# language_passive | migr_perp
table(articles$migr_perp, articles$language_passive)/rowSums(table(articles$migr_perp, articles$language_passive))
  # use of active language in 66% of cases if migrant, compared to 62% if not migrant
# struct_context_no | migr_perp
table(articles$migr_perp, articles$struct_context_no)/rowSums(table(articles$migr_perp, articles$struct_context_no))
  # among cases with migrant offender, more articles with no structural context compared to cases with non-migrant offender
# struct_problem_no | migr_perp
table(articles$migr_perp, articles$struct_problem_no)
  # about as many articles without structural discourse among migrant and non-migrant offenders
# blame_perp | migr_perp
table(articles$migr_perp, articles$blame_perp)/rowSums(table(articles$migr_perp, articles$blame_perp))
# about as many for migrant and non-migrant

# source
table(articles$source)  # more than half from Bild
# ipv_not_in_text | source
table(articles$source, articles$ipv_not_in_text)
  # most balanced for SZ, least for Bild which did not use term "IPV/DV" often
# episodic_framing | source
table(articles$source, articles$episodic_framing)/rowSums(table(articles$source, articles$episodic_framing))
  # Bild used episodic framing the most, but overall similar proportions
# language_passive | source: for each newspaper, how often used passive/active language?
table(articles$source, articles$language_passive)/rowSums(table(articles$source, articles$language_passive))
  # Spiegel used passive language (=2) the most, Bild the least
# ethn_victim | source
table(articles$source, articles$ethn_victim)/rowSums(table(articles$source, articles$ethn_victim))
  # proportionally, FAZ mentioned victim's ethnicity most
# ethn_perp | source
table(articles$source, articles$ethn_perp)/rowSums(table(articles$source, articles$ethn_perp))
  # FAZ and SZ mentioned perpetrator's ethnicity most
# blame_victim | source
table(articles$source, articles$blame_victim)  
  # victim blaming only perceived for Bild articles


##### Fließtext descriptions for ipv_type, gender_author, gender_victim bc multi-class ----

# ipv_type: physical (1), psychological (2) or both (3)
table(articles$ipv_type)/nrow(articles)  # no articles with only psychological, 84% on physical

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
  geom_bar(position = "fill") +
  coord_flip() +
  labs(y = "Proportion",
       title = "Linguistic Framing") +
  scale_fill_brewer(palette = 3,
                    labels = c("False", "True")) +
  theme(axis.title.y = element_blank(),
        legend.title = element_blank())
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



#### 3) Contextualisation ----
q3 <- articles %>%
  dplyr::select(struct_context_no, struct_problem_no, episodic_framing) %>%
  gather(variable, value)
ggplot(q3, aes(x = factor(variable), fill = factor(value))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion",
       title = "Missing Contextualisation") +
  scale_fill_brewer(palette = 7,
                    labels = c("False", "True")) +
  theme(axis.title.x = element_blank(),
        legend.title = element_blank())
# episodic_framing:  ~ 2/3 described incident as single, isolated (no history of incidents within relationship was given)
# struct_context_no: ~77% did not embed the incident in broader context, i.e. did not mention other similar cases
# struct_problem_no: ~10% described incident as structural issue, rest without any discourse on the problem



