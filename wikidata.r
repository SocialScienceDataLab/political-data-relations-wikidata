library("tidywikidatar")
library(dplyr)

mep_query <- tw_query(query = c(p = "P39", q = "Q27169"))
head(mep_query)

mep_query2 <- WikidataQueryServiceR::query_wikidata('SELECT ?item  ?itemLabel ?itemDescription
WHERE 
{ 
  ?item wdt:P39 wd:Q27169.
SERVICE wikibase:label { bd:serviceParam wikibase:language "en". }
}
')
head(mep_query2)

# complex query

complex_query <- WikidataQueryServiceR::query_wikidata('SELECT DISTINCT ?constituencyLabel ?partyLabel ?start ?electionLabel ?end ?causeLabel {
 wd:Q8016 p:P39 ?positionStatement . # all positions held by this person
  ?positionStatement ps:P39 [wdt:P279* wd:Q16707842] . # filter to positions which are a subclass of UK MP
 OPTIONAL { ?positionStatement pq:P768 ?constituency . }  # then find various specific values for each term
 OPTIONAL { ?positionStatement pq:P4100 ?party . }
 OPTIONAL { ?positionStatement pq:P580 ?start . }
 OPTIONAL { ?positionStatement pq:P2715 ?election . }
 OPTIONAL { ?positionStatement pq:P582 ?end . }
 OPTIONAL { ?positionStatement pq:P1534 ?cause . }
 SERVICE wikibase:label { bd:serviceParam wikibase:language "en" }
}
ORDER BY ?start')


mep_de_df <- tw_get_wikipedia_page_links(url = "https://en.wikipedia.org/wiki/List_of_members_of_the_European_Parliament_for_Germany,_2019-2024")
# columns in dataset
colnames(mep_de_df)
# titles
sample(mep_de_df$wikipedia_title,10)
# filter to MEPs - combine with previous query
mep_de_df <- mep_de_df %>% filter(qid %in% mep_query$id)

sample(mep_de_df$wikipedia_title,10)
mep_de_df <- mep_de_df %>% pull(qid) %>%
    tw_get_property(p = "P31")  %>% # instance of
    filter(value == "Q5") # human
mep_de_props <- mep_de_df$qid %>%
  tw_get()

mep_de_props
properties <- mep_de_props %>% 
  group_by(property) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  mutate(label=tw_get_property_label(property))
properties %>% head(20)

mep_de_props %>% 
  # filter: mastodon ID property
  filter(property=="P4033")

council_df <- tw_get_wikipedia_page_links(
  url = "https://en.wikipedia.org/wiki/List_of_members_of_the_European_Council")

# filtering to meaningful links
council_members <- council_df %>%
  pull(qid) %>%
  tw_get_property(p = "P31")  %>% # instance of
  filter(value == "Q5") # human
judges <- tw_query(query=c(p="P39",q="Q43575168"))

judges_props <-  judges %>%
  pull(id) %>%
  tw_get()

properties <- judges_props %>% 
  group_by(property) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  mutate(label=tw_get_property_label(property))
properties %>% head(20)

supreme_court <- tw_query(query=c(p="P39",q="Q11144"))


judges_props <-  supreme_court %>%
  pull(id) %>%
  tw_get()

properties <- judges_props %>% 
  group_by(property) %>% 
  tally() %>% 
  arrange(desc(n)) %>%
  mutate(label=tw_get_property_label(property))
properties %>% head(20)
