library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(ggrepel)



# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


# Henter data fra SSB via JSON, må hente url til datasette som vi oppsøker.
url <- "https://data.ssb.no/api/v0/no/table/05185/"

# legger inne query for hva vi er ute etter. ######
data <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "agg:Verdensdel2",
        "values": [
          "b11",
          "b12",
          "b13",
          "b14",
          "b2",
          "b3",
          "b4",
          "b5",
          "b6"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2005",
          "2006",
          "2007",
          "2008",
          "2009",
          "2010",
          "2011",
          "2012",
          "2013",
          "2014",
          "2015",
          "2016",
          "2017",
          "2018",
          "2019",
          "2020",
          "2021",
          "2022"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'
#####


# henter selve dataen fra SSB 
d.tmp <- POST(url, body = data, encode = "json", verbose())

# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
df <- fromJSONstat(content(d.tmp, "text"))

# df_label <- df %>%
#   group_by(landbakgrunn) %>%
#   filter(kjønn == "Kvinner") %>%
#   top_n(1, år)
# 
# 
# df %>%
#   filter(kjønn == "Kvinner") %>%
#   ggplot(aes(år, value, group = landbakgrunn, col = landbakgrunn)) +
#   geom_line(aes(group = landbakgrunn),size = 1) +
#   #geom_point(size = 2.5) +
#   labs(title = "Kvinnfolk innvandring",
#        x = "",
#        y = "antall innvandrere") +
#   scale_y_continuous(labels = scales::comma, breaks = seq(0,180000, 20000)) +
#   theme_classic() +
#   geom_text_repel(aes(label = landbakgrunn),
#                   data = df_label,
#                   nudge_x = 1.5,
#                   nudge_y = 1,
#                   na.rm = T) +
#   theme(legend.position="none")




# summerer value fra begge kjønn til ett, og filtrer bort det ene kjønne for å unngå dublikering av labels. 
df <- df %>%
  group_by(landbakgrunn, år) %>% 
  mutate(total_innvandring = sum(value)) %>% 
  filter(kjønn == "Menn")

# oppretter labels til hver inndeling. 
df_label <- df %>%
  group_by(landbakgrunn) %>%
  top_n(1, år)

# Plotter innvandring. 
df %>%
  ggplot(aes(år, total_innvandring, group = landbakgrunn, col = landbakgrunn)) +
  geom_line(aes(group = landbakgrunn),size = 1) +
  #geom_point(size = 2.5) +
  labs(title = "Total innvandring til Norge",
       x = "",
       y = "Antall innvandrere") +
  scale_y_continuous(labels = scales::comma, breaks = seq(0,300000, 20000)) +
  theme_classic() +
  geom_text_repel(aes(label = landbakgrunn),
                  data = df_label,
                  nudge_x = 1.5,
                  nudge_y = 1,
                  na.rm = T) +
  theme(legend.position="none")
















