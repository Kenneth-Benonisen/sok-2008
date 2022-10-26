library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(ggrepel)
library(janitor)


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


# Henter data fra SSB via JSON, må hente url til datasette som vi oppsøker.
url <- "https://data.ssb.no/api/v0/no/table/13215/"

# legger inne query for hva vi er ute etter. ######
data <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
          "15-74"
        ]
      }
    },
    {
      "code": "InnvandrKat",
      "selection": {
        "filter": "item",
        "values": [
          "B"
        ]
      }
    },
    {
      "code": "Landbakgrunn",
      "selection": {
        "filter": "item",
        "values": [
          "00",
          "194",
          "015a",
          "100c",
          "694c",
          "400",
          "200b",
          "794a"
        ]
      }
    },
    {
      "code": "NACE2007",
      "selection": {
        "filter": "agg:NACE260InnvGrupp2",
        "values": [
          "SNI-00-99",
          "SNI-01-03",
          "SNI-05-09",
          "SNI-10-33",
          "SNI-35-39",
          "SNI-41-43",
          "SNI-45-47",
          "SNI-49-53",
          "SNI-49.3",
          "SNI-55",
          "SNI-56",
          "SNI-58-63",
          "SNI-64-66",
          "SNI-68-75",
          "SNI-77-82",
          "SNI-78.2",
          "SNI-81.2",
          "SNI-84",
          "SNI-85",
          "SNI-86-88",
          "SNI-90-99",
          "SNI-00"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
          "2021"
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

df <- df %>% 
  clean_names()


df %>%
  mutate("Næring" = naering_sn2007,
         "Innvandring" = value) %>% 
  ggplot(aes(Innvandring, Næring, fill = landbakgrunn)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma, breaks = seq(0,200000, 20000))+
  theme_classic() +
  facet_wrap(~landbakgrunn) +
  theme(legend.position="none")

