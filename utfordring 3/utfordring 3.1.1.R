library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(ggrepel)


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


# Henter data fra SSB via JSON, må hente url til datasette som vi oppsøker.
url <- "https://data.ssb.no/api/v0/no/table/11155"

# legger inne query for hva vi er ute etter. 
data <- '
{
  "query": [
    {
      "code": "Kjonn",
      "selection": {
        "filter": "item",
        "values": [
          "0",
          "1",
          "2"
        ]
      }
    },
    {
      "code": "Alder",
      "selection": {
        "filter": "item",
        "values": [
         "15-74",
         "20-64",
         "20-66",
         "15-24",
         "25-39",
         "40-54",
         "55-74"
        ]
      }
    },
    {
      "code": "UtdNivaa",
      "selection": {
        "filter": "item",
        "values": [
          "TOT",
          "1-2",
          "3-5",
          "6-8"
        ]
      }
    },
    {
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Arbeidsledige",
          "ArbLedigProsent"
        ]
      }
    },
    {
      "code": "Tid",
      "selection": {
        "filter": "item",
        "values": [
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
         "2020"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'

# henter selve dataen fra SSB 
d.tmp <- POST(url, body = data, encode = "json", verbose())

# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat
df <- fromJSONstat(content(d.tmp, "text"))

# fjerner uønsket rekker
df <- df %>% 
  filter(statistikkvariabel != "Arbeidsledige (1 000 personer)")


# Lag en graf som viser arbeidsløsheten i prosent blant alder 15-24 og 20-64.
df %>%
  filter(alder == c("15-24 år"), kjønn == "Begge kjønn") %>% 
  ggplot(aes(år, value, group = utdanningsnivå, col = utdanningsnivå)) +
  geom_line(aes(group = utdanningsnivå),size = 1) +
  geom_point(size = 2.5) +
  labs(title = "Arbeidsledighet 15-24 år basert på utdanningsnivå",
       x = "",
       y = "% Arbeidsledighet") +
  theme_classic() +
  theme(legend.position = "bottom")


# samme graf bare på tvers av utdanningsnivå, alder 20-64. 
df %>%
  filter(alder == c("20-64 år"), kjønn == "Begge kjønn") %>% 
  ggplot(aes(år, value, group = utdanningsnivå, col = utdanningsnivå)) +
  geom_line(aes(group = utdanningsnivå),size = 1) +
  geom_point(size = 2.5) +
  labs(title = "Arbeidsledighet 20-64 år basert på utdanningsnivå",
       x = "",
       y = "% Arbeidsledighet") +
  theme_classic() +
  theme(legend.position = "bottom")











# test - alternativ plot

df %>%
  filter(alder == c("15-24 år"), kjønn == "Begge kjønn") %>% 
  ggplot(aes(år, value, group = utdanningsnivå, col = utdanningsnivå)) +
  geom_line(aes(group = utdanningsnivå),size = 1) +
  geom_point(size = 2.5) +
  labs(title = "Arbeidsledighet 15-24 år basert på utdanningsnivå",
       x = "",
       y = "% Arbeidsledighet") +
  theme_classic() +
  theme(legend.position="none") +
  geom_text_repel(data = df %>%
      group_by(utdanningsnivå) %>% 
      filter(value == max(value)), aes(x = "2020", y = c(11.5,15,9,9.5), label = sprintf("%s", utdanningsnivå)),
      color = "black"
      )
  
  



