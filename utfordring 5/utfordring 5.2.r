
# Benyttet pakker.
library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(ggrepel)
library(janitor)

# Kode for a kunne bruke norske bokstaver.
Sys.setlocale(locale="no_NO")

# Henter data fra SSB via JSON, må hente url til datasette som vi oppsøker.
url <- "https://data.ssb.no/api/v0/no/table/13215/"

# Legger inn query, for oppsøkt data. ######
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

# Henter selve dataen fra SSB.
d.tmp <- POST(url, body = data, encode = "json", verbose())

# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat.
df <- fromJSONstat(content(d.tmp, "text"))

# Korrigerer kolonnene slik at det er enklere å benytte. 
df <- df %>% 
  clean_names()

# Oppretter ny kolonne, hvor vi rydder i navnene som plotter y-aksen i grafen.
df <- df %>% 
  mutate(Næringer = case_when(
    naering_sn2007 == "00-99 ... Alle næringer" ~ "Alle næringer",
    naering_sn2007 == "00 ... Uoppgitt" ~ "Uoppgitt",
    naering_sn2007 == "01-03 ... Jordbruk, skogbruk og fiske" ~ "Jordbruk, skogbruk og fiske",
    naering_sn2007 == "05-09 ... Bergverksdrift og utvinning" ~ "Bergverksdrift og utvinning",
    naering_sn2007 == "10-33 ... Industri" ~ "Industri",
    naering_sn2007 == "35-39 ... Elektrisitet, vann og renovasjon" ~ "Elektrisitet, vann og renovasjon",
    naering_sn2007 == "41-43 ... Bygge- og anleggsvirksomhet" ~ "Bygge- og anleggsvirksomhet",
    naering_sn2007 == "45-47 ... Varehandel, motorvognreparasjoner" ~ "Varehandel, motorvognreparasjoner",
    naering_sn2007 == "49-53 ... Transport og lagring" ~ "Transport og lagring",
    naering_sn2007 == "49.3 ... Annen landtransport med passasjerer" ~ "Annen landtransport med passasjerer",
    naering_sn2007 == "55 ... Overnattingsvirksomhet" ~ "Overnattingsvirksomhet",
    naering_sn2007 == "56 ... Serveringsvirksomhet" ~ "Serveringsvirksomhet",
    naering_sn2007 == "58-63 ... Informasjon og kommunikasjon" ~ "Informasjon og kommunikasjon",
    naering_sn2007 == "64-66 ... Finansiering og forsikring" ~ "Finansiering og forsikring",
    naering_sn2007 == "68-75 ... Teknisk tjenesteyting, eiendomsdrift" ~ "Teknisk tjenesteyting, eiendomsdrift",
    naering_sn2007 == "77-82 ... Forretningsmessig tjenesteyting" ~ "Forretningsmessig tjenesteyting",
    naering_sn2007 == "78.2 ... Utleie av arbeidskraft" ~ "Utleie av arbeidskraft",
    naering_sn2007 == "81.2 ... Rengjøringsvirksomhet" ~ "Rengjøringsvirksomhet",
    naering_sn2007 == "84 ... Offentlig administrasjon, forsvar, sosialforsikring" ~ "Offentlig administrasjon, forsvar, sosialforsikring",
    naering_sn2007 == "85 ... Undervisning" ~ "Undervisning",
    naering_sn2007 == "86-88 ... Helse- og sosialtjenester" ~ "Helse- og sosialtjenester",
    naering_sn2007 == "90-99 ... Personlig tjenesteyting" ~ "Personlig tjenesteyting"
  ))

# Plotter alt.
df %>%
  mutate("Innvandring" = value) %>% 
  ggplot(aes(Innvandring, Næringer, fill = landbakgrunn)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma, breaks = seq(0,200000, 20000))+
  theme_classic() +
  facet_wrap(~landbakgrunn) +
  theme(legend.position="none")

# Reduserer plottet for å få bedre nyanser mellom yrkene.
df %>%
  mutate("Innvandring" = value) %>% 
  ggplot(aes(Innvandring, Næringer, fill = landbakgrunn)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma, breaks = seq(0, 200000, 10000))+
  theme_classic() +
  facet_wrap(~landbakgrunn) +
  theme(legend.position="none") +
  coord_cartesian(xlim = c(0,35000)) # zoomer kun i plottet og kutter ikke ut noen av verdiene.

# Filtrerer bort "alle næringer" fra df.
df %>%
  mutate("Innvandring" = value) %>%
  filter(Næringer != "Alle næringer") %>% 
  ggplot(aes(Innvandring, Næringer, fill = landbakgrunn)) +
  geom_col() +
  scale_x_continuous(labels = scales::comma, breaks = seq(0,40000, 5000)) +
  theme_classic() +
  facet_wrap(~landbakgrunn) +
  theme(legend.position="none")


