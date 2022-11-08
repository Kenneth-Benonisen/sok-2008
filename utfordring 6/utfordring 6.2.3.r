# pakker
library(rjstat)
library(httr)
library(PxWebApiData)
library(tidyverse)
library(janitor)




# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


# Henter data fra SSB via JSON, må hente url til datasette som vi oppsøker.
# tabell 12441 - sykefravær for lønnstakere (prosent).
url <- "https://data.ssb.no/api/v0/no/table/12441/"


# tabell 05111 - arbeidsledige (i prosent) fordelt på kjønn.
url_2 <- "https://data.ssb.no/api/v0/no/table/05111/" 


# legger inne query for hva vi er ute etter. ######
# Tabell 12411
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
      "code": "NACE2007",
      "selection": {
        "filter": "item",
        "values": [
          "00-99"
        ]
      }
    },
    {
      "code": "Sykefraver2",
      "selection": {
        "filter": "item",
        "values": [
          "Alt"
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
          "2019"
        ]
      }
    }
  ],
  "response": {
    "format": "json-stat2"
  }
}
'


# for tabell 05111
data_2 <- '
{
  "query": [
    {
      "code": "ArbStyrkStatus",
      "selection": {
        "filter": "item",
        "values": [
          "2"
        ]
      }
    },
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
      "code": "ContentsCode",
      "selection": {
        "filter": "item",
        "values": [
          "Prosent"
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
          "2019"
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
# For tabell 12441.
d.tmp <- POST(url, body = data, encode = "json", verbose())

# For tabell 05111.
d.tmp_2 <- POST(url_2, body = data_2, encode = "json", verbose())



# Henter ut innholdet fra d.tmp som tekst deretter bearbeides av fromJSONstat.
# For tabell 12411.
df_syk <- fromJSONstat(content(d.tmp, "text"))

# For tabell 05111.
df_arb_ledig <- fromJSONstat(content(d.tmp_2, "text"))


# Cleaning names.
df_syk <- df_syk %>% 
  clean_names()

df_arb_ledig <- df_arb_ledig %>% 
  clean_names()

# Bytter navn på variabel
names(df_syk)[names(df_syk) == "naering_sn2007"] <- "næring"
names(df_syk)[names(df_syk) == "statistikkvariabel"] <- "variabel_syk"
names(df_syk)[names(df_syk) == "value"] <- "value_syk"
names(df_arb_ledig)[names(df_arb_ledig) == "value"] <- "value_arb_ledig"


# Sammensveiser dataframes. 
df_combined <- left_join(df_syk, df_arb_ledig, by=c("kjonn", "ar")) %>% 
  select(kjonn, ar, everything())



# Oppretter plotter. 

fravaer_kvinner_syk <- df_combined %>%
  filter(kjonn == "Kvinner") %>% 
  ggplot(aes(x = ar, value_syk, fill = value_syk)) +
  geom_col() +
  theme_classic() +
  labs(
    title = "Sykefravær blant kvinner",
    y = "Sykefravær",
    x="") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_fill_gradient(low="grey", high="red") +
  theme(legend.position = "none")

fravaer_kvinner_arb <- df_combined %>%
  filter(kjonn == "Kvinner") %>% 
  ggplot(aes(x = ar, value_arb_ledig, fill = value_arb_ledig)) +
  geom_col() +
  theme_classic() +
  labs(
    title = "Arbeidsledighet blant kvinner",
    y = "Arbeidsledighet",
    x="") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_fill_gradient(low="grey", high="red") +
  theme(legend.position = "none")

fravaer_menn_syk <- df_combined %>%
  filter(kjonn == "Menn") %>% 
  ggplot(aes(x = ar, value_syk, fill = value_syk)) +
  geom_col() +
  theme_classic() +
  labs(
    title = "Sykefravær blant menn",
    y = "Sykefravær",
    x="") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_fill_gradient(low="grey", high="red") +
  theme(legend.position = "none")

fravaer_menn_arb <- df_combined %>%
  filter(kjonn == "Menn") %>% 
  ggplot(aes(x = ar, value_arb_ledig, fill = value_arb_ledig)) +
  geom_col() +
  theme_classic() +
  labs(
    title = "Arbeidsledighet blant menn",
    y = "Arbeidsledighet",
    x="") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) + 
  scale_fill_gradient(low="grey", high="red") +
  theme(legend.position = "none")


# Multiplott funksjon
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  
  if (is.null(layout)) {
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    
    for (i in 1:numPlots) {
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Plotter selve plottene i lag. 
multiplot(fravaer_kvinner_syk, fravaer_kvinner_arb, fravaer_menn_syk, fravaer_menn_arb, cols=1)






# testing

df_combined %>%
  filter(kjonn == "Kvinner") %>%
  rename(sykefravær = value_syk) %>% 
  ggplot(aes(x = ar, sykefravær, fill = sykefravær)) +
  geom_col() +
  theme_classic() +
  labs(
    title = "kvinner",
    x="") +
  scale_y_continuous(labels = scales::label_percent(scale = 1)) +
  scale_fill_gradient(low="grey", high="red") +
  theme(legend.position = "none")








