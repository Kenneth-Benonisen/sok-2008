# Nødvendige pakker. 
library(readr)
library(ggplot2) 
library(tidyverse)
library(RCurl)

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Samler inn dataen vi skal utforske. 
url <- getURL("https://raw.githubusercontent.com/Kenneth-Benonisen/sok-2008/main/utfordring%202/union_unempl.csv")
union <- read.csv(text = url)

# Endrer navnet på observasjon tilknyttett England. 
union$country <- gsub("United Kingdom", "UK", union$country)

# Bytter navn på en variabel slik at det blir mulig å sammensveise dataset senere
names(union)[names(union) == "country"] <- "region"

# Oppretter ny variabler for "Excess coverage". 
union$excess_coverage <-union$density + union$coverage #A sum
union$diff <-union$excess_coverage - union$unempl #A difference
union$mean_excess_unempl <-(union$excess_coverage + union$diff)/2 # A mean value

# Henter inn kart data for hele verden. 
mapdata <- map_data("world")

# Sammensveiser union og kart datasettet basert på region. 
df <- left_join(mapdata, union, by= "region")

# Fjerner unødvendige regioner hvor vi ikke har noen data. 
df <- df %>% 
  filter(!is.na(df$mean_unempl2015_2019))

# Mutate ny kolonne hvor vi kan sortere lønnskordinasjon på ulike numeriske nivå slik at det er mulig å plotte. 
df <- df %>% 
  mutate(coord_level = case_when(
    coord == "1. Fragmented wage bargaining" ~ 1,
    coord == "2. Some coordination" ~ 2,
    coord == "3. Procedural negotiation guidelines" ~ 3,
    coord == "4. Non-binding national norms" ~ 4,
    coord == "5. Binding national norms" ~ 5,
  ))

# Plotter arbeidsledighet.
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=unempl), color = "black") +
  scale_fill_gradient2(name = "Arbeidsledighet, 2019", low = "green", mid = "white", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
    )


# Plotter Fagforeningstetthet.
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=density), color = "black") +
  scale_fill_gradient2(name = "Fagforeningstetthet, 2019", low = "green", mid = "white", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# Plotter "excess coverage".
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=density), color = "black") +
  scale_fill_gradient2(name = "Excess coverage, 2019", low = "green", mid = "white", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# Plotter Kordinasjon av lønnsfastsettelse. 
df %>%
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=coord_level), color = "black") +
  scale_fill_gradient2(name = "Kordinering av lønn, 2019", low = "green", mid = "white", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )






