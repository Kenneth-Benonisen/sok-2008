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
union$excess_coverage <- union$coverage - union$density

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
  scale_fill_gradient2(name = "% Arbeidsledighet", low = "green", mid = "white", high = "red", na.value = "grey50") +
  labs(title = "Arbeidsledighet i Europa \n 2019") +
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
  scale_fill_gradient2(name = "Fagforening tetthet", low = "green", mid = "white", high = "red", na.value = "grey50") +
  labs(title = "Tetthet av fagforeninger i europa \n 2019") +
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
  geom_polygon(aes(fill=excess_coverage), color = "black") +
  scale_fill_gradient2(name = "Excess coverage", low = "green", mid = "white", high = "red", na.value = "grey50") +
  labs(title = "Excess coverage i europa \n 2019") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# Plotter Kordinasjon av lønnsfastsettelse. x
df %>%
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=coord_level), color = "black") +
  scale_fill_gradient2(name = "Nivå av kordinering",
                       low = "green", mid = "white", high = "red", na.value = "grey50", 
                       label = c("Fragmented wage bargaining",
                                 "Some coordination",
                                 "Procedural negotiation guidelines",
                                 "Non-binding national norms",
                                 "Binding national norms")) +
  labs(title = "Lønnskordinering i europa \n 2019") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )
