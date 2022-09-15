# You will need the following libraries for the assignment:

library(readr)
library(ggplot2) 
library(tidyverse) 

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# This loads the data with information about the variables of interest
union <- read_csv("C:/Users/Kenne/Downloads/union_unempl.csv")

# Changing the name of a single observation. 
# The below code changes all observations called "United Kingdom" to "UK" in the union data. 
union$country <- gsub("United Kingdom", "UK", union$country)

# Renaming a variable. The below code renames the variable "Country" to "Region".
names(union)[names(union) == "country"] <- "region"


# Creating a new variable. To create a map showing "Excess coverage",
# you need to create a new variable. 
union$excess_coverage <-union$density + union$coverage #A sum

union$diff <-union$excess_coverage - union$unempl #A difference

union$mean_excess_unempl <-(union$excess_coverage + union$diff)/2 # A mean value

# collecting mapdata for the entire world.
mapdata <- map_data("world")

# merging the two datasets together by region.
df <- left_join(mapdata, union, by= "region")

# removing unwanted region by filtering out rows where we dont have any data. 
df <- df %>% 
  filter(!is.na(df$mean_unempl2015_2019))

# Mutating new column named coord level so we are able to plot the different coordinating used by different regions.  
df <- df %>% 
  mutate(coord_level = case_when(
    coord == "1. Fragmented wage bargaining" ~ 1,
    coord == "2. Some coordination" ~ 2,
    coord == "3. Procedural negotiation guidelines" ~ 3,
    coord == "4. Non-binding national norms" ~ 4,
    coord == "5. Binding national norms" ~ 5,
  ))


# plotting unemployment 
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=unempl), color = "black") +
  scale_fill_gradient2(name = "Arbeidsledighet, 2019", low = "green", mid = "yellow", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
    )


# plotting Union density
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=density), color = "black") +
  scale_fill_gradient2(name = "Fagforeningstetthet, 2019", low = "green", mid = "yellow", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# plotting excess coverage ??
df %>% 
  ggplot(aes(x=long, y=lat, group = group)) +
  geom_polygon(aes(fill=density), color = "black") +
  scale_fill_gradient2(name = "Excess coverage, 2019", low = "green", mid = "yellow", high = "red", na.value = "grey50") +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    rect = element_blank()
  )


# plotting cordinating of payment rates. 
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






