library(PxWebApiData)
library(tidyverse)
library(janitor)
library(gglorenz)
library(ineq)

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
                Tid =c("2005","2020"), # Velg årene 2005 og 2020
                Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
                InntektSkatt="00", #Vi velger samlet inntekt
                ContentsCode="VerdiDesil", #Velger den høyeste verdien i desilen
                Region=c("5401","1902")) #Tromsø endret kommunenummer i 2020


### cleaning data ###

df <- data[[2]]

df <- df %>% 
  clean_names() %>% 
  subset(select = -n_astatus) %>% 
  na.omit()


## oppretter differansiert år ##

df_2020 <- df %>% 
  filter(tid == 2020)

df_2005 <- df %>% 
  filter(tid == 2005)


### Lager lorenz kurver ###


df %>%
  filter(tid == 2005) %>%
  ggplot(aes(x = value, colour = tid)) +
  stat_lorenz(desc = T) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Kumulativ prosent av inntekt før skatt",
       y = "Kumulativ prosent av innbyggerne for Troms",
       title = "Ulikhet iblandt innbyggerne i Troms (2005)") +
  annotate_ineq(df_2005$value)


df %>%
  filter(tid == 2020) %>%
  ggplot(aes(x = value, color = tid)) +
  stat_lorenz(desc = T) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  labs(x = "Kumulativ prosent av inntekt før skatt",
       y = "Kumulativ prosent av innbyggerne for Troms",
       title = "Ulikhet iblandt innbyggerne i Troms (2020)") +
  annotate_ineq(df_2020$value)



df %>% 
  filter(tid %in% c(2005, 2020)) %>%
  ggplot(aes(x = value, colour = tid)) +
  stat_lorenz(desc = TRUE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  hrbrthemes::theme_ipsum_rc() +
  labs(x = "Kumulativ prosent av inntekt før skatt",
       y = "Kumulativ prosent av innbyggerne for Troms",
       title = "Ulikhet iblandt innbyggerne i Troms")

