library(tidyverse)
library(readxl)
library(janitor)
library(gglorenz)
library(ineq)

# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")


GCIPrawdatatest <- read_excel("C:/Users/Kenne/Downloads/GCIPrawdatatest.xlsx", skip = 2)
#View(GCIPrawdatatest)

df <- GCIPrawdatatest %>% 
  clean_names()

df$gini <- 0
noc <- nrow(df)



for (i in seq(1, noc)){
  # Go to Row I to get the decile data
  df_2 <- unlist(df[i, 3:12])
  df$gini[i] <- Gini(df_2)
}

temp_df <- subset(
  df, country %in% c("United States", "Canada", "United Kingdom","Sweden","Finland","Norway", 
                     "Denmark"))

temp_df %>% 
  mutate("Land" = country) %>%
  mutate(Land=recode(Land, "Denmark" = "Danmark",
                     "Norway"="Norge",
                     "Sweden"="Sverige", 
                     "United States " = "USA",
                     "United Kingdom" = "England")) %>% 
  ggplot(aes(x=year, y=gini, color = Land)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Gini koeffisient for Skandinavia") +
  labs(x="År",
       y="Gini")
