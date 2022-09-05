library(tidyverse)
library(readxl)
library(janitor)
library(gglorenz)
library(ineq)


GCIPrawdatatest <- read_excel("C:/Users/Kenne/Downloads/GCIPrawdatatest.xlsx", skip = 2)
View(GCIPrawdatatest)


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
  df, country %in% c("United States","Sweden","Finland","Norway", 
                              "Denmark"))

ggplot(temp_df, 
       aes(x = year, y = gini, color = country)) +
  geom_line(size = 1) +
  theme_bw() +
  ylab("Gini") +
  ggtitle("Gini coefficients for Nordic countries")
