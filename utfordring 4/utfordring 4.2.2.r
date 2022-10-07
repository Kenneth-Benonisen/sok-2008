#Datasettet inneholder informasjon om generøsiteten av forldrepermisjonsordninger og mødres yrkesdeltakelse i 24 OECD-land i 2021
# Foreldrepermisjonene varierer mellom land. 
# For å gjøre ulike landers rettigheter sammenlignbare presenteres permisjonsordningers gunstighet
# ved hjelp av "full rate equivalent":
# Uker med 100% støtte  = Foreldrepermisjonens varighet i uker * utbetalingssats (i prosent av gjennomsnittlig inntekt) 

# Nødvendige pakker
library(tidyverse)
library(gridExtra)
library(grid)


# Kode for a kunne bruke norske bokstaver
Sys.setlocale(locale="no_NO")

# Henter datafilen
women <- read.csv2("https://uit-sok-2008-h22.github.io/Assets/women.csv")

# Omgjør fra cha til numeric
women$tot_full_rate<-as.numeric(women$tot_full_rate)
women$fem_emp_rate_0_2<-as.numeric(women$fem_emp_rate_0_2)
women$fem_emp_rate_6_14<-as.numeric(women$fem_emp_rate_6_14)


# Lager plots
kids_0_2 <- women %>%
  ggplot(aes(x=tot_full_rate,y=fem_emp_rate_0_2))+
  geom_point()+
  ylim(0, 100)+
  labs(x ="Uker med 100% støtte", y = "Yrkesdeltakelse blant mødre hvis yngste barn er 0-2 år") +
  theme_classic() +
  geom_smooth(method=lm, se=FALSE)

kids_6_14 <- women %>%
  ggplot(aes(x=tot_full_rate,y=fem_emp_rate_6_14))+
  geom_point()+
  ylim(0, 100)+
  labs(x ="Uker med 100% støtte", y = "Yrkesdeltakelse blant mødre hvis yngste barn er 6-14 år") +
  theme_classic() +
  geom_smooth(method=lm, se=FALSE)

# Illustrere plottene i et grid system. 
grid.arrange(kids_0_2,kids_6_14, nrow = 1,  top = textGrob("Sammenhengen mellom foreldrepermisjons lengde og mødres yrkesdelakelse etter yngste barns alder",gp=gpar(fontsize=20,font=3)))
