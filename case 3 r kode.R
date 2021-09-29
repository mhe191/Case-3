# Laster ned nødvendige pakker
library(tidyverse)
library(data.table)
library(cowplot)
library(corrr)
library(readxl)
library(httr)


# Henter co2 data
co2 <- fread("https://github.com/owid/co2-data/raw/master/owid-co2-data.csv")
codebook <- fread("https://github.com/owid/co2-data/raw/master/owid-co2-codebook.csv")

with(co2, table(country))


# Eksluderer alle samlekategorier
co2 <- 
  co2 %>%
  filter(country %in% c("Africa", "Asia", "Asia (excl. China & India)",
                        "Europe", "EU-27", "EU-28", "Europe (excl. EU-28)",
                        "Europe (excl. EU-27)", "Oceania", "North America",
                        "North America (excl. USA)", "Panama Canal Zone",
                        "South America", "International transport",
                        "Kuwaiti Oil Fires") == FALSE) %>% 
  select(year, co2) %>% 
  group_by(year) %>% 
  summarise(co2 = mean(co2, na.rm = TRUE)) %>% 
  ungroup() %>% 
  as_tibble(co2)
head(co2)


# Henter bnp per cap data
url <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx"
GET(url, write_disk(tmpfi <- tempfile(fileext = ".xlsx")))

mpd2020 <- read_excel(tmpfi, sheet = "Full data")
head(mpd2020)


# Beregner gjennomsnittlig BNP per cap
gdp <- 
  mpd2020 %>%
  select(year, gdppc) %>%
  filter(year >= 1750) %>% 
  group_by(year) %>% 
  summarise(bnp_pc = mean(gdppc, na.rm = TRUE)) %>% 
  ungroup()
head(gdp)


# Henter temperatur data 
temp <- fread("https://www.metoffice.gov.uk/hadobs/hadcrut4/data/current/time_series/HadCRUT.4.6.0.0.annual_ns_avg.txt")


# Gir nye navn på variablene
temp <- 
  temp %>%
  select(V1, V2) %>% 
  rename(year = V1,
         temperatur = V2)
head(temp)


# Legger datasettene sammen 
left_join(co2, gdp, by="year") %>% 
  left_join(., temp, by="year") -> dframe

dframe <- 
  dframe %>%
  filter(year >= 1850)
head(dframe)


# Lager plot 
p1 <-
  dframe %>% 
  ggplot(aes(x=year, y=bnp_pc)) +
  geom_line(lwd=1, col="dark green") +
  labs(x = " ",
       y = "Bruttonansjonalprodukt (US$) per capita") +
  theme_bw()

p2 <-
  dframe %>% 
  ggplot(aes(x=year, y=co2)) +
  geom_line(lwd=1, col="dark blue") +
  labs(x = " ") +
  theme_bw()

p3 <-
  dframe %>% 
  ggplot(aes(x=year, y=temperatur)) +
  geom_line(lwd=0.8, col="dark red") +
  labs(x = " ") +
  theme_bw()

plot_grid(p1, p2, p3, ncol = 3, labels = "AUTO") 


# Beregner korrelasjon 
dframe %>% 
  select(-year) %>% 
  correlate(diagonal = 1) %>% 
  fashion(decimals = 3)

# Oppgave 1
p4 <- 
  dframe %>% 
  ggplot(aes(x=year, y=bnp_pc/co2)) +
  geom_point(col="darkolivegreen1") +
  stat_smooth(col="darkorange") +
  labs(x = " ") +
  theme_bw()

p5 <- 
  dframe %>% 
  ggplot(aes(x=year, y=co2/temperatur)) +
  geom_point(col="deepskyblue1") +
  stat_smooth(col="firebrick") +
  labs(x = " ") +
  theme_bw()

plot_grid(p4, p5, ncol = 2, labels = "AUTO")


# Oppgave 2

# før 1950
dframe1 <- dframe[1:101 , ]

# etter 1950
dframe2 <- dframe[101:170 , ]

dframe1 %>% 
  select(-year) %>% 
  correlate(diagonal = 1) %>% 
  fashion(decimals = 3)

dframe2 %>% 
  select(-year) %>% 
  correlate(diagonal = 1) %>% 
  fashion(decimals = 3)


# Estimerer regresjonsmodeller
# Hele modellen 
library(mosaic)
fit1 <- lm(co2 ~ log(bnp_pc), data = dframe)
fit1

plotModel(fit1)

# Før 1950
fit2 <- lm(co2 ~ log(bnp_pc), data = filter(dframe, year < 1950))
fit2

plotModel(fit2)

# Etter 1950
fit3 <- lm(co2 ~ log(bnp_pc), data = filter(dframe, year >= 1950))
fit3

plotModel(fit3)

# Begge periodene i samme 
dframe <- dframe %>% 
  mutate(y1950 = year >= 1950)

fit4 <- lm(co2 ~ log(bnp_pc)*y1950, data = dframe)
fit4

plotModel(fit4)

# Etter 1950 fra modell
coef(fit4)[1]+coef(fit4)[3]

coef(fit4)[2]+coef(fit4)[4]

# Påvirker co2 temperatur
dframe %>% 
  ggplot(aes(x=co2, y=temperatur)) +
  geom_point() +
  theme_bw()
