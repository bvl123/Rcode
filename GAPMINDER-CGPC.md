# Rcode
For R Projects
library(tidyverse)
library(viridis)
library(psych)

GDP <- readr::read_csv("G:/gdppercapita_us_inflation_adjusted.csv", col_types = NULL)
co2 <- readr::read_csv("G:/co2_emissions_tonnes_per_person.csv", col_types = NULL)

GMD <- readr::read_csv("https://raw.githubusercontent.com/dbouquin/IS_608/master/NanosatDB_munging/Countries-Continents.csv")

GMD <- GMD[c(2,1)]

GMD <- dplyr::rename(GMD,
    country = Country
  )

GMD$country[which(GMD$country=="US")]<-"United States"
GMD$country[which(GMD$country=="Russian Federation")]<-"Russia"
GMD$country[which(GMD$country=="CZ")]<-"Czech Republic"

co2long <- co2 %>% tidyr::gather(key = "Year", value = "CO2E", -country, na.rm = TRUE)
CO2CL <- dplyr::left_join(co2long, GMD, by = "country")

GDPlong <- GDP %>% tidyr::gather(key = "Year", value = "GDPPC", -country, na.rm = TRUE)
GDPCL <- dplyr::left_join(GDPlong, GMD, by = "country")

by_year <- dplyr::left_join(GDPCL, CO2CL, by = c("country","Year","Continent"))

cor.test(by_year$GDPPC, by_year$CO2E, 
         method = "spearman")
                            
by_yearG <- by_year %>% group_by(Year,Continent) %>% 
  dplyr::summarize(meanGDPPC = mean(GDPPC, na.rm = TRUE))

by_yearC <- by_year %>% group_by(Year,Continent) %>% 
  dplyr::summarize(meanCO2E = mean(CO2E , na.rm = TRUE))

by_yearCG <- dplyr::left_join(by_yearC, by_yearG, by = c("Year", "Continent"))

#ggplot(by_yearCG,aes(x=meanGDPPC,y=meanCO2E,group = Continent, color = Continent)) + 
        #geom_point()+
        #facet_wrap(~ Continent) +
        #geom_smooth()

countryCGPC <- by_year %>%
    drop_na(CO2E) %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(cor = cor(GDPPC,CO2E), method = 'spearman') %>%
    arrange(desc(abs(cor)))
    
countryG <- by_year %>% group_by(country) %>% 
  dplyr::summarize(meanGDPPC = mean(GDPPC, na.rm = TRUE))

countryC <- by_year %>% group_by(country) %>% 
  dplyr::summarize(meanCO2E = mean(CO2E , na.rm = TRUE))

countrySumm <- countryCGPC %>%
  left_join(GMD, by='country') %>%
  left_join(countryG, by='country') %>%
  left_join(countryC, by='country') %>%
  drop_na(cor)


ggplot(countrySumm, aes(meanGDPPC, meanCO2E,label=country)) +
  geom_point(aes(shape=factor(sign(cor)), size=abs(cor), color=Continent, alpha=0.3)) +
  scale_x_log10() +
  scale_y_log10() +
  scale_radius(range = c(-10, 10)) +
  scale_shape_discrete(name="Mean Is", breaks=c(-1, 1), labels=c("Negative", "Positive"))+
  geom_text(data=countrySumm[countrySumm$cor > 0.7,], check_overlap = TRUE)
