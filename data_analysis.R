# Migration Data Analysis

library(tidyverse)
library(ggpubr)
library(stringr) 
library(dplyr)
#library(ggsubplot)
library(ggplot2)
library(maps)
#library(plyr)
#detach(package:plyr)
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggrepel)
library("scales") 

# Call data----
global_data   = read.csv("C:/luciana_datos/UCI/paper_raw/Global_south/chapter_12/data/International_migration_database.csv")
GS_names      = read.delim("C:/luciana_datos/UCI/paper_raw/Global_south/chapter_12/data/global_south.txt", header = TRUE)
global_data.t = global_data  %>% filter(Country.of.birth.nationality %in% GS_names$Country) %>% 
  filter(Variable == "Inflows of foreign population by nationality") %>%
  filter(Gender == "Total")
global_data.w = global_data  %>% filter(Country.of.birth.nationality %in% GS_names$Country) %>% 
  filter(Variable == "Inflows of foreign population by nationality") %>%
  filter(Gender == "Women")
global_data.s = global_data  %>% filter(Country.of.birth.nationality %in% GS_names$Country) %>% 
  filter(Variable == "Inflows of foreign population by nationality") %>%
  filter(Gender == "Share of women")
year_filter   = unique(global_data.w$Year)

# Extract data----
# Summarize per country of origin and year
global_data.w.1 = global_data.w %>% filter(Year %in% year_filter[1:22]) %>% 
  group_by(Country.of.birth.nationality,Year) %>%
  summarise(Total = sum(Value))
write.csv(global_data.w.1, file = "C:/luciana_datos/UCI/paper_raw/Global_south/chapter_12/data/Migration_country_origin.csv")

# Summarize per country of destination and year
global_data.w.2 = global_data.w %>% filter(Year %in% year_filter[1:22]) %>% 
  group_by(Country,Year) %>%
  summarise(Total = sum(Value))
write.csv(global_data.w.2, file = "C:/luciana_datos/UCI/paper_raw/Global_south/chapter_12/data/Migration_country_destination.csv")

# Make data for the graph----

# Change in data of migration from country of birth
a = unique(global_data.w.1$Country.of.birth.nationality)
country_change_migration = c()
for(i in a){
  temp   = global_data.w.1 %>% filter(Country.of.birth.nationality == i)
  temp.1 = (temp$Total[temp$Year==2019] - temp$Total[temp$Year==2000]-1)*100/(temp$Total[temp$Year==2000]+1)
  temp.2 = case_when(temp.1 > 10000 ~ "> 10000",
                     temp.1 > 1000 & temp.1 <= 10000 ~ "1000 - 10000",
                     temp.1 > 500 & temp.1 <= 1000 ~ "500 - 1000",
                     temp.1 > 100 & temp.1 <= 500 ~ "100 - 500",
                     temp.1 > 0 & temp.1 <= 100 ~ "0 - 100",
                     temp.1 <= 0 ~ "< 0")
  temp.3 = c(unique(temp$Country.of.birth.nationality),as.numeric(temp.1),temp.2)
  country_change_migration = rbind(country_change_migration, temp.3) 
}
colnames(country_change_migration) = c("Country","Change","Band")
rownames(country_change_migration) = NULL
country_change_migration = as.data.frame(country_change_migration)
write.csv(country_change_migration, file = "C:/luciana_datos/UCI/paper_raw/Global_south/chapter_12/data/country_change_migration.csv")

# Change in data of migration to country of origin
a.1 = unique(global_data.w.2$Country)
country_change_destination = c()
for(ii in a.1){
  temp   = global_data.w.2 %>% filter(Country == ii)
  temp.1 = (temp$Total[temp$Year==2019] - temp$Total[temp$Year==2009])*100/temp$Total[temp$Year==2009]
  temp.2 = c(unique(temp$Country),temp.1)
  country_change_destination = rbind(country_change_destination, temp.2) 
}
colnames(country_change_destination) = c("Country","Change")
rownames(country_change_destination) = NULL
country_change_destination = as.data.frame(country_change_destination)

# Completing missing points
country_change_destination$Change[country_change_destination$Country == "Türkiye"] = 
  (global_data.w.2$Total[global_data.w.2$Country == "Türkiye" & global_data.w.2$Year == 2019] - 
     global_data.w.2$Total[global_data.w.2$Country == "Türkiye" & global_data.w.2$Year == 2010])*100/global_data.w.2$Total[global_data.w.2$Country == 
                                                                                                                             "Türkiye" & global_data.w.2$Year == 2010]
country_change_destination$Change[country_change_destination$Country == "Switzerland"] = 
  (global_data.w.2$Total[global_data.w.2$Country == "Switzerland" & global_data.w.2$Year == 2019] - 
     global_data.w.2$Total[global_data.w.2$Country == "Switzerland" & global_data.w.2$Year == 2010])*100/global_data.w.2$Total[global_data.w.2$Country == 
                                                                                                                             "Switzerland" & global_data.w.2$Year == 2010]
country_change_destination$Change[country_change_destination$Country == "Poland"] = 
  (global_data.w.2$Total[global_data.w.2$Country == "Poland" & global_data.w.2$Year == 2019] - 
     global_data.w.2$Total[global_data.w.2$Country == "Poland" & global_data.w.2$Year == 2013])*100/global_data.w.2$Total[global_data.w.2$Country == 
                                                                                                                                 "Poland" & global_data.w.2$Year == 2013]
country_change_destination$Change[country_change_destination$Country == "Mexico"] = 
  (global_data.w.2$Total[global_data.w.2$Country == "Mexico" & global_data.w.2$Year == 2019] - 
     global_data.w.2$Total[global_data.w.2$Country == "Mexico" & global_data.w.2$Year == 2007])*100/global_data.w.2$Total[global_data.w.2$Country == 
                                                                                                                            "Mexico" & global_data.w.2$Year == 2007]
country_change_destination$Change[country_change_destination$Country == "Latvia"] = 
  (global_data.w.2$Total[global_data.w.2$Country == "Latvia" & global_data.w.2$Year == 2019] - 
     global_data.w.2$Total[global_data.w.2$Country == "Latvia" & global_data.w.2$Year == 2014])*100/global_data.w.2$Total[global_data.w.2$Country == 
                                                                                                                            "Latvia" & global_data.w.2$Year == 2014]
country_change_destination.1 = c()
for(ii in a.1){
  temp.0 = country_change_destination %>% filter(Country == ii)
  temp.1 = as.numeric(temp.0$Change)
  temp.2 = case_when(temp.1 > 1000 ~ "> 1000",
                     temp.1 > 500 & temp.1 <= 1000 ~ "500 - 1000",
                     temp.1 > 100 & temp.1 <= 500 ~ "100 - 500",
                     temp.1 > 0 & temp.1 <= 100 ~ "0 - 100",
                     temp.1 <= 0 ~ "< 0")
  temp.3 = c(temp.0$Country,temp.1,temp.2)
  country_change_destination.1 = rbind(country_change_destination.1, temp.3) 
}
colnames(country_change_destination.1) = c("Country","Change","Band")
rownames(country_change_destination.1) = NULL
country_change_destination.1 = as.data.frame(country_change_destination.1)
country_change_destination.1$Country[country_change_destination.1$Country == "Türkiye"] =
  "Turkey"
country_change_destination.1$Country[country_change_destination.1$Country == "United States"] =
  "United States of America"
write.csv(country_change_destination.1, file = "C:/luciana_datos/UCI/paper_raw/Global_south/chapter_12/data/country_change_destination.csv")

# Plotting map----
world   = ne_countries(scale = "medium", returnclass = "sf")
class(world)
world.1 = merge(world, country_change_migration, by.x = "name", by.y = "Country", all.x = TRUE)
world.2 = merge(world, country_change_destination.1, by.x = "name", by.y = "Country", all.x = TRUE)

# Migration from global south
ggplot(data = world.1) +
  geom_sf(aes(fill = as.factor(Band))) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Change in female migration from 2000 to 2019 from Global South countries", 
          subtitle = paste0("(", length(unique(country_change_migration$Country)), " countries)")) + 
  scale_fill_manual(breaks=c('> 10000', '1000 - 10000', '500 - 1000',
                               '100 - 500', '0 - 100', '< 0', 'NA'),
                      values = c("#b30000", "#e34a33", "#fc8d59","#fdbb84",
                                 "#fdd49e","#7fcdbb","#bdbdbd"),
                    name="Change (%)") 

# Destination countries from global south
ggplot(data = world.2) +
  geom_sf(aes(fill = as.factor(Band))) +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("Destination Countries - Change in female migration from 2009 to 2019 from Global South", 
          subtitle = paste0("(", length(unique(country_change_destination.1$Country)), " countries)")) + 
  scale_fill_manual(breaks=c('> 1000', '500 - 1000', '100 - 500',
                             '0 - 100', '< 0', 'NA'),
                    values = c("#08519c", "#3182bd", "#6baed6","#c6dbef",
                               "#fdd49e","#bdbdbd"),
                    name="Change (%)") +  theme(plot.title=element_text(size=12))

# Plotting time series
global_data.w.2_top10 = global_data.w.2 %>% filter(Country %in% c("United States",
                                                                "Türkiye","Canada",
                                                                "Spain","Germany",
                                                                "Korea","France",
                                                                "Italy","Australia",
                                                                "Portugal"))

global_data.w.2_top10$Country[global_data.w.2_top10$Country == "United States"] =
  "USA" 
data_ends = global_data.w.2_top10 %>%
  group_by(Country) %>%
  top_n(1, Year)

ggplot(global_data.w.2_top10, aes(x = Year, y = (Total), color = Country)) +
  geom_line(aes(color = Country),size = .9) + 
  geom_text_repel(
    aes(label = Country),data = data_ends,
    fontface = "bold",
    size = 3,
    direction = "y",
    xlim = c(2020.8, NA),
    hjust = 0,
    segment.size = .7,
    segment.alpha = .5,
    segment.linetype = "dotted",
    box.padding = .4,
    segment.curvature = -0.1,
    segment.ncp = 3,
    segment.angle = 20
  ) + theme(legend.position = "none") + labs(title = "", 
                                             x = "",
                                             y = "Immigrants (women)") + 
  scale_x_continuous(limits = c(2000, 2021.5),breaks = scales::pretty_breaks(n = 10)) + 
  scale_y_continuous(labels = comma) 


