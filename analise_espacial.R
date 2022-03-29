library(tidyverse)
library(rgdal)
library(tmap)

shp_ca_counties <- readOGR(dsn = "CA_Counties",
                          layer = "CA_Counties_TIGER2016",
                          encoding = "UTF8")

shp_california <- readOGR(dsn = "ca-places-boundaries",
                          layer = "CA_Places_TIGER2016")


tmp <- shp_ca_counties@data
places <- shp_california@data

tm_shape(shp_california) +
    tm_borders() +
    tm_shape(shp_ca_counties) +
    tm_borders()

# tmap_mode("view")
# 
# tm_shape(shp = shp_california) + 
#     tm_borders() +
#     tm_text(text = "NAME", size = 0.4)
# 
# tmap_mode("plot")

churn_idx_city <- tc %>% 
    mutate(flg_churn = ifelse(flg_churn == '1', 1, 0)) %>% 
    group_by(city) %>% 
    summarise(qtd_clientes = n(),
              qtd_churn = sum(flg_churn)) %>% 
    mutate(taxa_churn = qtd_churn / qtd_clientes) %>%
    arrange(desc(taxa_churn))

shp_california@data$NAME
churn_idx_city$city

churn_idx_city %>% filter(taxa_churn >= .8)

shp_california_dados <- merge(x = shp_california,
                              y = churn_idx_city,
                              by.x = "NAME",
                              by.y = "city")


tm_shape(shp = shp_california_dados) + 
    tm_fill(col = "taxa_churn", 
            # style = "quantile", 
            n = 4, 
            palette = "viridis", 
            legend.hist = T) +
    tm_borders(alpha = 0.8) +
    # tm_compass() +
    tm_layout(legend.outside = T) +
    tm_shape(shp_ca_counties) +
    tm_borders()

