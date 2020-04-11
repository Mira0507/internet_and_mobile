library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)

# master data frame 
# cell: utilization of mobile phone 
# inter: utilization of internet 
# gdp: udp growth
cell <- fread('API_IT.CEL.SETS.P2_DS2_en_csv_v2_887311.csv')
inter <- fread('API_IT.NET.USER.ZS_DS2_en_csv_v2_889089.csv')
gdp <- fread('API_NY.GDP.MKTP.KD.ZG_DS2_en_csv_v2_936081.csv')

# g20: country codes for g20 (except EU)
g20 <- c('AUS', 'CAN', 'USA', 'SAU', 'IND', 'RUS', 'ZAF', 'TUR', 'ARG',
         'BRA', 'MEX', 'FRA', 'DEU', 'ITA', 'GBR', 'IDN', 'JPN', 'KOR', 'CHN') 

# data cleaning for cell data 
cell_names <- as.vector(as.character(cell[1, ]))
names(cell) <- cell_names
cell1 <- cell[-1, ]

cell2 <- cell1 %>% 
        select('Country Name', 
               'Country Code', 
               'Indicator Name',
               'Indicator Code',
               as.character(1980:2019))
cell3 <- cell2[, -'2019']

cell4 <- cell3 %>% filter(cell3[[2]] %in% g20)
cell5 <- gather(cell4, Year, 
                Per_100_people, 
                -c('Country Name', 'Country Code', 'Indicator Name', 'Indicator Code'))

cell6 <- cell5 %>% filter(Year %in% as.character(1990:2018))

# plotting: cellular subscription in 1990-2018
plot_by_time_cell <- ggplot(cell6, aes(x = Year, y = Per_100_people, group = cell6[[1]])) + 
        geom_line(aes(color = cell6[[1]])) + 
        geom_point(aes(color = cell6[[1]])) + 
        ylab('Subscriptions per 100 people') + 
        ggtitle('Cellular Subscriptions in G20 Countries (1990-2018)') +
        theme(legend.title = element_blank(), axis.text.x=element_blank())  + 
        facet_wrap(~ cell6[[1]]) + 
        theme(legend.position = "none") 


# data cleaning for gdp
gdp1 <- gdp[ ,-c('V64', 'V65')]
g_name <- as.character(gdp1[1, c(1:4, 35:63)])
gdp2 <- gdp1[-1, c(1:4, 35:63)]
names(gdp2) <- g_name
