library(tidyverse)
library(data.table)
library(stringr)
library(ggplot2)

h <- head
g <- glimpse 
s <- summary
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
cell7 <- cell6
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

gdp3 <- gdp2 %>% filter(gdp2[[2]] %in% g20)
keep <- colnames(gdp3[1:4])
gdp4 <- gather(gdp3, year, gdp_growth, -keep)
names(gdp4) <- c("Country_Name", "Country_Code", "Indicator_Name", "Indicator_Code", "Year", "GDP_Growth")
names(cell7) <- c("Country_Name", "Country_Code", "Indicator_Name", "Indicator_Code", "Year", "Per_100_people")
cell8 <- cell7 %>% mutate(difference = lag(Per_100_people) - Per_100_people)

country_gdp <- function(df, code) {
        df %>% filter(GDP_Growth < 0, Country_Code == code)
}

country_cell <- function(df, code) {
        df %>% filter(Country_Code == code)
}

# comparing cell change vs gdp growth: inconclusive
bra_gdp <- country_gdp(gdp4, "BRA")
arg_gdp <- country_gdp(gdp4, "ARG")
saudi_gdp <- country_gdp(gdp4, "SAU")
rus_gdp <- country_gdp(gdp4, "RUS")
bra_cell <- country_cell(cell8, "BRA")
arg_cell <- country_cell(cell8, "ARG")
saudi_cell <- country_cell(cell8, "SAU")
rus_cell <- country_cell(cell8, "RUS")

# data cleaning for internet
cell_names <- as.vector(as.character(cell[1, ]))
names(inter) <- cell_names
inter1 <- inter[-1, ]
inter2 <- inter1 %>% filter(inter1[[2]] %in% g20) %>% 
        select(-as.character(1960:1989), -as.character(2018:2019)) %>% 
        select("Country Name":"2017")
names(inter2) <- c("Country_Name", 
                   "Country_Code", 
                   "Indicator_Name", 
                   "Indicator_Code", 
                   as.character(1990:2017))
keep1 = c("Country_Name", "Country_Code", "Indicator_Name", "Indicator_Code")
inter3 <- gather(inter2, Year, Percent, -keep1)

# plotting: internet usage in 1990-2018
plot_by_time_inter <- ggplot(inter3, aes(x = Year, y = Percent, group = Country_Name)) + 
        geom_line(aes(color = Country_Name)) + 
        geom_point(aes(color = Country_Name)) + 
        ylab('% of population') + 
        ggtitle('Individuals Using the Internet in G20 Countries (1990-2017)') +
        theme(legend.title = element_blank(), axis.text.x=element_blank())  + 
        facet_wrap(~ Country_Name) + 
        theme(legend.position = "none") 