# IMPORTING DATA
library(readxl)
library(readr)

## Data was taken and scraped from https://iprice.co.id/insights/mapofecommerce/
excel_sheets('Datasets/Data Map of E-Commerce Indonesia.xlsx')

q1_2017 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q1 2017', skip=2)
q2_2017 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q2 2017', skip=2)
q3_2017 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q3 2017', skip=2)
q4_2017 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q4 2017', skip=2)

q1_2018 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q1 2018', skip=2)
q2_2018 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q2 2018', skip=2)
q3_2018 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q3 2018', skip=2)
q4_2018 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q4 2018')

q1_2019 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q1 2019')
q2_2019 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q2 2019')
q3_2019 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q3 2019')
q4_2019 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q4 2019')

q1_2020 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q1 2020')
q2_2020 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q2 2020')
q3_2020 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q3 2020')
q4_2020 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q4 2020')

q1_2021 <- read_excel('Datasets/Data Map of E-Commerce Indonesia.xlsx', sheet = 'Q1 2021')
q2_2021 <- read_csv('Datasets/e-commerce_q2_2021.csv')
q3_2021 <- read_csv('Datasets/e-commerce_q3_2021.csv')
q4_2021 <- read_csv('Datasets/e-commerce_q4_2021.csv')

# EDA
## Let's take a look at the structure and data of a few selected datasets
head(q1_2017)
str(q1_2017)
head(q4_2018)
str(q4_2018)
head(q2_2021)
str(q2_2021)

## There are a few unnecessary columns for q1_2017 until q3_2018, and q4_2018 until q1_2021
## For the last 3 dataframes, most are useful.
## The columns that will be selected are data that contains names of platforms, web traffic, store rankings, and social media statistics

## Let's see the amount of NA values inside the dataframes
sum(is.na(q1_2017))
sum(is.na(q4_2018))
sum(is.na(q2_2021))

## There are some NA values, but upon further inspection, those values mostly exists outside the selected columns mentioned previously, but some still exist in those columns

## There are a different amounts of row length among the dataframes, this could mean that the data source does not consistently record all available e-commerce platforms from the beginning.
unique(q1_2017$Name)
unique(q4_2018$name)
unique(q2_2021$`Toko Online`)

## There is inconsistent amount of data present. Selecting e-commerce platforms whiche exists in all data can be done using sets operation 
library(RVenn)
shops <- list(tolower(q1_2017$Name), tolower(q2_2017$Name), tolower(q3_2017$Name), tolower(q4_2017$Name), tolower(q1_2018$Name), tolower(q2_2018$Name), tolower(q3_2018$Name),
     tolower(q4_2018$name), tolower(q1_2019$name), tolower(q2_2019$name), tolower(q3_2019$name), tolower(q4_2019$name), tolower(q1_2020$name),tolower(q2_2020$name), tolower(q3_2020$name), tolower(q4_2020$name), tolower(q1_2021$name),
     tolower(q2_2021$`Toko Online`), tolower(q3_2021$`Toko Online`), tolower(q4_2021$`Toko Online`))

venn = Venn(shops)
shops <- overlap(venn)
shops
# We have about 24 e-commerce platforms data

# DATA PREPARATION
library(dplyr)
library(tidyverse)
library(tidyr)
library(naniar)

## Selecting Necessary Rows & Columns for Performance Measures

## 'p' in variable names means performance measurements
## 'r' in variable names means app ranking measurements
## Both should be differentiated as ranking is not a continuous variable (it will mess up the group by procedure that I'll be doing later on)

### 2017
q1_2017_p <- q1_2017 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q2_2017_p <- q2_2017 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q3_2017_p <- q3_2017 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q4_2017_p <- q4_2017 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)

## Here i selected the max amount of followers in each social media, as it's the only viable data to acquire rather than summing it or taking the mean (followers progress over the year), and taking mean of the traffic as a measure of average web visits of the year
df2017_binded <- rbind(q1_2017_p,q2_2017_p,q3_2017_p,q4_2017_p)
str(df2017_binded)
df2017_binded[, 2:5] <- sapply(df2017_binded[,2:5], as.numeric)
df2017 <- df2017_binded %>% group_by(Name) %>%
  summarise(across(c(Twitter,Instagram,Facebook), max), Traffic=round(mean(Traffic),0))

### 2018
q1_2018_p <- q1_2018 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q2_2018_p <- q2_2018 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q3_2018_p <- q3_2018 %>% select(Name, Traffic, Twitter, Instagram, Facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q4_2018_p <- q4_2018 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)

df2018_binded <- rbind(q1_2018_p,q2_2018_p,q3_2018_p,q4_2018_p)
str(df2018_binded)
df2018_binded[, 2:5] <- sapply(df2018_binded[,2:5], as.numeric)
df2018 <- df2018_binded %>% group_by(Name) %>%
  summarise(across(c(Twitter,Instagram,Facebook), max), Traffic=round(mean(Traffic),0))

### 2019
q1_2019_p <- q1_2019 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q2_2019_p <- q2_2019 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q3_2019_p <- q3_2019 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q4_2019_p <- q4_2019 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)

df2019_binded <- rbind(q1_2019_p,q2_2019_p,q3_2019_p,q4_2019_p)
str(df2019_binded)
df2019 <- df2019_binded %>% group_by(Name) %>%
  summarise(across(c(Twitter,Instagram,Facebook), max), Traffic=round(mean(Traffic),0))

### 2020
q1_2020_p <- q1_2020 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q2_2020_p <- q2_2020 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q3_2020_p <- q3_2020 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q4_2020_p <- q4_2020 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)

df2020_binded <- rbind(q1_2020_p,q2_2020_p,q3_2020_p,q4_2020_p)
str(df2020_binded)
df2020 <- df2020_binded %>% group_by(Name) %>%
  summarise(across(c(Twitter,Instagram,Facebook), max), Traffic=round(mean(Traffic),0))

### 2021
q1_2021_p <- q1_2021 %>% select(name, traffic, twitter, instagram, facebook) %>%
  rename(Name=name, Traffic=traffic, Twitter=twitter, Instagram=instagram, Facebook=facebook) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),0)
q2_2021_p <- q2_2021 %>% select(`Toko Online`, `Pengunjung Web Bulanan`, Twitter, Instagram, Facebook) %>%
  rename(Name=`Toko Online`, Traffic=`Pengunjung Web Bulanan`) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),"0")
q3_2021_p <- q3_2021 %>% select(`Toko Online`, `Pengunjung Web Bulanan`, Twitter, Instagram, Facebook) %>% 
  rename(Name=`Toko Online`, Traffic=`Pengunjung Web Bulanan`) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),"0")
q4_2021_p <- q4_2021 %>% select(`Toko Online`, `Pengunjung Web Bulanan`, Twitter, Instagram, Facebook) %>% 
  rename(Name=`Toko Online`, Traffic=`Pengunjung Web Bulanan`) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% common_na_strings) %>%
  replace(is.na(.),"0")

q2_2021_p[2:5] <- as_tibble(apply(q2_2021_p[2:5], 2, function(x) as.numeric(gsub(pattern=",",replacement="",x,fixed=TRUE))))
q3_2021_p[2:5] <- as_tibble(apply(q3_2021_p[2:5], 2, function(x) as.numeric(gsub(pattern=",",replacement="",x,fixed=TRUE))))
q4_2021_p[2:5] <- as_tibble(apply(q4_2021_p[2:5], 2, function(x) as.numeric(gsub(pattern=",",replacement="",x,fixed=TRUE))))

df2021_binded <- rbind(q1_2021_p,q2_2021_p,q3_2021_p,q4_2021_p)
str(df2021_binded)
df2021 <- df2021_binded %>% group_by(Name) %>%
  summarise(across(c(Twitter,Instagram,Facebook), max), Traffic=round(mean(Traffic),0))

### Adding a column to represent year in which data was recorded
df2017$Year <- rep(2017,24)
df2018$Year <- rep(2018,24)
df2019$Year <- rep(2019,24)
df2020$Year <- rep(2020,24)
df2021$Year <- rep(2021,24)

### Concatenating all dataframe into one, then save a .csv file for dashboard purposes
df_p_prepped <- rbind(df2017,df2018,df2019,df2020,df2021)
write.csv(df_p_prepped, file="Datasets/e-commerce_performance.csv")

## Selecting Appstore and Playstore rankings
### For selecting these two, I decided to just use the last quarter's rankings since it is considered as the most recent ranking when recorded on that year.
nas <- c("-", "99","n/a")
r_2017 <- q4_2017 %>%
  select(Name, `Appstore Rank`, `Playstore Rank`) %>%
  rename(iosRank=`Appstore Rank`, androidRank=`Playstore Rank`) %>%
  filter(tolower(Name) %in% shops) %>%
  replace_with_na_all(~. %in% nas) %>%
  replace(is.na(.),"Not Ranked")

r_2018 <- q4_2018 %>%
  select(name, ios, android) %>%
  rename(Name=name, iosRank=ios, androidRank=android) %>%
  filter(tolower(Name) %in% shops)
r_2018$iosRank <- as.character(r_2018$iosRank)
r_2018$androidRank <- as.character(r_2018$androidRank)
r_2018 <- r_2018 %>%
  replace_with_na_all(~. %in% nas) %>%
  replace(is.na(.),"Not Ranked")

r_2019 <- q4_2019 %>%
  select(name, ios, android) %>%
  rename(Name=name, iosRank=ios, androidRank=android) %>%
  filter(tolower(Name) %in% shops)
r_2019$iosRank <- as.character(r_2019$iosRank)
r_2019$androidRank <- as.character(r_2019$androidRank)
r_2019 <- r_2019 %>%
  replace_with_na_all(~. %in% nas) %>%
  replace(is.na(.),"Not Ranked")

r_2020 <- q4_2020 %>%
  select(name, ios, android) %>%
  rename(Name=name, iosRank=ios, androidRank=android) %>%
  filter(tolower(Name) %in% shops)
r_2020$iosRank <- as.character(r_2020$iosRank)
r_2020$androidRank <- as.character(r_2020$androidRank)
r_2020 <- r_2020 %>%
  replace_with_na_all(~. %in% nas) %>%
  replace(is.na(.),"Not Ranked")

r_2021 <- q4_2021 %>%
  select(`Toko Online`, `Ranking AppStore`, `Ranking PlayStore`) %>%
  rename(Name=`Toko Online`, iosRank=`Ranking AppStore`, androidRank=`Ranking PlayStore`) %>%
  filter(tolower(Name) %in% shops)
r_2021[2:3] <- as_tibble(apply(r_2021[2:3], 2, function(x) gsub(pattern="#",replacement="",x,fixed=TRUE)))
r_2021 <- r_2021 %>%
  replace_with_na_all(~. %in% nas) %>%
  replace(is.na(.),"Not Ranked")



### Adding a column to represent year
r_2017$Year <- rep(2017,24)
r_2018$Year <- rep(2018,24)
r_2019$Year <- rep(2019,24)
r_2020$Year <- rep(2020,24)
r_2021$Year <- rep(2021,24)

### Concatenating all dataframes and then exporting it to a .csv file for dashboard purposes
df_r_prepped <- rbind(r_2017,r_2018,r_2019,r_2020,r_2021)
write.csv(df_r_prepped, file="Datasets/e-commerce_rankings.csv")

# VISUALIZATION
library(plotly)

## Selecting just top 5 e-commerce platforms in terms of performance
top5 <- c("tokopedia","shopee","lazada","blibli","bukalapak")
selected_data <- df_p_prepped %>%
  filter(tolower(Name) %in% top5)

## Plotting Web Traffic Performance
fig_web <- plot_ly(selected_data, x=~Year, y=~Traffic, split=~Name, color=~Name,
                   type="scatter", mode="lines+markers", width=900, height=600) %>%
  layout(title="Website Traffic of All E-commerce Platforms",
         xaxis=list(title="Year"),
         yaxis=list(title="Average Web Visits"),
         legend=(list(x=0, y=1,
                     title=list(text="<b>Platform</b>"),
                     font=list(size=10)))
         )
fig_web

## Plotting Social Media Popularity
### Plotting Twitter popularity
fig1 <- plot_ly(selected_data, x=~Year, y=~Twitter, split=~Name, legendgroup=~Name, color=~Name,
                type="scatter", mode="lines+markers") %>%
  layout(yaxis=list(title="Twitter"))

### Plotting Instagram popularity
fig2 <- plot_ly(selected_data, x=~Year, y=~Instagram, split=~Name, legendgroup=~Name, color=~Name,
                type="scatter", mode="lines+markers", showlegend=FALSE) %>%
  layout(yaxis=list(title="Instagram"))

### Plotting Facebook popularity
fig3 <- plot_ly(selected_data, x=~Year, y=~Facebook, split=~Name, legendgroup=~Name, color=~Name,
                type="scatter", mode="lines+markers", showlegend=FALSE) %>%
  layout(yaxis=list(title="Facebook"))

fig_socmed <- subplot(fig1, fig2, fig3, nrows=3,
                      shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
  layout(width=900, height=600,
         title="Social Media Followers of All E-commerce Platforms",
         xaxis=list(title="Year"),
         legend=list(x=1, y=0.5,
                     title=list(text="<b>Platform</b>"),
                     font=list(size=10))
         )
fig_socmed
  
# CONCLUSION
## With the limitations of data availability, I was only able to acquire a secondary data (was collected by another party) of average web visits/traffic including store rankings along with social media popularity (measured by followers).

## Based on the analysis that I have conducted, 4 of the top 5 well-known e-commerce in Indonesia experienced a downslide in terms of performance (measured by web traffic) during pandemic, with only Tokopedia and Lazada experienced an increase after 2020. Out of all other e-commerce platforms, Shopee retain its performance increase over the years, despite of pandemic.

## On the other hand, social media popularity does not give major information on how the e-commerce platform did in a certain year, but popularity increase sometimes equivalent to performance increase (more popular => more audience is interested). Different e-commerce platform excels in different social media platforms. Tokopedia is the most popular in twitter, Shopee is the most popular in Instagram, Lazada is the most popular in Facebook.

## This might be because of different e-commerce companies have different approaches in promoting their products to audience. Furthermore, a high initial followers (2017) probably was caused by the emerge of the company in each social media platform. Lazada being popular in Facebook would be likely caused by it being a foreign company too, having audiences not just from Indonesia. 

## Other than that, notice Shopee experiencing a significant increase since year 2019, indicating that it is performing great in reaching wider audience and gaining its interest.

## Overall, from this data itself it can be concluded that e-commerce experienced an increase in interest for some platforms. With Tokopedia and Shopee as the current most popular e-commerce platform, if we measure by its average web visits and comparing it to other e-commerce companies, people preferred online shopping during pandemic (the numbers are significantly dominated by those two companies). But it is still not enough to conclude so, because aside from website, applications also affect the e-commerce's performance. Getting a data on number of transactions done (which I was not able to) would show a more convincing insight rather than web visits.