# DQLab-Tetris_Capstone_Project
This repository contains my work for final capstone project at DQLab Tetris Program.

Dataset was taken and some are scraped from [iPrice](https://iprice.co.id/insights/mapofecommerce/). Unscraped data are in the RawData folder, while the rest are in Datasets folder.

The final product of this project is a dashboard created using R Shiny library. Datasets are gained from a proven hypothesis that was formed from a certain media headline. (My hypothesis and details can be seen in `EDA.R` file)

The followings are brief explanation of each files:
- `scrape.py` contains the scraping code using BeautifulSoup from [bs4](https://www.crummy.com/software/BeautifulSoup/bs4/doc/) library using python.
- `EDA.R` contains complete data analysis process from **importing, EDA, data preparation, visualization, and conclusion**.
- `server.R` contains backend code of shiny, mainly used for generating texts, plots that are reactive.
- `ui.R` contains frontend code of shiny, layouting and styling are coded here.