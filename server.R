library(shiny)
library(dplyr)
library(tidyverse)
library(readr)
library(plotly)

server <- function(input, output, session) {
  
  # Performance Data
  performance <- reactive({
    withProgress(message="Preparing e-commerce performance dataset...", value=0, {
      performance <- read_csv("Datasets/e-commerce_performance.csv")
    })
  })
  
  # Ranking Data
  ranking <- reactive({
    withProgress(message="Preparing e-commerce rankings dataset...", value=0, {
      ranking <- read_csv("Datasets/e-commerce_rankings.csv")
    })
  })
  
  # UI Output - Generate selectInput for store selection
  output$store_select_ui <- renderUI({
    selection <- unique(performance()$Name)
    
    selectInput("store_select",
                "Select an e-commerce platform",
                choices = selection
    )
  })
  
  # UI Output - Generate selectInput for platform selection
  output$platform_select_ui <- renderUI({
    selection <- unique(performance()$Name)
    
    selectizeInput("platform_select",
                "Select an e-commerce platform (max. 5)",
                choices = selection,
                multiple = TRUE,
                options = list(maxItems=5)
                )
  })
  
  # UI Output - Generate sliderInput for year selection
  output$year_slide_ui <- renderUI({
    selection <- unique(performance()$Year)
    
    sliderInput("year_slide",
                "Select a year",
                min=min(performance()$Year),
                max=max(performance()$Year),
                value=min(performance()$Year),
                sep="")
  })
  
  # UI Output - Generate E-commerce performance card
  observeEvent(c(input$store_select, input$year_slide), {
    platform <- input$store_select
    year <- input$year_slide
    performance <- performance()
    rankings <- ranking()
    
    selected_p <- performance %>%
      filter(Name==platform & Year==year)
    selected_r <- rankings %>%
      filter(Name==platform & Year==year)
    
    # E-commerce name and year
    output$e_commerce <- renderUI({
      paste(platform, " (", year, ")", sep="")
    })
    
    # Web visits
    output$traffic_stat <- renderUI({
      format(selected_p$Traffic, nsmall=0, big.mark=".")
    })
    # Twitter
    output$twitter_stat <- renderUI({
      format(selected_p$Twitter, nsmall=0, big.mark=".")
    })
    # Instagram
    output$instagram_stat <- renderUI({
      format(selected_p$Instagram, nsmall=0, big.mark=".")
    })
    # Facebook
    output$facebook_stat <- renderUI({
      format(selected_p$Facebook, nsmall=0, big.mark=".")
    })
    
    # Appstore Rank
    output$appstore_rank <- renderUI({
      if(selected_r$iosRank=="Not Ranked"){
        selected_r$iosRank
      }
      else{
        paste("#", selected_r$iosRank, sep="")
      }
    })
    # Playstore Rank
    output$playstore_rank <- renderUI({
      if(selected_r$androidRank=="Not Ranked"){
        selected_r$androidRank
      }
      else{
        paste("#", selected_r$androidRank, sep="")
      }
    })
  })
  
  # Plotly Output - Generate Plotly lineplot for Traffic
  
  observeEvent(input$platform_select, {
    platform <- input$platform_select
    data <- performance()
    
    output$traffic_line <- renderPlotly({
      if(length(platform)>0){
        selected_data <- data %>%
          filter(Name %in% platform)
        
        fig <- plot_ly(data=selected_data, x=~Year, y=~Traffic,split=~Name, color=~Name,
                       type="scatter", mode="lines", height=500) %>%
          layout(title=paste("Website Traffic of", platform),
                 xaxis=list(title="Year"),
                 yaxis=list(title="Average Web Visits"))
        
        if(length(platform)>1){
          fig<- fig %>% layout(title=paste("Website Traffic of", length(platform), "E-commerce platforms"),
                               legend=list(x=1,y=0.5,
                                           title=list(text="<b>Platform</b>"),
                                           font=list(size=10))
          )
        }
        
        fig
      }
      else{
        NULL
      }
    })
  })
  
  
  # Plotly Output - Generate Plotly lineplot for Social Media
  
  observeEvent(input$platform_select, {
    platform <- input$platform_select
    data <- performance() %>%
      filter(Name %in% platform)
    
    if(length(platform)>1){
      output$socialMedia_line <- renderPlotly({
        ## Plotting twitter performance
        fig1 <- plot_ly(data, x=~Year, y=~Twitter, split=~Name, legendgroup=~Name, color=~Name,
                        type="scatter", mode="lines") %>%
          layout(yaxis=list(title="Twitter"))
        
        ## Plotting instagram performance
        fig2 <- plot_ly(data, x=~Year, y=~Instagram, split=~Name, legendgroup=~Name, color=~Name,
                        type="scatter", mode="lines", showlegend=FALSE) %>%
          layout(yaxis=list(title="Instagram"))
        
        ## Plotting facebook performance
        fig3 <- plot_ly(data, x=~Year, y=~Facebook, split=~Name, legendgroup=~Name, color=~Name,
                        type="scatter", mode="lines", showlegend=FALSE) %>%
          layout(yaxis=list(title="Facebook"))
        
        ## Plotting all subplots into one plot
        fig <- subplot(fig1, fig2, fig3, nrows=3,
                       shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
          layout(height=500,
                 title=paste("Social Media Followers of", length(platform), "E-commerce Platforms"),
                 xaxis=list(title="Year"),
                 legend=list(x=1,y=0.5,
                             title=list(text="<b>Platform</b>"),
                             font=list(size=10))
          )
        
        fig
      })
    }
    else{
      output$socialMedia_line <- renderPlotly({
        if(length(platform)>0){
          selected_data <- data %>%
            filter(Name==platform)
          
          ## Plotting twitter performance
          fig1 <- plot_ly(selected_data, x=~Year, y=~Twitter, 
                          type="scatter", mode="lines") %>%
            layout(yaxis=list(title="Twitter"))
          
          ## Plotting instagram performance
          fig2 <- plot_ly(selected_data, x=~Year, y=~Instagram, 
                          type="scatter", mode="lines") %>%
            layout(yaxis=list(title="Instagram"))
          
          ## Plotting facebook performance
          fig3 <- plot_ly(selected_data, x=~Year, y=~Facebook, 
                          type="scatter", mode="lines") %>%
            layout(yaxis=list(title="Facebook"))
          
          ## Plotting all subplots into one plot
          fig <- subplot(fig1, fig2, fig3, nrows=3,
                         shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
            layout(height=500,
                   title=paste("Social Media Followers of", platform),
                   xaxis=list(title="Year"),
                   showlegend=FALSE
            )
          
          fig
        }
        else{
          NULL
        }
      })
    }
  })
  
  # Plotly Output - Generate Plotly barplot for Traffic
    observeEvent(input$year_slide, {
      year <- input$year_slide
      selected_data <- performance() %>%
        filter(Year==year)
      
      output$traffic_bar <- renderPlotly({
        fig <- plot_ly(selected_data, x=~Traffic, y=~Name, split=~Name, color=~Name,
                       type="bar", height=500, showlegend=FALSE) %>%
          layout(title=paste("Website Traffic of All E-commerce Platforms (",year,")",sep=""),
                 xaxis=list(title="Average Web Visits"),
                 yaxis=list(title="Platform",
                            categoryorder="total ascending"
                            )
                 )
      })
      
    })
  
  
  # Plotly Output - Generate Plotly barplot for Social media
    observeEvent(input$year_slide, {
      year <- input $year_slide
      data <- performance()
      
      output$socialMedia_bar <- renderPlotly({
        selected_data <- data %>%
          filter(Year==year)
        
        fig <- plot_ly(type="bar", height=500)
        ## Plotting twitter performance
        fig1 <- plot_ly(selected_data, x=~Name, y=~Twitter, split=~Name, legendgroup=~Name, color=~Name,
                        type="bar") %>%
          layout(yaxis=list(title="Twitter"))
        
        ## Plotting instagram performance
        fig2 <- plot_ly(selected_data, x=~Name, y=~Instagram, split=~Name, legendgroup=~Name, color=~Name,
                        type="bar", showlegend=FALSE) %>%
          layout(yaxis=list(title="Instagram"))
        
        ## Plotting facebook performance
        fig3 <- plot_ly(selected_data, x=~Name, y=~Facebook, split=~Name, legendgroup=~Name, color=~Name,
                        type="bar", showlegend=FALSE) %>%
          layout(yaxis=list(title="Facebook"))
        
        #fig <- fig %>% add_trace(data=selected_data, x=~Name, y=~Twitter, name="Twitter")
        #fig <- fig %>% add_trace(data=selected_data, x=~Name, y=~Instagram, name="Instagram")
        #fig <- fig %>% add_trace(data=selected_data, x=~Name, y=~Facebook, name="Facebook")
        
        fig <- subplot(fig1, fig2, fig3, nrows=3,
                               shareX=TRUE, titleX=TRUE, titleY=TRUE) %>%
          layout(title=paste("Social Media Followers of All E-commerce Platforms (",year,")",sep=""),
                              xaxis=list(title="Platform",
                                         tickangle=-45
                            ),
                 legend=list(x=1,y=0.5,
                             title=list(text="<b>Platform</b>"),
                             font=list(size=10)
                            )
                )
        
        fig
      })
    })
}