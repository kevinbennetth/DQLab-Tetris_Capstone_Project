library(shiny)
library(ggplot2)
library(plotly)

ui <- div(class="wrapper bg-gradient-secondary",
  tags$style(HTML("
    @import url('https://fonts.googleapis.com/css?family=Poppins');
    @import url('https://demos.creative-tim.com/test/argon-dashboard-pro/assets/css/argon-dashboard.min.css?v=2.0.0');
    @import url('https://maxst.icons8.com/vue-static/landings/line-awesome/line-awesome/1.3.0/css/line-awesome.min.css');
    
    label {
      font-size: 16px;
    }
    
    .navbar {
      margin-bottom: 24px;
    }
    
    .title {
      color: white;
    }
            ")
  ),
  div(class="navbar navbar-default bg-default justify-content-center",
      h1(class="title",
         "Top 5 Indonesian E-Commerce Platform Performances Dashboard")   
  ),
  div(class="container-lg justify-content-center h-100",
  ## TOP DASHBOARD
  ### Initially nothing is shown
  ### When which e-commerce platform is selected and year is selected:
  ### Display cards containing each performances numbers during that year (including app ranking in stores)
      div(class="card justify-content-center mb-4",
          div(class="card-header pb-0",
              htmlOutput("e_commerce",
                         container=tags$h3) # Outputs E-commerce name + year
              ),
          div(class="card-body",
              div(class="row",
                  div(class="col-lg-8 col-md-6", # Performance container
                      div(class="row", # Web visits container
                          h5(class="mb-0",
                             "Average Web Visits"),
                          htmlOutput("traffic_stat",
                                     container=tags$p,
                                     class="fs-3 fw-lighter")
                          ),
                      div(class="row", # Social media container
                          div(class="col-4", # Twitter
                              h5(class="mb-0",
                                "Twitter"),
                              htmlOutput("twitter_stat",
                                         container=tags$p,
                                         class="fs-3 fw-lighter mb-0"),
                              p(class="fs-6 fw-lighter",
                                "followers")
                              ),
                          div(class="col-4", # Instagram
                              h5(class="mb-0",
                                 "Instagram"),
                              htmlOutput("instagram_stat",
                                         container=tags$p,
                                         class="fs-3 fw-lighter mb-0"),
                              p(class="fs-6 fw-lighter",
                                "followers")
                              ),
                          div(class="col-4", # Facebook
                              h5(class="mb-0",
                                 "Facebook"),
                              htmlOutput("facebook_stat",
                                         container=tags$p,
                                         class="fs-3 fw-lighter mb-0"),
                              p(class="fs-6 fw-lighter",
                                "followers")
                              )
                          )
                      ),
                  div(class="col-lg-4 col-md-6 align-middle", # Rankings container
                      div(class="row",
                          div(class="col-6",
                              h5(class="mb-0",
                                 "Appstore Ranking"),
                              htmlOutput("appstore_rank",
                                         container=tags$p,
                                         class="fs-1 fw-lighter")
                              ),
                          div(class="col-6",
                              h5(class="mb-0",
                                 "Playstore Ranking"),
                              htmlOutput("playstore_rank",
                                         container=tags$p,
                                         class="fs-1 fw-lighter")
                              )
                          )
                      )
                  )
              )
          ),
      
  ## MIDDLE DASHBOARD
  ### Display line plot for Traffic & Social Media performance over the years for all e-commerce platforms``
  ### SelectInput on top for selecting which e-commerce platform performance users want to see
      div(class="card justify-content-center mb-4",
          div(class="card-header pb-0",
              div(class="row",
                  div(class="col-lg-4 col-md-6", # Title container
                      h4("E-commerce Platform Average Performance (2017-2021)")
                  ),
                  div(class="col-lg-8 col-md-6", # SelectInput container
                      uiOutput("platform_select_ui")
                  )
              )
          ),
          div(class="card-body",
              div(class="row", # Plot container
                  div(class="col-lg-6 col-md-12", # Traffic plot container
                      plotlyOutput("traffic_line", height="100%")
                  ),
                  div(class="col-lg-6 col-md-12", # Social Media plot container
                      plotlyOutput("socialMedia_line", height="100%")
                  )
              )
          )
      ),
  
  ## BOTTOM DASHBOARD
  ### Display bar plot for Traffic & Social Media performance during a certain year(defaults to 2017)
  ### SliderInput on top for selecting in which year
      div(class="card justify-content-center mb-4",
          div(class="card-header pb-0",
              div(class="row", # SliderInput container
                  div(class="col-lg-4 col-md-6",
                      h4("E-commerce Platform Average Performance Comparison by Year")
                  ),
                  div(class="col-lg-8 col-md-6",
                      uiOutput("year_slide_ui")
                  )
              )
          ),
          div(class="card-body",
              div(class="row", # Plot container
                  div(class="col-lg-6 col-md-12", # Traffic plot container
                      plotlyOutput("traffic_bar", height="100%")
                  ),
                  div(class="col-lg-6 col-md-12", # Social Media plot container
                      plotlyOutput("socialMedia_bar", height="100%")
                  ),
              )
          )
      ),
    div(class="d-flex justify-content-center",
        div(class="fs-5 text-white",
            "Dataset was taken from ",
            a(href="https://iprice.co.id/insights/mapofecommerce/", "iPrice")
            )
        )
  )
)
