library(shiny)
library(shinydashboard)
library(DT)
library(googleVis)
library(leaflet)
library(plotly)
library(rsconnect)

dashboardPage(skin = "red",
              
              dashboardHeader(title = "Women's Shoes Analysis",  titleWidth = 275),  
              
              dashboardSidebar(width = 275,
                               div(img(src = "shoes.jpg", width = "230", height = "100"), style="text-align: center;"),
                               sidebarMenu(
                                 menuItem("Background", tabName = "page1", icon = icon("star")),
                                 menuItem("The table", tabName = "theTable", icon = icon("star")),
                                 menuItem("Top 20 Brands", tabName = "top20", icon = icon("star")),
                                 menuItem("Top 20 Merchants", tabName = "idk", icon = icon("star")),
                                 menuItem("Shoe Styles", tabName = "styles", icon = icon("star"))
                               )),                  
              
              dashboardBody(
                tabItems(
                  ## -- 1st PAGE: 
                  tabItem(tabName = "page1",
                          fluidRow(
                            box(
                              h3("Background:", style="text-align: center;"),
                              br(),
                              h3("A sample dataset of 10,000 women's shoes and their product information from 2014-2017 using Datafiniti's product database"),
                              h4(""),
                              h4("Each shoe had multiple entries depending on each price found for it, so I took the average price of each shoe"),
                              h6("Note that this is a sample from a 93 million product data listings with 300 million price offers from 1000s of online retailers."),
                              br(),
                              h5("Sources:"),
                              h5(uiOutput("Datafiniti's Product Database")),
                              h5(uiOutput("Kaggle")), width = 12))),
                  
                  
                   ## -- 2nd Page
                   tabItem(tabName = "theTable", 
                           h3("Women's Shoes"),
                           fluidRow(
                             box(DT::dataTableOutput("tbl")))),
                 
                  ## 3rd PAGE 
                  tabItem(tabName = "top20",
                          h3("Deeper Dive: Top 20 Shoe Brands:"),
                          h6("(sorted by quantity of shoes available)"),
                          h6("The first 3 brands listed (Dearfoams, Bamboo, C Label) exclusively offer shoes in the cheap price range"),
                          h6("Surprising, about half (or 9/20) of the top brands don’t offer Sneakers"),
                          h6("About half (or 8/20) of the top brands offer expensive shoe styles"),
                          h6("Mid-priced shoes seem to be the most common among top brands"),
                          h6("Flats are the only style that top brands don’t offer in the expensive price range"),
                          fluidRow(
                            h5(""),
                            #selectInput(inputId= "priceCategory", label= "Price Range", choices = unique(shoes[, 'priceCategory'])),
                            column(7, plotlyOutput("catty", height= 850)),
                            column(5, plotlyOutput("brandsale", height= 400))),
                          br(),
                          fluidRow()),
                  
                  ## -- 4th PAGE:
                  tabItem(tabName = "discounts",
                          h5("hover over coordinates to see values"),
                          h5("Which brands have the highest sale ratio?"),
                          h5("what type of shoe goes on sale the most?"),
                          fluidRow(
                            column(5, plotlyOutput("typesale")))),
                         # fluidRow(#plotlyOutput("brands"))),  
                  
                  ## 5th page
                  tabItem(tabName = "idk",
                          h5("Ralph Lauren clearly offered the widest price range of styles"),
                          h5("Sears.com is the only multi-brand retailer offering luxury priced shoes"),
                          h5(""),
                          fluidRow(
                            column(7, plotlyOutput("merchStyl", height= 850)),
                            column(5, plotlyOutput("merchantsale")))),
                  
                  ## -- 6th PAGE:
                  tabItem(tabName = "styles",
                          h5("Boots (unsurprising) have the highest prices"),
                          h5("Boots also appear to have the widest price distribution, with Sandals not close behind"),
                          h5("Majority of styles are offered at cheap and mid-price ranges. All styles are also available at luxury prices"),
                          fluidRow(
                            plotlyOutput("brandpricey", width = 900)),
                          br(),
                          br(),
                          fluidRow(
                            plotlyOutput("stylesale", width= 700)))
                          )))
