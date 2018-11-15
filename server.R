library(shiny)
library(shinydashboard)
library(googleVis)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(leaflet)
#install.packages("ggiraph")
library(ggiraph)
library(rsconnect)

shinyServer(function(input,output){

  url = a("DataFiniti Product Data", href="https://datafiniti.co/products/product-data/")
  output$RollingStone <-renderUI({
    tagList(url)})
  
  url2 = a("Kaggle Dataset: Women's Shoe Prices",href="https://www.kaggle.com/datafiniti/womens-shoes-prices")
  output$Kaggle = renderUI({
    tagList(url2)})
  
  ############################################################################################
# OG DATA TABLE 
  
output$tbl = DT::renderDataTable({
  datatable(shoes, rownames=FALSE, options = list(pageLength = 20, autoWidth = FALSE), class = 'cell-border stripe', filter = 'top', width = 200) %>%
    formatStyle(., colnames(shoes))
    })
############################################################################################

  
  # new graph below this ....brands? 
  
############################################################################################
 # TOP 20 BRANDS

  categoryShoes = reactive({
    top20brands %>%
      filter(priceCategory == input$priceCategory)
  })
  
  # Top 20 Brands by Style and Price Range
  output$catty= renderPlotly({
   top20brands %>%  # categoryShoes() 
      ggplot(aes(x= reorder(brand, BrandTypeMaxPrice), y= ShoePrice)) +    
      geom_count(aes(fill=priceCategory, color= priceCategory), alpha = 0.7) + # ), position = "dodge"  , alpha= 0.5
      #geom_vline(xintercept = 1:21, size = 0.1) +
      labs(title="Top Brands by Style and Price Range", x = "", y="Shoe Price") +
      facet_wrap(~ type, scales = "free_y", ncol = 1, nrow= 5) +
      #coord_flip() +
      theme(axis.text.x= element_text(angle= 70, vjust=0.5, hjust=1, size = 9, face = "bold")) +
      theme(axis.text.y= element_text(size= 7)) +
      theme(panel.spacing = unit(0, "lines")) +
      theme(legend.title=element_blank()) +
      scale_colour_manual(values = c("pink", "maroon4", "violetred2")) +
      scale_fill_manual(values = c("pink", "maroon4", "violetred2"))    # cheap, expensive, mid
      #theme(legend.position="none") 
  })
    
    # theme(legend.position = 'none') 
    # ggplot(aes(x= reorder(brand, ShoePriceMax), y=ShoePrice)) +    
    # geom_point(aes(fill = type)) + 
    # facet_wrap(~ priceCategory, scales = "free_y") + 

  # Top 20 Brands: Ranked by Frequency of Discounts
  output$brandsale = renderPlotly({
    top20brands %>%
      ggplot() +  
      geom_point(aes(x=reorder(brand, BrandDiscountRatio), y= BrandDiscountRatio, color= "red")) +
      coord_flip() +
      labs(title="Top Brands: Ranked by Frequency of Discounts", x = "", y="Discount Ratio") +
      theme(axis.text.x = element_text(angle = 70, vjust=0.5, hjust=1, size = 7, face = "bold")) +
      theme(axis.text.y = element_text(size = 8)) +
      theme(plot.title = element_text(size = 9)) +
      theme(legend.position = 'none') 
  })
  
  ############################################################################################
  # SHOE Styles
  
  output$typey = renderPlotly({
    shoes %>%
      ggplot(aes_string(selectedX = input$selectedX, selectedY = input$selectedY)) +   # 
      geom_point(aes_string(input$selectedX, input$selectedY), fill = "indianred") + 
      #coord_flip() +
      labs(title="", x = "") +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 10, face = "bold"))
  })
  
  # Shoe Styles by price
  output$brandpricey= renderPlotly({
    top200brands %>%  # categoryShoes() 
      ggplot(aes(x= reorder(type, ShoePriceMax), y= ShoePrice)) +    
      geom_boxplot() +       # fill=brand
      geom_hline(yintercept = 40, linetype = "longdash", colour = "green") +
      geom_hline(yintercept = 150, linetype = "longdash", colour = "red") +
      coord_flip() +
      labs(title="Shoe Styles: Ranked by Price (hover for brand)", x = "", y="Shoe Price") +
      theme(axis.text.x= element_text(hjust=1, size = 7, face = "bold")) +
      theme(axis.text.y= element_text(size= 8, angle= 90)) +
      theme(panel.spacing = unit(0, "lines")) +
      theme(legend.title=element_blank()) +
      theme(legend.position = 'none') 
  })
  
  # Styles: Ranked by Frequency of Discounts
  output$stylesale = renderPlotly({
    shoes %>%
      ggplot() +  
      geom_point(aes(x=reorder(type, TypeDiscountRatio), y= TypeDiscountRatio, color= "red")) +
      coord_flip() +
      labs(title="Shoe Styles: Ranked by Frequency of Discounts", x = "", y="Discount Ratio") +
      theme(axis.text.x = element_text(hjust=1, size = 7, face = "bold")) +
      theme(axis.text.y = element_text(size = 8, angle = 90)) +
      theme(plot.title = element_text(size = 14)) +
      theme(legend.position = 'none') 
  })
  
  
  
############################################################################################
  
  # Top 20 MERCHANTS  by Style and Price Range
  output$merchStyl= renderPlotly({
    top20merchants %>%  # categoryShoes() 
      ggplot(aes(x= reorder(prices.merchant, BrandTypeMaxPrice), y= ShoePrice)) +    
      geom_count(aes(fill=brand, color = brand), alpha = 0.7) + # ), position = "dodge"  , alpha= 0.5
      #geom_vline(xintercept = 1:21, size = 0.1) +
      labs(title="Top 20 Merchants by Style (hover for brand)", x = "", y= "Shoe Price") +
      facet_wrap(~ type, scales = "free_y", ncol = 1, nrow= 5) +
      #coord_flip() +
      theme(axis.text.x= element_text(angle= 70, vjust=0.5, hjust=1, size = 9, face = "bold")) +
      theme(axis.text.y= element_text(size= 7)) +
      theme(panel.spacing = unit(0, "lines")) +
      theme(legend.title=element_blank()) +
      theme(legend.position="none") 
  })
  
  # TOP 20 Merchants: Ranked by Frequency of Discounts
  output$merchantsale = renderPlotly({
    top20merchants %>%
      ggplot(aes(x= MerchantDiscountRatio, y=reorder(prices.merchant, MerchantDiscountRatio))) +    
      geom_point(aes(color= "indianred")) +
      coord_flip() +
      labs(title="Top 20 Merchants: Ranked by Frequency of Discounts", x = "Discount Ratio", y="") +
      theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=1, size = 9, face = "bold")) +
      theme(axis.text.y = element_text(angle= 90, size = 5)) +
      theme(plot.title = element_text(size = 10)) +
      theme(legend.position = 'none') 
  })
  
})

