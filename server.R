shinyServer(function(input, output) {
 
  output$plot.sunburst.one <- renderSunburst({
    data.sunburst %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Qty.Name == "Number of items") %>%
      group_by(V1) %>%
      summarise(V2 = sum(Qty)) %>%
      sunburst(
        count = TRUE,
        explanation = 
          "function (d) {root = d;
     while (root.parent) {
     root = root.parent
     }
     p = (100*d.value/root.value).toPrecision(3);
     q = (root.value /1000000000).toPrecision(3);
     msg = p + '% of<br/>' + q + ' Bn';
     return msg;v
     }",
      legend = list(w = 150, h = 25, s = 5, t = 5),
      colors = list(
        range = 
          c("#1a9641", "#d01c8b", "#bf812d", "#2b83ba",
            "#9e9ac8", "#bcbddc",
            "#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0"), 
        domain = 
          c("Agriculture", "Energy", "Livestock and Meat", "Metals",
            "Export", "Import",
            "Americas", "Asia", "Europe", "Africa", "Oceania")))
  })

  output$plot.sunburst.two <- renderSunburst({
    data.sunburst %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Qty.Name == "Volume in cubic meters") %>%
      group_by(V1) %>%
      summarise(V2 = sum(Qty)) %>%
      sunburst(
        count = TRUE,
        explanation = 
          "function (d) {root = d;
     while (root.parent) {
     root = root.parent
     }
     p = (100*d.value/root.value).toPrecision(3);
     q = (root.value /1000000000).toPrecision(3);
     msg = p + '% of<br/>' + q + ' Bn';
     return msg;v
     }",
        legend = list(w = 150, h = 25, s = 5, t = 5),
        colors = list(
          range = 
            c("#1a9641", "#d01c8b", "#bf812d", "#2b83ba",
              "#9e9ac8", "#bcbddc",
              "#fb6a4a", "#fc9272", "#fcbba1", "#fee0d2", "#fff5f0"), 
          domain = 
            c("Agriculture", "Energy", "Livestock and Meat", "Metals",
              "Export", "Import",
              "Americas", "Asia", "Europe", "Africa", "Oceania")))
  })
  
  output$plot.treemap.import <- renderPlot({
    data.tree.import %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      group_by(Continent, Country) %>%
      summarise(`Total Items` = sum(`Number of items`, na.rm = TRUE),
                `Median Volume` = median(`Volume in cubic meters`, na.rm = TRUE)) %>%
      treemap(index = c("Continent", "Country"),
              vSize = "Total Items",
              vColor = "Median Volume",
              type = "value",
              title = "Total Quantity of International Trade (in Numbers)",
              title.legend = "Median Volume (in cubic meters)",
              fontsize.labels = 16)
  })
  
  output$plot.treemap.export <- renderPlot({
    data.tree.export %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      group_by(Continent, Country) %>%
      summarise(`Total Items` = sum(`Number of items`, na.rm = TRUE),
                `Median Volume` = median(`Volume in cubic meters`, na.rm = TRUE)) %>%
      treemap(index = c("Continent", "Country"),
              vSize = "Total Items",
              vColor = "Median Volume",
              type = "value",
              title = "Total Quantity of International Trade (in Numbers)",
              title.legend = "Median Volume (in cubic meters)",
              fontsize.labels = 16)
  })
  
  output$plot.ts.oecd <- renderPlot({
    ggplot(data.ts.oecd %>%
             filter((input$flow.ts %in% Flow) & (input$category.ts %in% Category.main) & Year >= input$year[1] & Year <= input$year[2]) %>%
             group_by(Country, Year) %>%
             summarise(Mean.Trade.USD = mean(Trade.USD)), aes(Year, Mean.Trade.USD)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      facet_geo(~ Country, grid = "oecd_grid1", scales = "free_y") +
      scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
      ylab("") +
      theme_bw() +
      ggtitle("Mean Trade Balance in USD billion (US$ Bn)")
  })
  
  output$plot.ts.eu <- renderPlot({
    ggplot(data.ts.eu %>%
             filter((input$flow.ts %in% Flow) & (input$category.ts %in% Category.main) & Year >= input$year[1] & Year <= input$year[2]) %>%
             group_by(Country, Year) %>%
             summarise(Mean.Trade.USD = mean(Trade.USD)), aes(Year, Mean.Trade.USD)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      facet_geo(~ Country, grid = "eu_grid1", scales = "free_y") +
      scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
      ylab("") +
      theme_bw(base_size = 14) +
      ggtitle("Mean Trade Balance in USD billion (US$ Bn)")
  })
  
  output$plot.ts.sea <- renderPlot({
    ggplot(data.ts.sea %>%
             filter((input$flow.ts %in% Flow) & (input$category.ts %in% Category.main) & Year >= input$year[1] & Year <= input$year[2]) %>%
             group_by(Country, Year) %>%
             summarise(Mean.Trade.USD = mean(Trade.USD)), aes(Year, Mean.Trade.USD)) +
      geom_line(color = "steelblue") +
      geom_point(color = "steelblue") +
      facet_geo(~ Country, grid = "sea_grid1", scales = "free_y") +
      scale_x_continuous(labels = function(x) paste0("'", substr(x, 3, 4))) +
      ylab("") +
      theme_bw(base_size = 14) +
      ggtitle("Mean Trade Balance in USD billion (US$ Bn)")
  })
  
  output$plot.sankey.import <- renderSankeyNetwork({
    sankeyNetwork(Links = data.sankey.import %>%
                    filter(Year >= input$year[1] & Year <= input$year[2]) %>%
                    group_by(source, target) %>%
                    summarise(trade.mean = mean(Trade.USD)) %>%
                    as.data.frame() %>%
                    mutate(value = round(trade.mean/sum(trade.mean)*100, 2)) %>%
                    select(source, target, value), 
                  Nodes = data.frame("name" = c("Metals", "Energy", "Agriculture", "Livestock and Meat",
                                                "Africa", "Americas", "Asia", "Europe", "Oceania")),
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 12, nodeWidth = 30)
    })
  
  output$plot.sankey.export <- renderSankeyNetwork({
    sankeyNetwork(Links = data.sankey.export %>%
                    filter(Year >= input$year[1] & Year <= input$year[2]) %>%
                    group_by(source, target) %>%
                    summarise(trade.mean = mean(Trade.USD)) %>%
                    as.data.frame() %>%
                    mutate(value = round(trade.mean/sum(trade.mean)*100, 2)) %>% 
                    select(source, target, value), 
                  Nodes = data.frame("name" = c("Metals", "Energy", "Agriculture", "Livestock and Meat",
                                                "Africa", "Americas", "Asia", "Europe", "Oceania")),
                  Source = "source", Target = "target",
                  Value = "value", NodeID = "name",
                  fontSize= 12, nodeWidth = 30)
  })
  
  colpal.choropleth <- function(x){
    if (x == 'Trade.Position'){
      'RdYlBu'
    }
    else {
      'Blues'
    }
  }
  
  output$plot.choropleth <- renderPlotly({
    data.choropleth %>%
      filter(Flow == input$flow.choropleth) %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Category.main %in% input$category.choropleth) %>%
      group_by(Country, Year, Category.main, ISO) %>% #Group across main category.
      summarize(Trade.USD = mean(Trade.USD)) %>%
      plot_geo() %>%
      add_trace(
        z = ~Trade.USD, color = ~Trade.USD, colors = colpal.choropleth(input$flow.choropleth),
        text = ~Country, locations = ~ISO, marker = list(line = list(color = toRGB("grey"), width = 0.5))
      ) %>%
      colorbar(title = 'Trade in USD (Millions)', tickprefix = '$') %>%
      layout(
        title = paste(input$category.choropleth, paste(input$flow.choropleth,'s',sep=''), 'by Countries', sep=' '),
        geo = list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = 'Mercator')
        )
      )
  })
  
  output$plot.choropleth.full <- renderPlotly({
    data.choropleth.full %>%
      filter(Flow == input$flow.choropleth.full) %>%
      filter(Year >= input$year[1] & Year <= input$year[2]) %>%
      filter(Category %in% input$category.choropleth.full) %>%
      group_by(Country, Year, Category, ISO) %>% #Group across main category.
      summarize(Trade.USD = mean(Trade.USD)) %>%
      plot_geo() %>%
      add_trace(
        z = ~Trade.USD, color = ~Trade.USD, colors = colpal.choropleth(input$flow.choropleth.full),
        text = ~Country, locations = ~ISO, marker = list(line = list(color = toRGB("grey"), width = 0.5))
      ) %>%
      colorbar(title = 'Trade in USD (Millions)', tickprefix = '$') %>%
      layout(
        title = paste(input$category.choropleth.full, paste(input$flow.choropleth.full,'s',sep=''), 'by Countries', sep=' '),
        geo = list(showframe = FALSE, showcoastlines = FALSE, projection = list(type = 'Mercator')
        )
      )
  })
  
  output$bubble <- renderPlotly({
    
    year = input$bubble_years
    continents = input$continent
    categories = input$category
    
    data_year = data_year[[year - 2006]]
    ## start data.cleaned
    
    data_year <- filter(data_year, Continent == continents)
    data_year <- filter(data_year, Category.main == categories) 

    
    dataf = dplyr::group_by(data_year, Country, Flow) %>%
      summarise(sum.trade = sum(Trade.USD), max(GDP.mil))  %>%
      spread(Flow, sum.trade)
    
    
    colnames(dataf) = c("Country", "GDP.mil", "Export.trade", "Import.trade", "Trade.position")
    
    # scale data
    dataf$Export.trade.norm = scale(dataf$Export.trade, center = F)
    dataf$Import.trade.norm = scale(dataf$Import.trade, center = F)
    
    
    slope <- 0.666051223553066e-3
    
    sizeref =  2. * max(dataf$GDP.mil) / (0.5 ** 2)
    dataf$size <- sqrt(dataf$GDP.mil * slope)
    
    
    colors <- c('#4AC6B7', '#1972A4', '#965F8A', '#FF7070', '#C61951')
   
    min_x = 0
    max_x = 6
    min_y = 0
    max_y = 6
    
    
    dataf$Country = tools::toTitleCase(dataf$Country)
    
    if(year == 2007) {
      dataf = dataf[order(-dataf$GDP.mil), ]
      dataf = dataf[1:10, ]
      top_10_countries <<- dataf$Country
    }
    else
      dataf = dataf %>% 
        filter(Country %in% top_10_countries )
    
    p <- plot_ly(dataf, x = ~Import.trade.norm, y = ~Export.trade.norm, size = ~size, color = ~Country, colors = colors,
                 type = 'scatter', mode = 'markers', sizes = c(min(dataf$size), max(dataf$size)),
                 marker = list(symbol = 'circle', sizemode = 'diameter',
                               sizeref = sizeref,
                               line = list(width = 2, color = '#FFFFFF')),
                 text = ~paste('Country:', Country, '<br>GDP:', GDP.mil, "<br>Export:", Export.trade,"<br>Import:", Import.trade)) %>%
      layout(title = paste('Import/Export vs. GDP:', year),
             xaxis = list(title = 'Standardised Import Scale',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(min_x, max_x),
                          #type = 'log',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwidth = 2),
             legend = list(orientation="v"),
             yaxis = list(title = 'Standardised Export Scale',
                          gridcolor = 'rgb(255, 255, 255)',
                          range = c(min_y, max_y),
                          #type = 'log',
                          zerolinewidth = 1,
                          ticklen = 5,
                          gridwith = 2),
             paper_bgcolor = 'rgb(243, 243, 243)',
             plot_bgcolor = 'rgb(243, 243, 243)')
    
    
    
  })
  

  output$plot.corr.rank.tradeposition <- DT::renderDataTable({
    data.r2.tradeposition
    })
  
  output$plot.corr.trellis.tradeposition <- renderPlot({
    data.corrtrellis.tradeposition %>%
      ggplot(aes(x=Trade.USD, y=GDP.mil)) +
      geom_point(shape=1) +
      stat_ellipse(type="norm", col="Red")+
      facet_wrap( ~ Country, ncol=4, scales = "free") +
      theme(strip.text.x = element_text(size = 12)) +
      xlab("Trade Balance in USD (Millions)") +
      ylab("GDP in USD (Millions)")
  })
  
  output$plot.corr.rank.import <- DT::renderDataTable({
    data.r2.import
  })
  
  output$plot.corr.trellis.import <- renderPlot({
    data.corrtrellis.import %>%
      ggplot(aes(x=Trade.USD, y=GDP.mil)) +
      geom_point(shape=1) +
      stat_ellipse(type="norm", col="Red")+
      facet_wrap( ~ Country, ncol=4, scales = "free") +
      theme(strip.text.x = element_text(size = 12)) +
      xlab("Import in USD (Millions)") +
      ylab("GDP in USD (Millions)")
  })
  
  output$plot.corr.rank.export <- DT::renderDataTable({
    data.r2.export
  })
  
  output$plot.corr.trellis.export <- renderPlot({
    data.corrtrellis.export %>%
      ggplot(aes(x=Trade.USD, y=GDP.mil)) +
      geom_point(shape=1) +
      stat_ellipse(type="norm", col="Red")+
      facet_wrap( ~ Country, ncol=4, scales = "free") +
      theme(strip.text.x = element_text(size = 12)) +
      xlab("Export in USD (Millions)") +
      ylab("GDP in USD (Millions)")
  })
  
  top5.import <- reactive({
    data.timeseries[,c("Country","Import.cat")] %>%
      filter(Country == input$country.timeseries) %>%
      select(Import.cat)
  })
  
  output$plot.timeseries.import <- renderPlot({
    data.consolidated %>%
      filter(Country == input$country.timeseries & Flow == "Import" & Category %in% top5.import()[[1]][[1]]) %>%
      ggplot(aes(x=Year, y=Trade.USD, colour=str_wrap(Category.clean,20))) + 
      geom_line(size=2) +
      scale_colour_brewer(palette = "Set1") +
      xlab(element_blank()) + 
      ylab("USD (Millions)") +
      labs(colour="Commodity Categories:") +
      theme(legend.position = "bottom")
  })
  
  top5.export <- reactive({
    data.timeseries[,c("Country","Export.cat")] %>%
      filter(Country == input$country.timeseries) %>%
      select(Export.cat)
  })
  
  output$plot.timeseries.export <- renderPlot({
    data.consolidated %>%
      filter(Country == input$country.timeseries & Flow == "Export" & Category %in% top5.export()[[1]][[1]]) %>%
      ggplot(aes(x=Year, y=Trade.USD, colour=str_wrap(Category.clean,20))) + 
      geom_line(size=2) +
      scale_colour_brewer(palette = "Set2") +
      xlab(element_blank()) + 
      ylab("USD (Millions)") +
      labs(colour="Commodity Categories:") +
      theme(legend.position = "bottom")
  })

})