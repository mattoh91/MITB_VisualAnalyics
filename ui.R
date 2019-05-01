header <- dashboardHeader(title = "Casualties of Trade War", titleWidth = 300)

sidebar <-  dashboardSidebar(
  sidebarMenu(
    #menuItem("Data Overview", icon = icon("table"), tabName = "summary"),
    menuItem("Trade Dependencies I", icon = icon("globe", lib="font-awesome"), tabName = "viz_1"),
    menuItem("Trade Dependencies II", icon = icon("globe", lib="font-awesome"), tabName = "viz_3"),
    menuItem("Trade Classification", icon = icon("percent", lib="font-awesome"), tabName = "viz_2"),
    menuItem("Trade Position by Time", icon = icon("play-circle", lib="font-awesome"), tabName = "viz_4"),
    menuItem("GDP-Trade R/S", icon = icon("usd", lib="glyphicon"), tabName = "viz_5"),
    menuItem("Drilldown by Country", icon = icon("crosshairs"), tabName = "viz_6"),
    sliderInput("year", "Select Year", min = 2007, max = 2016, value = c(2007, 2016), ticks = F, step = 1, sep = "")
    #submitButton("Process", icon = icon("refresh"))
  )
)

body <- dashboardBody(
  tabItems(

    tabItem(tabName = "viz_1", 
            tabsetPanel(type = "tabs",
                        tabPanel("Mean Trade Balance in USD$(Millions) of International Trade by Flow Type",
                                 box(title = "Import", width = 6,
                                     sankeyNetworkOutput("plot.sankey.import")),
                                 box(title = "Export", width = 6, 
                                     sankeyNetworkOutput("plot.sankey.export"))),
                        tabPanel("International Trade by Region and Main Commodity Categories",
                                 radioButtons("flow.choropleth", label = "Flow", choices = c("Import" = "Import", "Export" = "Export", "Trade Position" = "Trade.Position"), 
                                              selected = "Import", inline = TRUE),
                                 radioButtons("category.choropleth", label = "Commodity Type", choices = c("Metals" = "Metals", "Energy" = "Energy", "Agriculture" = "Agriculture", "Livestock and Meat" = "Livestock and Meat"),
                                              selected = c("Metals"), inline = TRUE),
                                 plotlyOutput("plot.choropleth", height = "600px")),
                        tabPanel("International Trade by Region and Commodity Sub-Categories",
                                 radioButtons("flow.choropleth.full", label = "Flow", choices = c("Import" = "Import", "Export" = "Export", "Trade Position" = "Trade.Position"), 
                                              selected = "Import", inline = TRUE),
                                 selectInput("category.choropleth.full", "Commodity Sub-Type", choices = unique(data.choropleth.full$Category)),
                                 plotlyOutput("plot.choropleth.full", height = "600px")))
    ),
    tabItem(tabName = "viz_2",
            tabsetPanel(type = "tabs",
                tabPanel("Trade Flow",
                  box(title = "Import", width = 6, 
                    plotOutput("plot.treemap.import")),
                  box(title = "Export", width = 6, 
                    plotOutput("plot.treemap.export"))),
                tabPanel("Commodity Type",
                  box(title = "Total Quantity of Trade (in numbers)", width = 6, 
                    sunburstOutput("plot.sunburst.one")),
                  box(title = "Total Volume of Trade(in cubic metres)", width = 6, 
                    sunburstOutput("plot.sunburst.two"))))
    ),
    tabItem(tabName = "viz_3", 
            radioButtons("flow.ts", "Flow", c("Import" = "Import", "Export" = "Export"), inline = TRUE),
            radioButtons("category.ts", "Commodity Type", c("Metals" = "Metals", "Energy" = "Energy", "Agriculture" = "Agriculture", "Livestock and Meat" = "Livestock and Meat"), 
                               inline = TRUE),
            tabsetPanel(type = "tabs",
                        tabPanel("Organization for Economic Cooperation and Development (OECD) Members",
                                 plotOutput("plot.ts.oecd", height = "700px")),
                        tabPanel("European Union (EU) Members",
                                 plotOutput("plot.ts.eu", height = "600px")), 
                        tabPanel("Southeast Asia",
                                 plotOutput("plot.ts.sea", height = "600px")))
    ),
    tabItem(tabName = "viz_4",
            div(style="display: inline-block;vertical-align:top; width: 150px;",
                selectInput("category", "Commodity Type:",
                            c("Metals" = "Metals",
                              "Agriculture" = "Agriculture",
                              "Livestock and Meat" = "Livestock and Meat",
                              "Energy" = "Energy")))
,
div(style="display: inline-block;vertical-align:top; width: 150px;",
    selectInput("continent", "Continent:",
                c("Asia" = "Asia",
                  "Europe" = "Europe",
                  "Africa" = "Africa",
                  "Americas" = "Americas",
                  "Oceania" = "Oceania"
                ))),
            sliderInput("bubble_years",
                        min = 2007, max = 2016, value = 2007, animate = animationOptions(interval = 2000), step = 1, ticks = FALSE, sep="", label="Years"
            ), 
            
            
            plotlyOutput("bubble",  height = "450px")
    ),
    
    tabItem(tabName = "viz_5",
            tabsetPanel(type = "tabs",
                        tabPanel("GDP and Trade Balance",
                                 fluidRow(
                                   box(title = "Countries Ranked by GDP-Trade Balance Correlation Coefficient (r)", 
                                       DT::dataTableOutput("plot.corr.rank.tradeposition"), height="700px", width=3),
                                   box(title = "GDP by Trade Balance per Country",
                                       plotOutput("plot.corr.trellis.tradeposition", height="600px"), height="700px", width=9)
                                 )
                                 
                        ),
                        tabPanel("GDP and Import",
                                 fluidRow(
                                   box(title = "Countries Ranked by GDP-Import Correlation Coefficient (r)", 
                                       DT::dataTableOutput("plot.corr.rank.import"), height="1000px", width=3),
                                   box(title = "GDP by Import per Country",
                                       plotOutput("plot.corr.trellis.import", height="900px"), height="1000px", width=9)
                                 )
                                 
                        ),
                        tabPanel("GDP and Export",
                                 fluidRow(
                                   box(title = "Countries Ranked by GDP-Export Correlation Coefficient (r)", 
                                       DT::dataTableOutput("plot.corr.rank.export"), height="800px", width=3),
                                   box(title = "GDP by Export per Country",
                                       plotOutput("plot.corr.trellis.export", height="700px"), height="800px", width=9)
                                 )
                        )
            )
    ),
    
    tabItem(tabName = "viz_6",
            fluidRow(
              column(selectInput("country.timeseries", label="Select country:",
                                 choices=unique(data.consolidated$Country),selectize=TRUE),
                     width=3,offset=0.5
              )
            ),
            fluidRow(
              box(title = "Top 5 Imports by Year",
                  plotOutput("plot.timeseries.import", height="600px"), width = 6),
              box(title = "Top 5 Exports by Year",
                  plotOutput("plot.timeseries.export", height="600px"), width = 6)
            )
    )
    
    ))

dashboardPage(header, sidebar, body, skin = "blue")