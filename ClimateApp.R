#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(tidyverse)
library(dplyr)
library(shinythemes)
library(RColorBrewer)
library(ggforce)
library(DT)
library(colourpicker)
library(hrbrthemes)
library(ggalt)
library(scales)
library(collapse)


globaltrends <- read.csv(file = 'global.csv')
OECD <- read.csv(file = 'OECD.csv')
VulnIndex <- read.csv(file = 'VulnIndex.csv')
OWID <- read.csv(file = "OWID.csv")
OECD <- as.data.frame(OECD)
globaltrends <- as.data.frame(globaltrends)
VulnIndex <- as.data.frame(VulnIndex)
OWID <- as.data.frame(OWID)

levels(OWID_merged$Region) <- c("Africa", "Asia", "Europe", "Latin America", "North America", "Oceania")

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Climate Change"),
                    dashboardSidebar(
                      sidebarMenu(
                        textOutput("res"),
                        id = "tabs",
                        menuItem("Key Indicators: Overview", tabName = "Indicators", icon=icon("tachometer-alt")),
                        menuItem("International Trends", tabName = "International", icon=icon("globe-americas")),
                            menuSubItem("Fuels and Materials", tabName = "Fuels", icon=icon("gas-pump")),
                            menuSubItem("Climate Vulnerability", tabName = "Vulnerability", icon=icon("water")),
                        conditionalPanel(
                          'input.tabs == "International"',
                        menuItem((selectInput("Region", "Select Region", choices = unique(VulnIndex$Region), selected='Asia')))
                      )
                      )
                    ),
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "Indicators",
                                h3("Key Indicators: Overview")),
                        tabItem(tabName = "International",
                                h3("International Trends"),
                                fluidRow(
                                  box(plotlyOutput("percap", height=500)),
                                  box(plotlyOutput("stacked1", height=250)),
                                  box(plotlyOutput("slopes", height=250))
                                )),
                            tabItem("Fuels",
                                    h3("Fuels and Materials"),
                                    fluidRow(
                                      box(plotlyOutput("em_by_fuels", height=300))
                                    )),
                            tabItem("Vulnerability",
                                    h3("Climate Vulnerability"),
                                    fluidRow(
                                      box(plotlyOutput("hist", height=300)),
                                      box(plotlyOutput("scatter", height=300)),
                                ))
                        
                      )
                    )
)

server <- function(input, output) { 
  
  output$res <- renderText({
    paste("You've selected:", input$tabs)
  })
  
  
# Reactive Function for Setting Earliest Year
  globaltrends1 <- reactive({
    globaltrends %>%
      filter(Year > 1940)
  })
  
# Line Plot for Global Trends
  output$em_by_fuels <- renderPlotly({
    ggplotly(
      ggplot(globaltrends1(), aes(x=Year)) + 
        geom_line(aes(y=Total, color = "Total")) +
        geom_line(aes(y=Gas.Fuel, color = "Gas Fuel")) +
        geom_line(aes(y=Liquid.Fuel, color = "Liquid")) +
        geom_line(aes(y=Solid.Fuel, color = "Solid")) +
        geom_line(aes(y=Cement, color = "Cement")) +
        geom_line(aes(y=Gas.Flaring, color = "Gas Flaring")) +
        scale_color_manual(name="Emissions Source",
                           values = c("Total" = "blue", "Gas Fuel" = "green", "Liquid" = "yellow", "Solid" = "orange", "Cement" = "red", "Gas Flaring" = "purple")))
  })

# Reactive Function that allows for Region Input
  merged_filt <- reactive({
    merged <- merged %>%
    filter(Region == input$Region, !is.na(co2_per_capita))
    average = mean(merged[,'co2_per_capita'])
    merged <- mutate(merged, Code =  ifelse(co2_per_capita >= average, "Above average", "Below average"))
  })
  
# Diverging Dot Plot
  output$percap <- renderPlotly({
    ggplotly(
      ggplot(merged_filt(), aes(x=reorder(Country, co2_per_capita),
            y=co2_per_capita, label=co2_per_capita)) + 
            geom_point(stat='identity', aes(col=Code), size=1) + 
            coord_flip() + theme(axis.text = element_text(size = 8))) 
  })
  
# Reactive Function for Selecting Year and Removing Regions
  OWID_grouped <- reactive({
    OWID_merged <- OWID_merged %>%
    filter(year > 1950) %>%
    group_by(Region, year) %>%
    summarize(co2_tot = sum(co2, na.rm=TRUE),
              pop_tot = sum(population, na.rm=TRUE),
              co2_cap_tot = co2_tot/pop_tot)
    OWID_merged
  })
  
# Stacked Area Chart 1
  output$stacked1 <- renderPlotly({
  ggplotly(
    ggplot(OWID_grouped(), aes(x=year)) + geom_area(aes(y=co2_tot, fill=Region)))
  })
  
  #  total_co2 <- reactive({
   #   sum(OWID_grouped()$co2_tot) })
    
    #total_pop <- reactive({
     # sum(OWID_grouped()$pop_tot) })

    OWID_grouped_2 <- reactive({
      OWID_merged_2 <- OWID_grouped() %>%
        filter(year==2000)
        
        total_co2 = sum(OWID_merged_2$co2_tot)
        total_pop = sum(OWID_merged_2$pop_tot)
        
        OWID_merged_2 <- OWID_merged_2 %>%
    mutate(co2_perc = (co2_tot/total_co2*100),
           pop_perc = (pop_tot/total_pop*100),
           Code = ifelse(pop_perc >= co2_perc, "Fewer emissions", "More emissions"))
    OWID_merged_2
  })
  
# Slope Chart
  output$slopes <- renderPlotly({
    ggplotly(
      ggplot(OWID_grouped_2()) + geom_segment(aes(x=1, xend=2, y=pop_perc, yend=co2_perc, color=Code)) + geom_vline(xintercept=1, linetype="dashed", size = .1) + geom_vline(xintercept=2, linetype="dashed", size=.1) +
        xlim(0.5,2.5) + ylim(0.9*min(OWID_grouped_2()$pop_perc, OWID_grouped_2()$co2_perc), (1.2*(max(OWID_grouped_2()$pop_perc, OWID_grouped_2()$co2_perc)))) +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              panel.grid = element_blank()) +
        geom_text(label = paste(OWID_grouped_2()$Region), y=OWID_grouped_2()$pop_perc, x=rep(0.9, NROW(OWID_grouped_2())), hjust=1.1, size=2) +
        geom_text(label = 'Percent of', x=1, y=1.2*(max(OWID_grouped_2()$pop_perc, OWID_grouped_2()$co2_perc)), hjust=0.8, size=3) +
        geom_text(label = 'World Population', x=1, y=1.15*(max(OWID_grouped_2()$pop_perc, OWID_grouped_2()$co2_perc)), hjust=0.8, size=3) +
        geom_text(label = 'Percent of', x=1.95, y=1.2*(max(OWID_grouped_2()$pop_perc, OWID_grouped_2()$co2_perc)), hjust=-0.01, size=3) +
        geom_text(label = 'Total Emissions', x=1.95, y=1.15*(max(OWID_grouped_2()$pop_perc, OWID_grouped_2()$co2_perc)), hjust=-0.01, size=3) + 
        labs(color = "Emissions Generation (Proportional"))
  })
  
  y08 <- reactive({
    OWID %>%
    filter(year==2008)%>%
    select(country, co2_per_capita)%>%
    rename(Country = country)
  })
  
  merged1 <- reactive({
    merged_y <- merge(x=VulnIndex, y=y08(), by = "Country")
    merged_y <- merged_y %>%
    mutate(co2 = co2_per_capita*OverallPop08*1000)
    merged_y
  })
  
  output$hist <- renderPlotly({
  ggplotly(
    ggplot(merged1(), aes(OverallCDI)) + geom_density(aes(fill=WBLendingClass), alpha = 0.8))
  })
  
  output$scatter <- renderPlotly({
    ggplotly(
      ggplot(merged1(), aes(x=OverallCV, y=OverallPC, color=IncomeStatus, label=Country)) +
        geom_point(alpha = (1/2)) + scale_color_brewer(palette = "Dark2") #+
        #geom_smooth(method="lm", se=FALSE)
    )
  })
}

shinyApp(ui, server)
