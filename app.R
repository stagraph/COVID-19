library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(dplyr)
library(readr)
library(tidyr)
library(lubridate)
library(maps)
library(ggplot2)
library(plotly)
library(leaflet)
library(shinyjqui)
library(leaflet.extras)
library(RColorBrewer)

{
load_source_data <- function(){
    # load confirmed
    world_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv") %>% 
        dplyr::rename(State = 'Province/State', Country = 'Country/Region' ) %>%
        gather(key="Date", value = "Confirmed", -State, -Country, -Lat, -Long) %>%
        mutate(Date = mdy(Date))
    #load recovered
    world_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv") %>% 
        dplyr::rename(State = 'Province/State', Country = 'Country/Region' ) %>%
        gather(key="Date", value = "Recovered", -State, -Country, -Lat, -Long) %>%
        mutate(Date = mdy(Date))
    #load deaths
    world_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv") %>% 
        dplyr::rename(State = 'Province/State', Country = 'Country/Region' ) %>%
        gather(key="Date", value = "Deaths", -State, -Country, -Lat, -Long) %>%
        mutate(Date = mdy(Date))
    #combine all source datasets
    finalDataset <- world_confirmed %>%
        left_join(world_recovered, by = c("State"="State", "Country"="Country", "Date"="Date")) %>%
        left_join(world_deaths, by = c("State"="State", "Country"="Country", "Date"="Date")) %>%
        select(Country,State, Lat,Long,Date, Cum_Confirmed= Confirmed, Cum_Recovered=Recovered, Cum_Deaths=Deaths) %>%
        group_by(Country,State, Lat,Long) %>%
        mutate(Daily_Confirmed = coalesce(Cum_Confirmed - lag(Cum_Confirmed,1, order_by = Date),0),
               Daily_Recovered = coalesce(Cum_Recovered - lag(Cum_Recovered,1, order_by = Date),0),
               Daily_Deaths = coalesce(Cum_Deaths - lag(Cum_Deaths,1, order_by = Date),0))
    return(finalDataset)
}

load_us_data <- function(){
    # load confirmed
    us_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv") %>% 
        dplyr::rename(State = 'Province_State', Country = 'Country_Region' ) %>%
        select(-UID, -iso2,-iso3, -code3, -FIPS,-Admin2, -Combined_Key, Long=Long_) %>%
        gather(key="Date", value = "Confirmed", -State, -Country, -Lat, -Long) %>%
        mutate(Date = mdy(Date))%>%
        group_by(State, Country, Date) %>%
        summarise(
            Daily_Confirmed = sum(Confirmed),
            Lat = first(Lat),
            Long = first(Long))
    # load deaths
    us_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv") %>% 
        dplyr::rename(State = 'Province_State', Country = 'Country_Region' ) %>%
        select(-UID, -iso2,-iso3, -code3, -FIPS,-Admin2, -Combined_Key,- Population, Long=Long_) %>%
        gather(key="Date", value = "Deaths", -State, -Country, -Lat, -Long) %>%
        mutate(Date = mdy(Date))%>%
        group_by(State, Country, Date) %>%
        summarise(
            Daily_Deaths = sum(Deaths),
            Lat = first(Lat),
            Long = first(Long))
    #combine all source datasets
    final_us_Dataset <- us_confirmed %>%
        left_join(us_deaths, by = c("State"="State", "Country"="Country", "Date"="Date")) %>%
        select(State, Lat=Lat.x,Long=Long.x,Date, Cum_Confirmed= Daily_Confirmed, Cum_Deaths=Daily_Deaths) %>%
        group_by(State, Lat,Long) %>%
        mutate(
            Country = "USA",
            Cum_Recovered = 0,
            Daily_Confirmed = coalesce(Cum_Confirmed - lag(Cum_Confirmed,1, order_by = Date),0),
            Daily_Recovered = 0,
            Daily_Deaths = coalesce(Cum_Deaths - lag(Cum_Deaths,1, order_by = Date),0))%>%
        select(Country, State, Lat, Long, Date, Cum_Confirmed, Cum_Recovered,Cum_Deaths, Daily_Confirmed, Daily_Recovered, Daily_Deaths)
    return(final_us_Dataset)
}    

# country - state statistics + some cleanning
data_state2 <- load_us_data()
data_state <- load_source_data() %>% 
    filter(Country != "US")%>%
    bind_rows(data_state2) %>%
    ungroup()%>%
    mutate(Country = case_when(Country == "US"  ~ "USA",
                               Country == "Czechia"  ~ "Czech Republic",
                               Country == "Congo (Brazzaville)"  ~ "Republic of Congo",
                               Country == "Congo (Kinshasa)"  ~ "Democratic Republic of the Congo",
                               Country == "Cabo Verde"  ~ "Cape Verde",
                               Country == "Cote d'Ivoire"  ~ "Ivory Coast",
                               Country == "Korea, South"  ~ "South Korea",
                               Country == "Taiwan*"  ~ "Taiwan",
                               Country == "United Kingdom"  ~ "UK",
                               Country == "North Macedonia"  ~ "Macedonia",
                               Country == "Burma"  ~ "Myanmar",
                               TRUE ~ Country))

# country statistics
data_country <- data_state %>%
    ungroup()%>%
    select(-State)%>%
    group_by(Country, Lat, Long, Date)%>%
    summarise(Daily_Confirmed = sum(Daily_Confirmed), Daily_Recovered = sum(Daily_Recovered), Daily_Deaths = sum(Daily_Deaths))

# get last date in dataset)
data_first_day <- min(data_state$Date) 
data_last_day <- max(data_state$Date) 

# get statistical numbers
data_total_confirmed <- data_country$Daily_Confirmed %>% sum()
data_total_recovered <- data_country$Daily_Recovered %>% sum()
data_total_deaths <- data_country$Daily_Deaths %>% sum()

# get statistical numbers
data_lastDay_confirmed <- data_country %>% ungroup() %>% filter(Date == data_last_day) %>% select(Daily_Confirmed) %>% sum()
data_lastDay_recovered <- data_country %>% ungroup() %>% filter(Date == data_last_day) %>% select(Daily_Recovered) %>% sum()
data_lastDay_deaths <- data_country %>% ungroup() %>% filter(Date == data_last_day) %>% select(Daily_Deaths) %>% sum()

data_world_map = map_data("world") %>%
    mutate(region = case_when(region == "Antigua"  ~ "Antigua and Barbuda",
                              region == "Barbuda"  ~ "Antigua and Barbuda",
                              region == "Trinidad"  ~ "Trinidad and Tobago",
                              region == "Tobago"  ~ "Trinidad and Tobago",
                              region == "Saint Vincent"  ~ "Saint Vincent and the Grenadines",
                              region == "Grenadines"  ~ "Saint Vincent and the Grenadines",
                              TRUE ~ region))
}       

ui <- dashboardPage(
    dashboardHeader(title = "COVID-19",
                    dropdownMenu(type = "messages",
                                 messageItem(from = "Reporting Mode",message = "This dashboard displays history data (D-1). ",icon = icon("info-circle")),
                                 messageItem(from = "Last Actualization",message = data_last_day,icon = icon("sync-alt"),))
                    ),
    dashboardSidebar(
        sidebarMenu(id = "menu",
            menuItem("Home", tabName = "home", icon = icon("search-location")), 
            menuItem("Owerview", tabName = "dashboard", icon = icon("search-location")), 
            menuItem("Map View", tabName = "map", icon = icon("map-marked")),
            menuItem("Analysis", tabName = "plot", icon =icon("chart-bar")),
            menuItem("About", tabName = "about", icon = icon("search-location")))
    ),
    dashboardBody(
        tabItems(
            {
            tabItem(tabName = "dashboard",
                    fluidRow(width=12,
                             box(width = 12,
                                 title = tagList(shiny::icon("info-circle"), " Overview"), 
                                 solidHeader = T,status = "primary", 
                                 valueBox( format(round(data_total_confirmed),  big.mark=","), "Total Cases",color = "yellow", width = 2, icon = icon("plus-circle")),
                                 valueBox( format(round(data_total_deaths),  big.mark=","),"Total Deaths", color = "red", width = 2,icon = icon("minus-circle")),
                                 valueBox( format(round(data_lastDay_confirmed),  big.mark=","), "New Cases",color = "yellow", width = 2,icon = icon("plus-circle")),
                                 valueBox( format(round(data_lastDay_deaths),  big.mark=","),"New Deaths",color = "red", width = 2,icon = icon("minus-circle")),
                                 valueBox( format(round(data_total_confirmed- data_total_recovered),  big.mark=",") , "Active Cases",color="maroon", width = 2,icon = icon("question-circle")),
                                 valueBox( format(round(data_total_recovered),  big.mark=","),"Total Recovered", color="olive", width = 2,icon = icon("check-circle"))),
                                     tabBox( title = tagList(shiny::icon("search"), "Detail"), width = 12,
                                              tabPanel("Total Cases",
                                                       fluidRow(
                                                           column(width = 8,
                                                                  checkboxInput("cb_total_cases_cb_log", "Log10(Total_Cases)",value=FALSE),
                                                                  plotlyOutput("total_cases_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                  plotlyOutput("total_cases_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                           column(width = 4,
                                                                  tabBox(width=12,
                                                                      tabPanel("Treemap",plotlyOutput("total_cases_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                      tabPanel("Table",dataTableOutput('total_cases_table')%>% withSpinner(color="#3c8dbc"))
                                                                  )
                                                                )
                                                            )
                                                       ),
                                              tabPanel("Total Deaths",
                                                       fluidRow(
                                                           column(width = 8,
                                                                  checkboxInput("cb_total_deaths_cb_log", "Log10(Total_Deaths)",value=FALSE),
                                                                  plotlyOutput("total_deaths_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                  plotlyOutput("total_deaths_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                           column(width = 4,
                                                                  tabBox(width=12,
                                                                         tabPanel("Treemap",plotlyOutput("total_deaths_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                         tabPanel("Table",dataTableOutput('total_deaths_table')%>% withSpinner(color="#3c8dbc"))
                                                                  )
                                                                )
                                                            )
                                                       ),
                                              tabPanel("Total Recovered",
                                                       fluidRow(
                                                           column(width = 8,
                                                                  checkboxInput("cb_total_recovered_cb_log", "Log10(Total_Recovered)",value=FALSE),
                                                                  plotlyOutput("total_recovered_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                  plotlyOutput("total_recovered_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                           column(width = 4,
                                                                  tabBox(width=12,
                                                                         tabPanel("Treemap",plotlyOutput("total_recovered_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                         tabPanel("Table",dataTableOutput('total_recovered_table')%>% withSpinner(color="#3c8dbc"))
                                                                  )
                                                           )
                                                       )
                                                       ),
                                              tabPanel("New Cases",
                                                       fluidRow(
                                                           column(width = 8,
                                                                  checkboxInput("cb_newcases_cb_log", "Log10(New_Cases)",value=FALSE),
                                                                  plotlyOutput("newcases_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                  plotlyOutput("newcases_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                           column(width = 4,
                                                                  tabBox(width=12,
                                                                         tabPanel("Treemap",plotlyOutput("newcases_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                         tabPanel("Table",dataTableOutput('newcases_table')%>% withSpinner(color="#3c8dbc"))
                                                                  )
                                                           )
                                                       )
                                                       ),
                                              tabPanel("New Deaths",
                                                       fluidRow(
                                                           column(width = 8,
                                                                  checkboxInput("cb_newdeaths_cb_log", "Log10(New_Deaths)",value=FALSE),
                                                                  plotlyOutput("newdeaths_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                  plotlyOutput("newdeaths_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                           column(width = 4,
                                                                  tabBox(width=12,
                                                                         tabPanel("Treemap",plotlyOutput("newdeaths_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                         tabPanel("Table",dataTableOutput('newdeaths_table')%>% withSpinner(color="#3c8dbc"))
                                                                  )
                                                           )
                                                       )
                                                       ),
                                             tabPanel("New Recovered",
                                                      fluidRow(
                                                          column(width = 8,
                                                                 checkboxInput("cb_newrecovered_cb_log", "Log10(New_Recovered)",value=FALSE),
                                                                 plotlyOutput("newrecovered_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                 plotlyOutput("newrecovered_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                          column(width = 4,
                                                                 tabBox(width=12,
                                                                        tabPanel("Treemap",plotlyOutput("newrecovered_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                        tabPanel("Table",dataTableOutput('newrecovered_table')%>% withSpinner(color="#3c8dbc"))
                                                                 )
                                                          )
                                                      )
                                                      ),
                                              tabPanel("Active Cases",
                                                       fluidRow(
                                                           column(width = 8,
                                                                  checkboxInput("cb_activecases_cb_log", "Log10(ActiveCases)",value=FALSE),
                                                                  plotlyOutput("activecases_map",height = "500px" )%>% withSpinner(color="#3c8dbc"),
                                                                  plotlyOutput("activecases_in_time",height = "500px" )%>% withSpinner(color="#3c8dbc")),
                                                           column(width = 4,
                                                                  tabBox(width=12,
                                                                         tabPanel("Treemap",plotlyOutput("activecases_in_treemap",height = "1000px" )%>% withSpinner(color="#3c8dbc")),
                                                                         tabPanel("Table",dataTableOutput('activecases_table')%>% withSpinner(color="#3c8dbc"))
                                                                  )
                                                           )
                                                       )
                                                   )
                                            )
                            )
                    )
                }
                ,
            {
            tabItem(tabName = "map",
                    fluidRow(width=12,
                             box(width = 12, collapsible = TRUE, collapsed = TRUE,
                                 title = tagList(shiny::icon("wrench"), "Setup"), 
                                 solidHeader = T, status = "warning",
                                 column(width = 6,
                                      dateRangeInput("map_dateRange", "From period", start="2020-01-01", format = "dd.mm.yyyy", end = lubridate::today(), min="2020-01-01",max= "2030-12-31", language = "en", separator=" to "),
                                      checkboxInput("map_ch_display_borders", "Display Coutries", value=F)),
                                 column(width = 6,
                                      selectInput("map_variableMode","Display Variable",choices = c("Total Cases","Total Deaths", "Total Recovered","New Cases","New Deaths", "New Recovered","Active Cases" ), selected = "Total Cases"),
                                        actionButton("map_btnRefresh","Load", width = "100%")
                                      )
                                )
                             ),
                    fluidRow(width=12,
                             box(width = 12,
                                 title = tagList(shiny::icon("map-marked"), "Map View"), 
                                 solidHeader = T,status = "primary", 
                                 jqui_resizable( leafletOutput("mapview", height = 700)))
                             )
                    )
            }
            ,
            {
                tabItem(tabName = "plot",
                        fluidRow(width=12,
                            box(width = 2,title = tagList(shiny::icon("wrench"), "Settings"), solidHeader = T, status = "warning",
                                selectInput("plot_dataset","Select Dataset",choices = c("Countries", "Regions" ), selected = "Countries"),
                                selectInput("plot_variable","Select Variable",choices = c("Total Cases","Total Deaths","Total Recovered", "New Cases", "New Deaths", "New Recovered", "Active Cases" ), selected = "Total Cases"),
                                selectInput("plot_time","Time Mode",choices = c("Date", "Days From Start","Days From 1 Case","Days From 10 Cases","Days From 100 Cases", "Days From 1 Death", "Days From 10 Deaths", "Days From 100 Deaths" ), selected = "Date"),
                                uiOutput("myUI_1"),
                                checkboxInput("plot_sum", "Stack Selected Regions", value=F),
                                checkboxInput("plot_Log", "Log10(y-axis)", value=F),
                                textInput("plot_title", "Plot Title"), 
                                actionButton("plot_btnRefresh","Load", width = "100%")),
                              box(width = 10,title = tagList(shiny::icon("chart-bar"), "Analysis"), solidHeader = T, status = "primary",
                                 jqui_resizable(plotlyOutput("plot_plot", height = 700 )))
                        )
                )
            }
            ,
            { 
                tabItem(tabName = "home", 
                        fluidRow(width=12,
                                 box(width = 4, title = "Countries Overview", solidHeader = T,status = "primary",icon = icon("search-location"),
                                    img(src="fig1.png",width = '100%'),
                                    HTML("<br/><br/>"),
                                    actionButton('jump1', 'Explore Countries', width = "100%", style=" font-weight:bold")),
                                 box(width = 4, title = "Countries / Regions Maps", solidHeader = T,status = "primary",icon = icon("map-marked"),
                                     img(src="fig2.png",width = '100%'),
                                     HTML("<br/><br/>"),
                                     actionButton('jump2', 'Show Maps', width = "100%",style="font-weight:bold")),
                                 box(width = 4, title = "Countries / Regions Analysis",solidHeader = T,status = "primary",icon = icon("chart-bar"),
                                     img(src="fig3.png",width = '100%'),
                                     HTML("<br/><br/>"),
                                     actionButton('jump3', 'Analyze Data', width = "100%", style="font-weight:bold")),
                                 )
                )
            }
            ,
            {
                tabItem(tabName = "about",
                        fluidRow(width=12,
                                 box(width = 12, title = "About", solidHeader = T,status = "primary",icon = icon("search-location"),
                                     HTML(paste("<h4><b>COVID-19 Dashdoard</b></h4>",
                                                "<p>This application displays information about the COVID-19 virus in individual coutries or regions. Application is based on the data repository of <a href='https://github.com/CSSEGISandData/COVID-19' target='_blank'>Johns Hopkins University Center for Systems Science and Engineering (JHU CSSE)</a>. ",
                                                "</p><p>This application is for presentation purposes and can not be used for policy or epidemiology purposes (NO WARRANTY).",
                                                "</p><p> Application is built on the OpenSource tools and libraries - <a  href='https://www.r-project.org/' target='_blank'><b>R</b></a>, <a  href='https://shiny.rstudio.com/' target='_blank'><b>Shiny</b></a>, <a  href='https://plotly.com/r/' target='_blank'><b>Plotly</b></a> & <a  href='https://leafletjs.com/' target='_blank'><b>Leaflet</b></a>.",
                                                "</p><p>In the case of any question, feel free to <a href='mailto:mgregor@maind.sk'>contact</a> the author of the app.",
                                                "More information about the app (source code, architecture, ...) will be published soon on the <a href='http://stagraph.com/Blog' target='_blank'>blog</a>.</p>",
                                                "<ul><li>Author: <a href='https://www.linkedin.com/in/hydrooffice/' target='_blank'>Milos Gregor</a></li>",
                                                "<li>Data and visualizations were prepared in the <a href='http://stagraph.com/' target='_blank'><b>Stagraph software</b></a>.</li>",
                                                "</ul>",
                                                "<p><br/><br/>",
                                                 "<a href='http://stagraph.com/' target='_blank'><img src='stagraph.png'  style='height:60px;margin-left:40px' /></a>" ,
                                                "<a  href='https://www.r-project.org/' target='_blank'><img src='Rlogo.png'  style='height:60px;margin-left:40px' /></a>",
                                                "</p>"
                                                )
                                          )
                                     ),
                        )
                )
            }
            
        )
    )
)

server <- function(input, output,session) {
    # simple onclick events for Home page
    observeEvent(input$jump1, {updateTabsetPanel(session, "menu",selected = "dashboard")})
    observeEvent(input$jump2, {updateTabsetPanel(session, "menu",selected = "map")})
    observeEvent(input$jump3, {updateTabsetPanel(session, "menu",selected = "plot")})
    
    # owerview
    {
    output$total_cases_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>%  
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Confirmed) )
                          , by=c("region"="Country")
                          )
                temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                    geom_polygon(aes(text = region))+
                    coord_quickmap()+
                    theme_void()+
                    labs(title="Cumulative Confirmed Cases in the World")
                if(input$cb_total_cases_cb_log) {
                    temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
                } 
                else {
                    temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
                }
                ggplotly(temp_plt) %>% layout(legend = list(x = 0))
        }
    )
    output$total_cases_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Confirmed) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>Confirmed Cases in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$total_cases_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(Cases= sum(Daily_Confirmed))%>%
                arrange(Date) %>%
                mutate(Cases = cumsum(Cases))
              temp_plt <-  
                  ggplot(temp_data_2)+
                    aes(Date, Cases)+
                    geom_line(color = "steelblue", size= 1)+
                    theme_bw()+
                    labs(x = NULL,y =NULL, title = "Cumulative Confirmed Cases Over Time in the World")+
                    scale_y_continuous(labels = scales::comma)
              if(input$cb_total_cases_cb_log) {
                  temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
              }
              ggplotly(temp_plt)
        }
    )
    output$total_cases_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            summarise(Total_Deaths = sum(Daily_Confirmed)) %>% 
            arrange(desc(Total_Deaths))
        , options = list(pageLength = 25)) 

    output$total_deaths_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>%  
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Deaths) )
                          , by=c("region"="Country"))
            temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                geom_polygon(aes(text = region))+
                coord_quickmap()+
                theme_void()+
                labs(title="Cumulative Deaths in the World")
            if(input$cb_total_deaths_cb_log) {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
            } 
            else {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
            }
            ggplotly(temp_plt) %>% layout(legend = list(x = 0))
        }
    )
    output$total_deaths_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Deaths) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>Deaths in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$total_deaths_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(Deaths= sum(Daily_Deaths))%>%
                arrange(Date) %>%
                mutate(Deaths = cumsum(Deaths))
            temp_plt <-  ggplot(temp_data_2)+
                aes(Date, Deaths)+
                geom_line(color = "steelblue", size= 1)+
                theme_bw()+
                labs(x = NULL,y =NULL, title = "Cumulative Deaths Over Time in the World")+
                scale_y_continuous(labels = scales::comma)
            if(input$cb_total_deaths_cb_log) {
                temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
            }
            ggplotly(temp_plt)
        }
    )
    output$total_deaths_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            summarise(Total_Cases = sum(Daily_Deaths)) %>% 
            arrange(desc(Total_Cases))
        , options = list(pageLength = 25)) 

    output$newcases_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>% 
                              filter(Date == data_last_day)%>% 
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Confirmed))
                          , by=c("region"="Country"))%>% mutate(Value = na_if(Value , 0))
            temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                geom_polygon(aes(text = region))+
                coord_quickmap()+
                theme_void()+
                labs(title="New Cases in the World")
            if(input$cb_newcases_cb_log) {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
            } 
            else {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
            }
            ggplotly(temp_plt) %>% layout(legend = list(x = 0))
        }
    )
    output$newcases_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                filter(Date == data_last_day)%>%
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Confirmed) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>New Cases in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$newcases_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(New_Cases= sum(Daily_Confirmed))%>%
                arrange(Date)
            temp_plt <- ggplot(temp_data_2)+
                aes(Date, New_Cases)+
                geom_line(color = "steelblue", size= 1)+
                theme_bw()+
                labs(x = NULL,y =NULL, title = "New Cases Over Time in the World")+
                scale_y_continuous(labels = scales::comma)
            if(input$cb_newcases_cb_log) {
                temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
            }
            ggplotly(temp_plt)
        }
    )
    output$newcases_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            filter(Date == data_last_day)%>%  
            summarise(Total_Cases = sum(Daily_Confirmed)) %>% 
            arrange(desc(Total_Cases))
        , options = list(pageLength = 25)) 
    output$newdeaths_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>% 
                              filter(Date == data_last_day)%>% 
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Deaths)  )
                          , by=c("region"="Country")
                          )%>% 
                mutate(Value = na_if(Value , 0))
            temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                geom_polygon(aes(text = region))+
                coord_quickmap()+
                theme_void()+
                labs(title="New Deaths in the World")
            if(input$cb_newdeaths_cb_log) {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
            } 
            else {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
            }
            ggplotly(temp_plt) %>% layout(legend = list(x = 0))
        }
    )   
    output$newdeaths_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                filter(Date == data_last_day)%>%
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Deaths) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>New Deaths in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$newdeaths_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(New_Cases= sum(Daily_Deaths))%>%
                arrange(Date)
            temp_plt <-  
                ggplot(temp_data_2)+
                aes(Date, New_Cases)+
                geom_line(color = "steelblue", size= 1)+
                theme_bw()+
                labs(x = NULL,y =NULL, title = "New Deaths Over Time in the World")+
                scale_y_continuous(labels = scales::comma)
            if(input$cb_newdeaths_cb_log) {
                temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
            }
            ggplotly(temp_plt)
        }
    )
    output$newdeaths_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            filter(Date == data_last_day)%>%  
            summarise(Total_Cases = sum(Daily_Deaths)) %>% 
            arrange(desc(Total_Cases))
        , options = list(pageLength = 25)) 
    
    output$total_recovered_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>%  
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Recovered) )
                          , by=c("region"="Country"))
            temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                geom_polygon(aes(text = region))+
                coord_quickmap()+
                theme_void()+
                labs(title="Cumulative Recovered in the World")
            if(input$cb_total_recovered_cb_log) {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
            } 
            else {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
            }
            ggplotly(temp_plt) %>% 
                layout(legend = list(x = 0))
        }
    )
    output$total_recovered_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Recovered) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>Recovered in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$total_recovered_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(Deaths= sum(Daily_Recovered))%>%
                arrange(Date) %>%
                mutate(Deaths = cumsum(Deaths))
            temp_plt <-  
                ggplot(temp_data_2)+
                aes(Date, Deaths)+
                geom_line(color = "steelblue", size= 1)+
                theme_bw()+
                labs(x = NULL,y =NULL, title = "Cumulative Recovered Over Time in the World")+
                scale_y_continuous(labels = scales::comma)
            if(input$cb_total_recovered_cb_log) {
                temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
            }
            ggplotly(temp_plt)
        }
    )
    output$total_recovered_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            summarise(Total_Cases = sum(Daily_Recovered)) %>% 
            arrange(desc(Total_Cases))
        , options = list(pageLength = 25)) 

    output$newrecovered_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>% 
                              filter(Date == data_last_day)%>% 
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Recovered)  )
                          , by=c("region"="Country")
                          )%>% 
                mutate(Value = na_if(Value , 0))
            temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                geom_polygon(aes(text = region))+
                coord_quickmap()+
                theme_void()+
                labs(title="New Recovered in the World")
            if(input$cb_newrecovered_cb_log) {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
            } 
            else {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
            }
            ggplotly(temp_plt) %>% layout(legend = list(x = 0))
        }
    )
    output$newrecovered_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                filter(Date == data_last_day)%>%
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Recovered) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>New Recovered in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$newrecovered_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(New_Cases= sum(Daily_Recovered))%>%
                arrange(Date)
            temp_plt <-  
                ggplot(temp_data_2)+
                aes(Date, New_Cases)+
                geom_line(color = "steelblue", size= 1)+
                theme_bw()+
                labs(x = NULL,y =NULL, title = "New Recovered Over Time in the World")+
                scale_y_continuous(labels = scales::comma)
            if(input$cb_newrecovered_cb_log) {
                temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
            }
            ggplotly(temp_plt)
        }
    )
    output$newrecovered_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            filter(Date == data_last_day) %>%  
            summarise(Total_Cases = sum(Daily_Recovered)) %>% 
            arrange(desc(Total_Cases))
        , options = list(pageLength = 25)) 
    
    output$activecases_map <- renderPlotly(
        {
            temp_data <- data_world_map %>%
                left_join(data_country %>%  
                              group_by(Country) %>% 
                              summarise(Value = sum(Daily_Confirmed)-sum(Daily_Recovered) )
                          , by=c("region"="Country"))
            temp_plt <- ggplot(temp_data, aes(long, lat, group=group, fill= Value)) +
                geom_polygon(aes(text = region))+
                coord_quickmap()+
                theme_void()+
                labs(title="Cumulative Active Cases in the World")
            if(input$cb_activecases_cb_log) {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80", trans = "log10")
            } 
            else {
                temp_plt <-  temp_plt + scale_fill_distiller(type="div" ,palette = 9, direction =-1, na.value="grey80")
            }
            ggplotly(temp_plt) %>% layout(legend = list(x = 0))
        }
    )
    output$activecases_in_treemap <- renderPlotly(
        {
            temp_data <- data_country %>%  
                group_by(Country) %>% 
                summarise(Value = sum(Daily_Confirmed)-sum(Daily_Recovered) )
            plot_ly(
                temp_data,
                labels = ~ Country,
                color = ~ Value,
                parents = NA,
                colors =rev(RColorBrewer::brewer.pal(9,"Spectral")) ,
                values = ~ Value,
                textinfo="label+value",  pathbar=list(visible= FALSE),
                type = 'treemap', showscale=FALSE,showlegend = FALSE,
                hovertemplate = "<b> %{label}</b><br>Count: %{value}<extra></extra>")%>% 
                layout(title="<b>Active Cases in Individual Countries</b>") %>% 
                hide_colorbar()%>% 
                layout(xaxis= list(showticklabels = FALSE),yaxis= list(showticklabels = FALSE))
        }
    )
    output$activecases_in_time <- renderPlotly(
        {
            temp_data_2 <- data_country %>%
                ungroup() %>%
                group_by(Date)%>%
                summarise(Deaths= sum(Daily_Confirmed)-sum(Daily_Recovered))%>%
                arrange(Date) %>%
                mutate(Deaths = cumsum(Deaths))
            temp_plt <-  
                ggplot(temp_data_2)+
                aes(Date, Deaths)+
                geom_line(color = "steelblue", size= 1)+
                theme_bw()+
                labs(x = NULL,y =NULL, title = "Active Cases Over Time in the World")+
                scale_y_continuous(labels = scales::comma)
            if(input$cb_activecases_cb_log) {
                temp_plt <- temp_plt + scale_y_log10(labels = scales::comma) 
            }
            ggplotly(temp_plt)
        }
    )
    output$activecases_table <- renderDT(
        data_country %>%  
            group_by(Country) %>% 
            summarise(Total_Cases = sum(Daily_Confirmed)- sum(Daily_Recovered)) %>% 
            arrange(desc(Total_Cases))
        , options = list(pageLength = 25)) 
    }
    
    
    # map
    {
    output$mapview <- renderLeaflet({
        input$map_btnRefresh
        map <- leaflet() %>%
            setView(lat=51.12421 , lng=29.26758, zoom = 4) %>%
            addProviderTiles(providers$CartoDB.Positron, group = "CartoDB.Positron")%>% 
            addProviderTiles(providers$CartoDB.DarkMatter, group = "CartoDB.Dark") %>%
            addProviderTiles(providers$Esri.WorldImagery, group = "ESRI.Imagery") %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap, group = "ESRI.Map") %>%
            addLayersControl(
                baseGroups = c("CartoDB.Positron", "CartoDB.Dark","ESRI.Imagery", "ESRI.Map" ),
                overlayGroups = c( "Borders","Values"),
                options = layersControlOptions(collapsed = TRUE)
            )%>%  
            addFullscreenControl(pseudoFullscreen =T)
        isolate(
            {
            #display borders
            if(input$map_ch_display_borders) {
                map <- map %>% 
                    addPolygons(data=map(database = "world", region=".",plot = FALSE, fill=T) , group = "Borders", color="steelblue", weight=2, fillOpacity = 0 )
            }
            #setup data for display
            temp_data <- data_state %>% 
                filter(Date >= input$map_dateRange[1], Date <= input$map_dateRange[2])
            if(input$map_variableMode == "Total Cases") {
                temp_data <- temp_data %>%  
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Confirmed)) 
            }
            else if(input$map_variableMode == "Total Deaths") {
                temp_data <- temp_data %>%  
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Deaths)) 
            }
            else if(input$map_variableMode == "Total Recovered") {
                temp_data <- temp_data %>%  
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Recovered)) 
            }
            else if(input$map_variableMode == "New Cases") {
                temp_data <- temp_data %>% 
                    filter(Date == data_last_day)%>%
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Confirmed)) 
            }
            else if(input$map_variableMode == "New Deaths") {
                temp_data <- temp_data %>% 
                    filter(Date == data_last_day)%>%
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Deaths)) 
            }
            else if(input$map_variableMode == "New Recovered") {
                temp_data <- temp_data %>% 
                    filter(Date == data_last_day)%>%
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Recovered)) 
            }
            else if(input$map_variableMode == "Active Cases") {
                temp_data <- temp_data %>%  
                    group_by(Country, State, Lat, Long) %>% 
                    summarise(Value = sum(Daily_Confirmed)-sum(Daily_Recovered)) 
            }
            
            temp_data <- temp_data %>% filter(Value >0)
            pal <- colorNumeric(palette = rev(brewer.pal(11,"Spectral")),domain = temp_data$Value)
            map <- map %>% 
                addCircleMarkers( data=temp_data,
                                  lat = ~ Lat,  
                                  lng = ~ Long,
                                  radius=~ scales::rescale(Value, to = c(5, 40), from = range(Value, na.rm =TRUE, finite = TRUE)),
                                  group = "Values", 
                                  fillColor = ~ pal(Value),color="black",
                                  opacity = .8, fillOpacity=.8,weight=1, 
                                  popup=paste( "<b>" ,temp_data$Country,   if_else(!is.na(temp_data$State),paste(" - ",temp_data$State),""),"</b><br/>", input$map_variableMode,": ",temp_data$Value ),
                                  labelOptions = labelOptions(textsize = "25px")
                                 ) %>% 
                addLegend( "bottomright",title =input$map_variableMode, pal = pal, values = temp_data$Value, opacity = 1)
        }
        )
        map
    }
    )
    }
    #plot
    {
        output$plot_plot <- renderPlotly({
            input$plot_btnRefresh
            if (!is.null(input$plot_regions)){
            isolate(
                {
                # setup dataset
                    temp_data <- data_state
                    if(input$plot_dataset == "Countries"){
                        temp_data <- temp_data %>%
                            mutate(Area = Country)
                    }else {
                        temp_data <- temp_data %>%
                            mutate(Area = if_else(
                                is.na(State),
                                paste(Country),
                                paste(Country, State, sep = " - ")
                            )
                            )
                    }
                    # set time variable
                    temp_first_record <- data.frame()
                    if(input$plot_time == "Days From 1 Case"){
                        temp_first_record <- temp_data %>% 
                            filter(Area %in% input$plot_regions)%>% 
                            group_by(Area) %>%
                            arrange(Date)%>%
                            filter(Cum_Confirmed >= 1) %>%
                            slice(n=1)%>%
                            select(Area,Date)
                    }
                    if(input$plot_time == "Days From 10 Cases"){
                        temp_first_record <- temp_data %>%
                            filter(Area %in% input$plot_regions)%>% 
                            group_by(Area) %>%
                            arrange(Date)%>%
                            filter(Cum_Confirmed >= 10) %>%
                            slice(n=1)%>%
                            select(Area,Date)
                    }
                    if(input$plot_time == "Days From 100 Cases"){
                        temp_first_record <- temp_data %>% 
                            filter(Area %in% input$plot_regions)%>% 
                            group_by(Area) %>%
                            arrange(Date)%>%
                            filter(Cum_Confirmed >= 100) %>%
                            slice(n=1)%>%
                            select(Area,Date)
                    }
                    if(input$plot_time == "Days From 1 Death"){
                        temp_first_record <- temp_data %>% 
                            filter(Area %in% input$plot_regions)%>% 
                            group_by(Area) %>%
                            arrange(Date)%>%
                            filter(Cum_Deaths >= 1) %>%
                            slice(n=1)%>%
                            select(Area,Date)
                    }
                    if(input$plot_time == "Days From 10 Deaths"){
                        temp_first_record <- temp_data %>% 
                            filter(Area %in% input$plot_regions)%>% 
                            group_by(Area) %>%
                            arrange(Date)%>%
                            filter(Cum_Deaths >= 10) %>%
                            slice(n=1)%>%
                            select(Area,Date)
                    }
                    if(input$plot_time == "Days From 100 Deaths"){
                        temp_first_record <- temp_data %>% 
                            filter(Area %in% input$plot_regions)%>% 
                            group_by(Area) %>%
                            arrange(Date)%>%
                            filter(Cum_Deaths >= 100) %>%
                            slice(n=1)%>%
                            select(Area,Date)
                    }
                    temp_data <- temp_data %>%
                        select(-Country, - State, -Lat, -Long)%>%
                        mutate(Value = case_when (
                                            input$plot_variable == "Total Cases" ~ (Cum_Confirmed),
                                            input$plot_variable == "Total Deaths" ~ (Cum_Deaths),
                                            input$plot_variable == "Total Recovered" ~ (Cum_Recovered), 
                                            input$plot_variable == "New Cases" ~ (Daily_Confirmed), 
                                            input$plot_variable == "New Deaths" ~ (Daily_Deaths), 
                                            input$plot_variable == "New Recovered" ~ (Daily_Recovered), 
                                            input$plot_variable == "Active Cases" ~ (Cum_Confirmed-(Cum_Recovered+Cum_Deaths))
                                        ))%>%
                        group_by(Area,Date) %>%
                        summarise(Value = sum(Value))
                    temp_filtered <- temp_data %>%
                        filter(Area %in% input$plot_regions)
                    # set time variable 2
                    if(input$plot_time == "Days From Start"){
                        temp_filtered <- temp_filtered %>%
                            mutate(Date = Date - data_first_day)
                    } else {
                        if (input$plot_time != "Date"){
                            temp_filtered <- temp_filtered %>%
                             filter(Area %in% (temp_first_record$Area))%>%
                             left_join(temp_first_record, by= c("Area"="Area"),suffix = c("", ".y"))%>%
                             mutate(Date = Date-Date.y)%>%
                             select(-Date.y) %>%
                            filter(Date>=0)
                        }
                    }
                    # draw graph
                    fplot <- ggplot(temp_filtered)
                    if(input$plot_sum){
                        fplot <- fplot + geom_area(aes(Date,Value,fill=Area), color="black", alpha=0.8, position = position_stack())
                    } else {
                        fplot <- fplot + geom_line(aes(Date,Value,color=Area), size=1)
                    }
                    fplot <- fplot + theme_bw()+
                        labs(y=input$plot_variable, x=input$plot_time, color=NULL, fill=NULL)+
                        scale_y_continuous()
                    if( nchar(input$plot_title)>0){
                        fplot <- fplot + labs(title = input$plot_title)
                    }
                    if(input$plot_Log==T){
                        fplot <- fplot + scale_y_log10() 
                    }
                    ggplotly(fplot)
                    }
                )
            }
         }
     )
    }
    # plot_regions
    output$myUI_1 <- renderUI({
        if(input$plot_dataset == "Countries"){
                names <- data_state %>%
                    select(Country) %>%
                    distinct() %>%
                    arrange(Country)
            } else {
                names <- data_state %>%
                    select(Country, State) %>%
                    mutate(Region = if_else(
                        is.na(State),
                        paste(Country),
                        paste(Country, State, sep = " - "))
                        )%>%
                    select(Region) %>%
                    distinct() %>%
                    arrange(Region)
            }
            selectInput("plot_regions",paste("Select", input$plot_dataset ),choices = (names), multiple = T, selected = NULL)
        })
}
# Run the application 
shinyApp(ui = ui, server = server)