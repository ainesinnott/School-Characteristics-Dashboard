library(shiny)
library(ggplot2)
library(maps)
library(plotly)
library(readxl)
library(dplyr)

# Reading in data on schools
schools <- read_excel(path = "Consolidated 6.0.xlsx")

#Updating variable types 
schools$Day_Fee = as.numeric(schools$Day_Fee)
schools$'5_Day_Fee' = as.numeric(schools$'5_Day_Fee')
schools$'7_Day_Fee' = as.numeric(schools$'7_Day_Fee')

# Getting the map data for Ireland
ireland_with_aran <- map_data("world", region = "Ireland")

# Splitting data into mainland and the Aran Islands
mainland <- subset(ireland_with_aran, group == "2")
aran <- subset(ireland_with_aran, group == "1")

# Default filter values
default_values <- list(
  "county" = "All",
  "search" = "",
  "total_p" = c(3, 1020),
  "prim_irish_lan" = "All",
  "gender" = "All",
  "fee" = "All",
  "total_pp" = c(5, 1518),
  "irish_lan" = "All",
  "total_fe" = c(150, 1456),
  "total_t" = c(1355, 32130),
  "DEIS" = "All",
  "ethos" = "All"
)

# Shiny UI
ui <- fluidPage(
  #Style features for title panel and horizontal scrolling
  tags$head(
    tags$style(
      HTML("
.sticky-title {
  position: sticky;
  top: 0;
  z-index: 1000;
  width: 100%;
  background-color: #2874a6;
    color: #74a8e8;
    border-bottom: 1px solid #ccc;
  border-radius: 5px;
  padding: 5px 10px; /* Adjusted padding */
    height: 80px; /* Adjusted height */
}
   .dataTables_wrapper {
          overflow-x: auto !important;
          overflow-y: auto !important;
        }
      ") 
    )
  ),
  #Title Panel
  div(
    class = "sticky-title",
    titlePanel("Schools in Ireland")
  ),
  #Background Colour for entire UI
  tags$style(HTML("body { background-color: #dcf8fb; }")),
  
  #Organising Content into tabs
  tabsetPanel(
    #Map tab
    tabPanel("Interactive Map",
             id = "mapTab", 
             style = "background-color: #dcf8fb; height: 100vh;",
             fluidRow(
               #Side panel for filter options
               column(
                 width = 3,  offset = 2,
                 style = "background-color: #87ceeb;border: 1px solid #ccc; border-radius: 5px; padding: 10px;",
                 #Institution Level drop-down List
                 selectInput("level",
                             "Institution Level:",
                             choices = c("Primary", "Post Primary", "Further Education", "Third Level"),
                             selected = "Primary"
                 ),
                 #Search Bar
                 textInput("search", "Search by School Name or Address:"),
                 #County Drop-Down List
                 selectInput("county","County",choices=c("All","Carlow","Cavan","Clare",
                                                         "Cork","Donegal","Dublin",
                                                         "Galway","Kerry","Kildare",
                                                         "Kilkenny","Laois","Leitrim",
                                                         "Limerick","Longford","Louth",
                                                         "Mayo","Meath","Monaghan","Offaly",
                                                         "Roscommon","Sligo","Tipperary",
                                                         "Waterford","Westmeath","Wexford",
                                                         "Wicklow"),
                             selected="All"),
                 #Filters that are available for primary Schools Only
                 conditionalPanel(
                   condition = "input.level == 'Primary'", 
                   sliderInput("total_p", "Total Students:",
                               min = 3, max = 1020,
                               value = c(3,1020)),
                   selectInput("prim_irish_lan","Irish Language Teaching:", choices = c("All","Y","N"), selected = "All"),
                   
                 ),
                 #Post Primary Schools Only 
                 conditionalPanel(
                   condition = "input.level == 'Post Primary'",
                   selectInput("gender","Gender:", choices = c("All","Boys","Girls","Mixed"), selected = "All"),
                   selectInput("fee","Fee Paying Status:", choices = c("All","Y","N"), selected = "All"),
                   sliderInput("total_pp", "Total Students:",
                               min = 5, max = 1518,
                               value = c(5,1518)),
                   selectInput("irish_lan","Irish Medium Teaching: ", choices = c("All","All pupils taught all subjects through Irish",
                                                                                  "Some pupils taught some subjects through Irish",
                                                                                  "Some pupils taught all subjects through Irish", 
                                                                                  "No subjects taught through Irish"), selected ="All")
                 ),
                 #Further Education Only
                 conditionalPanel(
                   condition = "input.level == 'Further Education'", 
                   sliderInput("total_fe", "Total Students:",
                               min = 150, max = 1456,
                               value = c(150,1456))
                   
                 ),
                 #Third Level Only 
                 conditionalPanel(
                   condition = "input.level == 'Third Level'", 
                   sliderInput("total_t", "Total Students:",
                               min = 1355, max = 32130,
                               value = c(1355,32130))
                   
                 ),
                 #Primary or Post-primary levels
                 conditionalPanel(
                   condition = "input.level=='Post Primary' || input.level=='Primary'",
                   selectInput("DEIS","DEIS:", choices =c("All","Y","N"),selected ="All"),
                   selectInput("ethos","Religious Ethos:", choices = c("All","Catholic",
                                                                       "Church of Ireland",
                                                                       "Interdenominational",
                                                                       "Jewish","Methodist",
                                                                       "Multidenominational",
                                                                       "Muslim","Presbyterian",
                                                                       "Quaker"), selected ="All")
                 ),
                 #Button for resetting all filters
                 actionButton("reset", "Reset Filters")
               ),
               #Adding map and positioning in center of page
               column(
                 width = 5,
                 style = "background-color: #dcf8fb; height: 800px;", 
                 plotlyOutput("mapPlot"),
                 #Adding the legend with bullet points
                 div(
                   p(HTML("
                    <ul style='list-style-type: disc; margin-left: -10px;'>
                    <li>To zoom in, click the 'Zoom' function (Magnifying Glass) in the top menu and highlight the area you would like to see more closely.</li>
                    <li>To pan, click the pan function (Crossing Arrows) and drag the map.</li>
                    <li>To return to the original map, click 'Reset Axes' (The Home Icon).</li>
                    </ul> "),
                     style = "color: #2874a6; font-size: 24px; margin-top: 200px; width:500px;")
                 )
               )
             )
    ),
    
    #Schools Data
    tabPanel("Schools Data",
             style = "background-color: #dcf8fb; color: #2874a6",
             
             dataTableOutput("schoolNames"))
  )
  
)

# Shiny Server
server <- function(input, output,session) {
  
  # Resetting all input values to their default when the reset button is clicked
  observeEvent(input$reset, {
    lapply(names(default_values), function(id) {
      if (grepl("total", id)) {
        updateSliderInput(session, id, value = default_values[[id]])
      } else {
        updateSelectInput(session, id, selected = default_values[[id]])
      }
    })
  })
  
  #Rendering Interactive Map
  output$mapPlot <- renderPlotly({
    #Filtering schools data based on user input
    filtered_data <- schools %>%
      filter(Level == input$level,
             if (input$county != "All") County == input$county else TRUE,
             if (input$ethos != "All") Ethos == input$ethos else TRUE,
             if (input$gender != "All") Gender == input$gender else TRUE,
             if (input$fee != "All") Fee == input$fee else TRUE,
             if (input$DEIS!="All") DEIS == input$DEIS else TRUE,
             if (input$irish_lan!="All")Irish_Lan == input$irish_lan else TRUE,
             if (input$prim_irish_lan !="All")Irish_Lan == input$prim_irish_lan else TRUE,
             grepl(input$search, Off_Name, ignore.case = TRUE) | grepl(input$search, Address, ignore.case = TRUE),
             if (input$level == "Primary") {
               Total >= input$total_p[1] & Total <= input$total_p[2]
             } else if(input$level== "Post Primary") {
               Total>=input$total_pp[1] &Total <= input$total_pp[2]
             }else if (input$level == "Further Education") {
               Total >= input$total_fe[1] & Total <= input$total_fe[2]
             } else if (input$level == "Third Level") {
               Total >= input$total_t[1] & Total <= input$total_t[2]
             } else {
               TRUE
             })
    
    #Creating ggplot of map
    gg <- ggplot() +
      geom_polygon(data = mainland, aes(x = long, y = lat), fill = "white", color = "black") +
      geom_polygon(data = aran, aes(x = long, y = lat), fill = "white", color = "black") +
      
      theme_minimal() +
      labs(title = "Map of Ireland") +
      geom_point(
        data = filtered_data,
        #Adding information to be displayed on hover
        aes(x = Long, y = Lat,
            text = paste("Name:", Off_Name, "<br>Address:", Address,
                         if (input$level == "Post Primary") paste("<br>Third Level Progression Rate: ", Third_Level_Progression, "%"),
                         ifelse(Day_Fee != 0, paste("<br>Day Fee: €", Day_Fee), ""),
                         ifelse(`5_Day_Fee` != 0, paste("<br>5 Day Fee: €", `5_Day_Fee`), ""),
                         ifelse(`7_Day_Fee` != 0, paste("<br>7 Day Fee: €", `7_Day_Fee`), "")
            )
            
        ), color = "skyblue1",
        
        size = 5, alpha = I(0.7)
      )
    #Converting ggplot object to plotly
    ggplotly(gg, tooltip = "text",width =500, height = 600) %>%
      layout(showlegend = FALSE)
  })
  
  #Rendering school data table
  output$schoolNames <- renderDataTable({
    # Outputting table of school characteristics
    data.frame(School_Name = schools$Off_Name,
               Roll_Number = schools$Roll_No,
               Address = schools$Address,
               Level = schools$Level,
               Ethos = schools$Ethos,  
               "Third Level Progression" = schools$Third_Level_Progression,
               "Total Students" = schools$Total,
               Gender = schools$Gender,
               "Island Status" = schools$Island,
               Gaeltacht = schools$Gaeltacht,
               Fee = schools$Fee,
               "Day Fee" = schools$'Day_Fee',
               "Five Day Fee" = schools$'5_Day_Fee',
               "Seven Day Fee" = schools$'7_Day_Fee',
               "Irish Language" = schools$Irish_Lan,
               DEIS = schools$DEIS
               
    )
  })
}

# Running the application 
shinyApp(ui = ui, server = server)
