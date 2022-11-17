#SET UP
#Load packages
pacman::p_load(ggplot2, 
               readxl, 
               dplyr, 
               tidyr, 
               DT, 
               knitr, 
               lubridate, 
               stringr, 
               here, 
               summarytools, 
               magrittr, 
               tidyverse, 
               flexdashboard,
               leaflet,
               shiny,
               plotly,
               data.table,
               dygraph,
               htmlwidget,
               sf,
               Hmisc,
               shinyjs,
               RColorBrewer,
               shinythemes,
               thematic,
               shades,
               shinyalert,
               mapview,
               mapshot,
               phantomjs,
               textclean
               )

#thematic_shiny()

#Load in the data
#Geographical data
ICB<- sf::st_read("U:/Data Vis/Data/Geography/Simplify_geography.geojson", 
                  as_tibble = TRUE)

#HES data
data<- read.csv("U:/Data Vis/Data/Bariatric_surgery/NHSD_bariatric surgery_v0.1.csv", check.names = FALSE,
                fileEncoding = "UTF-8")

#Get rid of provider organisation code breakdown, not useful
data<- subset(data,
              Organisation_breakdown != "Provider Organisation Code")

#Formatting and tidy the data
ICB$ICB22NM<- toupper(ICB$ICB22NM)
colnames(ICB)[which(names(ICB) == "ICB22NM")] <- "Org_Name"

data$MEASURE_VALUE <- as.numeric(as.character(data$MEASURE_VALUE))
data$MEASURE_VALUE[is.na(data$MEASURE_VALUE)] <- 0

data$MEASURE_NAME<- case_when(
  data$MEASURE_NAME == "DEMOGRAPHIC_DEPRIVATION" ~ "Deprivation",
  data$MEASURE_NAME == "DEMOGRAPHIC_AGE" ~ "Age",
  data$MEASURE_NAME == "DEMOGRAPHIC_ETHNICITY" ~ "Ethnicity",
  data$MEASURE_NAME == "DEMOGRAPHIC_GENDER" ~ "Gender",
  data$MEASURE_NAME == "PRIMARY_BARIATRIC_SURGICAL_PROCEDURE" ~ "Patient count",
  data$MEASURE_NAME == "RATE PER 100,000" ~ "Rate per 100,000",
  data$MEASURE_NAME == "PERCENTAGE_TREATED_WITHIN_ICB " ~ "Percentage of patients treated within ICB",
  TRUE ~ data$MEASURE_NAME
)

# data$Org_Name <- tolower(data$Org_Name)
# data$Org_Name <- capitalize(data$Org_Name)

#Create a CCG_small group so don't produce an output for these
CCG_small<- c("Bassetlaw CCG",
              "Blackburn with Darwen CCG",
              "Blackpool CCG",
              "Bury CCG",
              "Chorley and South Ribble CCG",
              "East Lancashire CCG",
              "East Staffordshire CCG",
              "Knowsley CCG",
              "Leicester City CCG",
              "Morecambe Bay CCG",
              "Oldham CCG",
              "Salford CCG",
              "South Sefton CCG",
              "Southport and Formby CCG",
              "Stockport CCG",
              "Trafford CCG",
              "Warrington CCG",
              "West Lancashire CCG"
)

#Not in function
'%notin%'<- Negate('%in%')

#Mapped data
#Join the data and geography data together
df_to_plot <- ICB %>% 
  left_join(data, by = c("Org_Name")) %>% 
  select(-c("OBJECTID", 
            "BNG_E",        
            "BNG_N",        
            "LONG",        
            "LAT",          
            "GlobalID",    
            "SHAPE_Length", 
            "SHAPE_Area")) 


#Subset the data to only relevant fields
df_to_plot<- subset(df_to_plot,
                    MEASURE_NAME %in% c("RATE PER 100,000",
                                        "PRIMARY_BARIATRIC_SURGICAL_PROCEDURE",
                                        "PERCENTAGE_TREATED_WITHIN_ICB"),
                    select = c(Financial_Year, 
                               ICB22CD, 
                               Org_Name, 
                               MEASURE_NAME, 
                               MEASURE_VALUE))

#Formatting and tidying
# df_to_plot$MEASURE_NAME[df_to_plot$MEASURE_NAME == 'PRIMARY_BARIATRIC_SURGICAL_PROCEDURE'] <- 'Patient count'
# df_to_plot$MEASURE_NAME[df_to_plot$MEASURE_NAME == 'RATE PER 100,000'] <- 'Rate per 100,000'
# df_to_plot$MEASURE_NAME[df_to_plot$MEASURE_NAME == 'PERCENTAGE_TREATED_WITHIN_ICB'] <- 'Percentage of patients treated within ICB'

df_to_plot$MEASURE_VALUE <- as.numeric(as.character(df_to_plot$MEASURE_VALUE))

#Tidy up the names of organisations
data$Org_Name<- trimws(textclean::mgsub(data$Org_Name,
                      c("NHS", "INTEGRATED CARE BOARD", "COMMISSIONING REGION"),
                      c("", "ICB", "")))

df_to_plot$Org_Name<- trimws(textclean::mgsub(df_to_plot$Org_Name,
                                              c("NHS", "INTEGRATED CARE BOARD", "COMMISSIONING REGION"),
                                              c("", "ICB", "")))


#Graph 1 data
graph1_data<- subset(data,
                     CURRENCY == "COUNT" &
                       MEASURE_NAME %in% c("PRIMARY_BARIATRIC_SURGICAL_PROCEDURE", 
                                           "GASTRIC_BALLOONS_BUBBLES", 
                                           "REVISION_PROCEDURE"))

graph1_data$MEASURE_VALUE<- as.numeric(as.character(graph1_data$MEASURE_VALUE))
graph1_data$MEASURE_VALUE[is.na(graph1_data$MEASURE_VALUE)] <- 0
graph1_data$MEASURE_NAME <- gsub("_", " ", graph1_data$MEASURE_NAME)
graph1_data$MEASURE_NAME<- tolower(graph1_data$MEASURE_NAME)
graph1_data$MEASURE_NAME <- capitalize(graph1_data$MEASURE_NAME)

#Adding percentage column for Graph 2
graph1_data<- group_by(graph1_data,
                       Financial_Year,
                       Org_Name) %>%
  mutate(Percent = round(MEASURE_VALUE/sum(MEASURE_VALUE)*100,0))

graph1_data$Percent[is.na(graph1_data$Percent)] <- 0

#Graph 2 data
graph2_data<- subset(data,
                     MEASURE_NAME == "COUNT_PRIMARY_BARIATRIC_SURGICAL_PROCEDURE_TYPE")

graph2_data$MEASURE_VALUE <- as.numeric(as.character(graph2_data$MEASURE_VALUE))
graph2_data$MEASURE_VALUE[is.na(graph2_data$MEASURE_VALUE)] <- 0
graph2_data$CURRENCY <- tolower(graph2_data$CURRENCY)
graph2_data$CURRENCY <- capitalize(graph2_data$CURRENCY)

#Adding percentage column for Graph 2
graph2_data<- group_by(graph2_data, Financial_Year, Org_Name) %>% 
  mutate(Percent = round(MEASURE_VALUE/sum(MEASURE_VALUE)*100,0))

#Change NaN values to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

graph2_data[is.nan(graph2_data)] <- 0

#Demographic graph
demographic_graph<- subset(data, MEASURE_ID %in% c("4.1", "4.2", "4.3", "4.4"))

#Adding percentage column for Graph 2
demographic_graph<- group_by(demographic_graph, Financial_Year, Org_Name, MEASURE_NAME) %>% 
  mutate(Percent = round(MEASURE_VALUE/sum(MEASURE_VALUE)*100,0))

#Change NaN values to 0
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

demographic_graph[is.nan(demographic_graph)] <- 0

#Colours for deprivation
#Define the number of colors you want
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(nb.cols)

#--------------------------------------------------------------------------------------

#APP

# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  theme = shinytheme("cerulean"),
  
  titlePanel("Primary bariatric surgery inequalities dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      #style = "position:fixed;width:inherit;",
      width = 2,
      shinyjs::useShinyjs(),
      id = "side-panel",
      h3("Breakdowns"),
      
      p("Please select from the various breakdowns below to filter the graphs, tables and map that appear on the ride hand plane"),
      
      conditionalPanel(condition = "input.conditionedPanelsTab1 == 2",
                       
                       selectInput("organisation_type", label = "Select Organisation Type:",
                                   choices = unique(data$Organisation_breakdown),
                                   selected = "National"),
                       
                       selectInput("organisation", label = "Select Organisation:",
                                   choices = unique(data$Org_Name),
                                   selected = "England")),
      
      conditionalPanel(condition = "input.conditionedPanelsTab1 == 3",
                       
                       selectInput("year", label = "Select Year:",
                                   choices = unique(data$Financial_Year),
                                   selected = "2021-2022"),
                       
                       selectInput("measure", label = "Select a measure type:",
                                   choices = unique(df_to_plot$MEASURE_NAME),
                                   selected = "Rate per 100,000")),
      
      conditionalPanel(condition = "input.conditionedPanelsTab1 == 4",
                       
                       selectInput("organisation_type1", label = "Select Organisation Type:",
                                   choices = unique(demographic_graph$Organisation_breakdown),
                                   selected = "National"),
                       
                       selectInput("organisation1", label = "Select Organisation:",
                                   choices = unique(demographic_graph$Org_Name),
                                   selected = "England"),
                       
                       selectInput("year1", label = "Select Year:",
                                   choices = unique(demographic_graph$Financial_Year),
                                   selected = "2021-2022"),
                       
                       selectInput("demographic", label = "Select Demographic:",
                                   choices = unique(demographic_graph$MEASURE_NAME),
                                   selected = "Age")),
      
      actionButton("reset", label = "Reset filters")
      
    ),
    
    mainPanel(id = "main",
              
              tabsetPanel(id = "conditionedPanelsTab1",
                          tabPanel("About and User Guide", 
                                   value=1,
                                   tags$h3("About this dashboard"),
                                   HTML("<p> The National Obesity Audit (NOA) was launched in April 2022 to bring together
                                comparable data from the different types of adult and children's weight management
                                services across England in order to drive improvement in quality of care available
                                to those living with overweight and obesity in England. NOA has been established by 
                                NHS England as part of the National Clinical Audit and Patient Outcomes Programme 
                                (NCAPOP). See <a href='https://digital.nhs.uk/data-and-information/clinical-audits-and-registries/national-obesity-audit'>National Obesity Audit</a> for more information.</p>
                                
                                <p> This dashboard contains ffinal data for 2017-18 to 2020-21 and provisional data for
                                2021-22 on people receiving bariatric surgical procedures. All data is sourced from 
                                Hospital Episode Statistics, NHS Digital. Further details on the methodology and data 
                                quality are on the <a href='https://digital.nhs.uk/data-and-information/publications/statistical/national-obesity-audit/bariatric-surgical-procedures-2021-22-provisional/content'>publication page</a>.
                                As the methodology used to derive these figures has been newly developed with clinical
                                input, these numbers should be viewed as developmental until NHS Digital further refine
                                the methdology with users and stakeholders.</p>
                                
                                <p> Note: Data for 2017-18 to 2020-21 are presented at National, NHS England Region, Integrated
                                Care Board (ICB) and Clinical Commissioning Group (CCG). For trend purposes CCGs have been mapped 
                                to April 2021 boundaries, incorporating CCG mergers. From 1st of July 2022, <a href='https://www.england.nhs.uk/integratedcare/integrated-care-in-your-area/'>Integrated Care Boards</a> 
                                were established within <a href='https://www.england.nhs.uk/integratedcare/integrated-care-in-your-area/'>Integrated Care Systems</a>
                                and replaced Sustanability and Transformation Plans (STPs). Data is also presented by ICB 
                                (rather than STP), whereby CCG data has been mapped to the ICBs in place post 1st of July 2022, 
                                which relfects latest boundary changes.</p>
                                "),
                                   tags$h3("User Guide"),
                                   tags$h5("Instructions:"),
                                   HTML("<p> <ul>
                                   <li> Use the left hand side panel to filter the graphs, tables or map that appear to
                                   what you are interested in viewing. </li>
                                   <li> When using the filters it is best to use them from top to bottom, starting with the
                                        organisational level desired. </li>
                                   <li> To reset the filters to default settings press the 'Reset Filters' button at the 
                                        bottom of the left hand side panel. </li>
                                   <li> To view the value of a specified visual element, hover over the data on the chart visual,
                                        it will expand the information. </li>
                                   <li> You are able to download all graphs by clicking on the camera icon on the top right hand
                                        side corner of the graph. </li> </ul> </p>
                                        
                                        <p> <span class = 'bolded'> Please note percentages may sum up to more or less than 100%
                                        due to effects of suppression, especially on small numbers. Users should be aware that 
                                        percentages calculated using the rounded numerator and denominator will differ from the
                                        percentage that could be calculated using unrounded numbers. <a href=https://digital.nhs.uk/data-and-information/data-tools-and-services/data-services/hospital-episode-statistics/users-uses-and-access-to-hospital-episode-statistics'>See HES Analysis guide (section 6.1.4)</a></span></p> 
                                        ")
                                   
                          ),
                          
                          tabPanel("Bariatric surgical procedures", 
                                   value =2,
                                   tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"
                                   ),
                                   br(),
                                   br(),
                                   span(textOutput("text"), style = "color:red"),
                                   useShinyalert(),
                                   plotlyOutput("graph1", height = 600),
                                   br(),
                                   br(),
                                   plotlyOutput("graph2", height = 600)),
                          
                          
                          tabPanel("Primary bariatric procedures at ICB level",
                                   value = 3,
                                   tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"
                                   ),
                                   br(),
                                   textOutput("mapTitle"),
                                   tags$head(tags$style("#mapTitle{font-size: 20px;}")),
                                   # 1. js to get width/height of map div
                                   tags$head(tags$script('
                                   var dimension = [0, 0];
                                   $(document).on("shiny:connected", function(e) {
                                   dimension[0] = document.getElementById("map").clientWidth;
                                   dimension[1] = document.getElementById("map").clientHeight;
                                   Shiny.onInputChange("dimension", dimension);
                                   });
                                   $(window).resize(function(e) {
                                   dimension[0] = document.getElementById("map").clientWidth;
                                   dimension[1] = document.getElementById("map").clientHeight;
                                   Shiny.onInputChange("dimension", dimension);
                                   });')),
                                   br(),
                                   leafletOutput("map", height = 600),
                                   br(),
                                   downloadButton("dl", "Download Map"),
                                   br(),
                                   br(),
                                   br(),
                                   dataTableOutput("table")),
                          
                          tabPanel("Demographic breakdowns for primary bariatric procedures",
                                   value = 4,
                                   tags$style(type="text/css",
                                              ".shiny-output-error { visibility: hidden; }",
                                              ".shiny-output-error:before { visibility: hidden; }"
                                   ),
                                   useShinyalert(),
                                   br(),
                                   span(textOutput("text1"), style = "color:red"),
                                   plotlyOutput("demographic_distribution", height = 600),
                                   br(),
                                   br(),
                                   plotlyOutput("graph3", 
                                                height = 1200,
                                                width = 1200))
              )
              
    )
  )
)


server <- function(input, output, session){
  
  #Changes the organisation in accordance to the organisation type chosen
  observe({
    updateSelectInput(session, "organisation", 
                      choices = unique(data$Org_Name[data$Organisation_breakdown == input$organisation_type #&
                                                     #data$MEASURE_VALUE != "*"
                      ]))
  })
  
  #Changes the organisation in accordance to the organisation type chosen
  observe({
    updateSelectInput(session, "organisation1", 
                      choices = unique(demographic_graph$Org_Name[demographic_graph$Organisation_breakdown == input$organisation_type1 #&
                                                                  #data$MEASURE_VALUE != "*"
                      ]))
  })
  
  #Reset button
  observeEvent(input$reset, {
    shinyjs::reset("side-panel")
    rv$click<- NULL
  })
  
  
  #Graph 1 - Bariatric surgery yearly counts
  output$graph1<- renderPlotly({
    if(input$organisation %notin% CCG_small){
      
      ggplotly(ggplot(subset(graph1_data,
                             Organisation_breakdown == input$organisation_type & 
                               Org_Name ==  input$organisation),
                      aes(x = Financial_Year, 
                          y = Percent,
                          fill = factor(MEASURE_NAME,
                                        levels = c("Gastric balloons bubbles",
                                                   "Revision procedure",
                                                   "Primary bariatric surgical procedure")),
                          text = paste0(#"<b>Organisation: </b>", input$organisation, "\n",
                                        "<b>Procedure type: </b>", MEASURE_NAME, "\n",
                                        "<b>Percent: </b>", Percent, "%"))) 
               + geom_bar(stat = "identity", 
                          position = "stack") 
               + scale_fill_manual(values = c("#89CFF0",
                                              "#6495ED",
                                              "#00008B"))
               #+ shades::lightness(scale_fill_brewer(palette = "Blues"), scalefac(0.9)) 
               + guides(fill=guide_legend(title="Procedure type")) 
               + labs(x = "Finanical Year", 
                      y = "Percent (%)")
               + theme(plot.margin = margin(t = 10,
                                            b = 10))
               + scale_y_continuous(expand = c(0, 0)), 
               tooltip = "text") %>%
        layout(
          annotations = list(x = 0,
                             y = 1,
                             text = paste0("Number of people having primary bariatric surgical procedures, revision surgical procedures or ", "\n", "gastric balloons & bubbles between 2017 and 2022 in ", input$organisation),
                             showarrow = F,
                             xref='paper',
                             yref='paper',
                             xanchor = 'left',
                             yanchor='bottom',
                             xshift=0,
                             yshift=0,
                             font = list(size=14, colour="grey")),
          hovermode = "x") %>%
        config(toImageButtonOptions = list(filename = paste0('Percentage_procedures_2017-2022_',input$Org_Name), 
                                           format = "png", 
                                           width = 1200,
                                           height = 700),
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))
      
    }})
  
  
  #Graph 2 - Type of bariatric surgery time-series
  
  output$graph2<- renderPlotly({
    
    if(input$organisation %notin% CCG_small){
      
      ggplotly(ggplot(subset(graph2_data,
                             Organisation_breakdown == input$organisation_type &
                               Org_Name == input$organisation),
                      aes(x = Financial_Year,
                          y = MEASURE_VALUE,
                          group = CURRENCY,
                          color = CURRENCY,
                          text = paste0(#"<b>Organisation: </b>", input$organisation, "\n", 
                                        "<b>Procedure type: </b>", CURRENCY, "\n", 
                                        "<b>Count: </b>", MEASURE_VALUE))) 
               + geom_line(stat = "identity") 
               + scale_color_manual("Procedure type", 
                                    values = c("#BCD2E8", 
                                               "#439CEF", 
                                               "#2F7AE5", 
                                               "#0836C1", 
                                               "#08204F")) 
               + scale_y_continuous(expand = c(0, 0)) 
               + labs(x = "Finanical Year", 
                      y = "Count")
               + theme(plot.margin = margin(t = 10,
                                            b = 10)),  
               tooltip = "text") %>%
        layout(annotations = list(x = 0, 
                                  y = 1, 
                                  text = paste0("Proportion of people who had primary bariatric surgical procedures between 2017 and 2022 by procedure type in ", "\n", input$organisation),
                                  showarrow = F, 
                                  xref='paper', 
                                  yref='paper',
                                  xanchor='left', 
                                  yanchor='bottom', 
                                  xshift=0, 
                                  yshift=0,
                                  font = list(size=14, 
                                              colour="grey")),
               hovermode = "x unified") %>%
        config(toImageButtonOptions = list(filename =paste0('Count_bariatric_surgery_type_2017-2022_', input$organisation), 
                                           format = "png", 
                                           width = 1200,
                                           height = 700),
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))
      
    }
    
  })
  
#Error message pop up for CCGs that are too small
  observeEvent(input$organisation, {
    if(input$organisation %in% CCG_small){
      shinyalert(
        title = "Error",
        text = "Cannot plot the data due to small numbers. Please try another breakdown.",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        imageUrl = "",
        animation = TRUE) 
    }
  })
  
  output$text<- renderText({
    if(input$organisation %in% CCG_small){
      
      paste("Cannot plot the data due to small numbers. Please try another breakdown.")
      
    }})
  
  mapFiltered<- reactive({
    rowsinbreakdown<- which(df_to_plot$Financial_Year == input$year
                            & df_to_plot$MEASURE_NAME == input$measure)
    df_to_plot[rowsinbreakdown,]
  })
  
  # Set up reactive values to store variables
  rv <- reactiveValues(click = NULL)
  
  # Asign the clicked shape ID to the reactive variable rv$click
  observeEvent(input$map_shape_click, {
    rv$click <- input$map_shape_click
  })
  
  
  output$mapTitle<- renderText({
    
    paste0("Map displaying primary bariatric procedure by ", input$measure, " in each ICB of residence for ", input$year)
    
  })
  
  
  #Map output

  # Create foundational leaflet map
  # and store it as a reactive expression
  foundational.map <- reactive({
    
    pal<- colorBin("Blues",
                   domain = mapFiltered()$MEASURE_VALUE,
                   na.color = "#808080",
                   bins = if(input$measure %in% "Rate per 100,000"){
                     seq(0,
                         max(mapFiltered()$MEASURE_VALUE)+5,
                         by = 5)
                   } else if(input$measure %in% "Patient count"){
                     seq(0,
                         max(mapFiltered()$MEASURE_VALUE)+100,
                         by = 100)
                   } else if(input$measure %in% "Percentage of patients treated within ICB"){
                     seq(0,
                         max(mapFiltered()$MEASURE_VALUE),
                         by = 20)
                   }
    )
    
      leaflet(mapFiltered()) %>%
        addTiles() %>%
        setView(lng=1.1743,
                lat=52.3555,
                zoom=6) %>%
        addPolygons(
          layerId = ~ICB22CD,
          fillColor = ~pal(MEASURE_VALUE),
          color = "grey",
          fillOpacity = 0.7,
          label = paste0(
            "<strong> Organisation: </strong> ",
            mapFiltered()$Org_Name, "<br/> ",
            "<strong> Year: </strong> ",
            input$year, "<br/> ",
            "<strong> Measure: </strong> ",
            input$measure, "<br/> ",
            "<strong> Value: </strong> ",
            mapFiltered()$MEASURE_VALUE) %>%
            lapply(htmltools::HTML),

          highlight = highlightOptions(
            color = "blue",
            bringToFront = TRUE
          )
        ) %>%
        addControl(tags$div(
          HTML(paste0("Map displaying the ", input$measure, " of each ICB in ", input$year))), 
          position = "bottomleft") %>%
        leaflet::addLegend(
          pal = pal, values = ~MEASURE_VALUE,
          opacity = 0.9, title = input$measure
        )
    
  }) # end of foundational.map()
  
  # render foundational leaflet map
  output$map <- leaflet::renderLeaflet({
    
    # call reactive map
    foundational.map()
    
  }) # end of render leaflet
  
  # store the current user-created version
  # of the Leaflet map for download in 
  # a reactive expression
  user.created.map <- reactive({
    
    # call the foundational Leaflet map
    foundational.map() %>%
      
      # store the view based on UI
      setView( lng = input$map_center$lng
               ,  lat = input$map_center$lat
               , zoom = input$map_zoom
      )
    
  }) # end of creating user.created.map()
  
  # create the output file name
  # and specify how the download button will take
  # a screenshot - using the mapview::mapshot() function
  # and save as a PDF
  output$dl <- downloadHandler(
    filename = paste0("Map_ICB_bariatric_surgery_", input$measure, "_", input$year, ".png")
    
    , content = function(file) {
      mapshot( x = user.created.map()
               , file = file
               , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
               , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
      )
    } # end of content() function
  ) # end of downloadHandler() function
  
  
  datatableFiltered<- reactive({
    rowsinbreakdown<- which(df_to_plot$Financial_Year == input$year)
    df_to_plot[rowsinbreakdown,]
  })
  
  datatable<- reactive({
    if(!is.null(rv$click)){
      
      st_set_geometry(datatableFiltered(), NULL) %>% 
        filter(ICB22CD == rv$click[1])
      
    } else {
      st_set_geometry(datatableFiltered(), NULL)
    }
    
  })
  
  
  output$table<- renderDataTable({
    
    DT::datatable(datatable(),
                  rownames = FALSE,
                  # extensions = c("Buttons"),
                  # options = list(dom = 'Bfrtip',
                  #                buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
                  colnames = c("Financial year",
                               "ICB Code",
                               "NHS Integrated Care Board name",
                               "Measure",
                               "Value"),
                  caption = paste0("Table displaying data seen in the map above.", "\n",
                  " If select area on map it will filter the datatable.", "\n",
                  " Select 'Reset filters' on the left hand side to reset the table")
                  )
    

    
  })
  
  output$rateGraph<- renderPlotly({
    
    ggplotly(ggplot(data = subset(data,
                                  Financial_Year == input$year &
                                    MEASURE_NAME == "Rate per 100,000")))
    
  })
  
 
  #Graph 4 - demographic distribution
  output$demographic_distribution<- renderPlotly({
    
    if(input$organisation1 %notin% CCG_small){
    
    myplot<- ggplotly(ggplot(NULL, aes(x = CURRENCY, 
                              y = Percent)) 
    + geom_bar(data = subset(demographic_graph,
                             Organisation_breakdown == input$organisation_type1 &
                               Org_Name ==  input$organisation1 &
                               Financial_Year == input$year1 &
                               MEASURE_NAME == input$demographic),
               aes(fill = CURRENCY,
                   text = paste0("Organisation: ", input$organisation1, "\n",
                                 "Demographic attribute: ", CURRENCY, "\n",
                                 "Percent: ", Percent, "%")),
               stat = "identity") 
    + geom_line(data = subset(demographic_graph,
                              Org_Name == "England" &
                                Financial_Year == input$year1 &
                                MEASURE_NAME == input$demographic),
                aes(group = 1,
                    color = "England",
                    text = paste0("Organisation: England", "\n",
                                   "Demographic attribute: ", CURRENCY, "\n",
                                   "Percent: ", Percent, "%")))
    + scale_colour_manual("", values = c("(18-24,1)" = "18-24",
                                         "(25-34,1)" = "25-34",
                                         "(35-44,1)" = "35-44",
                                         "(55-64,1)" = "55-64",
                                         "(65-74,1)" = "75 and over",
                                         "(Under 18,1)" = "Under 18",
                                         "(black,1" = "England"))
    + scale_fill_manual(values = mycolors)
    + guides(fill = guide_legend(title = paste0(input$demographic, " breakdown")))
    + labs(x = input$demographic, y = "Percent (%)")
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))
    + theme(plot.margin = margin(t = 10,
                                 b = 10))
    + scale_y_continuous(expand = c(0,0)),
    tooltip = "text") %>%
    layout(annotations = list(x = 0,
                              y = 1,
                              text = paste0("Graph displaying the ", tolower(input$demographic), " distribution for ", input$organisation1, " in ", input$year1),
                              showarrow = F, xref='paper', yref='paper',
                              xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                              font=list(size=14, colour="grey")),
           hovermode = "x") %>%
    config(toImageButtonOptions = list(filename=paste0("Distribution_", input$demographic, "_", input$organisation1, "_", input$year1), 
                                       format = "png", 
                                       width = 1200,
                                       height = 700),
           displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))
    
    for (i in 1:length(myplot$x$data)){
      if (!is.null(myplot$x$data[[i]]$name)){
        myplot$x$data[[i]]$name =  gsub("\\(","",str_split(myplot$x$data[[i]]$name,",")[[1]][1])
      }
    }
    
    myplot
    
}
  })
  
  #Demographic distribution graph 
  
  output$graph3<- renderPlotly({
    
    if(input$organisation1 %notin% CCG_small){
    
      ggplotly(ggplot(NULL, aes(x = Org_Name,
                                y = Percent))
               + geom_bar(data = subset(demographic_graph,
                                        Organisation_breakdown == input$organisation_type1 &
                                          Org_Name %notin% CCG_small &
                                          Financial_Year == input$year1 &
                                          MEASURE_NAME == input$demographic),
                          aes(fill = CURRENCY,
                              text = paste0("Organisation: ", Org_Name, "\n",
                                            "Demographic attribute: ", CURRENCY, "\n",
                                            "Percent: ", Percent, "%")),
                          stat = "identity",
                          color = "white",
                          position = position_stack(reverse = TRUE))
               + guides(fill=guide_legend(title=paste0(input$demographic, " breakdown")))
               + scale_fill_manual(values = mycolors)
               + coord_flip()
               + theme(axis.title.y = element_blank())
               + labs(y = "Percent (%)")
        + geom_bar(data = subset(demographic_graph,
                                 Organisation_breakdown == input$organisation_type1 &
                                   Org_Name == input$organisation1 &
                                   Org_Name %notin% CCG_small &
                                   Financial_Year == input$year1 &
                                   MEASURE_NAME == input$demographic),
                   aes(input$organisation1,
                       text = paste0("Organisation: ", Org_Name, "\n",
                                     "Demographic attribute: ", CURRENCY, "\n",
                                     "Percent: ", Percent, "%")),
                   alpha = 0,
                   size = 0.5,
                   color = "black",
                   stat = "identity",
                   position = position_stack(reverse = TRUE))
        + theme(plot.margin = margin(t = 10,
                                     b = 10))
        + scale_y_continuous(expand = c(0,0)),
        tooltip = "text") %>%
        layout(annotations = list(x = 0,
                                  y = 1,
                                  text = paste0("Proportion of ", tolower(input$demographic), " breakdown by each ", "\n", input$organisation_type1, " in ", input$year1),
                                  showarrow = F,
                                  xref='paper',
                                  yref='paper',
                                  xanchor='left',
                                  yanchor='bottom',
                                  xshift=0,
                                  yshift=0,
                                  font=list(size=14, colour="grey")),
               legend = list(orientation = "v")) %>%
        config(toImageButtonOptions = list(filename=paste0("Proportion_", input$demographic, "_", input$organisation_type1, "_", input$year1),
                                           format = "png",
                                           width = 1200,
                                           height = 700),
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))

    }
  })
  
  #Error message pop up for CCGs that are too small
  observeEvent(input$organisation1, {
    if(input$organisation1 %in% CCG_small){
      shinyalert(
        title = "Error",
        text = "Cannot plot the data due to small numbers. Please try another breakdown.",
        size = "s", 
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#AEDEF4",
        imageUrl = "",
        animation = TRUE) 
    }
  })
  
  output$text1<- renderText({
    if(input$organisation1 %in% CCG_small){
      
      paste("Cannot plot the data due to small numbers. Please try another breakdown.")
      
    }})
  
  }




# Run the app ----
shinyApp(ui = ui, server = server)
