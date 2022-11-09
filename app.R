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
               phantomjs)

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
  TRUE ~ data$MEASURE_NAME
)

# data$Org_Name <- tolower(data$Org_Name)
# data$Org_Name <- capitalize(data$Org_Name)

#Create a CCG_small group so don't produce an output for these
CCG_small<- c("NHS Bassetlaw CCG",
              "NHS Blackburn with Darwen CCG",
              "NHS Blackpool CCG",
              "NHS Bury CCG",
              "NHS Chorley and South Ribble CCG",
              "NHS East Lancashire CCG",
              "NHS East Staffordshire CCG",
              "NHS Knowsley CCG",
              "NHS Leicester City CCG",
              "NHS Morecambe Bay CCG",
              "NHS Oldham CCG",
              "NHS Salford CCG",
              "NHS South Sefton CCG",
              "NHS Southport and Formby CCG",
              "NHS Stockport CCG",
              "NHS Trafford CCG",
              "NHS Warrington CCG",
              "NHS West Lancashire CCG"
)

#Not in function
'%notin%'<- Negate('%in%')

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
df_to_plot$MEASURE_NAME[df_to_plot$MEASURE_NAME == 'PRIMARY_BARIATRIC_SURGICAL_PROCEDURE'] <- 'Patient count'
df_to_plot$MEASURE_NAME[df_to_plot$MEASURE_NAME == 'RATE PER 100,000'] <- 'Rate per 100,000'
df_to_plot$MEASURE_NAME[df_to_plot$MEASURE_NAME == 'PERCENTAGE_TREATED_WITHIN_ICB'] <- 'Percentage of patients treated within ICB'

df_to_plot$MEASURE_VALUE <- as.numeric(as.character(df_to_plot$MEASURE_VALUE))

##Create all years 
allyears<- df_to_plot %>%
  group_by(Org_Name, MEASURE_NAME, ICB22CD) %>%
  summarise(MEASURE_VALUE = sum(MEASURE_VALUE))

allyears$Financial_Year<- "All years"

test<- rbind(df_to_plot, allyears)

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
  
  titlePanel("Bariatric surgery inequalities dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      #style = "position:fixed;width:inherit;",
      width = 2,
      shinyjs::useShinyjs(),
      id = "side-panel",
      h2("Breakdowns"),
      
      p("Please select from the various breakdowns to filter the map, graphical and tabular data"),
      
      conditionalPanel(condition = "input.conditionedPanelsTab1 == 3",
                       
                       selectInput("organisation_type", label = "Select Organisation Type:",
                                   choices = unique(data$Organisation_breakdown),
                                   selected = "National"),
                       
                       selectInput("organisation", label = "Select Organisation:",
                                   choices = unique(data$Org_Name),
                                   selected = "England")),
      
      conditionalPanel(condition = "input.conditionedPanelsTab1 == 4",
                       
                       selectInput("year", label = "Select Year:",
                                   choices = unique(data$Financial_Year),
                                   selected = "2021-2022"),
                       
                       selectInput("measure", label = "Select a measure type:",
                                   choices = unique(df_to_plot$MEASURE_NAME),
                                   selected = "Rate per 100,000")),
      
      conditionalPanel(condition = "input.conditionedPanelsTab1 == 5",
                       
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
                          tabPanel("About", 
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
                                which relfects latest boundary changes. This aids a comparable time series for this dashboard and future releases</p>
                                ")
                          ),
                          
                          tabPanel("User guide",
                                   value=2,
                                   "main panel"),
                          
                          tabPanel("Bariatric Surgical Procedures", 
                                   value =3,
                                   br(),
                                   br(),
                                   useShinyalert(),
                                   plotlyOutput("graph1", height = 600),
                                   br(),
                                   br(),
                                   plotlyOutput("graph2", height = 600)),
                          
                          
                          tabPanel("Primary bariatric procedures at ICB level",
                                   value = 4,
                                   br(),
                                   textOutput("mapTitle"),
                                   tags$head(tags$style("#mapTitle{font-size: 20px;}")),
                                   br(),
                                   leafletOutput("map", height = 600),
                                   br(),
                                   downloadButton("map_down"),
                                   br(),
                                   br(),
                                   br(),
                                   dataTableOutput("table")),
                          
                          tabPanel("Demographic breakdowns for primary bariatric procedures",
                                   value = 5,
                                   useShinyalert(),
                                   br(),
                                   textOutput("text1"),
                                   plotlyOutput("demographic_distribution", height = 600),
                                   br(),
                                   br(),
                                   plotlyOutput("graph3", height = 800))
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
                          fill = MEASURE_NAME,
                          text = paste0(#"<b>Organisation: </b>", input$organisation, "\n",
                                        "<b>Procedure type: </b>", MEASURE_NAME, "\n",
                                        "<b>Percent: </b>", Percent, "%"))) 
               + geom_bar(stat = "identity", 
                          position = "stack") 
               + shades::lightness(scale_fill_brewer(palette = "Blues"), scalefac(0.9)) 
               + guides(fill=guide_legend(title="Procedure type")) 
               + labs(x = "Finanical Year", 
                      y = "Percent (%)")
               + theme(plot.margin = margin(t = 10,
                                            b = 10)), 
               tooltip = "text") %>%
        layout(
          annotations = list(x = 0,
                             y = 1,
                             text = paste0("Number of people having primary bariatric surgical procedures, revision surgical procedures or gastric balloons & bubbles ","\n"," between 2017 and 2022 in ", input$organisation),
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
    
    paste0("Map displaying primary bariatric procedure by ", tolower(input$measure), " in each ICB of residence for ", input$year)
    
  })
  
  #Map output
  output$map<- renderLeaflet({
    
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
      leaflet::addLegend(
        pal = pal, values = ~MEASURE_VALUE,
        opacity = 0.7, title = input$measure
      )
    
  })
  
  # map that will be downloaded
  mapdown <- reactive({
    # we need to specify coordinates (and zoom level) that we are currently viewing
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    mapFiltered() %>% setView(lng = (lngRng[1]+lngRng[2])/2, 
                              lat = (latRng[1]+latRng[2])/2, 
                              zoom = input$map_zoom)
  })
  
  output$map_down <- downloadHandler(
    filename = 'mymap.pdf',
    
    content = function(file) {
      # temporarily switch to the temp dir, in case you do not have write
      # permission to the current working directory
      owd <- setwd(tempdir())
      on.exit(setwd(owd)) 
      mapshot(mapdown(), file = file, cliprect = "viewport")
    }
  )
  
  #Create data frame for table data
  
  # datatableFiltered<- reactive({
  #   if(!is.null(rv$click)){
  # 
  #     st_set_geometry(mapFiltered(), NULL) %>% 
  #       filter(ICB22CD == rv$click[1])
  # 
  #   } else {
  #     st_set_geometry(mapFiltered(), NULL)
  #   }
  # 
  # })
  
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
    
    #       #if click id isn't null render the table
    #       if(!is.null(click$id)){
    #         output$table = DT::renderDataTable({
    #           selected
    #         }) 
    #       } 
    # 
    #}) 
    
    DT::datatable(datatable(),
                  rownames = FALSE,
                  extensions = c("Buttons"),
                  options = list(dom = 'Bfrtip',
                                 buttons = c('copy', 'csv', 'excel', 'pdf', 'print')),
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

  #Graph 4 - demographic distribution
  output$demographic_distribution<- renderPlotly({
    
    ggplotly(ggplot(NULL, aes(x = CURRENCY, 
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
                     text = paste0("Organisation: England", "\n",
                                   "Demographic attribute: ", CURRENCY, "\n",
                                   "Percent: ", Percent, "%")))
    + scale_fill_brewer("Blues")
    + guides(fill=guide_legend(title=paste0(input$demographic, " breakdown")))
    + labs(x = input$demographic, y = "Percent (%)")
    + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)),
    tooltip = "text") %>%
    layout(annotations = list(x = 0,
                              y = 1,
                              text = paste0(input$demographic, " distribution for ", input$organisation1, " in ", input$year1),
                              showarrow = F, xref='paper', yref='paper',
                              xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                              font=list(size=14, colour="grey")),
           hovermode = "x") %>%
    config(toImageButtonOptions = list(filename='Graph of bariatric surgery distribution', format = "png", width = 1200,height = 700),
           displaylogo = FALSE,
           modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))
    
    
    
      # ggplotly(ggplot(subset(demographic_graph,
      #                        Organisation_breakdown == input$organisation_type1 &
      #                          Org_Name ==  input$organisation1 &
      #                          Financial_Year == input$year1 &
      #                          MEASURE_NAME == input$demographic),
      #                 aes(x = CURRENCY,
      #                     y = Percent,
      #                     fill = CURRENCY,
      #                     text = paste0("Organisation: ", input$organisation1, "\n",
      #                                   "Percent: ", Percent, "%")))
      #          + geom_bar(stat = "identity") #fill = "lightblue")
      #          + scale_fill_gradient()
      # 
      #          + geom_line(aes(x = CURRENCY,
      #                          y = Percent,
      #                          group = 1,
      #                          text = paste0("Organisation: England", "\n",
      #                                         "Percent: ", Percent, "%")),
      #                      data = subset(demographic_graph,
      #                                     Org_Name == "England" &
      #                                       Financial_Year == input$year1 &
      #                                       MEASURE_NAME == input$demographic))
      #          + labs(x = input$demographic, y = "Percent (%)")
      #          + theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1)),
      #          tooltip = "text") %>%
      #   layout(annotations = list(x = 0,
      #                             y = 1,
      #                             text = paste0(input$demographic, " distribution for ", input$organisation1, " in ", input$year1),
      #                             showarrow = F, xref='paper', yref='paper',
      #                             xanchor='left', yanchor='bottom', xshift=0, yshift=0,
      #                             font=list(size=14, colour="grey")),
      #          hovermode = "x unified") %>%
      #   config(toImageButtonOptions = list(filename='Graph of bariatric surgery distribution', format = "png", width = 1200,height = 700),
      #          displaylogo = FALSE,
      #          modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))

  })
  
  #Demographic distribution graph 
  
  
  output$graph3<- renderPlotly({
    
    if(input$organisation1 %notin% CCG_small){
      
      ggplotly(ggplot(subset(demographic_graph,
                             Organisation_breakdown == input$organisation_type1 & 
                               Financial_Year == input$year1 &
                               MEASURE_NAME == input$demographic),
                      aes(fill = CURRENCY, 
                          x = Org_Name,
                          y = Percent, 
                          text = paste0("Organisation: ", Org_Name, "\n",
                                        "Demographic attribute: ", CURRENCY, "\n",
                                        "Percent: ", Percent, "%"))) 
               + geom_bar(stat = "identity", 
                          position = position_stack(reverse = TRUE)) 
               + guides(fill=guide_legend(title=paste0(input$demographic, " breakdown")))
               + scale_fill_brewer("Blues")
#               + shades::lightness(scale_fill_manual(values = mycolors), scalefac(0.9))  
               + coord_flip() 
               + theme(axis.title.y = element_blank()) 
               + labs(y = "Percent (%)"),
               tooltip = "text") %>%
        layout(annotations = list(x = 0, y = 1, 
                                  text = paste0("Proportion of ", input$demographic, " for ", input$organisation_type1, " in ", input$year1), 
                                  showarrow = F, xref='paper', yref='paper', 
                                  xanchor='left', yanchor='bottom', xshift=0, yshift=0,
                                  font=list(size=14, colour="grey")),
               legend = list(orientation = "h",
                             x = 0.5,
                             xanchor = "center")) %>%
        config(toImageButtonOptions = list(filename='Graph of counts within demographic attribute', format = "png", width = 1200,height = 700),
               displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoom2d", "lasso2d", "select2d", "autoScale2d"))
    }    
    
  })
  
  output$text1<- renderText({
    if(input$organisation1 %in% CCG_small){
      
      paste("Cannot plot the data due to small numbers. Please try another breakdown.")
      
    }})
  
  
}




# Run the app ----
shinyApp(ui = ui, server = server)

















































































#OLD APP!!!!

#Spatial data
#regions<- sf::st_read("U:/Data Vis/Data/Geography/England_regions.geojson", 
#                      as_tibble = TRUE)
#regions<- regions[, -c(1:3)]
#colnames(regions)[which(names(regions) == "EER13NM")] <- "Region"

#regions<- sf::st_read("U:/Data Vis/Data/Geography/England_regions(1).geojson", 
#                      as_tibble = TRUE)

#regions<- regions[, c("RGN21NM","geometry")]
#colnames(regions)[which(names(regions) == "RGN21NM")] <- "Region"

#regions<- read.csv("U:/Data Vis/Data/Geography/Regions.csv", check.names = FALSE,
#                fileEncoding = "UTF-8")
#regions<- regions[, c("RGN21NM","LONG","LAT")]
#colnames(regions)[which(names(regions) == "RGN21NM")] <- "Region"

#Countries
#England<- sf::st_read("U:/Data Vis/Data/Geography/England.geojson",
#                        as_tibble = TRUE)
#England<- England[, -c(1,2,4:12)]
#colnames(England)[which(names(England) == "CTRY21NM")] <- "Region"

#England<-  read.csv("U:/Data Vis/Data/Geography/England.csv", check.names = FALSE,
#                    fileEncoding = "UTF-8")
#England<- England[, c("CTRY21NM","LONG","LAT")]
#colnames(England)[which(names(England) == "CTRY21NM")] <- "Region"

#Merge spatial data
#geography<- rbind(regions, England)

#Data
#data<- read.csv("U:/Data Vis/Data/Bariatric_surgery/Bariatric surgery.csv", check.names = FALSE,
#                fileEncoding = "UTF-8")

#data$Year[data$Year == "1920"] <- "2019-2020"
#data$Year[data$Year == "2021"] <- "2020-2021"
#data$Year[data$Year == "2122"] <- "2021-2022"


#working_data<- data

#regional_data<- data %>%
#  group_by(Year, Region) %>%
# summarise(Patients = sum(Patients))

#all_years_regional_data<- regional_data %>%
#  group_by(Region) %>%
#  summarise(Patients = sum(Patients))

#all_years_regional_data$Year<- "All"

#regional_data<- rbind(regional_data, all_years_regional_data)

#regional_data<- read.csv("U:/Data Vis/Data/Bariatric_surgery/Bariatric_surgery_by_region.csv", check.names = FALSE,
#                         fileEncoding = "UTF-8")

#map <- regions %>% 
#  left_join(regional_data, by = c("Region"))

#map<- merge(geography, regional_data, by = "Region")

#---------------------------------------------------
#APP

# Define UI for app that draws a histogram ----
#ui <- fluidPage(
#ui <- bootstrapPage(
#  titlePanel("Bariatric surgery inequalities dashboard"),
#  sidebarLayout(
#    sidebarPanel(
#      h2("Breakdowns"),

#      p("Please select from the various breakdowns to filter the map, graphical and tabular data"),

#      selectInput("year", label = "Select a year:",
#                  choices = c(map$Year),
#                  selected = "All"),

#      selectInput("region", label = "Select a gender:",
#                  choices = c(map$Gender),
#                  selected = "All")
#    ),

#    mainPanel(
#      textOutput("selected_var"),
#      
#      leafletOutput("mymap", width = "100%", height = 400),
#      
#      #leafletOutput("leafplot2", width = "100%", height = 400),
#      
#      h2("Map of bariatric surgery distribution in England"),
#      
#   )
#  )
#)

# Define server logic ----
#server <- function(input, output, session) {

#  pal<- colorBin(
#    palette = "Blues",
#    domain = map$Patients,
#    bins = seq(0, max(map$Patients, na.rm=TRUE)+500, by = 500)
#  )

#  map$labels<- paste0(
#    "<strong> Region: </strong> ",
#    map$Region, "<br/> ",
#    "<strong> Patients: </strong> ",
#    map$Patients, "<br/> "
#  ) %>% 
#    lapply(htmltools::HTML)
#  
#  unfilteredData1 <- reactive({
#    map
#  })

#  filteredData2 <- reactive({
#    map[map$Year == input$year & map$gender == input$Gender,] 
#  })

#  output$leafplot1 <- renderLeaflet({

#    leaflet(unfilteredData1()) %>%
#      addTiles() %>%
#      setView(lng=-0.118092, lat=51.509865, zoom=6) %>%
#      addPolygons(#data = mapFiltered(),
#        fillColor = ~pal(Patients),
#        color = "grey",
#        fillOpacity = 0.7,
#        label = ~labels,
#        highlight = highlightOptions(
#          color = "blue",
#          bringToFront = TRUE
#        )
#      ) %>% 
#      leaflet::addLegend(
#        pal = pal, values = ~Patients,
#        opacity = 0.7, title = "Patients"
#      )

#  })

#  output$leafplot2 <- renderLeaflet({

#    leaflet(filteredData2()) %>%
#      addTiles() %>%
#      setView(lng=-0.118092, lat=51.509865, zoom=6) %>%
#      addPolygons(#data = mapFiltered(),
#        fillColor = ~pal(Patients),
#        color = "grey",
#        fillOpacity = 0.7,
#        label = ~labels,
#        highlight = highlightOptions(
#          color = "blue",
#          bringToFront = TRUE
#        )
#      ) %>% 
#      leaflet::addLegend(
#        pal = pal, values = ~Patients,
#        opacity = 0.7, title = "Patients"
#      )
#  })
#}

#  mapFiltered<- reactive({
#    rowsinbreakdown<- which(map$Year == input$year)
#    map[rowsinbreakdown,]
#  })

#  output$mymap<- renderLeaflet({
#    if(nrow(mapFiltered())==0){
#      return(NULL)
#    }
#    
#    leaflet(mapFiltered()) %>%
#      addTiles() %>%
#      setView(lng=-0.118092, lat=51.509865, zoom=6) %>%
#      addPolygons(#data = mapFiltered(),
#        fillColor = ~pal(Patients),
#        color = "grey",
#        fillOpacity = 0.7,
#        label = ~labels,
#        highlight = highlightOptions(
#          color = "blue",
#          bringToFront = TRUE
#       )
#      ) %>% 
#      leaflet::addLegend(
#        pal = pal, values = ~Patients,
#        opacity = 0.7, title = "Patients"
#      )
#  })
#}

# Run the app ----
#shinyApp(ui = ui, server = server)