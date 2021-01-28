library(shiny)
library(shiny.semantic)
library(rsconnect)
library(dplyr)
library(tidyr)
library(purrr)
library(leaflet)
library(lubridate)
library(geosphere)

#R
ships <- read.table(unz("ships_04112020.zip", "ships.csv"), header=T,nrows = 800000, quote="\"", sep=",",fill = T)

ship_type <- levels(as.factor(ships$ship_type))
ship_name <- levels(as.factor(ships$SHIPNAME))
ports <- levels(as.factor(ships$PORT))


ships =ships %>% group_by(SHIP_ID) %>% mutate(DISTANCE=distHaversine(cbind(LON, LAT),
                                                                     cbind(lag(LON), lag(LAT))))


ui <- shinyUI(semanticPage(
    suppressDependencies("bootstrap"),
    div(class = "ui inverted menu",style="background-color: #F8F9F9 ", 
        titlePanel(title = "Ships movement analysis"),style = "font-weight: 500; color:#FFFFFF;"
    ),
    tabset(tabs=
               list(
                   list(menu="Maximum Distance Sailed",content=
                            div(class = "ui grid",
                                div(class = "four wide column",
                                    div(class = "ui raised segment", style="margin-left: 10px; width: 100%",
                                        a(class="ui black ribbon label", "Select Ship type"),p(style="margin-left: 10px; width: 100%",dropdown_input("select_type", choices = ship_type, type = "search selection multiple")),
                                    ),div(class = "ui raised segment", style="margin-left: 10px; width: 100%",
                                          a(class="ui black ribbon label", "Select Ship name"),p(style="margin-left: 10px; width: 100%",
                                                                                                dropdown_input("select_name", choices = ship_name, type = "search selection multiple")
                                          )),div(class = "ui raised segment", p(style="margin-left: 10px; width: 100%",segment(
                                              h4("Maximum distance beween two observations (in m)"),  
                                              textOutput("distance"))
                                          ))),div(class="eight wide column",
                                                  leafletOutput("map_max")
                                          ))),list(menu="Ship's movements",content= div(leafletOutput("movemnt_map",width =  "100%"),absolutePanel(top = 10, right = 10,h5(icon("water"),"Select Port"),
                                                                                                                                                   dropdown_input(input_id = "select_port",default_text = "Ports", choices = ports, type = "ui fluid selection dropdown"))))))
))

server <- shinyServer(function(input, output, session) {
    #Selected type
    ship_type_df <- reactive({
        if(is.null(input$select_type)==T){return(ships)}
        ships[ships$ship_type==input$select_type,]
    })
    
    # Selected ship
    ship_name_df<- reactive({
        unique(ship_type_df()$SHIPNAME)
    })
    
    # update dropdown selection from ships type to ships name
    observeEvent(input$select_type, {
        update_dropdown_input(session, "select_name", 
                              choices = ship_name_df(),
                              choices_value = ship_name_df())
    })
    
    # Ship type and name
    selected_ship <- reactive({
        ships %>% filter(ship_type %in% input$select_type & SHIPNAME %in% input$select_name) 
        #%>% drop_na()
    })
    
    # distance
    distance_calculated <- reactive({
        selected_ship() %>% 
            top_n(1, DISTANCE) %>% arrange(desc(DATETIME)) %>% slice(1) %>% pull(DISTANCE)
    })
    
    # departure
    A1<- reactive({
        selected_ship()[which(selected_ship()$DISTANCE %in% distance_calculated())-1,] %>% top_n(1,DATETIME) %>% 
            mutate(Color="#83c13c")
    })
    
    # Arrival
    A2 <- reactive({
        selected_ship()[which(selected_ship()$DISTANCE %in% distance_calculated()),] %>% top_n(1,DATETIME) %>%  
            mutate(Color="#ec6762")  
    })
    
    # caluclated distance rendering
    output$distance <- renderText({
        if(dim(A1())[1]!=dim(A2())[1]){return(0)}
        else{
            paste0(prettyNum(round(abs(A1()$DISTANCE- A2()$DISTANCE),2),big.mark=",")," | ")
        }
    })
    
    #connect A1 and A2 with a polyline
    
    Connectthedots <- reactive({
        
        
        a1=A1() [c('SHIPNAME','LAT','LON')]
        colnames(a1)=c('SHIPNAME','lat1','long1')
        
        a2=A2()[c('LAT','LON')]
        colnames(a2)=c('lat2','long2')
        if(dim(a1)[1]!=dim(a2)[1]){}
        else
            cbind(a1,a2)
        
        
    })
    
    
    #Leaflet map of max distance  
    output$map_max <- renderLeaflet({
        leaflet(ships) %>% addTiles() %>%
            fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))  %>%addLegend("bottomright", 
                                                                                colors =c("#83c13c",  "#ec6762"),
                                                                                labels= c("Departure","Arrival")) 
    })
    
    
    observe({
        leafset= leafletProxy("map_max") %>%addTiles()  %>% clearShapes() %>%
            addProviderTiles("Stamen.TonerLite") %>%
            addCircles(data = A1(), 
                       lng = ~LON, lat = ~LAT,
                       radius = 20,
                       color = ~Color,
                       fill = ~Color,fillColor =~Color ) %>%
            addCircles(data = A2(), 
                       lng = ~LON, lat = ~LAT,
                       radius = 20,
                       color = ~Color,
                       fill = ~Color,
                       fillColor =~Color)
        if(!is.null(Connectthedots())){
        for(i in 1:nrow(Connectthedots())){
            leafset=addPolylines(leafset, lat = as.numeric(Connectthedots()[i, c(2, 4)]),
                                 lng = as.numeric(Connectthedots()[i,c(3,5)]),popup =  Connectthedots()[i,1],color = "#ffbd58",opacity = 0.3 )
            
        }
    }
        leafset
        
        
    })
    
    
    
    #Map for ships movement
    
    output$movemnt_map <- renderLeaflet(({
        
        with_progress(message = 'Monkeys are drawing the map ...',
                      value = 0, {
                          
                          if(input$select_port==""){shiproute=ships%>%  filter(is_parked !=1) %>%  group_by(SHIPNAME) %>% 
                              mutate(laglat=lag(LAT)) %>% mutate(laglong=lag(LON)) %>% select(SHIPNAME,LAT,LON,laglat,laglong) %>% 
                              drop_na() %>% sample_frac(size = 0.01) } 
                          else{        
                              shiproute=ships[ships$PORT==input$select_port,] %>%  filter(is_parked !=1) %>%  group_by(SHIPNAME) %>% 
                                  mutate(laglat=lag(LAT)) %>% mutate(laglong=lag(LON)) %>% select(SHIPNAME,LAT,LON,laglat,laglong) %>% 
                                  drop_na() %>% sample_frac(size = 0.01) 
                          }
                          
                          
                          leafset = leaflet(shiproute) %>% addTiles() %>%
                              fitBounds(~min(LON), ~min(LAT), ~max(LON), ~max(LAT))  %>%  addProviderTiles("Stamen.Toner")
                          for(i in 1:nrow(shiproute)){
                              inc_progress(1/nrow(shiproute))
                              leafset=addPolylines(leafset, lat = as.numeric(shiproute[i, c(2, 4)]),
                                                   lng = as.numeric(shiproute[i,c(3,5)]),color = "#ffd859",opacity = 0.5 )
                              
                          }})
        leafset
        
    }))
    
    
    
    
    
})

shinyApp(ui, server)