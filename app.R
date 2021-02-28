# PACKAGES ----------------------------------------------------------------
library(shiny)
library(tidyverse)
library(sf)
library(ggiraph)
library(shinyWidgets)
library(DT)
library(rgeos)
library(rgdal)
library(lwgeom)


# DATA --------------------------------------------------------------------
myGreys <- readRDS("myGreys.RDS")

myTheme <- function() { 
  
  theme_minimal() +
  theme(text = element_text(colour = myGreys[[5]]),
        axis.text = element_text(colour = myGreys[[8]]),
        panel.grid = element_line(colour = myGreys[[9]], linetype = "dotted", size = 0.025),
        axis.text.x = element_text(angle = 90, hjust = 1.0, vjust = 0.5),
        strip.text = element_text(face = "bold", colour = myGreys[[5]]),
        panel.spacing = unit(0.5, "cm"),
        legend.position = "top",
        title = element_text(colour = myGreys[[5]]),
        legend.title = element_text(colour = myGreys[[5]]),
        legend.text = element_text(margin = margin(r = 10, unit = "pt")),
        legend.text.align = 0)
  
}

dc <- readRDS("catchments.RDS") %>% filter(!st_is_empty(geometry)) #%>% filter(country == "Scotland")
dh <- readRDS("hydrometricAreas.RDS") %>% filter(HA %in% dc$HA) %>% rename(geometry = HAgeometry)
dr <- readRDS("regions.RDS") # %>% filter(country == "Scotland") 

sf::st_crs(dc) <- 4326
sf::st_crs(dh) <- 4326
sf::st_crs(dr) <- 4326



# FUNCTIONS ---------------------------------------------------------------
areaSummary <- function(dg) { 
    
    dg %>%
        mutate(area = st_area(geometry) %>% as.numeric) %>%
        as_tibble %>%
        summarise(area = sum(area)) %>%
        mutate(area_km2 = area/1000, area_km2 = area_km2 %>% plyr::round_any(1, round),
               area_Ha = area/10000, area_Ha = area_Ha %>% plyr::round_any(1, round)) %>%
        select(-area)
    
}



# UI ----------------------------------------------------------------------
ui <- fluidPage(
    
    tags$body(tags$div(id="ppitest", style="width:0.75in;visible:hidden;padding:0px")),
    
    tags$script('$(document).on("shiny:connected", function(e) {
                                    var w = window.innerWidth;
                                    var h = window.innerHeight;
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });
                                $(window).resize(function(e) {
                                    var w = $(this).width();
                                    var h = $(this).height();
                                    var d =  document.getElementById("ppitest").offsetWidth;
                                    var obj = {width: w, height: h, dpi: d};
                                    Shiny.onInputChange("pltChange", obj);
                                });'),
    
    fluidRow(
        
        # Dropdown button
        dropdownButton(width = "300px", icon = icon("gear"),
                       
                       fluidRow(column(width = 6, numericInput("width", label = "Plot width (inches):", value = 23)),
                                column(width = 6, numericInput("height", label = "Plot height (inches):", value = 12))),
                       fluidRow(column(width = 6, numericInput("multiplier", label = "Multiplier:", value = 0.5)))
                       
        ),
        
        br(),
        
        column(width = 2, 
               
               # Checkboxes
               checkboxGroupButtons(
                   inputId = "radioCountry",
                   label = NULL, 
                   choices = dc$country %>% unique %>% as.character,
                   status = "primary", 
                   direction = "vertical", 
                   selected = "Scotland", 
                   justified = TRUE
               ),
               
               br(),
               
               radioGroupButtons(
                   inputId = "radio",
                   label = NULL, 
                   choiceNames = c("Hydrometric area", "Regional"),
                   choiceValues = c("HA", "reg"),
                   status = "primary", 
                   direction = "vertical", 
                   selected = "reg", 
                   justified = TRUE
               ),
               
               br(),
               
               markdown("###### *Developed by Annie Visser-Quinn (annievisserquinn@gmail.com) as part of the Water Resilient Cities project, funded by UKRI EPSRC, grant number EP/N030419/1. Maintained by David Morrison (dh48@hw.ac.uk). Source code is available via github: https://github.com/avisserquinn/catchmentExploreR.*")
               
               ),
        
        column(width = 5, 
               girafeOutput("plotmapMain"),
               dataTableOutput("tableMain")
               ),
        
        column(width = 5, 
               girafeOutput("plotmapCatchments"),
               br(),
               dataTableOutput("tableSummary")
               )
        
    )

)





# SERVER ------------------------------------------------------------------
server <- function(session, input, output) {
    
    # PLOT DIMENSIONS
    observeEvent(list(input$pltChange, input$multiplier), { value <- (input$multiplier*input$pltChange$width/input$pltChange$dpi) %>% plyr::round_any(1, round); updateNumericInput(session = session, inputId = "width", value = value) })
    observeEvent(list(input$pltChange, input$multiplier), { value <- (input$multiplier*input$pltChange$height/input$pltChange$dpi) %>% plyr::round_any(1, round); updateNumericInput(session = session, inputId = "height", value = value) })

    
    # DATA
    Rdc <- reactive({ dc %>% filter(country %in% input$radioCountry) })
    Rdh <- reactive({ dh %>% filter(HA %in% Rdc()$HA) })
    Rdr <- reactive({ dr %>% filter(country %in% input$radioCountry) })
    
    
    mapdataMain <- reactive({ 
        dl <- list()
        if(input$radio == "HA") { dl$dt <- Rdh() %>% mutate(tooltip = paste0(HA, " - ", HAname), id = HA) }
        if(input$radio == "reg") { dl$dt <- Rdr() %>% mutate(tooltip = region, id = region) }
        return(dl)
    })
    
    
    mapdataMain_filtered <- reactive({ 
        dl <- list()
        if(input$radio == "HA") { dl$dtFiltered <- mapdataMain()$dt %>% filter(HA %in% input$plotmapMain_selected) }
        if(input$radio == "reg") { dl$dtFiltered <- mapdataMain()$dt %>% filter(region %in% input$plotmapMain_selected) }
        return(dl)
    })
    
    
    mapdataCatchment <- reactive({
        
        dl <- list()
        
        if(input$radio == "HA") { 
            dl$dtFiltered <- Rdc() %>% filter(HA %in% input$plotmapMain_selected) %>% mutate(tooltip = paste0(id, " - ", river, " @ ", location))
            dl$dtFilteredParents <- dl$dtFiltered %>% filter(parent == TRUE)
        }
        
        if(input$radio == "reg") { 
            dl$dtFiltered <- Rdc() %>% filter(region %in% input$plotmapMain_selected) %>% mutate(tooltip = paste0(id, " - ", river, " @ ", location))
            dl$dtFilteredParents <- dl$dtFiltered %>% filter(parent == TRUE)        }
        
        return(dl)
        
    })
 
    
    # PLOTS
    output$plotmapMain <- renderGirafe({
        
        g <- 
            mapdataMain()$dt %>%
            ggplot() + 
            geom_sf_interactive(aes(tooltip = tooltip, data_id = id), 
                                fill = myGreys[[10]], 
                                colour = myGreys[[9]], 
                                size = 0.25) +
            myTheme() +
            scale_x_continuous(expand = c(0,0)) +
            scale_y_continuous(expand = c(0,0))
        
        girafe(ggobj = g,
               width_svg = input$width,
               height_svg = input$height,
               
               options = list(opts_sizing(rescale = FALSE),
                              opts_tooltip(delay_mouseover = 500),
                              opts_hover(css = "fill:#607ebc"),
                              opts_selection(type = "multiple", css = "fill:#607ebc")))
        
    })

    
    observeEvent(mapdataCatchment(), { 
        
        output$plotmapCatchments <- renderGirafe({
            
            g <- 
                ggplot() + 
                geom_sf(data = mapdataMain_filtered()$dtFiltered, 
                        fill = myGreys[[10]], 
                        colour = myGreys[[8]], 
                        size = 0.25) +
                geom_sf_interactive(data = mapdataCatchment()$dtFilteredParents, 
                                    aes(tooltip = tooltip, data_id = id), 
                                    fill = myGreys[[1]], 
                                    colour = myGreys[[10]], 
                                    size = 0.25) +
                theme_void() +
                theme(plot.background = element_rect(fill = "transparent", colour = myGreys[[9]])) +
                scale_x_continuous(expand = c(0,0)) +
                scale_y_continuous(expand = c(0,0)) 
            
            girafe(ggobj = g,
                   options = list(opts_sizing(rescale = TRUE),
                                  opts_tooltip(delay_mouseover = 500), 
                                  opts_hover(css = "fill:#607ebc"),
                                  opts_selection(type = "multiple", css = "fill:#607ebc")))
            
        })
        
    })
    
    
    # TABLES
    output$tableMain <- renderDataTable({
        
        data <- 
            mapdataCatchment()$dtFilteredParents %>%
            as_tibble %>% 
            select(HA, country, region, id, river, location)
        
        if(length(input$plotmapCatchments_selected)) { data <- data %>% filter(id %in% input$plotmapCatchments_selected) }
        
        return(data)
        
    }, 
    selection = list(mode = "none"), 
    rownames = FALSE, 
    options = list(pageLength = 25, 
                   lengthMenu = c(25,50,100,250))
    )
    
    
    output$tableSummary <- renderDataTable({
        
        # Total area of land
        area0 = mapdataMain()$dt %>% areaSummary %>% mutate(Level = "Total")
        
        # Region or HA area
        area1 <- mapdataMain_filtered()$dtFiltered %>% areaSummary %>% mutate(Level = "Region or HA")
        
        # Catchment area, total
        area2 <- 
            mapdataCatchment()$dtFilteredParents %>% areaSummary %>% mutate(Level = "Catchments in region or HA")

        # Catchment area, selected
        if(length(input$plotmapCatchments_selected)) { area3 <- mapdataCatchment()$dtFilteredParents %>% filter(id %in% input$plotmapCatchments_selected) %>% areaSummary %>% mutate(Level = "Catchments selected") }
        
        area <- rbind(area0, area1, area2)
        if(length(input$plotmapCatchments_selected)) { area <- rbind(area, area3) }
        
        area <- 
            area %>% 
            filter(area_Ha != 0) %>%
            mutate(pcnt = lag(area_Ha),
                   pcnt = area_Ha/pcnt * 100,
                   pcnt = pcnt %>% plyr::round_any(1, round)) %>%
            select(-area_km2) %>%
            mutate(Variable = "Area (Ha)")
        
        count <- 
            tibble(area_Ha = c(mapdataCatchment()$dtFilteredParents %>% nrow(), 
                               mapdataCatchment()$dtFilteredParents %>% filter(id %in% input$plotmapCatchments_selected) %>% nrow()),
                   Level = c("Catchments in region or HA", "Catchments selected"),
                   Variable = "Count") %>%
            filter(area_Ha != 0) %>%
            mutate(pcnt = lag(area_Ha),
                   pcnt = (area_Ha/pcnt * 100),
                   pcnt = pcnt %>% plyr::round_any(1, round))
        
        DT <- rbind(area, count) %>% select(Variable, Level, Value = area_Ha, "Percent (of level above)" = pcnt)
        
        datatable(DT,
                  selection = list(mode = "none"), 
                  rownames = FALSE,
                  options = list(dom = 't', ordering = FALSE)) %>% 
            formatCurrency("Value", currency = "", interval = 3, mark = ",", digits = 0)

        })
    
}





# RUN APP -----------------------------------------------------------------
shinyApp(ui = ui, server = server)