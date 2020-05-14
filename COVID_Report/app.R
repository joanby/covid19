library(shiny)
library(shinythemes)
library(tidyverse) # %>%
library(magrittr)  # %<>%
library(lubridate)
library(plotly)
library(xts)
library(dygraphs)

ui <- fluidPage(
    shinythemes::themeSelector(),
    titlePanel("Análisis del COVID 19"),
    
    sidebarLayout(
        sidebarPanel(
            dateInput("date1", "Fecha Inicio: ", value = "2020-01-24"),
            dateInput("date2", "Fecha Fin: ", value = today()),
            uiOutput("pais"), ## RELLENAR DESDE SERVER CON PAISES
            checkboxInput("logscale", "Log Y: ", value = FALSE),
            sliderInput("alpha", "Selecciona el nivel de transparencia",
                        min = 0, max = 1, value = 0.5)
        ),
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Tabla", tableOutput("contents")),
                        tabPanel("Plot 1", plotOutput("plot1")),
                        tabPanel("Plot 2", dygraphOutput("plot2")),
                        tabPanel("Plot 3", plotOutput("plot3")),
                        tabPanel("Plot 4", plotlyOutput("plot4"))
                        )
        )
    )
)

server <- function(input, output, session) {

    myoriginaldata <- reactive({
        
        datos <- read.csv("covid_19_clean_complete.csv", stringsAsFactors = FALSE)
        
        colnames(datos) = c("Provincia_Estado",
                            "Pais_Region",
                            "Latitud", # N+ o S-
                            "Longitud", # E+ o W-
                            "Fecha",
                            "Casos_Confirmados",
                            "Casos_Muertos",
                            "Casos_Recuperados"
        )
        
        datos$Provincia_Estado %<>% factor()
        datos$Pais_Region %<>% factor()
        datos$Fecha %<>% mdy()
        
        return(datos)
    })
    
    mydata <- reactive({
        return(myoriginaldata() %>% filter(between(Fecha, input$date1, input$date2)))
    })
    
    primer_contagio <- reactive({
        myoriginaldata() %>%
            group_by(Pais_Region) %>%
            filter(Casos_Confirmados > 0) %>%
            summarise(Primer_Contagio = min(Fecha)-1)
    })
    
    output$pais <- renderUI({
        countries <- mydata() %>%
            select(Pais_Region) %>%
            arrange(Pais_Region) %>%
            unique()
        selectInput("pais", "Selecciona el país: ", choices = countries)
    })
    
    
    
    output$contents <- renderTable({
        mydata() %>% filter(Pais_Region == input$pais)
    })
    
    
    output$plot1 <- renderPlot({
        datos_por_fecha = aggregate(
            cbind(Casos_Confirmados, Casos_Muertos, Casos_Recuperados) ~ Fecha,
            data = mydata() %>% filter(Pais_Region == input$pais), 
            FUN = sum
        )
        datos_por_fecha$Casos_Enfermos = datos_por_fecha$Casos_Confirmados - datos_por_fecha$Casos_Muertos - datos_por_fecha$Casos_Recuperados
        
        logy= ""
        lims = c(0, 1.05*max(datos_por_fecha$Casos_Confirmados))
        
        if(input$logscale){
            logy = "y"
            datos_por_fecha %<>%
                filter(Casos_Confirmados > 0)
            lims = c(1, 1.05*max(datos_por_fecha$Casos_Confirmados))
        }
        
        
        
        plot(Casos_Confirmados ~ Fecha, data = datos_por_fecha, 
             col = "blue", type = "l", ylim = lims,
             main = paste0("Casos documentados por día en ", input$pais), 
             xlab = "Fecha", ylab = "Número de personas", log = logy)
        lines(Casos_Muertos ~ Fecha, data = datos_por_fecha, col = "red")
        lines(Casos_Recuperados ~ Fecha, data = datos_por_fecha, col = "green")
        
        legend("topleft", c("Confirmados", "Muertos", "Recuperados"), 
               col = c("blue", "red", "green"), pch = 1, lwd = 2)
    })
    
    output$plot2 <- renderDygraph({
        
        datos_por_fecha = aggregate(
            cbind(Casos_Confirmados, Casos_Muertos, Casos_Recuperados) ~ Fecha,
            data = mydata() %>% filter(Pais_Region == input$pais), 
            FUN = sum
        )
        datos_por_fecha$Casos_Enfermos = datos_por_fecha$Casos_Confirmados - datos_por_fecha$Casos_Muertos - datos_por_fecha$Casos_Recuperados

        datos_por_fecha_ts <- xts(x = datos_por_fecha[, 2:5],
                                  order.by = datos_por_fecha$Fecha)
        dygraph(datos_por_fecha_ts) %>%
            dyOptions(labelsUTC = TRUE, labelsKMB = TRUE,
                      fillGraph = TRUE, fillAlpha = input$alpha, 
                      drawGrid = FALSE, colors = "#D9AE55") %>%
            dyRangeSelector() %>%
            dyCrosshair(direction = "vertical") %>%
            dyHighlight(highlightCircleSize = 5, 
                        highlightSeriesBackgroundAlpha = 0.2,
                        hideOnMouseOut = FALSE) %>%
            dyRoller(rollPeriod = 2)
    })
    
    
    output$plot3 <- renderPlot({
        mydata() %>%
            filter(Fecha == ymd(input$date1)) %>%
            ggplot(aes(Longitud, Latitud)) +
            geom_point(aes(size = log(Casos_Confirmados+1), 
                           colour = log(Casos_Muertos+1))) +
            coord_fixed() +
            theme(legend.position = "bottom")
    })
    
    output$plot4 <- renderPlotly({
        data_first = mydata() %>%
            inner_join(primer_contagio(), by = "Pais_Region") %>%
            mutate(Dias_Desde_PC = as.numeric(Fecha - Primer_Contagio)) %>%
            filter(Dias_Desde_PC >= 0) %>%
            group_by(Dias_Desde_PC, Pais_Region) %>%
            summarise(Casos_Confirmados = sum(Casos_Confirmados),
                      Casos_Muertos = sum(Casos_Muertos),
                      Casos_Recuperados = sum(Casos_Recuperados)
                      )
        
        
        data_first %>%
            filter(Pais_Region %in% c("Spain", "Italy", "China",
                                      "US", "Germany", input$pais)) %>%
            ggplot(aes(x = Dias_Desde_PC, y = Casos_Confirmados)) +
            geom_line(aes(col = Pais_Region)) +  
            xlab("Días desde el primer contagio") +
            ylab("Número de personas contagiadas") + 
            ggtitle("Análisis por Cohortes") +
            theme(legend.position = "none") -> g
        
        ggplotly(g)
    })
    
}

shinyApp(ui = ui, server = server)
