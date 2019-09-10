#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(lubridate)
library(shiny)


dados_original <- readRDS("principais.rds")

dados <- dados_original %>% 
    filter(!is.na(GWh)) %>% 
    rename(municipio = `Município`) %>% 
    group_by(estacao) %>% 
    arrange(data) %>% 
    filter(between(wday(data),2,6)) %>%
    mutate(alpha = 1/0.995) %>% 
    mutate(fator_ewma  = cumprod(alpha)*1e-50 ) %>% 
    select(estacao, data, GWh, fator_ewma, tempmaxima, tempminima, temp_comp_media, municipio ) %>% 
    arrange(estacao, data) %>% 
    mutate(carga_amena = if_else(tempmaxima < 28 & tempminima > 15, GWh, 0  ) ) %>% 
    mutate(fator_nao_NA = if_else(carga_amena == 0, 0, fator_ewma)) %>% 
    mutate(
        carga_amena = if_else( is.na(carga_amena), 0, carga_amena),
        fator_nao_NA = if_else( is.na(fator_nao_NA), 0, fator_nao_NA)
    ) %>% 
    mutate(
        carga_ewma = cumsum(fator_nao_NA * carga_amena)/cumsum(fator_nao_NA) 
    ) %>% 
    mutate(carga_amena = if_else(carga_amena == 0, NA_real_, carga_amena)) %>% 
    mutate(erro = (GWh - carga_ewma)/carga_ewma)
    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Temperatura x Carga"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("municipio",
                        "Municipio:",
                        choices = sort(distinct(select(dados,municipio))$municipio),
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("carga"),
           plotOutput("erros")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$carga <- renderPlot({
        
        dados_escolhidos <- dados %>% 
            filter(municipio == input$municipio) %>% 
            filter(year(data) > 2003)
        
        ggplot(dados_escolhidos, aes(x = data, group = estacao)) +
            geom_line(aes( y = GWh), color = "blue") +
            geom_line(aes( y = carga_amena), color = "red") +
            geom_line(aes( y = carga_ewma), color = "black", size = 2 ) +
            scale_y_continuous(limits = c(0,NA)) +
            theme_light()

    })
    
    output$erros <- renderPlot({
        
        dados_escolhidos <- dados %>% 
            filter(municipio == input$municipio) %>% 
            filter(year(data) > 2003) %>% 
            mutate(
                estacao_ano = case_when(
                    month(data) %in% c(1,2,3) ~ "Verão", 
                    month(data) %in% c(6,8,9) ~ "Inverno", 
                    TRUE ~ "Outono/Primavera"
                )
            )
        ggplot(dados_escolhidos, aes(                 
                                     x = temp_comp_media, 
                                     y = erro, 
                                     color = estacao_ano)) +
            geom_point(
                alpha = 0.3
                ) +
            geom_smooth() +
            theme_light()
        
    })
    
    
    
}






# Run the application 
shinyApp(ui = ui, server = server)
