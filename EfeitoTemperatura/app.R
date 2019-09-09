#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(lubridate)

# dados_original <- readRDS("tudo.rds")

dados <- dados_original %>% 
    filter(!is.na(GWh)) %>% 
    filter(UF == "SP") %>%     
    group_by(estacao) %>% 
    arrange(data) %>% 
    filter(between(wday(data),2,6)) %>%
    mutate(alpha = 1/0.96) %>% 
    mutate(fator_ewma  = cumprod(alpha)*1e-50 ) %>% 
    select(estacao, data, GWh, fator_ewma, tempmaxima, tempminima ) %>% 
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
    mutate(carga_amena = if_else(carga_amena == 0, NA_real_, carga_amena))
    

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("estacao",
                        "Estacao:",
                        choices = distinct(select(dados,estacao))$estacao,
                        selected = 83630
                        )
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("carga")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$carga <- renderPlot({
        
        dados_escolhidos <- dados %>% 
            filter(estacao == input$estacao) %>% 
            filter(year(data) > 2003)
        
        ggplot(dados_escolhidos, aes(x = data)) +
            geom_line(aes( y = GWh), color = "blue") +
            geom_line(aes( y = carga_amena), color = "red") +
            geom_line(aes( y = carga_ewma), color = "black", size = 2 ) +
            scale_y_continuous(limits = c(0,NA)) +
            theme_light()

    })
}

# Run the application 
shinyApp(ui = ui, server = server)
