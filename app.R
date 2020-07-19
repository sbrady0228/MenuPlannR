#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(DT)
in_ingredients <- read.csv("data/20200717_ingredients.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Application title
    titlePanel("Menu Planner"),

    # Choice selection with recipes listed 
    sidebarLayout(
        sidebarPanel(
            uiOutput("recipes")
        ),

        # Output the shopping list table
        mainPanel(h1("Deli Section"),
                  br(),
           verbatimTextOutput("deli")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

        
    output$recipes <- renderUI({
        selectInput("recipes", "Choose Recipes", choices = unique(in_ingredients$Name), multiple = TRUE,
                    selectize = TRUE)
    })
    #select ingredients for the recipes chosen
    filtered <- reactive({
        if(is.null(input$recipes)){
            return(NULL)
        }
        
        in_ingredients %>%
            filter(Name %in% input$recipes) %>%
            select(Store.Section,Ingredient,Shopping.Quantity, Shopping.Metric)%>%
            group_by(Store.Section, Ingredient, Shopping.Metric) %>%
            mutate(Shopping.Quantity = sum(Shopping.Quantity))%>%
            ungroup()%>%
            mutate(Store.Section = factor(.$Store.Section,levels = c("Produce","Deli","Meat","Grocery","Dairy")))%>%
            mutate(Amount = paste0("(",.$Shopping.Quantity," ",.$Shopping.Metric,")")) %>%
            select(Store.Section, Ingredient, Amount) %>%
            arrange(Store.Section)
            
    })
    
    output$deli <- renderPrint({
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Deli")
        writeLines(new$Ingredient)
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
