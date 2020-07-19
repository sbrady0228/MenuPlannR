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
in_ingredients <- read.csv("data/20200719_ingredients.csv")

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
           verbatimTextOutput("deli"),
           h1("Dairy Section"),
           br(),
           verbatimTextOutput("dairy"),
           h1("Produce Section"),
           br(),
           verbatimTextOutput("produce"),
           h1("Meat Section"),
           br(),
           verbatimTextOutput("meat"),
           h1("Grocery Section"),
           br(),
           verbatimTextOutput("grocery")
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
            summarize(Shopping.Quantity = sum(Shopping.Quantity), .groups = 'drop')%>%
            ungroup()%>%
            mutate(Store.Section = factor(.$Store.Section,levels = c("Produce","Deli","Meat","Grocery","Dairy")))%>%
            mutate(Amount = paste0("(",.$Shopping.Quantity," ",.$Shopping.Metric,")")) %>%
            select(Store.Section, Ingredient, Amount) %>%
            arrange(Store.Section)
            
    })
    
    output$deli <- renderPrint({
        if(is.null(input$recipes)){
            return(writeLines("None"))
        }
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Deli")%>%
            mutate(Item = paste0(.$Ingredient," ", .$Amount))
        ifelse(length(new$Ingredient) > 0,
               return(writeLines(new$Item)),
                  return(writeLines("None")))
    })
    output$dairy <- renderPrint({
        if(is.null(input$recipes)){
            return(writeLines("None"))
        }
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Dairy")%>%
            mutate(Item = paste0(.$Ingredient," ", .$Amount))
        ifelse(length(new$Ingredient) > 0,
               return(writeLines(new$Item)),
               return(writeLines("None")))
    })
    output$grocery <- renderPrint({
        if(is.null(input$recipes)){
            return(writeLines("None"))
        }
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Grocery")%>%
            mutate(Item = paste0(.$Ingredient," ", .$Amount))
        ifelse(length(new$Ingredient) > 0,
               return(writeLines(new$Item)),
               return(writeLines("None")))
    })
    output$produce <- renderPrint({
        if(is.null(input$recipes)){
            return(writeLines("None"))
        }
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Produce")%>%
            mutate(Item = paste0(.$Ingredient," ", .$Amount))
        ifelse(length(new$Ingredient) > 0,
               return(writeLines(new$Item)),
               return(writeLines("None")))
    })
    output$meat <- renderPrint({
        if(is.null(input$recipes)){
            return(writeLines("None"))
        }
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Meat")%>%
            mutate(Item = paste0(.$Ingredient," ", .$Amount))
        ifelse(length(new$Ingredient) > 0,
               return(writeLines(new$Item)),
               return(writeLines("None")))
    })
    output$grocery <- renderPrint({
        if(is.null(input$recipes)){
            return(writeLines("None"))
        }
        new <- as.data.frame(filtered())%>%
            filter(.$Store.Section == "Grocery")%>%
            mutate(Item = paste0(.$Ingredient," ", .$Amount))
        ifelse(length(new$Ingredient) > 0,
               return(writeLines(new$Item)),
               return(writeLines("None")))
    })
    
    
    
    
    
}

# To deploy onto website
# library(rsconnect)
# rsconnect::setAccountInfo(name='sbrady', token='A6904AE34F71DA5BC0AF222C7223141E', secret='uZq5+IpKSSDqlFFyMkirNWDo2z8XlKYaRZmZ5hzF')
# deployApp()


# Run the application 
shinyApp(ui = ui, server = server)
