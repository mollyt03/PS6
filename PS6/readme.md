##
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

link to file  https://molly-t03.shinyapps.io/PS62/

https://www.shinyapps.io/admin/#/application/8497713

library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

This data is the data my group is using for the final project, it shows multiple variables and then weither a student graduated or dropped out. 
data <- read.csv("data/dataset.csv")


ui <- fluidPage(
  
  
  titlePanel("Predicting students' dropout and academic success"),
  tabsetPanel(
    # First page - general information
    tabPanel("General Information",
             fluidPage(
               
               # Explanation text
               mainPanel(
                 h3("Dataset Description"),
                 p("This dataset contains information about students' enrollment in higher education and their likelihood of dropping out or achieving academic success. The variables in the dataset describe", em("demographic data, social-economic factors and academic performance information"), ". This dataset includes",
                   strong(" ", nrow(data)), " rows and", strong(" ", ncol(data)), " variables."),
                 p("The following plots show the distribution of gender for the students in the dataset, where 1 = women and 0 = men."),
                 
                 #Plot
                 fluidRow(
                   column(6, plotOutput("gender_plot")),
                 )
               )
             )   
             
    ),
    
    # Second page - plot, these widgets allow the user to change the y axis to show the first semester grade or second semester grade. the user also has the option to change the color of the plot
    tabPanel("Plot",
             fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId = "yvar", label = "Y-axis variable",
                               choices = c("Curricular units 1st sem (grade)",
                                           "Curricular units 2nd sem (grade)"),
                               selected = "Curricular units 1st sem (grade)"),
                   selectInput(inputId = "colorvar", label = "Color",
                               choices = c("Green", "Blue"),
                               selected = "Green")
                 ),
                 mainPanel(
                   plotOutput(outputId = "myplot"),
                   textOutput(outputId = "description")
                 )
               )
             )
             
    ),
    
    # Third page - table. these widgets change the values in the table, the user can show multiple variables including tution fees, if they are in debt or not, and if the student recieved a scholarship and then compare it to the dropout/graduation variable. The summary at the bottom tells the user if there are missing values. 
    tabPanel("Table",
             fluidPage(
               titlePanel("Graduate or Dropout"),
               sidebarLayout(
                 sidebarPanel(
                   p("The following data can be used by college adminstration to figure out if admitting students on scholarship is worth it or not, depending on past data if students dropped out or graduated, weither they got a scholarship, are in debt, or if their tution fees are paid (1 = true, 0 = false)"),
                   checkboxGroupInput("checkGroup", 
                                      label = "Choose columns to display:", 
                                      choices = c("Scholarship holder", 
                                                  "Tuition fees up to date", 
                                                  "Debtor"),
                                      selected = character(0))
                 ),
                 mainPanel(
                   tableOutput("table"),
                   br(),
                   textOutput("summary")
                 )
               )
             )   
             
             
    )
  )
)







server <- function(input, output) {
  
  #Gender plot
  output$gender_plot <- renderPlot({
    ggplot(data, aes(x = Gender)) +
      geom_bar() +
      ggtitle("Gender Distribution")
  })
  
  #output of plot on second page
  output$myplot <- renderPlot({
    if(input$yvar == "Curricular units 1st sem (grade)"){
      yvar <- "Curricular units 1st sem (approved)"
    }else{
      yvar <- "Curricular units 2nd sem (approved)"
    }
    color <- if(input$colorvar == "Green"){"green"}else{"blue"}
    ggplot(data, aes(x = `Age at enrollment`, y = !!sym(yvar), color = factor(color))) + 
      geom_point() +
      labs(x = "Age at enrollment", y = "(input$yvar)", color = "Color")
  })
  
  #text description on second page
  output$description <- renderText({
    yvar <- if(input$yvar == "Curricular units 1st sem (grade)"){
      "Curricular units 1st sem (approved)"
    }else{
      "Curricular units 2nd sem (approved)"
    }
    range_min <- min(data[[yvar]])
    range_max <- max(data[[yvar]])
    paste("The range of", input$yvar, "is", range_min, "to", range_max)
  })
  
  #table on third page 
  output$table <- renderTable({
    cols <- c("Target", input$checkGroup)
    data[, cols, drop = FALSE]
  })
  
  output$summary <- renderText({
    missing_values <- sum(is.na(data[, c("Target", input$checkGroup), drop = FALSE]))
    paste("There are", missing_values, "missing observations.")
  })
  
  
}

install.packages('rsconnect')
# Run the application 
shinyApp(ui = ui, server = server)