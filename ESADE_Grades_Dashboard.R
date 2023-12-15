library(rsconnect)
library(readxl)
library(tidyverse)
library(data.table)
library(lubridate)
library(shiny)
library(shinythemes)
library(patchwork)
library(gridExtra)


df_esade <- read_xlsx("ESADE Grades.xlsx", sheet = 1, col_names = TRUE)
#df_hec <- read_xlsx("ESADE Grades.xlsx", sheet = 2, col_names = TRUE) #We don't use it!
str(df_esade)

#Some data cleaning for the dashboard needs
df_esade <- df_esade %>%
  mutate(weighted_average_multiplier= Qualification * Credits)
df_esade$MH <- replace_na(df_esade$MH,"No")
df_esade$Academic_Year <- as.factor(df_esade$Academic_Year)


#####Shiny Code
#User Interface

ui <- fluidPage(
  titlePanel("Adrià Grades Throughout Esade"),
  theme = shinytheme("yeti"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("grade", "Minimal Grade to display", min=6.5, max= 10, value = 6.5),
      checkboxGroupInput("program", "Academic Program", unique(df_esade$Program), selected = unique(df_esade$Program)),
      checkboxGroupInput("year", "Academic Year", choices = unique(df_esade$Academic_Year),
                         selected = unique(df_esade$Academic_Year)),
      checkboxGroupInput("honors", "Honors", c("Yes", "No"), unique(df_esade$MH))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Grade Distribution",
                 plotOutput("grades_distr")),
        tabPanel("Grade Distribution per Year",
                 plotly::plotlyOutput("grades_year")),
        tabPanel("Grade Evolution",
                 plotly::plotlyOutput("grades_evo")),
        tabPanel("Grade per Program",
                 plotly::plotlyOutput("program_grade")),
        tabPanel("Average Grade per Year",
                 plotly::plotlyOutput("year_grade")),
        tabPanel("Grade per Subject",
                 plotly::plotlyOutput("subject_grade")),
        tabPanel("Data",
                 DT::DTOutput("data"))
        
        )
      )
    )
  )


#Server Code
server <- function (input, output, session) {
  #We start defining a reactive that will be called many times.
  data <- reactive({
    df_esade %>%
      filter(Qualification >= input$grade) %>%
      filter(Academic_Year %in% input$year) %>%
      filter(Program %in% input$program) %>%
      filter(MH %in% input$honors)
  })
 
  output$grades_distr <- renderPlot({
    plot1 <- data() %>%
      ggplot(aes(x = Qualification)) +
      geom_boxplot(fill = "lightblue", colour = "black") +
      theme_light() +
      guides(fill = "none") +
      theme(axis.text.x = element_blank(),   
            axis.title.x = element_blank(),
            axis.text.y = element_blank())
    
    plot2 <- data() %>%
      ggplot(aes(x = Qualification)) +
      geom_histogram(fill = "lightblue", colour = "black") +
      theme_light() +
      guides(fill = "none") +
      theme(axis.text.y = element_blank(),   
            axis.title.y = element_blank())
    
    grid.arrange(plot1, plot2, nrow = 2)
  })
  
  output$grades_year <- plotly::renderPlotly({
    data() %>%
      ggplot(aes(x = round(Qualification, 1), fill = Academic_Year)) +
      geom_histogram(color = "black") +
      facet_grid(Academic_Year ~ .) +
      theme_minimal() +
      labs(title = "Grade Distribution per Academic Year", x = "Grades") +
      guides(fill = "none") +
      theme(axis.text.y = element_blank(),   
            axis.title.y = element_blank())
  })
  
  output$grades_evo <- plotly::renderPlotly({
    data() %>%
      group_by(Year) %>%
      summarise(Grade = round(sum(weighted_average_multiplier) / sum(Credits), 1)) %>%
      ggplot(aes(x = Year, y = Grade)) +
      geom_line() +
      geom_point() +
      labs(title = "Temporal Evolution of Grades", 
           subtitle="based on Weighted Average with Credits")+
      theme_minimal()
  })
  
  output$program_grade <- plotly::renderPlotly({
    data() %>%
      group_by(Program) %>%
      summarise(Grade = round(sum(weighted_average_multiplier)/sum(Credits),1)) %>%
      ggplot(aes(x=Program, y=Grade, fill=Program)) +
      geom_col(color="black") +
      labs(title="Weighted Average Grade per Program") +
      theme_minimal()
  })
  
  output$year_grade <- plotly::renderPlotly({
    data() %>%
      group_by(Academic_Year) %>%
      summarise(Grade = round(sum(weighted_average_multiplier) / sum(Credits), 1)) %>%
      ggplot(aes(x = Academic_Year, y = Grade, fill = Academic_Year)) +
      geom_col(color="black") +
      geom_point() +
      labs(fill="Academic Year")+
      theme_minimal() +
      labs(title = "Average Grade per Academic Year", 
           subtitle = "Measured with Weighted Average based on Credits",
           x="Academic Year") +
      guides(fill="none")
  }) 
  
  output$subject_grade <- plotly::renderPlotly({
    data() %>%
      group_by(Department) %>%
      summarise(Grade = round(sum(weighted_average_multiplier)/sum(Credits), 1)) %>%
      ggplot(aes(y=Department, x=Grade)) +
      geom_col(fill="grey") +
      geom_text(aes(label = Grade), vjust = -0.5, color = "black", size = 3) +
      theme_minimal() +
      labs(title = "Weighted Grade per Subject Department") +
      guides(fill="none")
  })
  
  output$data <- DT::renderDT({
    data() %>%
      select(-weighted_average_multiplier) %>%
      arrange(desc(Academic_Year), Department)
  })
}

#We create the dashboard!
shinyApp(ui=ui, server = server)

