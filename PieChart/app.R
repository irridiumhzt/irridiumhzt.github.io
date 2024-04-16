library(shiny)
library(tidyverse)
library(ggplot2)
library(dplyr)

survey <- read_csv("LGBT_Survey_DailyLife.csv") 

row.has.na <- apply(survey, 1, function(x){any(is.na(x))})
sum(row.has.na)
survey_clean <- survey[row.has.na,]
data = subset(survey_clean, CountryCode != "Average")

gay_data <- subset(data, subset == "Gay")
lesbian_data <- subset(data, subset == "Lesbian")
bisexual_man_data <- subset(data, subset == "Bisexual men")
bisexual_woman_data <- subset(data, subset == "Bisexual women")
trans_data <- subset(data, subset == "Transgender")

# Define UI for application that draws a histogram
ui <- fluidPage(
  selectInput("gender", label = "Select Your Sexuality:", choices = c("Gay","Lesbian","Bisexual Man","Bisexual Woman","Non-binary")),
  verbatimTextOutput("summary"),
  tableOutput("table"),
  
  sidebarLayout(
    sidebarPanel(sliderInput("countrycount","Number of Countries",min = 1,max = 28,value = 28)),
    mainPanel(plotOutput("distPlot"))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  selected_data <- reactive({
    if (input$gender == "Gay") {
      return(gay_data)
    } else if (input$gender == "Lesbian") {
      return(lesbian_data)
    }
    else if (input$gender == "Bisexual Man") {
      return(bisexual_man_data)
    }
    else if (input$gender == "Bisexual Woman") {
      return(bisexual_woman_data)
    }
    else return (trans_data)
  })
  
  output$distPlot <- renderPlot({
    
    g5_data <- selected_data() %>%
      filter(question_code == "g5") %>%
      select(CountryCode, answer) %>%
      mutate(answer = as.numeric(answer)) %>% 
      group_by(CountryCode) %>%
      summarise(number = mean(answer, na.rm = TRUE)) %>%
      arrange(desc(number)) %>%
      head(input$countrycount) 
    
    ggplot(g5_data, aes(x = reorder(CountryCode, number), y = number, fill = number)) +
      geom_bar(stat = "identity", position = "dodge", color = "black", alpha = 0.8) +
      scale_fill_gradient(low = "white", high = "blue") + # Color by satisfaction level
      labs(title = "LGBT Satisfaction rating by Country",
           x = "Country", y = "Average Satisfaction") +
      geom_text(aes(label = paste0(round(number), "%")), position = position_stack(vjust =0.5),size = 3)+
      theme_minimal() +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)