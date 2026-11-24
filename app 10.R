# ==============================================
# Product Category Analysis Dashboard using Shiny
# ==============================================

library(shiny)

# Dataset
product_data <- data.frame(
  Category = c("Electronics", "Clothing", "Appliances"),
  Sales = c(50000, 35000, 40000)
)

ui <- fluidPage(
  
  titlePanel("Product Category Analysis Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      h3("Product Categories"),
      checkboxGroupInput(
        "category",
        "Select Categories:",
        choices = product_data$Category,
        selected = product_data$Category
      )
    ),
    
    mainPanel(
      
      fluidRow(
        
        column(
          6,
          plotOutput("funnelChart", height = "300px")
        ),
        
        column(
          6,
          plotOutput("pieChart", height = "300px")
        )
        
      ),
      
      hr(),
      
      h3("Sales Data Table"),
      
      tableOutput("salesTable")
      
    )
    
  )
  
)

server <- function(input, output) {
  
  filteredData <- reactive({
    product_data[product_data$Category %in% input$category, ]
  })
  
  # Question 1 - Funnel Chart
  output$funnelChart <- renderPlot({
    
    data <- filteredData()
    data <- data[order(data$Sales, decreasing = TRUE), ]
    
    barplot(
      data$Sales,
      names.arg = data$Category,
      horiz = TRUE,
      col = c("steelblue", "orange", "forestgreen"),
      main = "Sales Conversion Funnel",
      xlab = "Sales ($)",
      xlim = c(0, max(product_data$Sales) + 10000)
    )
    
  })
  
  # Question 4 - Pie Chart
  output$pieChart <- renderPlot({
    
    data <- filteredData()
    
    pie(
      data$Sales,
      labels = paste(data$Category, "\n$", data$Sales),
      col = rainbow(nrow(data)),
      main = "Sales Distribution"
    )
    
  })
  
  # Question 2 - Sales Table
  output$salesTable <- renderTable({
    
    filteredData()
    
  })
  
}

shinyApp(ui = ui, server = server)