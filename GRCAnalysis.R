library(shiny)
library(arules)
library(dplyr)

ui <- fluidPage(
  # shinythemes
  shinythemes::themeSelector(),
  # Title
  titlePanel("GRC Data Analytics Toolkit"),
  
  sidebarLayout(
    sidebarPanel(
      # Inputs
      fileInput(inputId = "Dataset_Path",
                label = "Dataset Path:",
                multiple = FALSE,
                accept = ".csv"),
      
      numericInput(inputId = "min_support", label = "Enter Minimum Support:", value = NULL),
      numericInput(inputId = "min_confidence", label = "Enter Minimum Confidence:", value = NULL),
      actionButton("submit", "Run Analysis")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Cleaning Status", textOutput(outputId = "status")),
        tabPanel("Association", verbatimTextOutput("results")),
        tabPanel("Data Visualization",
                 plotOutput("payment_type"),
                 plotOutput("total_boxplot"),
                 plotOutput("city_spending_barplot"),
                 plotOutput("age_spending_plot")),
        tabPanel("K-Means Clustering",
                 sidebarLayout(
                   sidebarPanel(
                     numericInput("num_clusters", "Number of Clusters", value = 3, min = 2, max = 4),
                     actionButton("cluster", "Run K-Means")
                   ),
                   mainPanel(
                     dataTableOutput("cluster_table")
                   )
                 )
        )
      )
    )
  )
)

server <- function(input, output) {
  #----Data Cleaning----
  data <- reactive({
    req(input$Dataset_Path)
    data <- read.csv(input$Dataset_Path$datapath)
    duplicated_rows <- sum(duplicated(data))
    new_data <- distinct(data)
    new_duplicated_rows <- sum(duplicated(new_data))
    missing_values <- sum(is.na(new_data))
    
    output$status <- renderText({
      paste("Dataset loaded and cleaned successfully.",
            "Previous duplicated rows:", duplicated_rows,        
            "Duplicated rows now:", new_duplicated_rows,
            "Missing values count:", missing_values)
    })
    new_data
  })
  
  #----Association rules----
  output$results <- renderPrint({
    req(input$submit)
    minSupport <- input$min_support
    minConfidence <- input$min_confidence
    items_data <- read.transactions(textConnection(data()$items), sep = ",")
    if (!is.na(minSupport) && !is.na(minConfidence)) {
      Apriori_data <- apriori(items_data, parameter = list(supp = minSupport, confi = minConfidence, minlen = 2))
      rules <- as(Apriori_data, "data.frame")  
      return(rules)
    } else {
      return("Please enter valid numbers for support and confidence!")
    }
  })
  
  #----K-means Clustering----
  run_kmeans <- eventReactive(input$cluster, {
    req(data())
    n_clusters <- input$num_clusters
    scaled_data <- scale(data()[, c("age", "total")])
    kmeans.results <- kmeans(scaled_data, centers = n_clusters, nstart = 20)
    clustered_data <- cbind(data(), cluster = as.factor(kmeans.results$cluster))
    return(clustered_data[!duplicated(clustered_data$customer), ])  # Remove duplicates based on customer
  })
  
  output$cluster_table <- renderDataTable({
    kmeans_results <- run_kmeans()
    if (!is.null(kmeans_results)) {
      kmeans_results[, c("customer", "age", "total", "cluster")]
    }
  })
  
  #----Data Visualization----
  output$payment_type <- renderPlot({
    req(input$submit)
    x <- table(data()$paymentType)
    percentage <- paste0(round(100 * x / sum(x)), "%")
    pie(x, labels = percentage, main = "Comparing the payment type", col = c("light green", "light blue"))
    legend("bottomleft", legend = c("Cash", "Credit"), fill = c("light green", "light blue"))
  })
  
  output$total_boxplot <- renderPlot({
    req(input$submit)
    boxplot(x = data()$total, main = "Distribution of total spending", xlab = "Total spending")
  })
  
  output$city_spending_barplot <- renderPlot({
    req(input$submit)
    totals_by_city <- aggregate(total ~ city, data = data(), FUN = sum)
    totals_by_city <- totals_by_city[order(totals_by_city$total, decreasing = TRUE), ]
    barplot(totals_by_city$total, names.arg = totals_by_city$city,
            xlab = "City", ylab = "Total Spending",
            main = "Relation between each city and its total spending", col = "purple")
  })
  
  output$age_spending_plot <- renderPlot({
    req(input$submit)
    totals_by_age <- aggregate(total ~ age, data = data(), FUN = sum)
    plot(totals_by_age$age, y = totals_by_age$total,
         xlab = "Age", ylab = "Total Spending",
         main = "Relation between each age and its total spending", col = "black")
  })
}

shinyApp(ui = ui, server = server)

