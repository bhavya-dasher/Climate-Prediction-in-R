library(shiny)
library(caret)
library(randomForest)
library(ggplot2)

shinyServer(function(input, output, session) {
  data <- reactive({
    req(input$data_file)
    data <- read.csv(input$data_file$datapath)
    data
  })
  
  output$my_image <- renderImage({
    list(src = "G:/bhavya/New folder/Wallpapers/3D-Animated-HD-Wallpapers-by-techblogstop-16.jpg", width="400px", height="350px")
  }, deleteFile = FALSE)
  model <- eventReactive(input$train_model, {
    set.seed(42)
    train_indices <- sample(1:nrow(data()), 0.7 * nrow(data()))
    train_data <- data()[train_indices, ]
    test_data <- data()[-train_indices, ]
    
    predictors <- c(input$predictor1, input$predictor2, input$predictor3)
    target <- input$target
    
    predictor_data <- train_data[, predictors]
    target_data <- train_data[, target]
    
    model <- randomForest(x = predictor_data, y = target_data, ntree = 500)
    model
  })
  
  predictions <- eventReactive(model(), {
    predict(model(), newdata = test_data)
  })
  
  mae <- eventReactive(predictions(), {
    mean(abs(predictions() - test_data[, target]))
  })
  
  rmse <- eventReactive(predictions(), {
    sqrt(mean((predictions() - test_data[, target])^2))
  })
  
  observeEvent(input$train_model, {
    updateTabsetPanel(session, "tabs", "Visualizations")
  })
  
  output$boxplot <- renderPlot({
    ggplot(data(), aes(x = cut(data()[, input$predictor1], breaks = seq(0, 45, by = 5), right = FALSE), y = data()[, input$target])) +
      geom_boxplot(fill = "orange") +
      labs(title = "Humidity by Temperature Range", x = "Temperature Range", y = "Humidity")
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data(), aes(x = data()[, input$predictor1], y = data()[, input$target])) +
      geom_point(color = "blue") +
      labs(title = "Temperature vs. Humidity", x = "Mean Temperature", y = "Humidity")
  })
  
  output$mae <- renderText({
    paste("Mean Absolute Error:", mae())
  })
  
  output$rmse <- renderText({
    paste("Root Mean Squared Error:", rmse())
  })
})