#Librerias
library(shiny)
library(MASS)
library(ggplot2)

#Cargamos el Dataset Boston 
data("Boston")

#Modelo de regresión lineal
modelo <- lm(medv ~ rm + ptratio + lstat, data = Boston)

#Shiny
ui <- fluidPage(
  titlePanel("Modelo de Regresión Lineal - Boston"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Variables predictoras"),
      sliderInput("rm", "Promedio de habitaciones (rm):", 
                  min = min(Boston$rm), max = max(Boston$rm), 
                  value = mean(Boston$rm), step = 0.1),
      sliderInput("ptratio", "Relación alumnos/profesor (ptratio):", 
                  min = min(Boston$ptratio), max = max(Boston$ptratio),
                  value = mean(Boston$ptratio), step = 0.1),
      sliderInput("lstat", "% bajo nivel socioeconómico (lstat):", 
                  min = min(Boston$lstat), max = max(Boston$lstat),
                  value = mean(Boston$lstat), step = 0.1),
      hr(),
      h4("Predicción del valor medio de vivienda (medv):"),
      textOutput("prediccion")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Resumen del modelo", verbatimTextOutput("summary")),
        tabPanel("Correlaciones", plotOutput("corrPlot")),
        tabPanel("Predicción gráfica", plotOutput("predPlot"))
      )
    )
  )
)

server <- function(input, output) {
  
  output$summary <- renderPrint({
    summary(modelo)
  })
  
  #Gráfico de correlaciones
  output$corrPlot <- renderPlot({
    cor_data <- cor(Boston[, c("medv", "rm", "ptratio", "lstat")])
    ggplot(as.data.frame(as.table(cor_data)), aes(Var1, Var2, fill = Freq)) +
      geom_tile() +
      geom_text(aes(label = round(Freq, 2))) +
      scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = 0) +
      theme_minimal() +
      labs(title = "Matriz de Correlación", fill = "Correlación")
  })
  
  #Sliders
  output$prediccion <- renderText({
    nuevo <- data.frame(
      rm = input$rm,
      ptratio = input$ptratio,
      lstat = input$lstat
    )
    pred <- predict(modelo, nuevo)
    paste0(round(pred, 2), " (en miles de USD)")
  })
  
  #Grafico de prediccion
  output$predPlot <- renderPlot({
    nuevo <- data.frame(
      rm = input$rm,
      ptratio = input$ptratio,
      lstat = input$lstat
    )
    pred <- predict(modelo, nuevo)
    ggplot(Boston, aes(x = lstat, y = medv)) +
      geom_point(alpha = 0.6, color = "gray50") +
      geom_smooth(method = "lm", color = "blue", se = FALSE) +
      geom_point(aes(x = input$lstat, y = pred), color = "red", size = 3) +
      labs(title = "Predicción de MEDV según LSTAT",
           x = "% Bajo nivel socioeconómico (lstat)",
           y = "Valor medio (medv)") +
      theme_minimal()
  })
}

#Ejecutar la app
shinyApp(ui = ui, server = server)
