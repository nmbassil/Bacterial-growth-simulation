library(shiny)
library(ggplot2)
library(fGarch)

ui <- fluidPage(
  sidebarPanel(
    sliderInput(inputId = "pH", label = ("Please select the pH"), min = 0, max = 14, value = 7, step = 0.5),
    sliderInput(inputId = "temperature", label = ("Please select the temperature in Celsius"), min = (-10), max = 90, value = 37, step = 5),
    sliderInput(inputId = "frame", label = "Time in hours", min = 0, max = 10, value = 0, animate = animationOptions(interval = 600))
  ),
  mainPanel(
    plotOutput(outputId = 'growth')
    #tableOutput("table")
  ),
  imageOutput(outputId = 'figure')
)

#pH <- c('5.5-8.5 (7)','7.5-11.5 (10)', '6-9.5 (8)', '3.5-6.5 (4)', '6-9.5 (7.5)', '7-11 (10)')
#Temperature <- c('5-45 (37)', '10-45 (30)', '4-30 (15)', '10-35 (25)', '40-80 (70)', '30-65 (55)')
#Bacteria <- c('Escherichia coli', 'Bacillus pseudofirmus', 'Psychrobacter luti', 'Leptospirillum ferrooxidans', 'Thermus aquaticus', 'Clostridium paradoxum')
#Classification <- c('Neutrophile & Mesophile', 'Alkaliphile', 'Psychrophile', 'Acidophile', 'Thermophile', 'Alkaliphile & Thermophile')
#the.table <- data.frame(Bacteria, pH, Temperature, Classification)

server <- function(input, output) {
#  output$table <- renderTable(
#    the.table, colnames = TRUE
#  )
  the.data <- eventReactive(input$frame, {
    if(input$frame == 0){ # This builds the initial frame
      x <- rnorm(2**input$frame, mean = 1, sd = 0.1)
      y <- rnorm(2**input$frame, mean = 2, sd = 0.1)
      z <- as.factor(rep('Escherichia coli', length(x)))
      ecoli <- data.frame(x=x, y=y, z=z)
      x <- rnorm(2**input$frame, mean = 2, sd = 0.1)
      y <- rnorm(2**input$frame, mean = 1, sd = 0.1)
      z <- as.factor(rep('Thermus aquaticus', length(x)))
      taquaticus <- data.frame(x=x, y=y, z=z)
      x <- rnorm(2**input$frame, mean = 3, sd = 0.1)
      y <- rnorm(2**input$frame, mean = 2, sd = 0.1)
      z <- as.factor(rep('Psychrobacter luti', length(x)))
      pluti <- data.frame(x=x, y=y, z=z)
      x <- rnorm(2**input$frame, mean = 2, sd = 0.1)
      y <- rnorm(2**input$frame, mean = 2, sd = 0.1)
      z <- as.factor(rep('Bacillus pseudofirmus', length(x)))
      bpseudofirmus <- data.frame(x=x, y=y, z=z)
      x <- rnorm(2**input$frame, mean = 1, sd = 0.1)
      y <- rnorm(2**input$frame, mean = 1, sd = 0.1)
      z <- as.factor(rep('Leptospirillum ferrooxidans', length(x)))
      aligni <- data.frame(x=x, y=y, z=z)
      x <- rnorm(2**input$frame, mean = 3, sd = 0.1)
      y <- rnorm(2**input$frame, mean = 1, sd = 0.1)
      z <- as.factor(rep('Clostridium paradoxum', length(x)))
      cparadoxum <- data.frame(x=x, y=y, z=z)
      the.df <- rbind(ecoli, bpseudofirmus, pluti, aligni, taquaticus, cparadoxum)
    } else {
        x <- rnorm(2**(input$frame*dsnorm(input$temperature, 28, 12, 0.5)*dnorm(input$pH, 7, 1.1)*50), mean = 1, sd = input$frame/50) 
        y <- rnorm(2**(input$frame*dsnorm(input$temperature, 28, 12, 0.5)*dnorm(input$pH, 7, 1.1)*50), mean = 2, sd = input$frame/50) 
        z <- as.factor(rep('Escherichia coli', length(x)))
        ecoli <- data.frame(x=x, y=y, z=z)
        x <- rnorm(2**(input$frame*dsnorm(input$temperature, 62, 12, 0.4)*dnorm(input$pH, 7.7, 1)*50), mean = 2, sd = input$frame/50)
        y <- rnorm(2**(input$frame*dsnorm(input$temperature, 62, 12, 0.4)*dnorm(input$pH, 7.7, 1)*50), mean = 1, sd = input$frame/50)
        z <- as.factor(rep('Thermus aquaticus', length(x)))
        taquaticus <- data.frame(x=x, y=y, z=z)
        x <- rnorm(2**(input$frame*dsnorm(input$temperature, 17, 8, 1.5)*dsnorm(input$pH, 7.8, 1.1, 0.8)*40), mean = 3, sd = input$frame/50)
        y <- rnorm(2**(input$frame*dsnorm(input$temperature, 17, 8, 1.5)*dsnorm(input$pH, 7.8, 1.1, 0.8)*40), mean = 2, sd = input$frame/50)
        z <- as.factor(rep('Psychrobacter luti', length(x)))
        pluti <- data.frame(x=x, y=y, z=z)
        x <- rnorm(2**(input$frame*dsnorm(input$temperature, 28, 10, 0.8)*dsnorm(input$pH, 9.6, 1.2, 0.7)*57), mean = 2, sd = input$frame/50)
        y <- rnorm(2**(input$frame*dsnorm(input$temperature, 28, 10, 0.8)*dsnorm(input$pH, 9.6, 1.2, 0.7)*57), mean = 2, sd = input$frame/50)
        z <- as.factor(rep('Bacillus pseudofirmus', length(x)))
        bpseudofirmus <- data.frame(x=x, y=y, z=z)
        x <- rnorm(2**(input$frame*dsnorm(input$temperature, 23, 8, 0.7)*dsnorm(input$pH, 2.8, 1.1, 1.9)*40), mean = 1, sd = input$frame/50)
        y <- rnorm(2**(input$frame*dsnorm(input$temperature, 23, 8, 0.7)*dsnorm(input$pH, 2.8, 1.1, 1.9)*40), mean = 1, sd = input$frame/50)
        z <- as.factor(rep('Leptospirillum ferrooxidans', length(x)))
        aligni <- data.frame(x=x, y=y, z=z)
        x <- rnorm(2**(input$frame*dsnorm(input$temperature, 48, 12, 0.5)*dsnorm(input$pH, 9.2, 1.2, 0.7)*60), mean = 3, sd = input$frame/50) 
        y <- rnorm(2**(input$frame*dsnorm(input$temperature, 48, 12, 0.5)*dsnorm(input$pH, 9.2, 1.2, 0.7)*60), mean = 1, sd = input$frame/50) 
        z <- as.factor(rep('Clostridium paradoxum', length(x)))
        cparadoxum <- data.frame(x=x, y=y, z=z)
        the.df <- rbind(ecoli, bpseudofirmus, pluti, aligni, taquaticus, cparadoxum)
    }
  })
  output$growth <- renderPlot({
    ggplot(data = the.data(), aes(x=x, y=y, colour = z)) + geom_point(size=4) + theme(legend.position = 'bottom', legend.text = element_text(size = 15, face = 'italic') , axis.title = element_blank(), legend.title = element_blank(), axis.ticks = element_blank(), axis.text = element_blank())
  })
  output$figure <- renderImage({
    list(src = './pic.jpg', width=450)
  }, deleteFile=FALSE)
}
shinyApp(ui = ui, server = server)
