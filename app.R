# Load Libraries ----
library(shiny)
library(shinyBS)
library(shinydashboard)
library(boastUtils)
library(ggplot2)

# Global Constants and functions ----
N <- 10000
prop1 <- 0.1704
lab1Pop <- data.frame(
  color = rep(c("blue", "red"), times = c(prop1 * N, (1 - prop1) * N)),
  x = runif(N, min = -5, max = 5),
  y = runif(N, min = 0, max = 5)
)
lab1Pop <- lab1Pop[sample.int(nrow(lab1Pop)),]

prop2 <- 0.2
lab2Pop <- data.frame(
  color = rep(c("green", "yellow"), times = c(prop2 * N, (1 - prop2) * N)),
  x = runif(N, min = -5, max = 5),
  y = runif(N, min = 0, max = 5)
)
lab2Pop <- lab2Pop[sample.int(nrow(lab2Pop)),]

## Main Plot
mainPlot1 <- ggplot(
  data = lab1Pop,
  mapping = aes(x = x, y = y, color = color, shape = color)
) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "blue" = boastUtils::psuPalette[1],
      "red" = boastUtils::psuPalette[2]
    )
  ) +
  scale_shape_manual(
    values = c("blue" = 15, "red" = 17)
  ) +
  geom_hline(yintercept = -0.5, size = 2, color = "black") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(title = NULL) +
  xlab(NULL) +
  ylab(NULL)

mainPlot2 <- ggplot(
  data = lab2Pop,
  mapping = aes(x = x, y = y, color = color, shape = color)
) +
  geom_point(size = 2) +
  scale_color_manual(
    values = c(
      "green" = boastUtils::boastPalette[3],
      "yellow" = boastUtils::boastPalette[6]
    )
  ) +
  scale_shape_manual(
    values = c("green" = 15, "yellow" = 17)
  ) +
  geom_hline(yintercept = -0.5, size = 2, color = "black") +
  theme_bw() +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(title = NULL) +
  xlab(NULL) +
  ylab(NULL)

# Define the UI ----
ui <- list(
  dashboardPage(
    skin = "purple",
    ## Header ----
    dashboardHeader(
      title = "STAT 200 Sampling",
      titleWidth = 250
    ),
    ## Sidebar ----
    dashboardSidebar(
      width = 250,
      sidebarMenu(
        id = "pages",
        menuItem("Overview", tabName = "overview", icon = icon("tachometer-alt")),
        menuItem("Bead Box for Lab 3.1", tabName = "lab1", icon = icon("flask")),
        menuItem("Bead Box for Lab 3.2", tabName = "lab2", icon = icon("flask")),
        menuItem("References", tabName = "references", icon = icon("leanpub"))
      ),
      tags$div(
        class = "sidebar-logo",
        boastUtils::sidebarFooter()
      )
    ),
    #Dashboard Body
    dashboardBody(
      tabItems(
        ## First tab - Overview ----
        tabItem(
          tabName = "overview",
          h1("Stat 200 Sampling"),
          p("Use this app to simulate the Bead Boxes that are part of
            Labs 3.1 and 3.2."),
        ),
        ### Lab 1 ----
        tabItem(
          tabName = "lab1",
          h2("Bead Box for Lab 3.1"),
          p("The box below represents the population of Centre County,
            Pennsylvania, which contains State College and Penn State. Each bead
            (shape) in the box represents a different person who lives in Centre
            County. The blue-square beads represent persons of color while
            red-triangular beads represent persons who are white."),
          p("The box contains some number of blue-square beads (persons of color)
            and some number of red-triangular beads (white persons) based upon 
            figures from the US Census Bureau. Your goal is to figure out what
            proportion of Centre County's population are persons of color (the
            blue-square beads)."),
          p("When you are ready to take a random sample of 30 beads from the box,
            click the Draw a Sample button. Your random sample will appear below
            the black line in a nice grid."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "drawSample1",
              label = "Draw a Sample",
              icon = icon("utensil-spoon"),
              style = "default",
              size = "large"
            ),
            plotOutput("lab1Plot"),
            uiOutput("sampleFreq1"),
            uiOutput("sampleProp1"),
            uiOutput("sampleSize1")
          ),
          tags$script(HTML(
          "$(document).ready(function() {
          document.getElementById('lab1Plot').setAttribute('aria-label',
          `A box containing a mixture of blue-square and red-triangular beads
          with a black line at the bottom. Pressing the Draw a Sample button will
          move 30 randomly sampled beads below the line for you to examine.`)})"
          ))
        ),
        ### Lab 2 ----
        tabItem(
          tabName = "lab2",
          h2("Bead Box for Lab 3.2"),
          p("The box below represents Penn State students at the Unversity Park
            (UP) campus. Each bead represents a different student. The color and
            shape of the bead indicates each student's COVID-19 vaccination
            status: yellow-triangular beads are vaccinated students while green-
            square beads are unvaccinated students."),
          p("The box contains some number of yellow-triangular beads 
            (vaccinated students) and green-square beads (unvaccinated students)
            based upon university figures. Your goal is to figure out what
            proportion of students at the University Park campus of Penn State
            are vaccinated."),
          p("When you are ready to take a random sample of 30 beads from the box,
            click the Draw a Sample button. Your random sample will appear below
            the black line in a nice grid."),
          div(
            style = "text-align: center;",
            bsButton(
              inputId = "drawSample2",
              label = "Draw a Sample",
              icon = icon("utensil-spoon"),
              style = "default",
              size = "large"
            ),
            plotOutput("lab2Plot"),
            uiOutput("sampleFreq2"),
            uiOutput("sampleProp2"),
            uiOutput("sampleSize2")
          ),
          tags$script(HTML(
            "$(document).ready(function() {
          document.getElementById('lab2Plot').setAttribute('aria-label',
          `A box containing a mixture of green-square and yellow-triangular beads
          with a black line at the bottom. Pressing the Draw a Sample button will
          move 30 randomly sampled beads below the line for you to examine.`)})"
          ))
        ),
        tabItem(
          tabName = "references",
          h2("References"),
          p(     #shinyBS
            class = "hangingindent",
            "Bailey, E. (2015), shinyBS: Twitter bootstrap components for shiny,
             R package. Available from
            https://CRAN.R-project.org/package=shinyBS"
          ),
          p(     #Boast Utilities
            class = "hangingindent",
            "Carey, R. (2019), boastUtils: BOAST Utilities, R Package.
             Available from
            https://github.com/EducationShinyAppTeam/boastUtils"
          ),
          p(     #shinydashboard
            class = "hangingindent",
            "Chang, W. and Borges Ribeio, B. (2018), shinydashboard: Create
            dashboards with 'Shiny', R Package. Available from
            https://CRAN.R-project.org/package=shinydashboard"
          ),
          p(     #shiny
            class = "hangingindent",
            "Chang, W., Cheng, J., Allaire, J., Xie, Y., and McPherson, J.
            (2019), shiny: Web application framework for R, R Package.
            Available from https://CRAN.R-project.org/package=shiny"
          ),
          br(),
          br(),
          br(),
          boastUtils::copyrightInfo()
        )
      )
    )
  )
)

# Define the Server ----
server <- function(input, output, session) {
  ## Plot two main plots ----
  output$lab1Plot <- renderPlot({
    return(mainPlot1)
  })

  output$lab2Plot <- renderPlot({
    return(mainPlot2)
  })

  ## Draw the sample for blue/red ----
  observeEvent(input$drawSample1, {
    sample1 <- lab1Pop[sample.int(nrow(lab1Pop), size = 30), ]
    sample1$x <- rep(c(-5, -3, -1, 1, 3, 5), 5)
    sample1$y <- rep(c(-1, -1.5, -2, -2.5, -3), 6)

    nBlue <- length(which(sample1$color == "blue"))

    output$lab1Plot <- renderPlot({
    mainPlot1 +
      geom_point(data = sample1,
                 mapping = aes(x = x, y = y, color = color),
                 size = 4)
    })

    output$sampleFreq1 <- renderUI({
      paste("There are", nBlue, "blue-square beads in your sample.")
    })

    output$sampleProp1 <- renderUI({
      paste0("Your sample's proportion of blue-square beads is ",
             round(nBlue / 30, 2), ".")
    })

    output$sampleSize1 <- renderUI({
      paste("n = 30")
    })
  })

  ## Draw the samlpe for green/yellow ----
  observeEvent(input$drawSample2, {
    sample2 <- lab2Pop[sample.int(nrow(lab2Pop), size = 30), ]
    sample2$x <- rep(c(-5, -3, -1, 1, 3, 5), 5)
    sample2$y <- rep(c(-1, -1.5, -2, -2.5, -3), 6)

    nGreen <- length(which(sample2$color == "green"))

    output$lab2Plot <- renderPlot({
      mainPlot2 +
        geom_point(data = sample2,
                   mapping = aes(x = x, y = y, color = color),
                   size = 4)
    })

    output$sampleFreq2 <- renderUI({
      paste("There are", nGreen, "green-square beads in your sample.")
    })

    output$sampleProp2 <- renderUI({
      paste0("Your sample's proportion of green-square beads is ",
             round(nGreen / 30, 2), ".")
    })

    output$sampleSize2 <- renderUI({
      paste("n = 30")
    })
  })
}

# Boast App Call ----
boastUtils::boastApp(ui = ui, server = server, config = list("log" = FALSE))