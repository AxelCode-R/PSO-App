
shinyUI(fluidPage(
    div(
      h2("App zum visualisieren eines 2D PSOs"),
      hr(),
      br(),
      textAreaInput("fun",
      "Die zu minimierende R-Funktion, die ein scalar zurückgibt (x := axis_1; y := axis_2):",
      value="-20 * exp(-0.2 * sqrt(0.5 *((x-1)^2 + (y-1)^2))) -
          exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) +
          exp(1) + 20", width=600, height=100),
      numericRangeInput("range", "Definitionsbereich von x und y:", value = c(-10, 10), step = 0.1, separator = "bis"),
      numericInput("resolution", "Grid Resolution:", value=0.1),
      actionButton("render", "Render Plot"),
      hr(),
      plotlyOutput("plot_3D", width="900px", height="900px")
    )
))
