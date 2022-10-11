
shinyUI(fluidPage(
    div(
      h2("App zum visualisieren eines 2D PSOs"),
      hr(),
      br(),
      textAreaInput("fun", "Die zu minimierende Funktion (x := pos[1]; y := pos[2]):", value="-20 * exp(-0.2 * sqrt(0.5 *((pos[1]-1)^2 + (pos[2]-1)^2))) -
          exp(0.5*(cos(2*pi*pos[1]) + cos(2*pi*pos[2]))) +
          exp(1) + 20"),
      actionButton("render", "Render Plot"),
      hr(),
      addSpinner(plotlyOutput("plot_3D", width="900px", height="900px"))
    )
))
