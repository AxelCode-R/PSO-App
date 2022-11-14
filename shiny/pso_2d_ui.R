pso_2d_ui <- function(){
  div(
    div(
      textAreaInput(
        "pso_2d_fun",
        "R Function to Minimize (with x1 and x2 variables):",
         value="-20 * exp(-0.2 * sqrt(0.5 *((x1-1)^2 + (x2-1)^2))) -
              exp(0.5*(cos(2*pi*x1) + cos(2*pi*x2))) +
              exp(1) + 20",
        width=600,
        height=100
      ),
      numericRangeInput("pso_2d_range_x1", "Domain of x1:", value = c(-10, 10), step = 0.1, separator = "-"),
      numericRangeInput("pso_2d_range_x2", "Domain of x2:", value = c(-10, 10), step = 0.1, separator = "-"),
      numericInput("pso_2d_resolution", "Grid Resolution:", value = 200, max = 2000, min = 100),
      numericInput("pso_2d_iter", "Iterations:", value=20, step = 1),
      actionButton("pso_2d_save_settings", "Save Settings"),
      disabled(actionButton("pso_2d_grid_preview", "Grid Preview", style="margin-left:10px;"))
    ),
    div(
      actionButton("pso_2d_start_pso", "Start PSO")
    )
  )
}
