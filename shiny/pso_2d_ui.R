pso_2d_ui <- function(){
  div(
    div(style="display:box",
      p("Sources for Test Problems:"),
      tags$a(href="https://www.sfu.ca/~ssurjano/optimization.html", "Optimization Test Problems"),
      br(),
      br(),
      tags$a(href="https://en.wikipedia.org/wiki/Test_functions_for_optimization", "Constrained Optimization Test Problems"),
    ),
    div(style="display:box",
      actionButton("pso_2d_fun_drop_wave", "DROP-WAVE"),
      actionButton("pso_2d_fun_ackley", "ACKLEY"),
      actionButton("pso_2d_fun_rosenbrock", "Rosenbrock constraint cubic and line"),
      actionButton("pso_2d_fun_gole", "Gomez and Levy constraint"),
      actionButton("pso_2d_fun_no_convergence", "No Convergence")
    ),
    div(
      textAreaInput(
        "pso_2d_fun_fit",
        "R Function to Minimize (with x1 and x2 variables):",
         value="-20*exp(-0.2*sqrt(0.5*((x1-1)^2+(x2-1)^2)))-
                  exp(0.5*(cos(2*pi*x1)+cos(2*pi*x2)))+
                  exp(1)+20",
        width=600,
        height=100
      ),
      textAreaInput(
        "pso_2d_fun_const",
        "Penalty Constraint R Function (with x1 and x2 variables):",
        value="0",
        width=600,
        height=100
      ),
      numericRangeInput("pso_2d_range_x1", "Domain of x1:", value = c(-10, 10), step = 0.1, separator = "-"),
      numericRangeInput("pso_2d_range_x2", "Domain of x2:", value = c(-10, 10), step = 0.1, separator = "-"),
      numericInput("pso_2d_resolution", "Grid Resolution:", value = 200, max = 2000, min = 100),
      #div(style="width:300px", progressBar(id = "pso_2d_settings", value = 0, total = 100, unit_mark="%", range_value=NULL)),
      div(style="width:300px",fluidRow(column(12, shinyWidgets::progressBar(id = "pso_2d_settings1", value = 0, display_pct = T)))),
      actionButton("pso_2d_save_settings", "Save Settings"),
      disabled(actionButton("pso_2d_grid_preview", "Grid Preview", style="margin-left:10px;"))
    ),
    hr(),
    div(
      awesomeRadio("pso_2d_variant", "Variants", choices = c("global best (standard)", "local", "self-adaptive velocity"), selected = "global best (standard)", inline = T),
      numericInput("pso_2d_iter", "Iterations:", value=50, step = 1, min = 2, max = 1000),
      numericInput("pso_2d_s", "Swarm Size:", value=5, step = 1, min = 2, max = 100),
      #numericRangeInput("pso_2d_inertia_weight", "Inertia Weight from :", value = c(0, 1.2), step = 0.05, separator = " to "),
      div( style="display: flex;",
        numericInput("pso_2d_inertia_weight_w0", "Inertia Weight from :", value=1, step = 0.05, width = 150),
        br(),
        numericInput("pso_2d_inertia_weight_wN", "  to :", value=0.2, step = 0.05, width=150)
      ),
      numericInput("pso_2d_coef_p", "C_p:", value=0.5, step = 0.1),
      numericInput("pso_2d_coef_g", "C_g:", value=0.5, step = 0.1),
      hidden(numericInput("pso_2d_k", "k:", value=2, step = 1, min = 2)),
      div(style="width:300px",fluidRow(column(12, shinyWidgets::progressBar(id = "pso_2d_settings2", value = 0, display_pct = T)))),
      actionButton("pso_2d_start_pso", "Start PSO"),
      br(),
      plotlyOutput("pso_2d_pso_plot_line", width=800, height=500),
    ),
    hr(),
    div(
      div(style="width:300px",fluidRow(column(12, shinyWidgets::progressBar(id = "pso_2d_settings3", value = 0, display_pct = T)))),
      actionButton("pso_2d_render_anim", "Render 2D-PSO Animation"),
      plotlyOutput("pso_2d_pso_plot", width=800, height=800)
    ),
    hr(),
    div(
      div(style="width:300px",fluidRow(column(12, shinyWidgets::progressBar(id = "pso_2d_settings4", value = 0, display_pct = T)))),
      actionButton("pso_2d_render_anim_details", "Render 2D-PSO Animation with Details"),
      plotlyOutput("pso_2d_pso_plot_details", width=800, height=800)
    )
  )
}
