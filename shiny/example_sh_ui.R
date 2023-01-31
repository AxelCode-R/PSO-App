example_sh_ui <- function(){

    div(
      h4("Example of how to the standard PSO in solving Portfolio Optimization problems"),
      pre("The goal is to optimize the sharp-ratio of a given portfolio, using the standard PSO.
          To increase the complexity, the problem is discrete with integer amounts of assets.
          Therefore the User has to define his budget.
          To generalize the use of this App, the User can input his own return data for the expected returns and the variances of each asset."),
      numericInput("exp_nav", "Budget", 5000, min = 100, max = 10^6, step = 100),
      rHandsontableOutput("exp_rhand_returns")

    )

}
