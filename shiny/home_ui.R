home_ui <- function(){
  div(style="width:100%",
    h4("Particle Swarm Optimization (PSO)"),
    p("The PSO was developed by J. Kennedy as a global optimization method based on swarm intelligence
      and presented to the public in 1995 by Eberhart and Kennedy. The original PSO was intended
      to resemble a flock of birds flying through the sky without collisions."),
    div(style="width:100%; text-align:center; margin:5px;", img(src='birds.jpg', align = "center", width="50%")),
    p("The PSO was adapted in Evolutionary Computation to exploit a set of potential
      solutions in high dimensions and to find the optima by cooperating with other
      particles in the swarm. Since it does not require gradient information,
      it is easier to apply than other global optimization methods. It can find the optimum by
      considering only the result of the function to be optimized. This means that the function
      can be arbitrarily complex and it is still possible to reach the global optimum.
      Other advantages are the low computational costs, since only basic mathematical
      operators are used, the extensibility and the simplicity."),
    br(),
    br(),
    p("This app allows the user to test different variants of the PSO with their own 2D test functions and constraints.
      The results are animated in a 2D representation and the optimization parameters can be adjusted."),
    br(),
    p("I hope you have fun playing around with the PSO and learn more about its capabilities and behavior in the process!"),
    br(),
    br(),
    p("If you want to know more about the PSO, you can have a look at my Master-Thesis at:"),
    a("GitHub", href="https://github.com/AxelCode-R/Asset-Allocation-using-Particle-Swarm-Optimization-in-R"),
    br(),
    a("Master-Thesis", href="https://axelcode-r.github.io/Asset-Allocation-using-Particle-Swarm-Optimization-in-R/"),
    br(),
    a("Master-Thesis (PDF)", href="https://axelcode-r.github.io/Asset-Allocation-using-Particle-Swarm-Optimization-in-R/Master_Thesis.pdf")
  )
}
