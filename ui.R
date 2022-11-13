shinyUI(dashboardPage(
  title = "PSO App",
  dashboardHeader(
    title = "PSO App"
  ),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home", verify_fa = FALSE)),
      menuItem("PSO 2D", tabName = "pso_2d", icon = icon("gear", verify_fa = FALSE))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "pso_2d",
        home_ui()
      ),
      tabItem(tabName = "pso_2d",
        pso_2d_ui()
      )
    )
  )

))
