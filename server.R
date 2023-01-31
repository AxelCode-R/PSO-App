shinyServer(function(input, output, session) {

  if (identical(Sys.getenv("R_CONFIG_ACTIVE"), "shinyapps")) {
    chromote::set_default_chromote_object(
      chromote::Chromote$new(chromote::Chrome$new(
        args = c("--disable-gpu",
                 "--no-sandbox",
                 "--disable-dev-shm-usage", # required bc the target easily crashes
                 c("--force-color-profile", "srgb"))
      ))
    )
  }

  pso_2d_server(input, output, session)

  example_sh_server(input, output, session)
})
