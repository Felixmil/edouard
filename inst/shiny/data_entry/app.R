# Load UI and Server
source("ui.R")
source("server.R")

# Run the app
shinyApp(ui = ui, server = server)
