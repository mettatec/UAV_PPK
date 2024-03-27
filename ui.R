source("global.R")
source("modules/homePoint-module.R")

# Define UI for application that draws a histogram
shinyUI(dashboardPage(
    skin = "black",
    dashboardHeader(title = "Mettatec Apps | UAV PPK", titleWidth = 280,
                    tags$li(class="dropdown",div(class= "btn-auth",
                                                 shinyauthr::logoutUI(id = "logout", label = "Exit")),
                            
                    )
                    ),
    dashboardSidebar(
        disable = TRUE
    ),
    dashboardBody(
        tags$head(
            tags$link(rel="shortcut icon", href="favicon.ico"),
            tags$script(src = "shinyjsComplements.js"),
            tags$footer("© 2024 Mettatec - all rights reserved", # strong() = bold
                        align = "center", 
                        style = "
                 position:fixed;
                 bottom:0px;
                 width:100%;
                 height:40px; 
                 color: black;
                 font-size:15px;
                 padding: 0px;
                 background-color: #e3e3e3;
                 z-index: 100;
                ")
        ),
        # add login panel UI function
        shinyauthr::loginUI(id = "login", title= div(img(src="mettatec_logo.png", align="center", style="width:60%; height:60%;"),
                                                     h2("Log In"))),
        ##Conditional panels for the loading page
        conditionalPanel(
          condition = "output.setupComplete",
          uiOutput("panelPrincipal")),
        conditionalPanel(
          condition = "!output.setupComplete",
          div(img(src="mettatec.gif", style = 'position: absolute; top: 50%; left: 50%; transform: translate(-50%, -50%);'))
        )
    
    )
))
