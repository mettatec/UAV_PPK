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
            tags$script(src = "shinyjsComplements.js")
        ),
        # add login panel UI function
        shinyauthr::loginUI(id = "login", title= div(img(src="mettatec_logo.png", align="center", style="width:60%; height:60%;"),
                                                     h2("Log In"))),
        
        uiOutput("panelPrincipal"),
        
        HTML('<footer style="background-color:#e3e3e3; height:50px; width:100%;margin-top:100px;">
                <p>&copy; 2024 Mettatec - all rights reserved</p>
              </footer>'
             )
    
    )
))
