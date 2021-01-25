# Define UI for application that draws a histogram
shinyUI(fluidPage(

    theme = shinytheme("darkly"),

    # Application title
    titlePanel("Kenya Occupation"),

    fluidPage(

        sidebarLayout(
            sidebarPanel = sidebarPanel(

                shiny::checkboxInput(inputId = "loadings",
                                     label = "Show Loadings",
                                     value = FALSE
                                     ),

                width = 2

            ),

            mainPanel = shiny::mainPanel(
                plotlyOutput("pcaplot")
                )

        ),

        shiny::fluidRow(

            plotOutput(outputId = "pplot")

        )

        # shiny::fluidRow(
        #     column(
        #         width = 3,
        #         plotlyOutput("farmer", height = "500px")
        #     ),
        #     column(
        #         width = 3,
        #         plotlyOutput("pigs", height = "500px")
        #     ),
        #     column(
        #         width = 3,
        #         plotlyOutput("donkeys", height = "500px")
        #     ),
        #     column(
        #         width = 3,
        #         plotlyOutput("camels", height = "500px")
        #     )
        # )
    )

))
