#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$pcaplot <- renderPlotly({
        dat %>%
            highlight_key(~County) %>%
            plot_ly(source = "pca") %>%
            add_text(
                x = ~ PC1,
                y = ~ PC2,
                text = ~ County,
                showarrow = F,
                hoverinfo = "none"
            ) %>%
            layout(plot_bgcolor='rgb(34, 34, 34)') %>%
            layout(paper_bgcolor='rgb(34, 34, 34)') %>%
            highlight(color = "white",
                      opacityDim = 1,
                      on = "plotly_hover",
                      off = "plotly_deselect")
    })

    county_selected <- shiny::reactive({

    })

    output$pigs <- renderPlotly({

        d <- event_data(event = "plotly_click",
                        source = "pca")
        if (is.null(d))
            return(NULL)

        county_selected <- d$key

        ls %>%
            mutate(.color = ifelse(County == county_selected,
                                   "red",
                                   "yellow")
                   ) %>%
            plot_ly() %>%
            add_bars(
                y = ~ forcats::fct_reorder(County, Pigs),
                x = ~Pigs,
                color = ~.color,
                colors = c("red", "yellow")
            ) %>%
            layout(plot_bgcolor='rgb(34, 34, 34)') %>%
            layout(paper_bgcolor='rgb(34, 34, 34)') %>%
            layout(yaxis = list(title = ""),
                   showlegend = FALSE)
    })
    output$farmer <- renderPlotly({

        d <- event_data(event = "plotly_click",
                        source = "pca")
        if (is.null(d))
            return(NULL)

        county_selected <- d$key

        ls %>%
            mutate(.color = ifelse(County == county_selected,
                                   "red",
                                   "yellow")
                   ) %>%
            plot_ly() %>%
            add_bars(
                y = ~ forcats::fct_reorder(County, Farming),
                x = ~Farming,
                color = ~.color,
                colors = c("red", "yellow")
            ) %>%
            layout(plot_bgcolor='rgb(34, 34, 34)') %>%
            layout(paper_bgcolor='rgb(34, 34, 34)') %>%
            layout(yaxis = list(title = ""),
                   showlegend = FALSE)
    })
    output$donkeys <- renderPlotly({

        d <- event_data(event = "plotly_click",
                        source = "pca")
        if (is.null(d))
            return(NULL)

        county_selected <- d$key

        ls %>%
            mutate(.color = ifelse(County == county_selected,
                                   "red",
                                   "yellow")
                   ) %>%
            plot_ly() %>%
            add_bars(
                y = ~ forcats::fct_reorder(County, Donkeys),
                x = ~Donkeys,
                color = ~.color,
                colors = c("red", "yellow")
            ) %>%
            layout(plot_bgcolor='rgb(34, 34, 34)') %>%
            layout(paper_bgcolor='rgb(34, 34, 34)') %>%
            layout(yaxis = list(title = ""),
                   showlegend = FALSE)
    })
    output$camels <- renderPlotly({

        d <- event_data(event = "plotly_click",
                        source = "pca")
        if (is.null(d))
            return(NULL)

        county_selected <- d$key

        ls %>%
            mutate(.color = ifelse(County == county_selected,
                                   "red",
                                   "yellow")
                   ) %>%
            plot_ly() %>%
            add_bars(
                y = ~ forcats::fct_reorder(County, Camels),
                x = ~Camels,
                color = ~.color,
                colors = c("red", "yellow")
            ) %>%
            layout(plot_bgcolor='rgb(34, 34, 34)') %>%
            layout(paper_bgcolor='rgb(34, 34, 34)') %>%
            layout(yaxis = list(title = ""),
                   showlegend = FALSE)
    })

    output$pplot <- renderPlot({

        d <- event_data(event = "plotly_click",
                        source = "pca")
        if (is.null(d))
            return(NULL)

        county_selected <- d$key

        dat %>%
            mutate(selected = County == county_selected) %>%
            select(selected, Farming:Rabbits) %>%
            ggparcoord(
                columns = 2:15,
                groupColumn = 1,
                scale = "center",
                order = "skewness",
                showPoints = TRUE,
                mapping = ggplot2::aes(size = 0.5)
            ) +
            scale_size_identity() +
            scale_color_manual(values = c( "gray20", "green")) +
            dark_theme_light() +
            theme(legend.position = "FALSE",
                  plot.background = element_rect(fill = "#222222"),
                  panel.background = element_rect(fill = "#222222")
                  )

    })

})




# library(GGally)
#
dat %>%
    mutate(selected = County == "MARSABIT") %>%
    select(selected, Farming:Rabbits) %>%
    ggparcoord(
        columns = 2:15,
        groupColumn = 1,
        scale = "globalminmax",
        order = "skewness",
        showPoints = TRUE,
        mapping = ggplot2::aes(size = 1)
        ) +
    scale_size_identity() +
    scale_color_manual(values = c( "gray20", "#fca311")) +
    dark_theme_light() +
    theme(legend.position = "FALSE",
          plot.background = element_rect(fill = "#222222"),
          panel.background = element_rect(fill = "#222222")
    )


dat %>%
    mutate(obs = 1:nrow(.),
           selected = County == "MARSABIT") %>%
    select(obs, selected, Farming:Rabbits) %>%
    highlight_key(~obs) %>%
    plot_ly() %>%
    add_trace(
        type = 'parcoords',
        # line = list(color = ~selected),
        dimensions = list(
            list(label = "Farming", values = ~Farming),
            list(label = "ExoticCattle_Dairy", values = ~ExoticCattle_Dairy),
            list(label = "ExoticCattle_Beef", values = ~ExoticCattle_Beef),
            list(label = "IndigenousCattle", values = ~IndigenousCattle),
            list(label = "Sheep", values = ~Sheep),
            list(label = "Goats", values = ~Goats),
            list(label = "Camels", values = ~Camels),
            list(label = "Donkeys", values = ~Donkeys),
            list(label = "Pigs", values = ~Pigs),
            list(label = "IndigenousChicken", values = ~IndigenousChicken),
            list(label = "ExoticChicken_Layers", values = ~ExoticChicken_Layers),
            list(label = "ExoticChicken_Broilers", values = ~ExoticChicken_Broilers),
            list(label = "Beehives", values = ~Beehives),
            list(label = "Rabbits", values = ~Rabbits)
        )
        ) %>%
    highlight()
