pacman::p_load(shiny, tidyverse, worldfootballR, gradienttext, ggiraph)

data <- read_csv(here::here("data", "league_one_match_xg.csv"))

cols <- c("#e76254","#1e466e")

sub <- "Each dot reprents a match in the 2022/2023 season. Those points above <br>the reference line indicate the team conceded more xG than created. <br>Those points below the line show an xG difference in the team's favour"


ui <- shinyUI(fluidPage(
  ggiraphOutput("plot1",
                height = "40%",
                width = "100%")
))


server <- shinyServer(function(input, output) {
  
  output$plot1 <- renderggiraph({
    plot <- 
      ggplot(data,
             aes(
               x = xg_for,
               y = xg_against,
               fill = (xg_for - xg_against)
             )) +
      ggiraph::geom_point_interactive(
        aes(tooltip = tooltip_lab, 
            data_id = match_id,
            onclick = onclick),
        shape = 21,
        size = 2.6,
        stroke = 0.8
      ) +
      geom_abline(intercept = 0,
                  slope = 1,
                  linewidth = 0.1) +
      labs(
        title = gradienttext::make_gradient("League One xG By Game", cols),
        subtitle = sub,
        x = "xG For",
        y = "xG Against",
        caption = "**Viz**: @AnalyticsOxford<br>**Data**: Fotmob.com"
      ) +
      facet_wrap(. ~ team, ncol = 6, nrow = 4) + # split to one facet per team
      theme_minimal() +
      theme(
        legend.position = "none",
        strip.text = element_text(size = 10, face = "bold"),
        plot.title = ggtext::element_markdown(size = 40, face = "bold"),
        plot.subtitle = ggtext::element_markdown(size = 14, face = "bold"),
        plot.title.position = "plot",
        axis.title = element_text(face = "bold"),
        plot.caption = ggtext::element_markdown()
      ) +
      scale_fill_gradient2(low = "#e76254", high = "#1e466e")
    
    tooltip_css <- "background-color:#282828;color:#f2f2f2;padding:10px;border-radius:5px;font-family:IBMPlexSans"
    
    ggiraph::girafe(ggobj = plot,
                    height_svg = 8,
                    width_svg = 10,
                    options = list(
                      opts_hover_inv(css = "opacity:0.1;"),
                      opts_hover(css = "fill:red;"),
                      opts_tooltip(css = tooltip_css)
                    ))
  })
  
})

shinyApp(ui = ui, server = server)


shinyApp(ui = ui, server = server)