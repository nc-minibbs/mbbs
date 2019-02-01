#------------------------------------------------------------------------------#
# Plotting and HTML functions used in the MBBS site
#------------------------------------------------------------------------------#


plot_trend <- function(dt, dt_grouped){
  ggplot(
    data = dt,
    aes(x = year, y = count)
  ) + 
    geom_line(
      data = dt,
      aes(group = route),
      # aes(group = route, tooltip = route, data_id = route), 
      color = "grey80",
      size  = 0.25
    ) +
    geom_line(
      data = dt_grouped,
    ) + 
    geom_point(
      data = dt_grouped,
      # aes(tooltip = round(count, 2)),
      size = 1
    ) + 
    scale_y_continuous(
      "Count",
      expand = c(0, 0),
      limits = c(0, max(dt[["count"]]) + 1)
    ) +
    scale_x_continuous(
      "Year",
      expand = c(.01, 0),
      breaks = seq(2000, 2018, by = 2)
    ) +
    stat_smooth(method = "lm", se = FALSE) +
    theme_minimal() +
    theme(
      panel.grid.major = element_line(color = "grey95"),
      panel.grid.minor = element_blank(),
      axis.title.x     = element_blank(),
      axis.title.y     = element_text(angle = 0, vjust = 0.5),
      axis.text.x      = element_text(hjust = 1, angle = 45, vjust = 1)
    )
}

# interactive_plot <- function(plot){
#   x <- girafe( code = print(plot), width_svg = 3, height_svg = 2)
#   girafe_options(x, opts_hover(css = "stroke:orange;stroke-width:2px;"))
# }

# plot_to_string <- function(plot){
#  htmltools::as.tags(plot)
# }

convert_img <- function(plot, width, height){
  tmpFile <- tempfile(fileext = ".png")
  ggsave(filename = tmpFile, plot = plot, width = width, height = height, dpi = 72)
  knitr::image_uri(tmpFile)
  
}

plot_sparkline <- function(x){
  as.character(htmltools::as.tags(
    sparkline(
      x,
      fillColor = FALSE,
      normalRangeMin = -diff(range(x)) * 0.09,
      normalRangeMax =  diff(range(x)) * 0.09,
      spotColor      = FALSE,
      minSpotColor   = FALSE,
      maxSpotColor   = FALSE
    )))
}

create_details <- function(common_name, sci_name, img_trend){
  
  more_link <- sprintf("https://www.allaboutbirds.org/guide/%s", 
                       stringr::str_replace_all(common_name, " ", "_"))
  
  htmltools::div(
    htmltools::p(
      htmltools::a(href = more_link, sci_name)
    ),
    htmltools::img(src = "noun_Bird_2164644.png"),
    img_trend
  )
}