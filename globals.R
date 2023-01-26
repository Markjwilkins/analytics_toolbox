pacman::p_load(tidyverse, StatsBombR, glue, here, ggsoccer)


# Pull SB data and save to .RDS file --------------------------------------

pull_sb_data <- function(competition, season) {
  ##load competitions
  comp <-
    FreeCompetitions() |> filter(competition_id == competition, season_name == season)
  
  ##league and season name
  league <-
    unique(comp$competition_name) |> str_to_lower() |> str_replace(" ", "_")
  season <- unique(comp$season_name)
  
  ##pull matches
  matches <- FreeMatches(comp)
  
  ##pull data and clean locations
  data <-
    StatsBombR::free_allevents(matches) |> allclean() |> get.opposingteam()
  
  ##save match data to RDS file
  saveRDS(data, here::here("data", glue::glue("{league}_{season}.RDS")))
}

# pull_sb_data(competition = 55, season = 2020)


# Full SB pitch -----------------------------------------------------------

full_sb <- function(df) {
  df |>
    ggplot() +
    ##full SB pitch
    ggsoccer::annotate_pitch(dimensions = pitch_statsbomb) +
    ##reverse y axis
    scale_y_reverse() +
    theme_pitch()
}


# Half SB pitch -----------------------------------------------------------

half_sb <- function(df) {
  df |>
    ggplot() +
    ##full SB pitch
    ggsoccer::annotate_pitch(dimensions = pitch_statsbomb) +
    coord_flip(xlim = c(50, 120),
               ylim = c(-15, 105)) +
    theme_pitch(aspect_ratio = 60 / 120)
}


# Progressive pass --------------------------------------------------------

progressive_pass <- function(df) {
  df |>
    mutate(
      start = sqrt((120 - location.x) ^ 2 + (40 - location.y) ^ 2),
      end = sqrt((120 - pass.end_location.x) ^ 2 + (40 - pass.end_location.y) ^
                   2),
      is_prog = ifelse(end <= 0.75 * start,
                       1,
                       0)
    )
}

# Add logo ----------------------------------------------------------------

##from: https://themockup.blog/posts/2019-01-09-add-a-logo-to-your-plot/
add_logo <-
  function(plot_path,
           logo_path,
           logo_position,
           logo_scale = 10) {
    # Requires magick R Package https://github.com/ropensci/magick
    
    # Useful error message for logo position
    if (!logo_position %in% c("top right", "top left", "bottom right", "bottom left")) {
      stop(
        "Error Message: Uh oh! Logo Position not recognized\n  Try: logo_positon = 'top left', 'top right', 'bottom left', or 'bottom right'"
      )
    }
    
    # read in raw images
    plot <- magick::image_read(plot_path)
    logo_raw <- magick::image_read(logo_path)
    
    # get dimensions of plot for scaling
    plot_height <- magick::image_info(plot)$height
    plot_width <- magick::image_info(plot)$width
    
    # default scale to 1/10th width of plot
    # Can change with logo_scale
    logo <-
      magick::image_scale(logo_raw, as.character(plot_width / logo_scale))
    
    # Get width of logo
    logo_width <- magick::image_info(logo)$width
    logo_height <- magick::image_info(logo)$height
    
    # Set position of logo
    # Position starts at 0,0 at top left
    # Using 0.01 for 1% - aesthetic padding
    
    if (logo_position == "top right") {
      x_pos = plot_width - logo_width - 0.01 * plot_width
      y_pos = 0.01 * plot_height
    } else if (logo_position == "top left") {
      x_pos = 0.01 * plot_width
      y_pos = 0.01 * plot_height
    } else if (logo_position == "bottom right") {
      x_pos = plot_width - logo_width - 0.01 * plot_width
      y_pos = plot_height - logo_height - 0.01 * plot_height
    } else if (logo_position == "bottom left") {
      x_pos = 0.01 * plot_width
      y_pos = plot_height - logo_height - 0.01 * plot_height
    }
    
    # Compose the actual overlay
    magick::image_composite(plot, logo, offset = paste0("+", x_pos, "+", y_pos))
    
  }


# Pull team logos from fotmob ---------------------------------------------

##worldfootballR::fotmob_get_league_ids()
pull_logos <- function(league_id) {
  ##base url
  url <- "https://www.fotmob.com/api/leagues?id="
  
  ##url with league code
  complete_url <- glue::glue("{url}{league_id}")
  
  ##pull league JSON
  league_json <- jsonlite::fromJSON(complete_url)
  
  ##league and season for file save
  league_name <-
    league_json$seostr |> str_to_lower() |> str_replace("-", "_")
  league_season <-
    league_json$overview$season |> str_replace("/", "_")
  
  team_info <-
    league_json$matches$data$allMatches$home |>
    distinct() |>
    select(-2) |>
    mutate(league = league_name)
  
  ##function to construct logo image path
  logo_image <- function(team_id, width = 20) {
    glue::glue("https://images.fotmob.com/image_resources/logo/teamlogo/{team_id}.png")
  }
  
  ##ombine data
  data <-
    team_info |>
    mutate(image_link = logo_image(team_id = unique(team_info$id))) |>
    rename(fotmob_team_name = "name")
  
  ##save to .csv for future reference
  write_csv(data, here::here(
    "output/team_logos",
    glue::glue("{league_name}_{league_season}_logos.csv")
  ))
  
}


# International pitch dimensions for Fotmob -------------------------------
##from: https://github.com/RobWHickman/Rteta/blob/master/R/pitch_plot.R

plot_international_pitch <- function(unit = "meters", theme = "light", type = "full_pitch", orientation = "horizontal", direction_arrow = FALSE){
  
  if(theme == "light"){
    pitch_color <- "white"
    line_color <- "grey10"
  } else if(theme == "dark"){
    pitch_color <- "grey20"
    line_color <- "grey95"
  }
  
  length <- 105
  width <- 68
  
  mid_point_x = length / 2
  mid_point_y = width / 2
  
  goal_width <- 7.32
  
  center_circle_radius <- 9.15
  penalty_distance <- 10.97
  penalty_box_distance <- 16.5
  six_yard_box_distance <- 5.5
  
  yards_per_meter <- 1.09361
  
  if(unit == "yards"){
    length <- length / yards_per_meter
    width <- width / yards_per_meter
    
    mid_point_x = length / 2
    mid_point_y = width / 2
    
    goal_width <- goal_width / yards_per_meter
    
    center_circle_radius <- center_circle_radius / yards_per_meter
    penalty_distance <- penalty_distance / yards_per_meter
    penalty_box_distance <- penalty_box_distance / yards_per_meter
    six_yard_box_distance <- six_yard_box_distance / yards_per_meter
  }
  
  circle_function <- function(center, radius, npoints = 100){
    tt <- seq(0, 2*pi, length.out = npoints)
    xx <- center[1] + radius * cos(tt)
    yy <- center[2] + radius * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  center_circle_data <- circle_function(center = c(mid_point_x, mid_point_y), radius = center_circle_radius, npoints = 500)
  
  penalty_arc_left_data <- circle_function(center = c(penalty_distance, mid_point_y), radius = center_circle_radius, npoints = 500) %>%
    filter(x >= penalty_box_distance)
  
  penalty_arc_right_data <- circle_function(center = c(length - penalty_distance, mid_point_y), radius = center_circle_radius, npoints = 500) %>%
    filter(x <= length - penalty_box_distance)
  
  if(type == "full_pitch"){
    
    aspect_ratio <- width/length
    
    p <- ggplot() +
      geom_rect(aes(xmin = 0, xmax = length, ymin = 0, ymax = width), size = 0.5, colour = line_color, fill = pitch_color) + # outside border
      geom_rect(aes(xmin = 0, 
                    xmax = penalty_box_distance, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # left penalty box
      geom_rect(aes(xmin = length - penalty_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # right penalty box
      geom_rect(aes(xmin = 0, 
                    xmax = six_yard_box_distance, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # left 6 yard box
      geom_rect(aes(xmin = length - six_yard_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # right 6 yard  box
      geom_segment(aes(x = mid_point_x, xend = mid_point_x, y = 0, yend = width), size = 0.5, colour = line_color) + # center pitch
      geom_segment(aes(x = length, xend = length, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_segment(aes(x = 0, xend = 0, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_point(aes(x = mid_point_x, y = mid_point_y), size = 0.5, colour = line_color) + # center spot
      geom_point(aes(x = penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # left penalty spot
      geom_point(aes(x = length - penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # right penalty spot
      geom_path(data = center_circle_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_left_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_right_data, aes(x,y), size = 0.5, colour = line_color) +
      theme_void() +
      theme(plot.background = element_rect(fill = pitch_color),
            aspect.ratio = width/length)
    
  } else if(type == "offensive_half"){
    
    center_circle_data <- center_circle_data %>%
      filter(x >= length / 2)
    
    aspect_ratio <- width/(length/2)
    
    p <- ggplot() +
      geom_rect(aes(xmin = length / 2, xmax = length, ymin = 0, ymax = width), size = 0.5, colour = line_color, fill = pitch_color) + # outside border
      geom_rect(aes(xmin = length - penalty_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # offensive penalty box
      geom_rect(aes(xmin = length - six_yard_box_distance, 
                    xmax = length, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # offensive 6 yard  box
      geom_segment(aes(x = length, xend = length, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_point(aes(x = length - penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # offensive penalty spot
      geom_path(data = center_circle_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_right_data, aes(x,y), size = 0.5, colour = line_color) +
      theme_void() +
      theme(plot.background = element_rect(fill = pitch_color),
            aspect.ratio = aspect_ratio)
  } else if(type == "defensive_half"){
    
    center_circle_data <- center_circle_data %>%
      filter(x <= length / 2)
    
    aspect_ratio <- width/(length/2)
    
    p <- ggplot() +
      geom_rect(aes(xmin = 0, xmax = length / 2, ymin = 0, ymax = width), size = 0.5, colour = line_color, fill = pitch_color) + # outside border
      geom_rect(aes(xmin = 0, 
                    xmax = penalty_box_distance, 
                    ymin = mid_point_y - goal_width/2 - penalty_box_distance, 
                    ymax = mid_point_y + goal_width/2 + penalty_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # defensive penalty box
      geom_rect(aes(xmin = 0, 
                    xmax = six_yard_box_distance, 
                    ymin = mid_point_y - goal_width/2 - six_yard_box_distance, 
                    ymax = mid_point_y + goal_width/2 + six_yard_box_distance), 
                size = 0.5, 
                alpha = 0.0, 
                colour = line_color) + # defensive 6 yard box
      geom_segment(aes(x = 0, xend = 0, y = mid_point_y - goal_width/2, yend = mid_point_y + goal_width/2), size = 2, colour = line_color) + # goal line
      geom_point(aes(x = penalty_distance, y = mid_point_y), size = 0.5, colour = line_color) + # defensive penalty spot
      geom_path(data = center_circle_data, aes(x,y), size = 0.5, colour = line_color) +
      geom_path(data = penalty_arc_left_data, aes(x,y), size = 0.5, colour = line_color) +
      theme_void() +
      theme(plot.background = element_rect(fill = pitch_color),
            aspect.ratio = aspect_ratio)
  } 
  
  if(orientation == "vertical"){
    
    p <- p +
      scale_y_reverse() +
      theme(aspect.ratio = 1/aspect_ratio) +
      coord_flip()
  }
  
  if(direction_arrow){
    p <- p +
      theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
      geom_segment(aes(x = 40, xend = 65, y = -5, yend = -5), size = 0.5, colour = line_color, arrow = arrow(length = unit(0.03, "npc")))
  }  
  
  p
}

