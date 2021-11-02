library(tidyverse)
library(ggforce)
library(cowplot)

fun.draw_rink <- function() {
  
  xseq <- seq(-4, 4, length = 100)
  theta1 <- seq(0, 2 * pi, length = 300)
  theta <- seq(0, 2 * pi, length = 300)
  dd <- (5 + 7 / 12) / 2
  
  ## Blank NHL Rink
  rink <- ggplot(data = data.frame(x = 1, y = 1), aes(x, y)) + 
    
    geom_path(data = data.frame(x = c(15, 
                                      87 + 13 * sin(seq(0, pi / 2, length = 20)), 
                                      87 + 13 * sin(seq(pi / 2, 0, length = 20)), 
                                      15),
                                y = c(-42.5, 
                                      -42.5 + 15 - 15 * cos(seq(0, pi / 2, length = 20)), 
                                      42.5 - 15 + 15 * cos(seq(pi / 2, 0, length = 20)), 
                                      42.5))
    ) + 
    
    ## Goal Lines
    geom_path(data = data.frame(x = c(89),
                                y = c(42.5 - 15 + sqrt(15^2 - (15 - 11)^2), 
                                      -(42.5 - 15 + sqrt(15^2 - (15 - 11)^2)))), 
              color = 'red') + 
    
    
    ## Nets
    geom_path(data = data.frame(x = c(90, 92, 92, 90)), y = c(-3, -3, 3, 3)) + 
    
    ## Restricted Area
    geom_segment(aes(x = 89, y = -11, xend = 100, yend = -14), color = 'red') + 
    geom_segment(aes(x = 89, y = 11, xend = 100, yend = 14), color = 'red') + 
    
    ## Blue Lines
    geom_segment(aes(x = 25, y = -42.5, xend = 25,  yend = 42.5), color = 'blue', size = 1) + 
    
    ## Crease
    geom_polygon(data = data.frame(x = 1 * c(89, 83+xseq^2 / 4^2 * 1.5, 89),
                                   y = c(-4, xseq, 4)), 
                 
                 color = 'red', 
                 fill = 'deepskyblue2') + 
    
    
    ## Faceoff Dots+ 
    geom_polygon(data = data.frame(y = 22 + 1 * cos(theta), x = 69 + 1 * sin(theta)), color = 'red', fill = 'red') + 
    geom_polygon(data = data.frame(y = -22 + 1 * cos(theta), x = 69 + 1 * sin(theta)), color = 'red', fill = 'red') +
    
    ## Faceoff Circles
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 - 2, yend = 22 + 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 + 2, yend = 22 + 0.75, xend = 69 + 6), color= 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, yend = 22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 - 2, yend = -22 + 0.75, xend = 69 - 6), color= 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 + 2, yend = -22 + 0.75, xend = 69 + 6), color= 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 - 2, yend = -22 - 0.75, xend = 69 - 6), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 + 2, yend = -22 - 0.75, xend = 69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 + 2, yend = 22 - 0.75, xend = 69 + 6), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = 69 - dd, yend = 22 - 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = 22 - 15, x = 69 + dd, yend = 22 - 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = 69 + dd, yend = 22+17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 15, x = 69 - dd, yend = 22 + 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = 69 - dd, yend = -22 + 17, xend = 69 - dd), color = 'red') + 
    geom_segment(aes(y = -22 + 15, x = 69 + dd, yend = -22 + 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = -22 - 15, x = 69 - dd, yend = -22 - 17, xend = 69 - dd), color= 'red') + 
    geom_segment(aes(y = -22 - 15, x = 69 + dd, yend = -22 - 17, xend = 69 + dd), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 + 2, yend = 22 + 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 + 0.75, x = 69 - 2, yend = 22 + 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 + 2, yend = 22 - 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = 22 - 0.75, x = 69 - 2, yend = 22 - 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 + 2, yend = -22 + 3.75, xend = 69 + 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 - 2, yend = -22 - 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 + 0.75, x = 69 - 2, yend = -22 + 3.75, xend = 69 - 2), color = 'red') + 
    geom_segment(aes(y = -22 - 0.75, x = 69 + 2, yend = -22 - 3.75, xend = 69 + 2), color = 'red') + 
    geom_path(data = data.frame(y = 22 + 15 * cos(theta), x = 69 + 15 * sin(theta)), color = 'red') + 
    geom_path(data = data.frame(y = -22 + 15 * cos(theta), x = 69 + 15 * sin(theta)), color = 'red') + 
    
    theme_void()
  
}
halfrink <- fun.draw_rink() + coord_fixed(ratio = 1)
halfrink

#FFCCD8 lighter red
#CCE1FF lighter blue
#CCF5FF lighter crease blue


# Setting up colour values
NHL_red <- "#C8102E" # Use #C8102E for original red in the rules, #FFE6EB for lighter hue
NHL_blue <- "#0033A0" # Use #0033A0 for original blue in the rules, #E6EFFF for lighter hue
NHL_light_blue <- "#41B6E6" # Use #41B6E6 for original crease blue in the rules, #E6F9FF for lighter hue

nhl_rink_plot <- function () {
  
  # Plotting an NHL rink completely following the NHL rule book:
  # https://cms.nhl.bamgrid.com/images/assets/binary/308893668/binary-file/file.pdf
  # Line widths, lengths, colours, all followed as closely as possible
  
  ggplot() +
    
    # Faceoff circles
    geom_circle(aes(x0 = 0, y0 = 0, r = 15), colour = NHL_blue, size = 2 / 12) + # Centre
    geom_circle(aes(x0 = 69, y0 = 22, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Right
    geom_circle(aes(x0 = 69, y0 = -22, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Right
    geom_circle(aes(x0 = -69, y0 = 22, r = 15), colour = NHL_red, size = 2 / 12) + # Top-Left
    geom_circle(aes(x0 = -69, y0 = -22, r = 15), colour = NHL_red, size = 2 / 12) + # Bottom-Left
    
    # Hash marks in T-R/B-R/T-L/B-R order, groups of four
    geom_tile(aes(x = 66.125, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 66.125, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = 71.875, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = 37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = 6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -66.125, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = -37.77, width = 2 / 12, height = 2), fill = NHL_red) +
    geom_tile(aes(x = -71.875, y = -6.23, width = 2 / 12, height = 2), fill = NHL_red) +
    
    # Centre line
    geom_tile(aes(x = 0, y = 0, width = 1, height = 85), fill = NHL_red) + # Centre line
    
    # Faceoff dots - Plot AFTER centre lines for centre ice circle to show up above
    geom_circle(aes(x0 = 0, y0 = 0, r = 6 / 12), colour = "#FF99B4", fill = "#FF99B4", size = 0) + # Centre dot with unique red
    geom_circle(aes(x0 = 69, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Right
    geom_circle(aes(x0 = 69, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Right
    geom_circle(aes(x0 = -69, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Top-Left
    geom_circle(aes(x0 = -69, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Bottom-Left
    
    geom_circle(aes(x0 = 20.5, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Right
    geom_circle(aes(x0 = 20.5, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Right
    geom_circle(aes(x0 = -20.5, y0 = 22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Top-Left
    geom_circle(aes(x0 = -20.5, y0 = -22, r = 1), colour = NHL_red, fill = NHL_red, size = 0) + # Neutral Bottom-Left
    
    # Ells surrounding faceoff dots
    geom_tile(aes(x = 65, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Right
    geom_tile(aes(x = 73, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 65, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 73, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = 65, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Right
    geom_tile(aes(x = 73, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 65, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 73, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 66.92, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = 71.08, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = -65, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Top-Left
    geom_tile(aes(x = -73, y = 22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -65, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -73, y = 21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = 24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = 19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    geom_tile(aes(x = -65, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) + # Bottom-Left
    geom_tile(aes(x = -73, y = -22.83, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -65, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -73, y = -21.17, width = 4, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = -24.25, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -66.92, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    geom_tile(aes(x = -71.08, y = -19.75, width = 2 / 12, height = 3), fill = NHL_red) +
    
    # Referee crease
    geom_arc(aes(x0 = 0, y0 = -42.5, start = -pi / 2, end = pi / 2, r = 10), colour = NHL_red) +
    
    # Left goalie crease
    geom_tile(aes(x = -86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
    geom_arc_bar(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
    geom_tile(aes(x = -86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = -86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_arc(aes(x0 = -89, y0 = 0, start = atan(4.5/4) - 0.01, end = pi - atan(4.5 / 4) + 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
    geom_tile(aes(x = -85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    geom_tile(aes(x = -85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    
    # Right goalie crease
    geom_tile(aes(x = 86.75, y = 0, width = 4.5, height = 8), fill = NHL_light_blue) +
    geom_arc_bar(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r0 = 4, r = 6), fill = NHL_light_blue, colour = NHL_light_blue, size = 1 / 12) + # manually adjusted arc
    geom_tile(aes(x = 86.75, y = -4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_tile(aes(x = 86.75, y = 4, width = 4.5, height = 2 / 12), fill = NHL_red) +
    geom_arc(aes(x0 = 89, y0 = 0, start = -atan(4.5/4) + 0.01, end = -pi + atan(4.5 / 4) - 0.01, r = 6), colour = NHL_red, size = 2 / 12) + # manually adjusted arc
    geom_tile(aes(x = 85, y = 3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    geom_tile(aes(x = 85, y = -3.75, width = 2 / 12, height = 0.42), fill = NHL_red) +
    
    # Goalie nets placed as rectangles
    geom_tile(aes(x = -90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Left # with grey fills
    geom_tile(aes(x = 90.67, y = 0, width = 3.33, height = 6), fill = "#E5E5E3") + # Right
    
    # Trapezoids
    geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Left
    geom_polygon(aes(x = c(-100, -100, -89, -89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Left 
    geom_polygon(aes(x = c(100, 100, 89, 89), y = c(10.92, 11.08, 7.08, 6.92)), fill = NHL_red) + # Right
    geom_polygon(aes(x = c(100, 100, 89, 89), y = c(-10.92, -11.08, -7.08, -6.92)), fill = NHL_red) + # Right
    
    # Lines
    geom_tile(aes(x = -25.5, y = 0, width = 1, height = 85), fill = NHL_blue) + # Left Blue line
    geom_tile(aes(x = 25.5, y = 0, width = 1, height = 85),  fill = NHL_blue) + # Right Blue line
    geom_tile(aes(x = -89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Left goal line (73.5 value is rounded from finding intersect of goal line and board radius)
    geom_tile(aes(x = 89, y = 0, width = 2 / 12, height = 73.50), fill = NHL_red) + # Right goal line
    
    # Borders as line segments - plotted last to cover up line ends, etc.
    geom_line(aes(x = c(-72, 72), y = c(42.5, 42.5))) + # Top
    geom_line(aes(x = c(-72, 72), y = c(-42.5, -42.5))) + # Bottom
    geom_line(aes(x = c(-100, -100), y = c(-14.5, 14.5))) + # Left
    geom_line(aes(x = c(100, 100), y = c(-14.5, 14.5))) + # Right
    geom_arc(aes(x0 = 72, y0 = 14.5, start = pi / 2, end = 0, r = 28)) + # Top-Right
    geom_arc(aes(x0 = 72, y0 = -14.5, start = pi, end =  pi / 2, r = 28)) + # Bottom-Right
    geom_arc(aes(x0 = -72, y0 = 14.5, start = - pi / 2, end = 0, r = 28)) + # Top-Left
    geom_arc(aes(x0 = -72, y0 = -14.5, start = pi, end =  3 * pi / 2, r = 28)) + # Bottom-Left
    
    # Fixed scale for the coordinate system  
    coord_fixed() +
    theme_void()
}

rink <- nhl_rink_plot()
rink

box <- tibble(
  x.plot = 100,
  y.plot = 42.5
)

NB_olive <- "#7c7610"
NB_gold <- "#f5d20e"

library(png)
img <- readPNG("nb_image.png")


# plot shots
rink + geom_point(data = viz_df, aes(x=x.plot, y=y.plot)) +
ggtitle("North Bay Battalion shot attempts\n as of Oct 31/21") +
  ggeasy::easy_center_title() +
  labs(caption = "data retrieved from ontariohockeyleague.com") +
  annotate("text", x = c(-50, 50), y=50, label = c("away", "home"),
           color = NB_olive)


rink + stat_density2d(data = viz_df, aes(x=x.plot, y=y.plot, fill = ..level.., alpha = ..level..),
               size = .01, bins = 6, geom = "polygon") +
  scale_fill_gradient(low = "black", high = "springgreen") +
  scale_alpha(range = c(0.1, 0.8), guide = FALSE) +
  ggeasy::easy_center_title() +
  labs(caption = "data retrieved from ontariohockeyleague.com") +
  annotate("text", x = c(-50, 50), y=50, label = c("away", "home"),
           color = NB_olive) +
  ggtitle("North Bay Battalion shot attempt density\n as of Oct 31/21") +
  ggeasy::easy_center_title() +
  theme(legend.position = "none")


