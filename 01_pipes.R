library(tidyverse)
library(gganimate)
library(RColorBrewer)

n = round(16, 0)

pipes <- expand.grid(
  x = NA,
  x_end = NA,
  y = NA,
  y_end = NA,
  group = 1:n
) 

## Area is square are (0,0) | (1,1)

# Equally spaced dots
## seq(0, 1, len = n)

# Nerf the y coordinate
## rep(c(0,rnorm(1)), n/2)
## rep(c(1,rnorm(1)), n/2)

pipes <- pipes |>
  mutate(
    x = rep(rnorm(2), n/2),
    x_end = lag(x,1),
    diff_x = abs(x_end-x),
    x = x + 2*diff_x*(group-2),
    x_end = x_end + 2*diff_x*(group-2),
    y = rep(c(0,1), n/2),
    y_end = rep(c(1,0), n/2),
  ) |>
  slice(rep(1:n(), each = 2)) |>
  mutate(
    x_end = lag(x_end, 1),
    y_end = lag(y_end, 1)
  )

temp <- ggplot(pipes,
               aes(
                 x = x,
                 y = y,
                 xend = x_end,
                 yend = y_end
               )) +
  theme_void() +
  theme(plot.background = element_rect(fill = "gray10"),
        plot.margin = margin(20, 20, 20, 20),
        legend.position = "none") +
  geom_segment(aes(color = group),
               linewidth = 2) +
  scale_color_gradient(low = "yellow", high = "black", na.value = NA) +
  coord_cartesian(xlim = c(min(pipes$x, na.rm = T)-2.5, 
                           max(pipes$x_end, na.rm = T)+2.5),
                  ylim = c(min(pipes$y, na.rm = T)-0.5,
                           max(pipes$y_end, na.rm = T)+0.5))

ggsave(temp, filename = "temp.png", 
       width = 1920, height = 1280, 
       units = "px", dpi = 300)
