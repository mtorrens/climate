
library(plyr)
library(dplyr)
library(readr)
library(lubridate)
library(highcharter)
library(stringr)
library(ggplot2)
library(viridis)
library(scales)
#devtools::install_github('jbkunst/jbkmisc')
library(jbkmisc)

df <- read_csv("http://bl.ocks.org/bricedev/raw/458a01917183d98dff3c/sf.csv")

df[1:4, 1:4]

names(df) <- names(df) %>% 
  str_to_lower() %>% 
  str_replace(" ", "_")
  #str_replace("\s+", "_")

df <- df %>% 
  mutate(id = seq(nrow(df)),
         date2 = as.Date(ymd(date)),
         tmstmp = datetime_to_timestamp(date2),
         month = month(ymd(date)))

# dsmax <- df %>%
#   select(x = tmstmp,
#          y = max_temperaturec) %>% 
#   list.parse3()
 
# dsmin <- df %>% 
#   select(x = tmstmp, y = min_temperaturec) %>% 
#   list.parse3()



# hc <- highchart() %>% 
#   hc_chart(
#     type = "line"
#     ) %>%
#   hc_xAxis(
#     type = "datetime",
#     tickInterval = 30 * 24 * 3600 * 1000,
#     labels = list(format = "{value: %b}")
#   ) %>% 
#   hc_yAxis(
#     min = 0,
#     labels = list(format = "{value} C")
#   ) %>% 
#   hc_add_series(
#     data = dsmax, name = "max"
#   ) %>% 
#   hc_add_series(
#     data = dsmin, name = "min"
#     ) %>% 
#   hc_add_theme(
#     hc_theme_smpl()
#     )

# hc

# hc <- hc %>% 
#   hc_chart(
#     type = "column"
#     ) %>% 
#   hc_plotOptions(
#     series = list(
#       stacking = "normal"
#     )
#   )

# hc

# dsmax <- df %>% 
#   mutate(color = colorize_vector(mean_temperaturec, "A"),
#          y = max_temperaturec - min_temperaturec) %>% 
#   select(x = tmstmp,
#          y,
#          name = date,
#          color,
#          mean = mean_temperaturec,
#          max = max_temperaturec,
#          min = min_temperaturec) %>% 
#   list.parse3()

# # Some tooltips to make it a little *intercative*
# x <- c("Min", "Mean", "Max")
# y <- sprintf("{point.%s}", tolower(x))
# tltip <- tooltip_table(x, y)

# hc <- highchart() %>% 
#   hc_chart(
#     type = "column",
#     polar = TRUE
#   ) %>%
#   hc_plotOptions(
#     series = list(
#       stacking = "normal",
#       showInLegend = FALSE
#     )
#   ) %>% 
#   hc_xAxis(
#     gridLineWidth = 0.5,
#     type = "datetime",
#     tickInterval = 30 * 24 * 3600 * 1000,
#     labels = list(format = "{value: %b}")
#   ) %>% 
#   hc_yAxis(
#     max = 30,
#     min = -10,
#     labels = list(format = "{value} C"),
#     showFirstLabel = FALSE
#     ) %>% 
#   hc_add_series(
#     data = dsmax
#   ) %>% 
#   hc_add_series(
#     data = dsmin,
#     color = "transparent",
#     enableMouseTracking = FALSE
#   ) %>% 
#   hc_add_theme(
#     hc_theme_smpl()
#   ) %>% 
#   hc_tooltip(
#     useHTML = TRUE,
#     headerFormat = as.character(tags$small("{point.x:%d %B, %Y}")),
#     pointFormat = tltip
#   )

# hc

ggplot(df, aes(date2,
               ymin = min_temperaturec,
               ymax = max_temperaturec,
               color = mean_temperaturec)) + 
  geom_linerange(size = 1.3, alpha = 0.75) +
  scale_color_viridis(NULL, option = "D") +
  scale_x_date(labels = date_format("%b"), breaks = date_breaks("month")) + 
  ylim(-10, 35) + 
  labs(title = "San Francisco Wather Radial",
       subtitle = "It would be nice if someone do this with the animation package",
       caption = "Other example for ggplot2 vs base #boring but #fun",
       x = NULL, y = NULL) +
  coord_polar() + 
  #theme_jbk() +
  theme_jbk(base_family = "Helvetica", plot_title_family = "Helvetica") + 
  theme(legend.position = "bottom")



