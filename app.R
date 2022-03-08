library(tidyverse)
library(gapminder)
library(dash)
library(dashHtmlComponents)
library(dashCoreComponents)
library(plotly)
library(maps)

plot_world <- function(yr, colnm){
  world <- map_data('world') %>%
    filter(lat > -56)
  
  countries <- read_csv('./data/world_country.csv',
                        col_select = c(4, 3, 2)
  )
  colnames(countries) <- c('country', 'long', 'lat')
  
  col_var <- sym(colnm)
  
  gap_geo <- gapminder %>%
    inner_join(countries)
  
  param <- get_para(yr, colnm)
  title_txt <- param[[1]]
  scale <- param[[2]]
  
  world_map <- world %>%
    ggplot() +
    geom_polygon(data = world,
             aes(long, lat, group = group),
             color = 'white', fill = 'lightgray', size = 0.1) +
    annotate(geom='text', x = -150, y = 90, label = title_txt,
             color = 'darkgray', size = 4) +
    annotate(geom='text', x = 178, y = 85, label = yr,
             color = 'darkgray', size =  9) +
    geom_point(data = filter(gap_geo, year == 2002),
               aes(long, lat, color = continent, size = !!col_var),
               alpha = 0.7) +
    scale_size(range = c(0, scale)) +
    xlab(NULL) +
    ylab(NULL) +
    theme_bw() +
    theme(axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          legend.position = 'none'
    )
  world_map
}

get_para <- function(yr, colnm){
  col_var <- sym(colnm)
  col_name_df <- data.frame(row.names = c('lifeExp', 'pop', 'gdpPercap'),
                           val = c('Life Expectancy', 'Population', 'GDP per Capita'))
  
  col_max_scale_df <- data.frame(row.names = c('lifeExp', 'pop', 'gdpPercap'),
                                val = c(6, 15, 8))
  
  summary_df <- gapminder %>% group_by(year) %>% summarise(sum = sum(!!col_var))
  scale <- summary_df %>% filter(year == yr) %>% select(sum) %>% pull()
  scale <- scale / max(summary_df$sum) * col_max_scale_df[colnm, 1]
  list(col_name_df[colnm, 1], scale)
}

plot_world(2002, 'pop')

app <- Dash$new(external_stylesheets = "https://codepen.io/chriddyp/pen/bWLwgP.css")

fig <- plot_world(2002, 'pop')

app$layout(
  dbcContainer(
    list(
      dccDropdown(
        id='col-select',
        options = list(list(label = 'Life Expectancy', value = 'lifeExp'),
                       list(label = "Population", value = 'pop'),
                       list(label = 'GDP per Capita', value = 'gdpPercap')),
        value = 'pop'
      ),
      dccGraph(id='plot-area')
    )
  )
)

app$callback(
  output('plot-area', 'figure'),
  list(input('col-select', 'value')),
  function(xcol){
    ggplotly(plot_world(2002, xcol))
  }
)

app$run_server(host = '0.0.0.0')