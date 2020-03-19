
# libs --------------------------------------------------------------------



library(osmdata)
library(sf)
library(ggplot2)
library(ggtext)
library(dplyr)


# Define locations --------------------------------------------------------


download_details <-list(durham = list(location = "Durham, NC", 5.5, 5.5),
                    manhattan = list(location = "Central Park, NYC", 6.5, 6.5),
                    muenster = list(location = "Muenster, Germany", 5.5, 5.5),
                    vilseck = list(location = "Vilseck, Germany", 4.5, 4.5)
                    )

clip_details <-list(durham = list(location = "Durham, NC", 5, 5),
                    manhattan = list(location = "Central Park, NYC", 6, 6),
                    muenster = list(location = "Muenster, Germany", 5, 5),
                    vilseck = list(location = "Vilseck, Germany", 2.5, 2.5)
                    )



# Get Bboxes --------------------------------------------------------------



bboxes <- purrr::map(download_details, ~AOI::getAOI(clip = .x, km = TRUE) )
clipboxes <- purrr::map(clip_details, ~AOI::getAOI(clip = .x, km = TRUE) )

# visual check
# AOI::check(bboxes[["durham"]])
# AOI::check(bboxes[["manhattan"]])
# AOI::check(bboxes[["muenster"]])
# AOI::check(bboxes[["vilseck"]])




# helpers -----------------------------------------------------------------

get_feature <- function(bbox, feature, value){
    bbox %>% 
        osmdata::opq() %>%
        osmdata::add_osm_feature(key = feature, value = value)
}

add_weights <- function(sf_df){
    
    sf_df <- dplyr::mutate(sf_df,
                    line_weights = case_when(highway == "motorway" ~ 1.75,
                                             highway == "trunk" ~ 1.5,
                                             highway == "primary" ~ 1.25,
                                             highway == "secondary" ~ 0.75,
                                             highway == "tertiary" ~ 0.5,
                                             highway == "residential" ~ 0.25,
                                             highway == "unclassified" ~ 0.25,
                                             highway == "service" ~ 0.25,
                                             TRUE ~ 0.25))
    
    return(sf_df)
    
    
}


add_weights_water <- function(sf_df){
  
  sf_df <- dplyr::mutate(sf_df,
                         line_weights = case_when(waterway == "river" ~ 1.75,
                                                  waterway == "stream" ~ 1,
                                                  TRUE ~ 1))
  
  return(sf_df)
  
  
}


# Download data -----------------------------------------------------------


# highways
highways_osm <- purrr::map(bboxes,
                           get_feature,
                           feature = "highway",
                           value = NULL)


transport_highway_sf <- purrr::map(highways_osm,
                                   ~osmdata::osmdata_sf(.x)) 



# water
water_osm <- purrr::map(bboxes,
                         get_feature,
                         "water",
                        NULL)


water_sf <- purrr::map(water_osm,
                                  ~osmdata::osmdata_sf(.x))



#
waterway_full_osm <- purrr::map(bboxes,
                                get_feature,
                                "waterway",
                                NULL)

waterway_full_sf <- purrr::map(waterway_full_osm,
                          ~osmdata::osmdata_sf(.x))



#
water_natural_osm <- purrr::map(bboxes,
                           get_feature,
                           "natural",
                           "water")


waternatural_sf <- purrr::map(water_natural_osm,
                          ~osmdata::osmdata_sf(.x))



#
water_reservoir_osm <- purrr::map(bboxes,
                                get_feature,
                                "landuse",
                                "reservoir")


waterres_sf <- purrr::map(water_reservoir_osm,
                              ~osmdata::osmdata_sf(.x))



# theme -------------------------------------------------------------------

bg_col <- "grey40"
road_col <- "white"
# water_col <- "#6cb2c1"
# water_col <- "#0586c6"
# water_col <- "#3284ad"
# water_col <- "#33abc6"
# water_col <- "#53adc1"
# water_col <- "#3FCBE8"
water_col <- "#76C0E8"


theme_set(theme_void())
theme_update(
    panel.background = element_rect(fill = bg_col,
                                    color = bg_col),
    # plot.background = element_blank(),
    plot.background = element_rect(fill = bg_col,
                                   color = bg_col),
    plot.margin = margin(7, 7, 30, 7),
    panel.border = element_rect(color = road_col,
                                size = 10,
                                fill = "transparent",
                                ),
    plot.title = element_markdown(color = road_col,
                                  family = "HP Simplified Light",
                                  size = 50,
                                  face = "bold",
                                  hjust = 0.5,
                                  margin = margin(t = 12, b = 12)),
    plot.subtitle = element_markdown(color = road_col,
                                     family = "HP Simplified Light", 
                                     size = 19,
                                     hjust = 0.5,
                                     margin = margin(t = 0, b = 0))
)


# plot --------------------------------------------------------------------

extrafont::loadfonts()

# hrbrthemes::update_geom_font_defaults(family = "HP Simplified Light")
# hrbrthemes::update_geom_font_defaults(family = "Arrial Narrow")
# hrbrthemes::update_geom_font_defaults(family = hrbrthemes::font_rc)


# Durham ------------------------------------------------------------------



durham_map <- ggplot() +
  
  geom_sf(data = water_sf$durham$osm_polygons,
          fill = water_col,
          color = water_col) +
  
  # geom_sf(data = waterway_sf$durham$osm_lines,
  #         fill = water_col,
  #         color = water_col) +
  # 
  
  geom_sf(data = waterway_full_sf$durham$osm_lines,
          fill = water_col,
          color = water_col,
          size = waterway_full_sf$durham$osm_lines %>% 
            add_weights_water() %>% pull(line_weights)) +
  
  geom_sf(data = waternatural_sf$durham$osm_polygons,
          fill = water_col,
          color = water_col) +
  geom_sf(data = waternatural_sf$durham$osm_multipolygons,
          fill = water_col,
          color = water_col) +
  
  
  geom_sf(data = transport_highway_sf$durham$osm_lines,
          fill = road_col,
          color = road_col,
          size = transport_highway_sf$durham$osm_lines %>% 
            add_weights() %>% pull(line_weights),
          show.legend = TRUE) +
  
  geom_sf(data = transport_highway_sf$durham$osm_multipolygons,
          fill = road_col,
          color = road_col,
          size = transport_highway_sf$durham$osm_multipolygons %>% 
            add_weights() %>% pull(line_weights),
          show.legend = TRUE) +

  
  geom_sf(data = waterres_sf$durham$osm_polygons,
          fill = water_col,
          color = water_col) +

  
  
  
  coord_sf(xlim = {clipboxes$durham %>% sf::st_bbox()}[c(1,3)],
           ylim = {clipboxes$durham %>% sf::st_bbox()}[c(2,4)],
           expand = TRUE) +
  labs(title = "Durham") 

# New York -------------------------------------------------------------------




manhattan_map <- ggplot() +
   

    geom_sf(data = water_sf$manhattan$osm_polygons,
            fill = water_col,
            color = water_col) +
    

  geom_sf(data = waterway_full_sf$manhattan$osm_lines,
          fill = water_col,
          color = water_col,
          size = waterway_full_sf$manhattan$osm_lines %>% 
            add_weights_water() %>% pull(line_weights)) +
  
    
    geom_sf(data = waternatural_sf$manhattan$osm_polygons,
            fill = water_col,
            color = water_col) +
    geom_sf(data = waternatural_sf$manhattan$osm_multipolygons,
            fill = water_col,
            color = water_col) +
  
  
  
  geom_sf(data = waterres_sf$manhattan$osm_polygons,
          fill = water_col,
          color = water_col) +
  
     geom_sf(data = transport_highway_sf$manhattan$osm_lines,
             fill = road_col,
             color = road_col,
             size = transport_highway_sf$manhattan$osm_lines %>% 
                 add_weights() %>% pull(line_weights)) +
  

    
    coord_sf(xlim = {clipboxes$manhattan %>% sf::st_bbox()}[c(1,3)],
             ylim = {clipboxes$manhattan %>% sf::st_bbox()}[c(2,4)],
             expand = TRUE) +
    labs(title = "Manhattan")


# Muenster ----------------------------------------------------------------




muenster_map <- ggplot() +
  
  
  geom_sf(data = water_sf$muenster$osm_polygons,
          fill = water_col,
          color = water_col) +

  geom_sf(data = waterway_full_sf$muenster$osm_lines,
          fill = water_col,
          color = water_col,
          size = waterway_full_sf$muenster$osm_lines %>% 
            add_weights_water() %>% pull(line_weights)) +
  
  geom_sf(data = waternatural_sf$muenster$osm_polygons,
          fill = water_col,
          color = water_col) +
  geom_sf(data = waternatural_sf$muenster$osm_multipolygons,
          fill = water_col,
          color = water_col) +
  geom_sf(data = transport_highway_sf$muenster$osm_lines,
          fill = road_col,
          color = road_col,
          size = transport_highway_sf$muenster$osm_lines %>% 
            add_weights() %>% pull(line_weights)) +
  
 
  
  geom_sf(data = waterres_sf$muenster$osm_polygons,
          fill = water_col,
          color = water_col) +
  
  coord_sf(xlim = {clipboxes$muenster %>% sf::st_bbox()}[c(1,3)],
           ylim = {clipboxes$muenster %>% sf::st_bbox()}[c(2,4)],
           expand = TRUE) +
  labs(title = "Münster") 


# Vilseck ----------------------------------------------------------------




vilseck_map <- ggplot() +
  
  
  geom_sf(data = water_sf$vilseck$osm_polygons,
          fill = water_col,
          color = water_col) +
  
  
  geom_sf(data = waterway_full_sf$vilseck$osm_lines,
          fill = water_col,
          color = water_col,
          size = waterway_full_sf$vilseck$osm_lines %>% 
            add_weights_water() %>% pull(line_weights)) +
  
  geom_sf(data = waternatural_sf$vilseck$osm_polygons,
          fill = water_col,
          color = water_col) +
  geom_sf(data = waternatural_sf$vilseck$osm_multipolygons,
          fill = water_col,
          color = water_col) +
  
  geom_sf(data = waterres_sf$vilseck$osm_polygons,
          fill = water_col,
          color = water_col) +
  
  geom_sf(data = transport_highway_sf$vilseck$osm_lines,
          fill = road_col,
          color = road_col,
          size = transport_highway_sf$vilseck$osm_lines %>% 
            add_weights() %>% pull(line_weights) * 2.25) +
  
  
  geom_sf(data = transport_highway_sf$vilseck$osm_polygons,
          fill = road_col,
          color = road_col,
          size = transport_highway_sf$vilseck$osm_polygons %>% 
            add_weights() %>% pull(line_weights) * 2.25) +
  

  
  coord_sf(xlim = {clipboxes$vilseck %>% sf::st_bbox()}[c(1,3)],
           ylim = {clipboxes$vilseck %>% sf::st_bbox()}[c(2,4)],
           expand = TRUE) +
  
  
  labs(title = "Vilseck") 

# bind --------------------------------------------------------------------

library(patchwork) 

patch_map <- durham_map + manhattan_map + muenster_map + vilseck_map +
  plot_layout(ncol = 4)

ggsave(filename = "./figs/patch_in_2.jpg",
       plot = patch_map,
       width = 60.5,
       height = 15.5,
       units = "in",
       dpi = 300,
       limitsize = FALSE)


pdftools::pdf_convert("./figs/patch_cm.pdf",
                      format = "jpeg",
                      filenames = "./figs/patch_conv.jpeg",
                      dpi = 300)
