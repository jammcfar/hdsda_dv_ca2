library(geojsonio)

library(rmapshaper)
library(sp)

states_json <- geojson_json(as_tibble(states), geometry = "polygon", group = "group")

states_sp <- geojson_sp(states_json)

## Plot the original
plot(states_sp)

states_simp <- ms_simplify(states_sp)


head(states)
head(dat_map)


## this is the wrong file!!
map_json <- geojson_json(dat_world, geometry = "polygon", group = "group")

map_sp <- geojson_sp(map_json)

map_simp <- ms_simplify(map_sp, keep = 0.05)

coord_bits <- as_tibble(raster::geom(map_simp))

map_reduce <-
  as_tibble(map_simp)[1:50, ] %>%
  mutate(
    group = as.numeric(trimws(group)),
    order = as.numeric(trimws(order))
  ) %>%
  right_join(coord_bits, by = c(group = "object")) %>%
  select(
    long = x,
    lat = y,
    group,
    order,
    region,
    subregion
  )




## naniar::vis_miss(dat_j)


## join ggplot2 stuff
dat_world <- ggplot2::map_data("world")

## alot match, but many dont. Will have to go in manually
dat_world <- dat_world %>%
  as_tibble() %>%
  mutate(subregion = NA) %>%
  filter(region != "Antarctica") %>%
  arrange(region)

## map the map smaller
map_json <- geojson_json(dat_world, geometry = "polygon", group = "group")

map_sp <- geojson_sp(map_json)

map_simp <- ms_simplify(map_sp, keep = 0.1, keep_shapes = T)

map_simp <- ms_simplify(map_json, keep = 0.1, keep_shapes = T)
map_simp@proj4string
coord_bits <- as_tibble(raster::geom(map_simp))

broom_test <- broom::tidy(map_simp)
broom_test$id[1:100]
length(unique(broom_test$id))
rgdal::readOGR(map_simp)
json_simp <- jsonlite::fromJSON(map_simp) %>% as_tibble()
str(json_simp)
str(json_simp)

world_reduce <-
  map_simp@data %>%
  as_tibble() %>%
  mutate(
    group = as.numeric(trimws(group)),
    order = as.numeric(trimws(order))
  ) %>%
  group_by(region) %>%
  mutate(region_id = cur_group_id()) %>%
  right_join(coord_bits, by = c(region_id = "object")) %>%
  select(
    long = x,
    lat = y,
    group,
    order,
    region,
    subregion
  )
rworldmap::countriesLow %>%
  as_tibble() %>%
  ggplot(aes(x = LON, y = LAT, fill = POP_EST)) +
  geom_polygon(colour = "grey80", size = 0.5)

test <- rworldmap::countriesLow %>% as_tibble()
test$LON[1:100]

world_reduce %>%
  ggplot(aes(x = long, y = lat, group = region, text = region)) +
  geom_polygon(colour = "grey80", size = 0.5) +
  geom_text(aes(label = region), colour = "white")

ggplot(data = rworldmap::countriesLow) %>%
  geom_sf()
fortify(rworldmap::countriesLow) %>% as_tibble()
