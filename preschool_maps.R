# ## Export data for clusters
# cp_clusters_pre <- st_read("Centros poblados/CP_P.shp") %>%
#   as.data.frame %>%
#   select(XGD, YGD, CODCP) %>%
#   mutate(across(matches('(CODCP)'), as.character))
#
# write.arff(cp_clusters_pre, file = 'Centros poblados/clustering.arff')
#
## Import clusters with different measures
# ccpp_clusters <- c(
# "Centros poblados/cluster_min2/clustering_result025/",
# "Centros poblados/cluster_min2/clustering_result050/",
# "Centros poblados/cluster_min2/clustering_result075/",
# "Centros poblados/cluster_min2/clustering_result125/",
# "Centros poblados/cluster_min2/clustering_result1/",
# 'Centros poblados/cluster_min2/clustering_result15/',
# 'Centros poblados/cluster_min2/clustering_result175/',
# "Centros poblados/cluster_min2/clustering_result2/"
#   ) %>%
#   map(import_clustering) %>%
#   reduce(full_join, by = 'codccpp') %>%
#   select(codccpp, matches('id'), matches('noise'))
# 
# write_csv(ccpp_clusters, file = 'Centros poblados/ccpp_clusters.csv')
# ccpp_clusters <- read_csv("Centros poblados/ccpp_clusters.csv") %>%
#   mutate(across(where(is.numeric), as.integer))
# 
## Import centros poblados info
# corrected_minedu <- read_csv('Centros poblados/corrected_geo-minedu.csv') %>%
#   bind_rows(read_csv('Centros poblados/corrected_geo-minedu2.csv')) %>%
#   mutate(across(where(is.numeric), as.integer64))
# 
centros_poblados <- readRDS('Centros poblados/centros_poblados.rds')
# 
## Create cluster areas
# cluster_list <- inner_join(centros_poblados %>% select(codccpp), ccpp_clusters) %>%
#   mutate(
#     id025 = if_else(noise025 == TRUE, 0L, id025),
#     id050 = if_else(noise050 == TRUE, 0L, id050),
#     id075 = if_else(noise075 == TRUE, 0L, id075),
#     id1 = if_else(noise1 == TRUE, 0L, id1),
#     id125 = if_else(noise1 == TRUE, 0L, id125),
#     id15 = if_else(noise15 == TRUE, 0L, id15),
#     id175 = if_else(noise15 == TRUE, 0L, id175),
#     id2 = if_else(noise2 == TRUE, 0L, id2)
#   ) %>%
#   select(-matches('noise'))
# #
# cluster_areas <- c('id2', 'id175', 'id15','id125', 'id1', 'id075', 'id050', 'id025') %>%
#   map(add_area, data = cluster_list) %>%
#   map(right_join, y = cluster_list) %>%
#   map(~ ungroup(.x) %>% select(codccpp, starts_with('area'))) %>%
#   reduce(inner_join) %>%
#   replace_na(value = 0L)
#
# Save
# write_csv(cluster_areas, 'Centros poblados/cluster_areas.csv')
# write_csv(cluster_list, 'Centros poblados/cluster_list.csv')
# 
cluster_areas <- read_csv("Centros poblados/cluster_areas.csv") %>%
  mutate(
    across(matches("area"), ~ units::set_units(.x, value = km^2)),
    codccpp = as.integer(codccpp)
  )

cluster_list <- read_csv("Centros poblados/cluster_list.csv") %>%
  mutate(across(where(is.numeric), as.integer))

cluster_errors <- readRDS("Centros poblados/cluster_errors.rds") %>%
  filter(area_cluster > units::set_units(100, value = km^2)) %>%
  select(codccpp)

identical_ccpp <- centros_poblados %>%
  as_tibble() %>%
  select(-geometry) %>%
  group_by(ubigeo, nomcp) %>%
  filter(n() > 1) %>%
  mutate(group_id = cur_group_id()) %>%
  ungroup() %>%
  select(codccpp, group_id) %>% 
  anti_join(cluster_errors)

############## DONT RUN ###############################################
## identical_ccpp.1 <- centros_poblados %>%
##   as_tibble() %>%
##   select(-geometry) %>%
##   group_by(ubigeo, nomcp) %>%
##   filter(n() > 1) %>%
##   mutate(group_id.1 = cur_group_id(), n.1 = n()) %>%
##   ungroup() %>%
##   select(codccpp, group_id.1) %>% 
##   anti_join(cluster_errors)
##
## identical_ccpp.2 <- centros_poblados %>%
##   as_tibble() %>%
##   select(-geometry) %>%
##   filter(!is.na(idccpp)) %>%
##   group_by(idccpp) %>%
##   filter(n() > 1) %>%
##   mutate(group_id.2 = cur_group_id(), n.2 = n()) %>%
##   ungroup() %>%
##   select(codccpp, group_id.2)
## 
## identical_ccpp <- full_join(identical_ccpp.1, identical_ccpp.2) %>%
##   absorb_groups(group_id.1, group_id.2)
#######################################################################

# clusters <- inner_join(cluster_list %>% as_tibble() %>% select(-geometry), cluster_areas) %>%
#   select_id(var = "id025", start = TRUE) %>%
#   select_id(var = "id050") %>%
#   select_id(var = "id075") %>%
#   select_id(var = "id1") %>%
#   select_id(var = "id125") %>%
#   select_id(var = "id15") %>%
#   select_id(var = "id175") %>%
#   select_id(var = "id2") %>%
#   left_join(identical_ccpp) %>%
#   recodify_id()
# 
# areas <- clusters %>%
#   inner_join(centros_poblados %>% select(codccpp)) %>%
#   add_area('cluster', data = .)
# 
# clusters %<>% left_join(areas)
# 
# saveRDS(clusters, file = 'Centros poblados/cluster_errors.rds') # RUN WITHOUT DOING ANTIJOIN OF CLUSTER ERRORS
# saveRDS(clusters, file = 'Centros poblados/clusters.rds')
clusters <- readRDS(file = "Centros poblados/clusters.rds")
# 
# Merge all
# centros_poblados %<>% inner_join(clusters)
# 
# correction_sisfoh <- read_csv("Centros poblados/correction_ccpp-sisfoh.csv") %>%
#   left_join(clusters %>% select(-area_cluster)) %>%
#   mutate(ubigeo = as.integer(ubigeo), idccpp = as.integer64(idccpp)) %>%
#   select(-codccpp)
#   
# conversion_table <- centros_poblados %>% as_tibble() %>%
#   select(cluster, ubigeo, idccpp, matches('nomcp')) %>%
#   pivot_longer(cols = matches('nomcp'), values_to = 'nomcp', values_drop_na = TRUE) %>%
#   select(-name) %>%
#   bind_rows(correction_sisfoh) %>%
#   mutate(
#     ubigeo = str_pad(ubigeo, 6, pad = "0"),
#     idccpp = str_pad(idccpp, 10, pad = "0"),
#     nomcp = stri_enc_toutf8(nomcp, validate = TRUE),
#     )
# 
# dbWriteTable(db_conn, name = "conversion_ccpp", conversion_table, temporary = FALSE, overwrite = TRUE)
# 
# cluster_summary <- centros_poblados %>%
#   as_tibble() %>%
#   select(-geometry) %>%
#   ungroup() %>%
#   mutate(across(!matches("(^idccpp$)|(^nregion$)|(^area_cluster$)|(^geometry$)|(^nomcp)"), as.integer)) %>%
#   group_by(cluster) %>%
#   mutate(n = n(), group = if_else(n == 1, 0L, cluster), .before = 1) %>%
#   group_by(idccpp) %>%
#   arrange(n) %>%
#   mutate(across(matches("(2015)|(2017)"), ~ if_else(row_number() == 1, ., 0L, 0L))) %>%
#   group_by(cluster) %>%
#   summarise(
#     across(matches("(2015)|(2017)"), sum, na.rm = TRUE),
#     across(matches("(nregion)|(rural)|(category)"), calculate_mode, na.rm = TRUE),
#     height = mean(height),
#     capital = if_else(is.na(capital), 0L, capital) %>% max(),
#     area_cluster = head(area_cluster, 1),
#     group = head(group, 1),
#     n = head(n, 1),
#     )
# 
# cluster_summary.geo <- centros_poblados %>% 
#   select(cluster, codccpp) %>%
#   left_join(cluster_summary)
# 
# saveRDS(cluster_summary, file = 'Centros poblados/cluster_summary.rds')
# saveRDS(cluster_summary.geo, file = 'Centros poblados/cluster_summary-geo.rds')
# dbWriteTable(db_conn, name = "clusters", cluster_summary %>% mutate(area_cluster = as.numeric(area_cluster)), temporary = FALSE, overwrite = TRUE)

cluster_summary.geo <- readRDS(file = "Centros poblados/cluster_summary-geo.rds")

peru_sf <- ne_states(country = "Peru", returnclass = "sf") %>%
  select(geometry) %>%
  st_transform(crs = "+proj=moll +datum=WGS84")

#colours <- seq_gradient_pal("#132B43", "#56B1F7")(seq(0, 1, length.out = 12))
colours <- seq_gradient_pal("#003f5c", "#ffa600")(seq(0, 1, length.out = 12))

# All points plot
ggsave("Centros poblados/clusters.png",
  plot =
    ggplot() +
      geom_sf(data = peru_sf, fill = "#f7fcfd", alpha = 0.75, lwd = 0.25) +
      geom_sf(data = cluster_summary.geo, aes(color = group), size = 0.1, alpha = 0.5) +
      continuous_scale("color", "noise_clusters", highlight_palette(colours, range = range(cluster_summary.geo$group), target = c(-0.25, 0.25)),
        guide = guide_colourbar(nbin = max(cluster_summary.geo$group) / 100) # Give guide plenty bins
      ) +
      theme_bw(),
  device = "png", height = 13333.3333333, width = 10000, units = "px"
)

# Noise plot
ggsave("Centros poblados/clusters_noise.png",
  plot =
    ggplot() +
      geom_sf(data = peru_sf, fill = "#f7fcfd", alpha = 0.75, lwd = 0.25) +
      geom_sf(data = cluster_summary.geo %>% filter(group == 0), aes(color = group), size = 0.1, alpha = 0.5) +
      continuous_scale("color", "noise_clusters", highlight_palette(colours, range = range(cluster_summary.geo$group), target = c(-0.25, 0.25)),
        guide = guide_colourbar(nbin = max(cluster_summary.geo$group) / 100) # Give guide plenty bins
      ) +
      theme_bw(),
  device = "png", height = 13333.3333333, width = 10000, units = "px"
)

# One per group plot
ggsave("Centros poblados/oneincluster.png",
       plot =
         ggplot() +
         geom_sf(data = peru_sf, fill = "#f7fcfd", alpha = 0.75, lwd = 0.25) +
         geom_sf(data = cluster_summary.geo %>% arrange(cluster, idccpp) %>% filter(row_number() == 1) %>% mutate(area_cluster = as.numeric(area_cluster)), 
                 aes(color = group, size = area_cluster), alpha = 0.5) +
         continuous_scale("color", "noise_clusters", highlight_palette(colours, range = range(cluster_summary.geo$group), target = c(-0.25, 0.25)),
                          guide = guide_colourbar(nbin = max(cluster_summary.geo$group) / 100) # Give guide plenty bins
         ) +
         theme_bw(),
       device = "png", height = 13333.3333333, width = 10000, units = "px"
)

ggsave("Centros poblados/bigclusters.png",
       plot =
         ggplot() +
         geom_sf(data = peru_sf, fill = "#f7fcfd", alpha = 0.75, lwd = 0.25) +
         geom_sf(data = cluster_summary.geo %>% arrange(cluster, idccpp) %>% mutate(area_cluster = as.numeric(area_cluster)) %>% filter(between(area_cluster, 1000, 3000)), 
                 aes(color = area_cluster)) +
         continuous_scale("color", "noise_clusters", highlight_palette(colours, range = range(cluster_summary.geo$group), target = c(-0.25, 0.25)),
                          guide = guide_colourbar(nbin = max(cluster_summary.geo$group) / 100) # Give guide plenty bins
         ) +
         theme_bw(),
       device = "png", height = 13333.3333333, width = 10000, units = "px"
)
