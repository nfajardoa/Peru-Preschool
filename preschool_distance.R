survey1 <- tbl(db_conn, "eceqtn_2016_2p") %>%
  left_join(tbl(db_conn, "ece")) %>%
  select(rural, matches("(distance)|(transport)"))

survey2 <- tbl(db_conn, "eceqtn_2018_4p") %>%
  left_join(tbl(db_conn, "ece")) %>%
  select(rural, matches("(school\\.distance)|(transport)"))

distance <- union_all(survey1, survey2) %>%
  collect() %>%
  pivot_longer(
    cols = matches("school"),
    names_to = c("type", "variable"),
    names_pattern = "(.*)\\.(.*)"
  ) %>%
  pivot_wider(id_cols = c(rural, type), names_from = variable, values_from = value, values_fn = list) %>%
  unnest(cols = c(distance, transport)) %>%
  mutate(distance_km = (distance / 60) * transport) %>%
  drop_na(rural, distance_km)

ggsave_pdf(
  file = "plots/commdistance.pdf",
  plot =
    ggplot(distance %>% filter(type == "preschool") %>% mutate(rural = if_else(rural == TRUE, "Rural", "Urban"))) + 
    geom_histogram(aes(x = distance_km, y=..count../sum(..count..)), fill = colors[5]) + 
    facet_grid(cols = vars(rural)) +
    coord_cartesian(xlim = c(0,30)) +
    scale_y_continuous(
      breaks = breaks_width(width = 0.25),
      expand = c(0, 0),
      labels = percent_format(),
      sec.axis = dup_axis(breaks = extended_range_breaks())
    ) +
    scale_x_continuous(
      expand = c(0, 0),
      breaks = extended_range_breaks(),
      labels = number_format(big.mark = "", accuracy = 1)
    ) +
    theme_thesis() +
    theme(axis.title.y = element_blank(), axis.title.x = element_blank(), legend.position = "bottom"),
  device = "pdf", height = 60, width = 150, units = "mm"
)

quantile(distance$distance_km[distance$type == "preschool"], probs = 0.99)
summary_distance <- distance %>%
  group_by(rural, type) %>%
  summarise(distance_km = summary(distance_km) %>% list())

summary_distance$distance_km
#4 for all (see parameters of select_id)
