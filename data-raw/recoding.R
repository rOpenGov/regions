
regional_gdp <- get_eurostat ( "nama_10r_2gdp")

names ( regional_gdp )

unique ( regional_gdp$unit)







plot_1  <- left_join ( map_nuts_2, sbs_b05_workforce %>%  
                         filter ( .data$time == "2015" ) %>%
                         regions::recode_nuts() %>%
                         filter ( .data$typology == "nuts_level_2" ) %>%
                         mutate ( geo = .data$code_2016 )) %>%
  make_plot ( )

plot_1

%>%
  left_join ( map_nuts_2 ) %>%
  make_plot ( )

sbs_b05_workforce %>%
  regions::recode_nuts() %>%
  mutate ( geo = .data$code_2016 ) %>%
  left_join ( map_nuts_2 ) %>%
  make_plot ( )

make_plot (  )

rd_workforce <- get_eurostat_json (
  id = "rd_p_persreg", 
  filters = list ( sex = "T",     
                   prof_pos = "TOTAL",
                   sectperf = "TOTAL", 
                   unit = "FTE" )
)

persreg <- get_eurostat(tolower("RD_P_PERSREG"))

names ( persreg   )
recoded_indicator <- sbs_b05_workforce %>% 
  regions::recode_nuts()

recoded_indicator <- rd_workforce %>% 
  regions::recode_nuts()

recoded_indicator %>%
  filter ( .data$time %in% c("2008", "2018"))


recoding_summary <- recoded_indicator %>%
  filter ( .data$time %in% c("2008", "2018")) %>%
  group_by ( .data$time ) %>%
  mutate ( observations  = nrow(.data)) %>% 
  ungroup() %>%
  mutate ( typology_change = ifelse ( grepl("Recoded", .data$typology_change), 
                                      yes = "Recoded", 
                                      no = .data$typology_change) ) 

recoding_summary %>% 
  filter ( .data$typology_change == "Recoded", 
           .data$typology == "nuts_level_2")

sum (map_nuts_2$geo %in% recoding_summary$geo)

sum (map_nuts_2$geo %in% validated_indicator$geo )

recoding_summary  %>%
  group_by ( .data$typology_change, .data$time ) %>%
  summarize ( values_missing = sum(is.na(.data$values)), 
              values_present = sum(!is.na(.data$values)), 
              pct = values_present / (values_present + values_missing ))

recoding_summary

recoded_on_map %>%
  select ( all_of(c("geo", "time", "values", "typology_change"))) %>%
  as.data.frame() %>%
  group_by ( .data$time, .data$typology_change ) %>%
  summarize ( values_present = sum(!is.na(.data$values)),
              values_missing = sum(is.na(.data$values)), 
              pct = values_present / (values_present + values_missing ))

rd_workforce %>% 
  mutate ( type = "before") %>%
  select ( all_of(c("geo", "time", "values", "type"))) %>%
  right_join ( map_nuts_2 )

sum(is.na(df_2$values))

df_1 <- map_nuts_2 %>%
  left_join (rd_workforce %>%
               mutate ( type =  "after") %>%
               select ( all_of(c("geo", "time", "values", "type"))) %>%
               filter ( .data$time == "2009" ), 
             by = "geo") 
df_2 <- map_nuts_2 %>%
  left_join (recoded_indicator %>%
               mutate ( type =  "after") %>%
               mutate ( geo = .data$code_2016 ) %>%
               select ( all_of(c("geo", "time", "values", "type"))) %>%
               filter ( .data$time == "2009" ))

sum(is.na(df_2$values))
sum(is.na(df_1$values))

make_plot ( df_1 )
make_plot ( df_2 )

make_plot <- function(dat) {
  
  dat %>%
    ggplot () +
    geom_sf(aes(fill=values),
            color="dim grey", size=.1) + 
    scale_fill_gradient( low ="#FAE000", high = "#00843A") +
    guides(fill = guide_legend(reverse=T, title = "LAI")) +
    facet_wrap ( facets = "time") +
    labs(title="Employment in Coal and Lignite Mining",
         subtitle = "persons employed by regions",
         caption="\ua9 EuroGeographics for the administrative boundaries 
                \ua9 Tutorial and ready-to-use data on greendeal.dataobservatory.eu", 
         fill = NULL) +
    theme_light() + 
    theme(legend.position=c(.92,.7)) +
    coord_sf(xlim=c(-22,48), ylim=c(34,70))
}




map_nuts_2 %>%
  left_join (recoded_indicator %>%
               mutate ( type =  "after") %>%
               select ( all_of(c("geo", "time", "values", "type"))) %>%
               filter ( .data$time == "2009" )) %>%
  ggplot () +
  geom_sf(aes(fill=values),
          color="dim grey", size=.1) + 
  scale_fill_gradient( low ="#FAE000", high = "#00843A") +
  guides(fill = guide_legend(reverse=T, title = "LAI")) +
  facet_wrap ( facets = "time") +
  labs(title="Employment in Coal and Lignite Mining",
       subtitle = "persons employed by regions",
       caption="\ua9 EuroGeographics for the administrative boundaries 
                \ua9 Tutorial and ready-to-use data on greendeal.dataobservatory.eu", 
       fill = NULL) +
  theme_light() + 
  theme(legend.position=c(.92,.7)) +
  coord_sf(xlim=c(-22,48), ylim=c(34,70))

sbs_b05_workforce <- get_eurostat_json (
  id = "sbs_r_nuts06_r2", 
  filters = list ( nace_r2 = "B05",     # Mining of coal and lignite
                   indic_sb = "V16110" # persons employed 
  )
)

dat <- 
  
  plot_1  <- left_join ( map_nuts_2, sbs_b05_workforce %>%  
                           filter ( .data$time == "2015" ) ) %>%
  make_plot ( )

plot_1

before <- sbs_b05_workforce %>%
  mutate ( type = "before") %>%
  select ( all_of (c("geo", "time", "values", "type")))

after <- sbs_b05_workforce %>% 
  mutate ( type = "after" ) %>%
  regions::recode_nuts() %>%
  mutate ( geo  = .data$code_2016 ) %>%
  select ( all_of (c("geo", "time", "values", "type")))

map_nuts_2 %>%
  left_join ( 
    after %>%
      full_join ( before, 
                  by = c("geo", "time", "values", "type") ), 
    by = 'geo'
  ) %>%
  filter ( .data$time %in% c("2018")) %>%
  mutate ( present = case_when (
    ! is.na(.data$values) & .data$type == "after" ~ "after", 
    ! is.na(.data$values) & .data$type == "before" ~ "before", 
    TRUE ~ "none")
  ) %>%
  mutate ( type = forcats::fct_relevel(.data$type, c("before", "after")) ) %>%
  ggplot () +
  geom_sf(aes(fill=present)
          ) + 
  scale_fill_manual( values = c("#00348A", "#4EC0E4", 'grey80') ) +
  guides(fill = guide_legend(reverse=T, title = NULL)) +
  facet_wrap ( facets = "type") +
  labs(title="Employment in Coal and Lignite Mining",
       subtitle = "persons employed by regions",
       caption="\ua9 EuroGeographics for the administrative boundaries 
                \ua9 Tutorial and ready-to-use data on greendeal.dataobservatory.eu", 
       fill = NULL) +
  theme_minimal() + 
  theme(legend.position="none") +
  coord_sf(xlim=c(-22,48), ylim=c(34,70))
