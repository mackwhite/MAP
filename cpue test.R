test <- read_csv('data/map_years1thru20_clean_zerofilled.csv')
test1 <- test |> filter(catch_code != "ZERO" & calendar_year == 2024) |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |>
      select(common_name) |> distinct()

test2 <- test |> filter(calendar_year == 2024) 

df1 <- test2 |> 
      filter(site %in% c("RB8", "RB9", "RB10", "RB11", "RB13")) |>
      filter(common_name %in% c('American eel', 'Asian Swamp Eel',
                                'Bluegill', "Coastal Shiner", "Hogchoker",
                                'Mayan Cichlid', 'Peacock eel', 
                                'Redear', 'Spotted sunfish', 'Striped mojarra',
                                'Unidentified mojarra species', 'No fishes collected')) |> 
      group_by(common_name, month, site, bout) |> 
      summarize(
            count = sum(catch_number, na.rm = TRUE),
            count_m = count/distance,
            count_100m = count_m*100,
            biomass = sum(weight_g * catch_number, na.rm = TRUE),
            biomass_m = biomass/distance,
            biomass_100m = biomass_m*100
            ) |> 
      ungroup() |> 
      distinct() |> 
      group_by(month, site, bout) |> 
      summarize(
            tot_bm = sum(biomass_m)
      ) |> 
      ungroup()

df1 |> 
      # group_by(site) |> 
      mutate(scalebm = scale(tot_bm, center = TRUE)) |> 
      # ungroup() |> 
      ggplot(aes(x = month, y = scalebm)) +
      geom_point() + 
      geom_smooth(method = "loess", se = TRUE) + 
      facet_wrap(~site)
      

