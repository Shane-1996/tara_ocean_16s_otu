XX: Tara Ocean 16s OTU
================
Shane
2020-11-19

# Data import

``` r
data = read_csv("data/site_phylum_pct.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   site = col_character(),
    ##   phylum = col_character(),
    ##   pct = col_double()
    ## )

``` r
location = 
  read_excel(
    "data/OM.CompanionTables.xlsx", 
    sheet = 2, 
    col_names=TRUE
  ) %>% 
  janitor::clean_names() %>% 
  mutate(
    site = sample_label_tara_station_number_environmental_feature_size_fraction,
    location = ocean_and_sea_regions_iho_general_sea_areas_1953_mrgid_registered_at_www_marineregions_com
  ) %>% 
  select(site, location)
```
