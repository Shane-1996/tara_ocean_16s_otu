Basic Plot: Tara Ocean 16s OTU
================
Shane
2020-11-19

# Data import

``` r
otu = read_tsv("data/miTAG.taxonomic.profiles.release.tsv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_double(),
    ##   Domain = col_character(),
    ##   Phylum = col_character(),
    ##   Class = col_character(),
    ##   Order = col_character(),
    ##   Family = col_character(),
    ##   Genus = col_character(),
    ##   OTU.rep = col_character()
    ## )

    ## See spec(...) for full column specifications.

# Tidy

``` r
otu_tidy = otu %>% 
  pivot_longer(
    starts_with("TARA_"),
    names_to = "site",
    values_to = "count",
    values_drop_na = TRUE,
  ) %>% 
  janitor::clean_names()
```

``` r
otu_site = otu_tidy %>% 
  select("site", "phylum", "count") %>% 
  arrange(site, phylum)
```

# Cumulative histogram

``` r
otu_sum = 
  otu_site %>%
  group_by(site, phylum) %>%
  summarise(sum = sum(count)) 
```

    ## `summarise()` regrouping output by 'site' (override with `.groups` argument)

``` r
order = otu_sum %>% select("site") %>% unique() %>% tibble::rowid_to_column("order") %>% as_tibble()

ots_order = left_join(order, otu_sum, by = "site")
```

``` r
otu_sum %>% 
  filter(site == "tara_004_srf_0_22_1_6") %>% 
  mutate(
    pct = round(sum / sum(sum), 5)
  )
```

    ## # A tibble: 0 x 4
    ## # Groups:   site [0]
    ## # … with 4 variables: site <chr>, phylum <chr>, sum <dbl>, pct <dbl>

``` r
pct = function(x) {
  
  pct = otu_sum %>% 
  filter(site == x) %>% 
  mutate(
    pct = round(sum / sum(sum), 5)
  )
  
  tibble((pct))
}
```

``` r
output = vector("list", length = 7784)

for (i in 1:7784) {
  output[[i]] = pct(otu_sum$site[[i]])
}


output_df = bind_rows(output) %>% unique() %>% select(-sum)
```

``` r
output_other = 
  bind_rows(
    output_df  %>%  
    group_by(site) %>% 
    filter(pct < 0.01) %>% 
    summarise(pct = sum(pct)) %>%
    mutate(phylum = "other")
  )
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
output_1 = 
  output_df %>% 
  filter(pct >= 0.01) %>% 
  bind_rows(output_other) %>% 
  arrange(site, phylum)
```

``` r
plot = ggplot(output_1, aes(x=site, y=pct, fill=phylum)) + 
  geom_col(position = 'stack', width = 0.6) +
  theme(axis.text.x = element_text(angle = 270, hjust = 1), legend.position = "right") +
  scale_fill_manual(values=c("#333300", "#339900", "#00FF00", "#000033", "#660099", "#990099", "#660000", "#FF3366", "#FF9900", "#996600", "#3399FF", "#006699", "#0000CC"))
```

``` r
write_csv(output_1, "data/site_phylum_pct.csv")
```
