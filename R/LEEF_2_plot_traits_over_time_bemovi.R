LEEF_2_plot_traits_over_time_bemovi <- function(
    db,
    method = "mag_25"
){

  CiliateTraits <- db_read_table(
    db = db,
    table = paste0("bemovi_", method, "__morph_mvt") %>%
      group_by(species, bottle, timestamp, temperature, resources, salinity) %>%
#      collect() %>%
      summarize(mean_area = mean(mean_area),
                mean_perimeter = mean(mean_perimeter),
                mean_ar = mean(mean_ar),
                max_gross_speed = mean(max_gross_speed),
                net_speed = mean(net_speed))

  # colnames(CiliateTraits25)

  CiliateTraits$timestamp2 <- as_date(CiliateTraits$timestamp)

  CiliateTraits$day <- as.numeric(CiliateTraits$timestamp2 - min(CiliateTraits$timestamp2))

  CiliateTraitsSummLong <- CiliateTraits %>%
    pivot_longer(cols = c("mean_area","mean_perimeter","mean_ar","max_gross_speed","net_speed"))



# Plotting ----------------------------------------------------------------


  CiliateTraits <- CiliateTraits %>%
    mutate(conditions = paste0("Tem: ", temperature,
                               "\n Res: ", resources,
                               "\n Sal: ", salinity))

  Conditions <- CiliateTraits %>%
    ungroup() %>%
    select(bottle, conditions) %>%
    distinct()

  Cond <- Conditions %>%
    ggplot(aes(1, 1)) +
    facet_grid(bottle~"Conditions")+
    geom_text(aes(label=conditions),size=5, show.legend = F) +
    theme_void()+
    theme(legend.position = "top",
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          strip.text = element_blank())


  a <- CiliateTraitsSummLong %>%
    filter(name=="mean_area") %>%
    ggplot(aes(day, value, col=species))+
    geom_line() +
    facet_grid(bottle~name, scales = "free") +
    theme_bw() +
    theme(strip.text.y = element_blank())

  b <- CiliateTraitsSummLong %>%
    filter(name=="mean_ar") %>%
    ggplot(aes(day, value, col=species))+
    geom_line() +
    facet_grid(bottle~name, scales = "free")+
    theme_bw() +
    theme(strip.text.y = element_blank())

  c <- CiliateTraitsSummLong %>%
    filter(name=="max_gross_speed") %>%
    ggplot(aes(day, value, col=species))+
    geom_line() +
    facet_grid(bottle~name, scales = "free")+
    theme_bw() +
    theme(strip.text.y = element_blank())

  d <- CiliateTraitsSummLong %>%
    filter(name=="net_speed") %>%
    ggplot(aes(day, value, col=species))+
    geom_line() +
    facet_grid(bottle~name, scales = "free")+
    theme_bw()

  Cond + a + b + c + d + plot_layout(ncol = 6, widths = c(2,5,5,5,5), guides = "collect") +
    plot_annotation(title = "Ciliate traits at 25x")  &
    theme(plot.title = element_text(size = 30),
          legend.position = "top")
}
