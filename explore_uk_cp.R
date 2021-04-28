library(dplyr)
library(vroom)
library(forcats)

df <- vroom::vroom(list.files("data/UKCP/tasmax_quantiles/", full.names = TRUE, pattern = "*quantiles")) 
df_fil <- df %>% 
  #filter(year %in% c(1985, 2075) & stat == 0.5)
  filter(stat %in% c("0", "0.025", "0.05", "0.5", "0.95", "0.975", "1"))

df_fil$month <- month.abb[df_fil$month]
df_fil$month <- factor(df_fil$month, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun",
                                                   "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df_fil$year <- as.factor(df_fil$year)

# write_fst(df_fil, "data/UKCP/tasmax_quantiles/01_df_shiny_filtered.fst")
# vroom_write(df_fil, "data/UKCP/tasmax_quantiles/01_df_shiny.csv")
# write_feather(df_fil, "data/UKCP/tasmax_quantiles/01_df_shiny.feather")
# saveRDS(df_fil, "data/UKCP/tasmax_quantiles/01_df_shiny.RDS")
# qsave(df_fil, "data/UKCP/tasmax_quantiles/01_df_shiny.qs")

ggplot(data = df_fil, aes(y = fct_rev(month)))+
  geom_density_ridges(aes(x = value, fill = year), alpha = 0.6, colour = "transparent") +
  scale_fill_manual(values = c("#DAA03DFF", "#616247FF"), labels = c("1985", "2075")) +
  theme_ridges(grid = T, center_axis_labels = TRUE)+
  labs(y = "Month", x = expression(Temperature^{o}~C), 
       fill = "Year", title = "Temperatures across UK MSOAs")

