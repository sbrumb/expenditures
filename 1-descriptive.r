ce <- read_csv('data/ce_summary.csv') %>%
  dplyr::rename(quintile = Quintile) %>%
  gather(category, amount, -quintile) %>%
  mutate(quintile = str_wrap(quintile, 10)) 

ggplot(ce, aes(quintile, amount, fill = category)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, name = NULL, option = "plasma",
                     begin = .35, end = .9) +
  scale_y_continuous(labels = dollar) +
  geom_text(aes(label = dollar(amount)), size = 2.5, family = 'Lato',
            position = position_stack(vjust = 0.5)) +
  theme_sb +
  labs(x = NULL, y = NULL)

ggsave('plots/ce_summary.pdf', width = 6.5, height = 3.5, device = cairo_pdf)
ggsave('plots/ce_summary.png', width = 6.5, height = 3.5)

ggplot(ce, aes(quintile, amount, fill = category)) +
  geom_col(position = position_fill()) +
  scale_fill_viridis(discrete = TRUE, name = NULL, option = "plasma",
                     begin = .35, end = .9) +
  scale_y_continuous(labels = percent) + 
  geom_text(aes(label = dollar(amount)), size = 2.5, family = "Lato",
            position = position_fill(vjust = 0.5)) +
  theme_sb +
  labs(x = NULL, y = NULL)
  
ggsave('plots/ce_share.pdf', width = 6.5, height = 3.5, device = cairo_pdf)
ggsave('plots/ce_share.png', width = 6.5, height = 3.5)
