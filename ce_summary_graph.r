source("common.R")

inputdir <- '~/dissertation/figures/'
outputdir <- '~/dissertation/figures/'

ce <- read.csv(paste0(inputdir, 'ce_summary.csv')) %>%
  melt() %>%
  mutate(Quintile = str_wrap(Quintile, 10))

ggplot(ce, aes(Quintile, value, fill = variable)) +
  geom_col() +
  scale_fill_viridis(discrete = TRUE, name = NULL, option = "plasma",
                     begin = .35, end = .9) +
  scale_y_continuous(labels = dollar) +
  geom_text(aes(label = dollar(value)), size = 2.5, family = 'Lato',
            position = position_stack(vjust = 0.5)) +
  theme_sb +
  labs(x = NULL, y = NULL)

ggsave(paste0(outputdir, 'ce_summary.pdf'), width = 6.5, height = 3.5, device = cairo_pdf)
ggsave(paste0(outputdir, 'ce_summary.png'), width = 6.5, height = 3.5)

ggplot(ce, aes(Quintile, value, fill = variable)) +
  geom_col(position = position_fill()) +
  scale_fill_viridis(discrete = TRUE, name = NULL, option = "plasma",
                     begin = .35, end = .9) +
  scale_y_continuous(labels = percent) + 
  geom_text(aes(label = dollar(value)), size = 2.5, family = "Lato",
            position = position_fill(vjust = 0.5)) +
  theme_sb +
  labs(x = NULL, y = NULL)
  
ggsave(paste0(outputdir, 'ce_share.pdf'), width = 6.5, height = 3.5, device = cairo_pdf)
ggsave(paste0(outputdir, 'ce_share.png'), width = 6.5, height = 3.5)
