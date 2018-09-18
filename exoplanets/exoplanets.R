# load tidyverse package
library(tidyverse)

# load tsv file with exoplanet data and modify mass column
exoplanets <- read.delim2("exoplanets.txt", header = TRUE, 
                          stringsAsFactors = FALSE) %>%
  as_data_frame() %>%
  mutate(mass = mass %>% 
           sub(pattern = ",", replacement = ".") %>%
           as.numeric())

# quantify number of exoplanets observed per method
# complete the dataset for years without new discoveries
# calculate the total number of new discoveries per year
exoplanets_plot <- exoplanets %>%
  filter(method != "known") %>%
  group_by(year, method) %>%
  count() %>%
  ungroup() %>%
  complete(year = 1989:2018, method) %>%
  replace_na(list(n = 0)) %>%
  group_by(year) %>%
  mutate(n_year = sum(n)) %>%
  ungroup()

# set levels for the detection methods, and rank by most succesful in 2018
method_levels <- c("Transit", "Radial velocity", 
                   "Microlensing", "Imaging", "Timing")
names(method_levels) <- exoplanets_plot %>%
  filter(year == "2018") %>%
  arrange(-n) %>%
  .$method %>%
  unique()

# revalue method descriptions and apply factor levels
exoplanets_plot <- exoplanets_plot %>%
  mutate(method = plyr::revalue(method, method_levels),
         method = factor(method, levels = method_levels))

# plot number of new discoveries per year, per method
p1 <- exoplanets_plot %>%
  ggplot(aes(x = year, y = n, fill = method)) +
  geom_bar(aes(group = method), stat = "identity") +
  geom_text(data = exoplanets_plot %>%
              filter(method == "Imaging"), 
            aes(label = n_year, y = n_year + 35)) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(1989, 2018, 1)) +
  scale_y_continuous(breaks = seq(0, 1500, 250)) +
  labs(x = "", y = "Number of exoplanets", 
       title = "Number of discovered exoplanets per year",
       fill = "Method") +
  theme_bw() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

# calculate cummulative number of exoplanets discovered per year, per method
exoplanets_plot_cum <- exoplanets_plot %>%
  group_by(method) %>%
  mutate(n_cum = cumsum(n),
         n_year_cum = cumsum(n_year)) %>%
  ungroup()

# plot cummulative number of new discoveries per year, per method
p2 <- exoplanets_plot_cum %>%
  ggplot(aes(x = year, y = n_cum, fill = method)) +
  geom_bar(aes(group = method), stat = "identity") +
  geom_text(data = exoplanets_plot_cum %>%
              filter(method == "Imaging"), 
            aes(label = n_year_cum, y = n_year_cum + 100)) +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(1989, 2018, 1)) +
  scale_y_continuous(breaks = seq(0, 4500, 500)) +
  labs(x = "Year", y = "Number of exoplanets", 
       title = "Cummulative number of discovered exoplanets",
       fill = "Method") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

# combine both plots into a single plot using patchwork
p0 <- p1 + p2 + patchwork::plot_layout(ncol = 1)

# print to file
ggsave(p0, file = "exoplanets.pdf", width = 10, height = 11)

exoplanets_plot_cum %>%
  ggplot(aes(x = year, y = n_cum, col = method, group = method)) +
  geom_line() +
  scale_fill_brewer(palette = "Set2") +
  scale_x_continuous(breaks = seq(1989, 2018, 1)) +
  scale_y_continuous(breaks = seq(0, 4500, 500)) +
  labs(x = "Year", y = "Number of exoplanets", 
       title = "Cummulative number of discovered exoplanets",
       fill = "Method") +
  theme_bw() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

ggsave(p1, file = "exoplanets_single.pdf", width = 10, height = 5)

# get solar systems for compiled plot
solar_system <- exoplanets %>%
  filter(method == "known") %>%
  mutate(mass_log = log10(mass))

# plot properties of exoplanets
p3 <- exoplanets %>%
  filter(method != "known") %>%
  mutate(mass_log = log10(mass),
         method = plyr::revalue(method, method_levels),
         method = factor(method, levels = method_levels)) %>%
  ggplot(aes(x = mass_log, y = radius)) +
  geom_point(aes(col = method), alpha = 0.5) +
  geom_point(data = solar_system,
             col = "white") +
  ggrepel::geom_text_repel(data = solar_system,
                           aes(label = name)) +
  scale_x_continuous(breaks = seq(-4, 2, 1)) +
  scale_y_continuous(breaks = seq(0, 2, 0.2), limits = c(0, 2)) +
  scale_color_brewer(palette = "Set2") +
  labs(x = "Jovian mass (log10)", y = "Jovian radius", 
       title = "Properties of exoplanets (solar system planets in white)",
       col = "Method") +
  theme_bw() +
  theme(legend.position = "bottom",
        panel.grid = element_blank())

ggsave(p3, file = "exoplanets_properties.pdf", width = 6, height = 5)
