library(tidyverse)

# data from: https://commons.wikimedia.org/wiki/Data:Ncei.noaa.gov/weather/New_York_City.tab

# load temperature
nyc_temp <- read_tsv("temp_NYC.txt") %>%
  as_tibble() %>%
  separate(date, into = c("year", "month"), sep = "-", remove = FALSE) %>%
  mutate(year = as.numeric(year),
         month = as.numeric(month),
         month = factor(month, labels = c("January", "February", "March", 
                                          "April", "May", "June",
                                          "July", "August", "September", 
                                          "October", "November", "December")),
         date = as.Date(paste0(date, "-01"))) %>%
  gather(-date, -year, -month, key = "parameter", value = "measurement") %>%
  filter(parameter %in% c("high.temp", "avg.hi.temp", "avg.lo.temp", "low.temp")) %>%
  mutate(parameter = factor(parameter, levels = c("high.temp", "avg.hi.temp", 
                                                  "avg.lo.temp", "low.temp")))

# get mean temperature per month per parameter across all years
mean_nyc_temp <- nyc_temp %>%
  group_by(parameter, month) %>%
  summarize(mean_measurement = mean(measurement, na.rm = TRUE)) %>%
  ungroup()

# calculate mean and standard error of temperature anomaly per year
nyc_temp_anomaly_plot <- nyc_temp %>%
  inner_join(mean_nyc_temp, by = c("parameter", "month")) %>%
  mutate(anomaly = measurement - mean_measurement) %>%
  group_by(parameter, year) %>%
  summarize(mean_anomaly = mean(anomaly, na.rm = TRUE),
            sem_anomaly = sd(anomaly, na.rm = TRUE)/sqrt(12)) %>%
  ungroup() %>%
  filter(parameter %in% c("avg.hi.temp", "avg.lo.temp")) %>%
  mutate(parameter = factor(parameter, levels = c("avg.hi.temp", "avg.lo.temp"),
                            labels = c("Average annual high", 
                                       "Average annual low")))

# plot the temperature anomaly
p <- nyc_temp_anomaly_plot %>%
  ggplot(aes(x = year, y = mean_anomaly)) +
  geom_ribbon(aes(ymin = mean_anomaly - sem_anomaly,
                  ymax = mean_anomaly + sem_anomaly),
              alpha = 0.2, fill = "red", col = NA, show.legend = FALSE) +
  geom_hline(yintercept = 0, col = "grey") +
  geom_line(size = 1, col = "red") +
  geom_smooth(method = "lm", col = "blue", size = 1, se = FALSE) +
  scale_x_continuous(breaks = seq(1800, 2100, 10)) +
  facet_wrap(~parameter, nrow = 2) +
  labs(x = "Year", y = "Temperature anomaly (°C)", col = "",
       title = "Temperatures in New York City (1876-2019)") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(colour = "black", size = 15),
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text.x = element_text(size = 15),
        plot.title = element_text(size = 17),
        axis.title = element_text(size = 17),
        aspect.ratio = 1/3)
ggsave(p, file = "temp_NYC.pdf", width = 10, height = 6)

# get linear regression report
lm_report <- nyc_temp_anomaly_plot %>%
  split(.$parameter) %>%
  map(function(x) {
    lm_res <- lm(formula = mean_anomaly ~ year, data = x)
  })
lm_report

# get stats
lm_report %>%
  map(broom::glance) %>%
  bind_rows(.id = "parameter") %>%
  select(parameter, adj.r.squared, p.value)
