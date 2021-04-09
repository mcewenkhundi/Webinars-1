library(tidyverse)
library(NHSRdatasets)

# file management http://theautomatic.net/2018/07/11/manipulate-files-r/#:~:text=Files%20can%20be%20deleted%20with,add%20the%20parameter%20recursive%20%3D%20TRUE.
#https://fs.r-lib.org/index.html
plot_fn <- function(org_code, data) {
  data %>%
    ggplot(mapping = aes(x = period, y = attendances)) +
    geom_point() +
    geom_line() +
    labs(title = org_code)
}

unlink(here::here("figures"), recursive = TRUE)

dir_create <- function(file_path = here::here("figures")) {
  if (dir.exists(file_path)) {
    unlink(file_path, recursive = TRUE)
    dir.create(file_path)
  } else {
    dir.create(file_path)
  }
}

dir_create()


ae_attendances %>%
  filter(org_code %>%
    str_starts("R")) %>%
  group_by(org_code, period) %>%
  summarise(across("attendances", sum)) %>%
  group_nest() %>%
  filter(map_dbl(data, nrow) == 36) %>%
  # ungroup() %>%
  mutate(
    plot = map2(org_code, data, plot_fn),
    filename = paste0(here::here(), "/figures/", org_code, ".png"),
  ) %>%
  select(filename, plot) %>%
  head(3) %>%
  mutate(nothing = walk2(filename, plot, ggsave))
# understanding the use of walk inside and outside of plots
walk2(ggsave)










groups(.Last.value)
