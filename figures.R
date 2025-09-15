
library(tidyverse)
library(vars)
# library(extrafont)


source("import_data.R")

main_color <- "steelblue"

# Fix encoding issues for Cyrillic characters
options(encoding = "UTF-8")
Sys.setlocale("LC_ALL", "Bulgarian_Bulgaria.utf8")

# Set global theme
theme_set(theme_minimal() + 
  theme(
    text = element_text(family = "Times New Roman", colour = "black", size = 14),
    plot.title = element_text(family = "Times New Roman", colour = "black"),
    axis.title = element_text(family = "Times New Roman", colour = "black", size=14),
    legend.text = element_text(family = "Times New Roman", colour = "black", size = 14),
    axis.text = element_text(colour = "black", size=14),
    strip.text = element_text(colour = "black", size=14)
  ))

# Set default colors for geoms
update_geom_defaults("point", list(colour = main_color))
update_geom_defaults("line", list(colour = main_color))
update_geom_defaults("col", list(fill = main_color))
update_geom_defaults("ribbon", list(fill = main_color))
update_geom_defaults("bar", list(fill = main_color))
update_geom_defaults("errorbarh", list(color = main_color))

save_plots <- function(plot, filename, formats = c("png", "pdf", "svg", "jpeg"), width = 8, height = 6, dpi = 600) {
  for (fmt in formats) {
    output_file <- file.path("figures", paste0(filename, ".", fmt))
    if (fmt == "pdf") {
    ggsave(
      filename = output_file,
      plot = plot,
      device = cairo_pdf,
      width = width,
      height = height,
      dpi = dpi
    )      
    } else {
     ggsave(
      filename = output_file,
      plot = plot,
      device = fmt,
      width = width,
      height = height,
      dpi = dpi
    ) 
    }
  }
}

dt_abs_w <- dt_abs %>%
  pivot_wider(
    id_cols = c(date),
    names_from = Id,
    values_from = c(Rel, RelM),
  ) %>%
  rowwise() %>%
  mutate(
    RelStd = sd(c_across(starts_with("Rel_")), na.rm = TRUE),
    RelMStd = sd(c_across(starts_with("RelM_")), na.rm = TRUE),
  ) %>%
  ungroup() %>%
  left_join(
    dt_money_w %>% dplyr::select(date, M1, M2, Deposits3M, Deposits2Y),
    by = "date"
  )

# Fig 2
std_rel_prices_annual <- rel_annual %>%
  filter(
    Name == "StdDevAll"
  ) %>%
  ggplot(aes(x = Year, y = Value)) +
  geom_line(linewidth = 1) +
  labs(
    x = "Година",
    y = "Стандартно отклонение"
  ) + 
  scale_x_continuous(breaks = c(1981, 1985, 1991, 1997, 2000, 2005, 2009, 2015, 2020, 2024))

save_plots(std_rel_prices_annual, filename="fig-2-std-rel-prices-annual")

std_rel_prices_annual

# Fig 3

sector_summaries1 <- dtl %>%
    group_by(sector, before90) %>%
    summarize(
        min = min(rel_price, na.rm = TRUE),
        max = max(rel_price, na.rm = TRUE),
        mean = mean(rel_price, na.rm = TRUE),
        sd = sd(rel_price, na.rm = TRUE)
    )

fig_before_after <- sector_summaries1 %>%
    filter(sector != "Total") %>%
    mutate(
        sector = recode(sector, !!!sector_bg_str_short), # Convert sector names to Bulgarian
    ) %>%
    ggplot(aes(x = sd, y = reorder(sector, sd), fill = before90)) +
    geom_col(position = "dodge", alpha = 0.7) +
    labs(
        x = "Стандартно отклонение",
        y = "",
        fill = "Период"
    ) +
    theme(legend.position = "bottom")

save_plots(fig_before_after, "fig-3-std-dev-rel-prices-before-after-1990")

fig_before_after

# Fig 1

mean_diffs_sector <- dtl %>%
    filter(sector != "Total") %>%
    group_by(sector) %>%
    do({
        t_test <- t.test(rel_price ~ before90, data = ., var.equal = FALSE)
        tibble(
            sector = unique(.$sector),
            mean_diff = t_test$estimate[2] - t_test$estimate[1],
            stderr = t_test$stderr
        )
    })

# Plot the mean differences as a scatterplot with error bars

fig_diffs_sector <- mean_diffs_sector %>%
    mutate(
        sector = recode(sector, !!!sector_bg_str_short) # Convert sector names to Bulgarian
    ) %>%
    ggplot(aes(x = mean_diff, y = reorder(sector, mean_diff))) +
    geom_point(size = 3) +
    geom_errorbarh(
        aes(xmin = mean_diff - 2*stderr, xmax = mean_diff + 2*stderr),
        height = 0.2,
    ) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
    labs(
        # title = "Средна разлика на относителните цени преди и след 1990 г. по сектори",
        x = "Средна разлика",
        y = ""
    )

save_plots(fig_diffs_sector, "fig-1-average-rel-price-difference-before-after-1990")

fig_diffs_sector

# Fig 4

fit_rel_std_var <- function(d, start_date=as.Date("1996-01-01"), end_date=as.Date("2026-01-01"), ...) {
  d_tmp <- d %>%
    mutate(
      RelStdDiff = log(RelMStd) - dplyr::lag(log(RelMStd)),
      M2d = log(M2) - dplyr::lag(log(M2))
    ) %>%
    filter(!is.na(M2d), !is.na(RelStdDiff)) %>%
    filter(
      date > start_date,
      date < end_date
    ) %>%
    dplyr::select(RelStdDiff, M2d)
  
  VAR(d_tmp, ...)
}

var_model_std_m2 <- dt_abs_w %>%
  fit_rel_std_var(lag.max = 8, ic = "AIC", season=12)

irf_model_m2 <- irf(
    var_model_std_m2,
    impulse = "M2d",
    response = c("RelStdDiff"), 
    n.ahead = 24,
    boot = TRUE,
    ortho = TRUE
)

irf_data <- irf_model_m2$irf %>%
  as.data.frame() %>%
  mutate(period = 1:n()) %>%
  pivot_longer(
    cols = -period,
    names_to = "variable",
    values_to = "response"
  ) %>%
  # Extract shock and response variable names
  separate(variable, into = c("response_var", "shock_var"), sep = "\\.")

# Extract confidence intervals if available
if (!is.null(irf_model_m2$Lower)) {
  lower_data <- irf_model_m2$Lower %>%
    as.data.frame() %>%
    mutate(period = 1:n()) %>%
    pivot_longer(cols = -period, names_to = "variable", values_to = "lower") %>%
    separate(variable, into = c("response_var", "shock_var"), sep = "\\.")
  
  upper_data <- irf_model_m2$Upper %>%
    as.data.frame() %>%
    mutate(period = 1:n()) %>%
    pivot_longer(cols = -period, names_to = "variable", values_to = "upper") %>%
    separate(variable, into = c("response_var", "shock_var"), sep = "\\.")
  
  plot_data <- irf_data %>%
    left_join(lower_data, by = c("period", "response_var", "shock_var")) %>%
    left_join(upper_data, by = c("period", "response_var", "shock_var"))
} else {
  plot_data <- irf_data
}

# Create the plot
irf_plot <- plot_data %>%
  mutate(
    shock_var = "",
    response_var = ""
  ) %>%
  ggplot(aes(x = period, y = response)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  {if("lower" %in% names(plot_data)) 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)
  } +
  facet_grid(response_var ~ shock_var, 
             scales = "free_y",
            ) +
  labs(
    title = "",
    x = "Месеци",
    y = "Промяна"
  )

save_plots(irf_plot, "fig-4-irf-std-dev-money", width = 12, height = 8)

irf_plot

# Fig 5

fit_rel_m_var <- function(d, start_date=as.Date("1996-01-01"), end_date=as.Date("2026-01-01"), ...) {
  d_tmp <- d %>%
    mutate(
      Food = RelM_Food,
      Machine = RelM_Machinery,
      M2d = log(M2) - dplyr::lag(log(M2))
    ) %>%
    filter(!is.na(M2d), !is.na(Food)) %>%
    filter(
      date > start_date,
      date < end_date
    ) %>%
    dplyr::select(Food, Machine, M2d)
  
  VAR(d_tmp, ...)
}

var_fm_m <- dt_abs_w %>%
  fit_rel_m_var(lag.max = 8, ic = "AIC")

irf_var_fm_m <- irf(
    var_fm_m, 
    impulse = "M2d",
    response = c("Food", "Machine"),
    n.ahead = 32,
    boot = TRUE,
    ortho = TRUE
)

irf_data_sectors <- irf_var_fm_m$irf %>%
  as.data.frame() %>%
  mutate(period = 1:n()) %>%
  pivot_longer(
    cols = -period,
    names_to = "variable",
    values_to = "response"
  ) %>%
  # Extract shock and response variable names
  separate(variable, into = c("response_var", "shock_var"), sep = "\\.")

if (!is.null(irf_var_fm_m$Lower)) {
  lower_data <- irf_var_fm_m$Lower %>%
    as.data.frame() %>%
    mutate(period = 1:n()) %>%
    pivot_longer(cols = -period, names_to = "variable", values_to = "lower") %>%
    separate(variable, into = c("response_var", "shock_var"), sep = "\\.")
  
  upper_data <- irf_var_fm_m$Upper %>%
    as.data.frame() %>%
    mutate(period = 1:n()) %>%
    pivot_longer(cols = -period, names_to = "variable", values_to = "upper") %>%
    separate(variable, into = c("response_var", "shock_var"), sep = "\\.")
  
  plot_data_sectors <- irf_data_sectors %>%
    left_join(lower_data, by = c("period", "response_var", "shock_var")) %>%
    left_join(upper_data, by = c("period", "response_var", "shock_var"))
} else {
  plot_data_sectors <- irf_data_sectors
}

# Create the plot
irf_plot_sectors <- plot_data_sectors %>%
  mutate(
    response_var = "",
    shock_var = ifelse(
      shock_var == "Food", "Хранителна промишленост",
      "Машиностроене"
    )
  ) %>%
  ggplot(aes(x = period, y = response)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  {if("lower" %in% names(plot_data)) 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2)
  } +
  facet_grid(response_var ~ shock_var) + 
  labs(
    title = "",
    x = "Месеци",
    y = "Промяна"
  )

save_plots(
    irf_plot_sectors,
    "fig-5-irf-relative-prices-money",
    width = 12,
    height = 8
)
