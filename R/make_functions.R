#' Make functions
#'
#' These functions make different objects that can be exported like plots and summary tables.
#'
#' @param raw_log a `data.frame` containing raw psat logs for an individual.
#' @param output_dir a `character` string containing the path to save plot or table.
#'
#' @export

make_summary_plot <- function(raw_log, output_dir = NULL) {
  pdt_long <- raw_log |>
    dplyr::select(date_time, depth, temperature) |>
    tidyr::pivot_longer(
      cols = c(depth, temperature),
      names_to = "variable",
      values_to = "value"
    ) |>
    # Source - https://stackoverflow.com/a/32758968
    # Posted by irJvV
    # Retrieved 2026-08-29, License - CC BY-SA 3.0
    dplyr::mutate(
      variable = paste0(
        toupper(substr(variable, 1, 1)),
        substr(variable, 2, nchar(variable))
      )
    )

  fish_id <- unique(raw_log$fish_id)

  profile <- ggplot2::ggplot(
    pdt_long,
    ggplot2::aes(x = date_time, y = value, colour = variable)
  ) +
    ggplot2::facet_wrap(~variable, scales = "free", ncol = 1) +
    ggplot2::geom_point(size = 1) +
    ggplot2::scale_x_datetime(
      labels = scales::date_format("%m/%y"),
      date_breaks = "1 month"
    ) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      legend.position = "none",
      strip.background = ggplot2::element_blank()
    ) +
    ggplot2::labs(x = "Date (mm/yy)", title = fish_id)

  if (!is.null(output_dir)) {
    path <- file.path(
      output_dir,
      paste0("depth_profile_", fish_id, ".png")
    )
    ggplot2::ggsave(
      filename = path,
      plot = profile,
      width = 11,
      height = 8.5,
    )

    cli::cli_alert_success(
      "Saved plot at the following location {.path {path}}"
    )
  }
  return(profile)
}

#' @param raw_log a `data.frame` containing raw psat logs for an individual.
#' @param output_dir a `character` string containing the path to save plot or table.
#'
#' @export

make_summary_table <- function(raw_log, date_range, time_res, window) {
  t0 <- msg_start("summary")

  # ---- first we need to create time resoluation and sequence
  # for eavery day end....
  raw_log_res <- raw_log |>
    dplyr::mutate(
      time_res = paste(time_res, "sec")
    ) |>
    dplyr::group_by(date) |>
    dplyr::mutate(
      day_end = as.POSIXct(
        paste(
          date,
          "23:59:59"
        ),
        tz = "UTC"
      )
    ) |>
    arrange(date_time)

  # ----- next we need to complete (expand.grid) on a sequency that takes the min date an time
  # for a given day
  raw_log_fill <- raw_log_res |>
    tidyr::complete(
      date_time = seq(
        from = min(date_time, na.rm = TRUE),
        to = day_end[1],
        by = time_res[1]
      )
    )

  summary_log <- raw_log_fill |>
    # 1. Compute rolling metrics across regular time grid
    mutate(
      depth_run = slide_dbl(
        depth,
        safe_max,
        .before = window[1] - 1,
        .after = 0,
        .complete = FALSE
      ),
      depth_run = if_else(!is.na(depth), depth_run, NA_real_)
    ) |>
    # 2. Summarize depth & temperature per date
    summarize(
      # Handle all-NA depth days cleanly
      depth_min = if (all(is.na(depth))) {
        NA_real_
      } else {
        min(depth_run, na.rm = TRUE)
      },
      depth_max = if (all(is.na(depth))) {
        NA_real_
      } else {
        max(depth_run, na.rm = TRUE)
      },

      # Extract valid temperatures within the day's depth range
      temp_min = {
        # Strip out padded NA values from raw daily temperatures first
        valid_temps <- temperature[!is.na(temperature)]

        if (all(is.na(depth)) || length(valid_temps) == 0) {
          # Fallback for all-NA depth days or missing depth bounds
          safe_range_val(valid_temps, "min")
        } else {
          # Filter temps within positive depth bounds [lower_bound, upper_bound]
          d_lower <- min(depth_min, depth_max)
          d_upper <- max(depth_min, depth_max)

          sub_t <- temperature[
            !is.na(temperature) &
              !is.na(depth) &
              depth >= d_lower &
              depth <= d_upper
          ]

          if (length(sub_t) > 0) {
            min(sub_t)
          } else {
            safe_range_val(valid_temps, "min")
          }
        }
      },
      temp_max = {
        valid_temps <- temperature[!is.na(temperature)]

        if (all(is.na(depth)) || length(valid_temps) == 0) {
          safe_range_val(valid_temps, "max")
        } else {
          d_lower <- min(depth_min, depth_max)
          d_upper <- max(depth_min, depth_max)

          sub_t <- temperature[
            !is.na(temperature) &
              !is.na(depth) &
              depth >= d_lower &
              depth <= d_upper
          ]

          if (length(sub_t) > 0) {
            max(sub_t)
          } else {
            safe_range_val(valid_temps, "max")
          }
        }
      },
      .groups = "drop"
    )

  t1 <- Sys.time()
  cli::cli_alert_success(
    "Daily summary took {round(as.numeric(difftime(t1, t0, units = 'mins')), 2)} minutes"
  )
  return(summary_log)
}

# iniloc <- iniloc[!is.na(iniloc$date), ]
# t0 <- Sys.time()
# print(paste("Starting daily summary..."))

# day <- seq.Date(as.Date(rg.date[1]), as.Date(rg.date[2]), by = 'day')

# time_res <- 630

# pdt_binned <- pdt |>
#   group_by(date) |>
#   reframe(
#     time_bin = seq.POSIXt(
#       from = min(date_time, na.rm = TRUE),
#       to = as.POSIXct(paste(date[1], "23:59:59"), tz = "UTC"),
#       by = time_res
#     )
#   )

library(dplyr)

library(slider)

time_res <- 630
window <- round(3600 / time_res)

# Helper function to avoid max() warnings on all-NA windows

raw_log <- pdt
iniloc_f <- iniloc |>
  dplyr::filter(fish %in% raw_log$fish_id)
#
tag_date <- iniloc_f$date[iniloc_f$event == "tag"]

pop_date <- iniloc_f$date[iniloc_f$event == "pop"]

raw_log_filter <- raw_log |>
  dplyr::filter(date >= as.Date(tag_date), date <= as.Date(pop_date))

raw_log_res <- raw_log_filter |>
  dplyr::mutate(
    time_res = paste(time_res, "sec")
  ) |>
  dplyr::group_by(date) |>
  dplyr::mutate(
    day_end = as.POSIXct(
      paste(
        date,
        "23:59:59"
      ),
      tz = "UTC"
    )
  )

raw_log_fill <- raw_log_res |>
  tidyr::complete(
    date_time = seq(
      from = min(date_time, na.rm = TRUE),
      to = day_end[1],
      by = time_res[1] # your time_res
    )
  )

summary_log <- raw_log_fill |>
  # 2. Compute rolling metrics across the regular time grid
  dplyr::mutate(
    # 1. Safe rolling max depth
    depth_run = slider::slide_dbl(
      depth,
      safe_max,
      .before = window[1] - 1,
      .after = 0,
      .complete = FALSE
    ),
    depth_run = dplyr::if_else(!is.na(depth), depth_run, NA_real_)
  ) |>
  dplyr::summarize(
    depth_min = if (all(is.na(depth))) {
      NA_real_
    } else {
      safe_range_val(depth_run, "min")
    },
    depth_max = if (all(is.na(depth))) {
      NA_real_
    } else {
      safe_range_val(depth_run, "max")
    },

    # Subset temperatures matching depth range
    temp_subset = list(temperature[
      depth >= depth_min & depth <= depth_max & !is.na(temperature)
    ]),

    # 2. Safe temperature ranges with fallbacks
    temp_min = {
      sub_vals <- temp_subset[[1]]
      if (length(sub_vals) > 0) {
        min(sub_vals)
      } else {
        safe_range_val(temperature, "min")
      }
    },
    temp_max = {
      sub_vals <- temp_subset[[1]]
      if (length(sub_vals) > 0) {
        max(sub_vals)
      } else {
        safe_range_val(temperature, "max")
      }
    },
    .groups = "drop"
  ) |>
  dplyr::select(-temp_subset)


library(dplyr)
library(tidyr)
library(slider)

summary_log <- raw_log_fill |>
  # 1. Compute rolling metrics across regular time grid
  mutate(
    depth_run = slide_dbl(
      depth,
      safe_max,
      .before = window[1] - 1,
      .after = 0,
      .complete = FALSE
    ),
    depth_run = if_else(!is.na(depth), depth_run, NA_real_)
  ) |>
  # 2. Summarize depth & temperature per date
  summarize(
    # Handle all-NA depth days cleanly
    depth_min = if (all(is.na(depth))) {
      NA_real_
    } else {
      min(depth_run, na.rm = TRUE)
    },
    depth_max = if (all(is.na(depth))) {
      NA_real_
    } else {
      max(depth_run, na.rm = TRUE)
    },

    # Extract valid temperatures within the day's depth range
    temp_min = {
      # Strip out padded NA values from raw daily temperatures first
      valid_temps <- temperature[!is.na(temperature)]

      if (all(is.na(depth)) || length(valid_temps) == 0) {
        # Fallback for all-NA depth days or missing depth bounds
        safe_range_val(valid_temps, "min")
      } else {
        # Filter temps within positive depth bounds [lower_bound, upper_bound]
        d_lower <- min(depth_min, depth_max)
        d_upper <- max(depth_min, depth_max)

        sub_t <- temperature[
          !is.na(temperature) &
            !is.na(depth) &
            depth >= d_lower &
            depth <= d_upper
        ]

        if (length(sub_t) > 0) {
          min(sub_t)
        } else {
          safe_range_val(valid_temps, "min")
        }
      }
    },
    temp_max = {
      valid_temps <- temperature[!is.na(temperature)]

      if (all(is.na(depth)) || length(valid_temps) == 0) {
        safe_range_val(valid_temps, "max")
      } else {
        d_lower <- min(depth_min, depth_max)
        d_upper <- max(depth_min, depth_max)

        sub_t <- temperature[
          !is.na(temperature) &
            !is.na(depth) &
            depth >= d_lower &
            depth <= d_upper
        ]

        if (length(sub_t) > 0) {
          max(sub_t)
        } else {
          safe_range_val(valid_temps, "max")
        }
      }
    },
    .groups = "drop"
  )

summary_log |>
  tail()

load(
  "/Users/benhlina/Library/CloudStorage/Dropbox/Dal-Post Doc/data/Geolocation for Jena/summaries/sm.transmit.199637.RData"
)

dplyr::as_tibble(sm) |>
  tail()

library(dplyr)

sm_tbl <- as_tibble(sm$sm)

# make sure both dates are the same type (Date vs POSIXct can silently break joins)
sm_tbl <- sm_tbl |>
  mutate(date = as.Date(day))

summary_log <- summary_log |>
  mutate(date = as.Date(date))

# dates in sm but NOT in summary_log
in_sm_not_log <- sm_tbl |>
  anti_join(summary_log, by = "date") |>
  arrange(date)

# dates in summary_log but NOT in sm
in_log_not_sm <- summary_log |>
  anti_join(sm_tbl, by = "date") |>
  arrange(date)

nrow(in_sm_not_log)

nrow(in_log_not_sm)

in_sm_not_log |> select(date)
in_log_not_sm |> select(date)

sm |>
  filter(date %in% in_sm_not_log$date)
in_sm_not_log |>
  print(n = 3000)

t <- pdt |>
  group_by(date) |>
  summarise(
    min_depth = min(depth, na.rm = TRUE) * -1,
    max_depth = max(depth, na.rm = TRUE) * -1,
    min_temp = min(temperature, na.rm = TRUE),
    max_temp = max(temperature, na.rm = TRUE)
  )

sm_tbl

library(ggplot2)
summary_log <- summary_log |>
  mutate(
    depth_min = depth_min * -1,
    depth_max = depth_max * -1
  )

ggplot() +
  # geom_point(data = t, aes(x = date, y = min_depth), colour = "green") +
  geom_point(data = sm_tbl, aes(x = date, y = Temp_max), colour = "red") +
  geom_point(
    data = summary_log,
    aes(x = date, y = temp_max),
    shape = 21,
    size = 4,
    fill = NA
  )
