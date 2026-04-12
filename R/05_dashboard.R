summarize_colony_dashboard <- function(df) {
  if (nrow(df) == 0) {
    return(list(
      active = 0L,
      breeders = 0L,
      experiment_ready = 0L,
      reservations = 0L,
      recent_births = 0L,
      recent_ended = 0L
    ))
  }

  today <- Sys.Date()
  list(
    active = sum(df$alive %in% TRUE, na.rm = TRUE),
    breeders = sum(df$alive %in% TRUE & df$is_breeder %in% TRUE, na.rm = TRUE),
    experiment_ready = sum(df$alive %in% TRUE & df$experiment_ready %in% TRUE, na.rm = TRUE),
    reservations = sum(df$reservation_status %in% c("reserved", "hold"), na.rm = TRUE),
    recent_births = sum(!is.na(df$dob) & df$dob >= today - 30, na.rm = TRUE),
    recent_ended = sum(!is.na(df$end_date) & df$end_date >= today - 30, na.rm = TRUE)
  )
}

empty_action_queue <- function() {
  tibble::tibble(
    priority = character(),
    action = character(),
    mouse_id = character(),
    detail = character()
  )
}

genotype_distribution_by_line <- function(df, top_n = 10) {
  df |>
    dplyr::filter(alive %in% TRUE, !is.na(mouse_line), !is.na(raw_genotype)) |>
    dplyr::count(mouse_line, raw_genotype, sort = TRUE) |>
    dplyr::group_by(mouse_line) |>
    dplyr::slice_max(n, n = top_n, with_ties = FALSE) |>
    dplyr::ungroup()
}

birth_death_timeline <- function(df, months_back = 6) {
  start_date <- Sys.Date() - (months_back * 30)

  births <- df |>
    dplyr::filter(!is.na(dob), dob >= start_date) |>
    dplyr::transmute(event_month = as.Date(format(dob, "%Y-%m-01")), event_type = "Birth")

  deaths <- df |>
    dplyr::filter(!is.na(end_date), end_date >= start_date) |>
    dplyr::transmute(event_month = as.Date(format(end_date, "%Y-%m-01")), event_type = "Ended")

  dplyr::bind_rows(births, deaths) |>
    dplyr::count(event_month, event_type, name = "count")
}

build_action_queue <- function(df) {
  if (nrow(df) == 0) {
    return(empty_action_queue())
  }

  missing_parents <- df |>
    dplyr::filter(alive %in% TRUE, (is.na(sire_id) | is.na(dam_id))) |>
    dplyr::transmute(
      priority = "High",
      action = "Missing parent metadata",
      mouse_id,
      detail = "Live mouse is missing sire or dam information."
    )

  aging_pups <- df |>
    dplyr::filter(alive %in% TRUE, !is.na(age_days), age_days >= 18, age_days <= 28) |>
    dplyr::transmute(
      priority = "Medium",
      action = "Aging pup check",
      mouse_id,
      detail = paste("Mouse is", age_label, "old and likely needs weaning or cohort review.")
    )

  reservation_conflicts <- df |>
    dplyr::filter(reservation_status %in% c("reserved", "hold"), !alive %in% TRUE) |>
    dplyr::transmute(
      priority = "High",
      action = "Reservation conflict",
      mouse_id,
      detail = "Mouse is no longer active but still has an active reservation."
    )

  dplyr::bind_rows(missing_parents, aging_pups, reservation_conflicts) |>
    dplyr::arrange(factor(priority, levels = c("High", "Medium", "Low")), mouse_id)
}

plot_genotype_distribution <- function(df) {
  plot_df <- genotype_distribution_by_line(df)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No genotype data available"))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = stats::reorder(mouse_line, n), y = n, fill = raw_genotype)) +
    ggplot2::geom_col(position = "stack") +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Active mice", fill = "Genotype", title = "Genotype Distribution By Line") +
    ggplot2::guides(fill = ggplot2::guide_legend(ncol = 2)) +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot",
      axis.text.y = ggplot2::element_text(size = 11)
    )
}

plot_birth_death_timeline <- function(df) {
  plot_df <- birth_death_timeline(df)
  if (nrow(plot_df) == 0) {
    return(ggplot2::ggplot() + ggplot2::theme_void() + ggplot2::labs(title = "No recent events"))
  }

  ggplot2::ggplot(plot_df, ggplot2::aes(x = event_month, y = count, fill = event_type)) +
    ggplot2::geom_col(position = "dodge") +
    ggplot2::labs(x = NULL, y = "Count", fill = NULL, title = "Births And Ended Mice") +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::theme(
      legend.position = "bottom",
      plot.title.position = "plot"
    )
}
