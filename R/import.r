#' Import data
#' 
#' @year Year (numeric)
#' @side atp/wta (string)
#' 
#' @examples
#' import(2017, "wta")

import <- function(year, side = c("atp", "wta")) {
  dat_list <- vector("list", length(year))
  for (i in 1:length(year)) {
    dat_list[[i]] <- fread(glue("data/{side}/{side}_matches_{year}.csv"))
    sets <- vapply(dat_list[[i]]$score, parse_score, numeric(2))
    dat_list[[i]][, winner_sets := sets[1,]]
    dat_list[[i]][, loser_sets := sets[2,]]
  }
  dat <- rbindlist(dat_list) |>
    _[score != "W/O"]
  dat[surface == "Carpet", surface := "Grass"]

  dat[, .(
      winner_sets,
      loser_sets,
      winner_name,
      loser_name,
      winner_id,
      loser_id,
      surface,
      time = anytime::anydate(dat$tourney_date) |>
        format.Date("%Y-%m")
  )]
}

#' Determine number of sets won by each player from score
parse_score <- function(score) {
  if (length(score) != 1) stop("score must be a single string", call. = FALSE)
  split_score <- score |>
    str_replace(" RET", "") |>
    str_replace(" DEF", "") |>
    str_split_1(" ") |> 
    purrr::map(str_split_1, pattern = "-")
  vapply(split_score, \(x) c(x[1] > x[2], x[2] > x[1]), numeric(2)) |>
    rowSums()
}
