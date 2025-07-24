#' Import data from tennis-data.co.uk

import_tdcu <- function(year, side = c("atp", "wta")) {
  dat_list <- vector("list", length(year))
  for (i in 1:length(year)) {
    xls <- glue("data/{side}/{year[i]}.xls")
    xlsx <- glue("data/{side}/{year[i]}.xlsx")
    if (file.exists(xls)) {
      dat_list[[i]] <- readxl::read_excel(xls, sheet=2, na = c("", "N/A", "NA"))
    } else if (file.exists(xlsx)) {
      dat_list[[i]] <- readxl::read_excel(xlsx, na = c("", "N/A", "NA"))
    }
    if (class(dat_list[[i]]$Date)[1] == 'character') {
      dat_list[[i]]$Date <- lubridate::parse_date_time(dat_list[[i]]$Date, orders=c("ymd", "mdy"))
    }
    names(dat_list[[i]])[grep("Best", names(dat_list[[i]]))] <- "BestOf"
    dat_list[[i]] <- dat_list[[i]][, c("Wsets", "Lsets", "Winner", "Loser", "Surface", "Date")]
  }
  dat <- rbindlist(dat_list) |>
    _[!is.na(Wsets) & !is.na(Lsets)]
  dat$Winner <- fix_name(dat$Winner)
  dat$Loser <- fix_name(dat$Loser)

  # Prepare to return
  monthChar <- month(dat$Date)
  monthChar[nchar(monthChar)==1] <- paste0("0", monthChar[nchar(monthChar)==1])
  TimeFactor <- as.factor(paste(year(dat$Date), monthChar, sep="-"))

  list(
    dat = dat,
    PlayerID = unique(c(dat$Winner, dat$Loser)),
    SurfaceID = c("Hard", "Clay", "Grass"),
    Winner = match(dat$Winner, PlayerID),
    Loser <- match(dat$Loser, PlayerID),
    Surface <- match(dat$Surface, SurfaceID),
    Time <- as.numeric(TimeFactor),
    TimeID <- levels(TimeFactor)
  )
}
