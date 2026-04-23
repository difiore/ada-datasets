#' @title GPX Visualizer
#' @description Allows a user to select, load, and visualize a GPX file. If no filename is specified, it opens an interactive file chooser so the user can select one. The function then parses the track points and renders an interactive map with an elevation profile panel below it (if the GPX file includes elevation data).
#' @param file_path Optional. A path to a GPX file. If \code{NULL} (the default), a system file-chooser dialog is opened so the user can browse for the file interactively.
#' @param color_by One of \code{"elevation"} (default) or \code{"speed"}. Controls which variable is used to color the track line.
#' @return Invisibly returns a list with two elements:
#'   \describe{
#'     \item{track}{A \code{data.frame} of parsed track points.}
#'     \item{map}{The \code{leaflet} map widget.}
#'   }
#'
#' @details The function requires the following packages to be installed: \code{sf}, \code{leaflet}, \code{ggplot2}, and \code{rstudioapi}. Install any missing ones with \code{install.packages()}.
#'
#' GPX files can contain waypoints, routes, and/or tracks.  This function reads \emph{track} segments and (\code{<trkpt>} elements). If the file contains no track data an informative error is thrown.
#'
#' @examples
#' \dontrun{
#' # Interactive file picker
#' result <- explore_gpx()
#'
#' # Supply a path directly
#' result <- explore_gpx("my_hike.gpx", color_by = "elevation")
#'
#' # Access the parsed data frame
#' head(result$track)
#' }
#'
#' @export

plotgpx <- function(file_path = NULL, color_by = c("elevation", "speed")) {

  # ── 0. Check dependencies ----
  required_pkgs <- c("sf", "lubridate", "xml2", "leaflet", "ggplot2", "rstudioapi", "base64enc")
  missing_pkgs  <- required_pkgs[!vapply(required_pkgs, requireNamespace, quietly = TRUE, FUN.VALUE = logical(1))]
  if (length(missing_pkgs) > 0) {
    stop(
      "The following packages are required but not installed:\n  ",
      paste(missing_pkgs, collapse = ", "), "\n",
      "Install them with: install.packages(c(",
      paste0('"', missing_pkgs, '"', collapse = ", "), "))"
    )
  }

  color_by <- match.arg(color_by)

  # ── 1. File selection ----
  if (is.null(file_path)) {
    if (!requireNamespace("rstudioapi", quietly = TRUE) ||
        !rstudioapi::isAvailable()) {
      stop("rstudioapi is not available. Please supply a file_path directly, or run this function inside RStudio.")
    }
    message("Opening file chooser — please select a GPX file.")
    file_path <- rstudioapi::selectFile(
      caption    = "Select a GPX file",
      label      = "Select",
      filter     = "GPX Files (*.gpx)"
    )
    if (is.null(file_path) || !nzchar(file_path)) {
      stop("No file selected.")
    }
  }

  if (!file.exists(file_path)) {
    stop("File not found: ", file_path)
  }
  if (!grepl("\\.gpx$", file_path, ignore.case = TRUE)) {
    warning("The selected file does not have a .gpx extension... attempting to parse anyway")
  }

  # ── 2. Load GPX via {sf} ----
  message("Loading: ", basename(file_path))

  # sf exposes each GPX layer separately; "track_points" gives one row per point with ele and time already parsed as numeric and POSIXct
  pts_sf <- tryCatch(
    sf::st_read(file_path, layer = "track_points", quiet = TRUE),
    error = function(e) stop(
      "Could not read 'track_points' layer from this GPX file.\n",
      "Original error: ", conditionMessage(e)
    )
  )

  if (nrow(pts_sf) == 0) {
    stop("No track points found in this GPX file.")
  }

  # Extract lon/lat from the sfc geometry column and build a plain data frame
  coords    <- sf::st_coordinates(pts_sf)
  track     <- as.data.frame(pts_sf)
  track$lon <- coords[, "X"]
  track$lat <- coords[, "Y"]

  # Ensure ele and time columns exist even if absent from the file
  if (!"ele"  %in% names(track)) track$ele  <- NA_real_
  if (!"time" %in% names(track)) track$time <- as.POSIXct(NA)

  # GDAL's GPX driver zeros the time component before sf ever sees it. We extract the raw ISO 8601 strings directly from the XML and parse with lubridate::ymd_hms(), which handles the format reliably.
  raw_xml    <- xml2::read_xml(file_path)
  trkpts_xml <- xml2::xml_find_all(raw_xml, "//*[local-name()='trkpt']")
  time_strs  <- vapply(trkpts_xml, function(n) {
    ch <- xml2::xml_find_first(n, "*[local-name()='time']")
    if (inherits(ch, "xml_node")) xml2::xml_text(ch) else NA_character_
  }, character(1))
  track$time <- lubridate::ymd_hms(time_strs, tz = "UTC")

  # ── 3. Derived metrics ----
  # Haversine distance (metres) between consecutive points
  haversine <- function(lat1, lon1, lat2, lon2) {
    R   <- 6371000  # Earth radius in metres
    phi <- (lat2 - lat1) * pi / 180
    lam <- (lon2 - lon1) * pi / 180
    a   <- sin(phi / 2)^2 +
      cos(lat1 * pi / 180) * cos(lat2 * pi / 180) * sin(lam / 2)^2
    2 * R * asin(sqrt(a))
  }

  n <- nrow(track)
  dist_m <- c(0, haversine(track$lat[-n], track$lon[-n],
                           track$lat[-1],  track$lon[-1]))
  track$dist_cumulative_km <- cumsum(dist_m) / 1000

  # Speed (km/hr) — only meaningful when timestamps are present
  if (!all(is.na(track$time))) {
    dt_s <- c(NA, as.numeric(diff(track$time), units = "secs"))
    track$speed <- ifelse(dt_s > 0, (dist_m / dt_s) * 3.6, NA)
  } else {
    track$speed <- NA_real_
  }

  # ── 4. Color palette for the map ----
  color_var <- if (color_by == "speed" && !all(is.na(track$speed))) {
    track$speed
  } else {
    track$ele
  }

  use_color <- !all(is.na(color_var))

  if (use_color) {
    color_domain <- range(color_var, na.rm = TRUE)
    pal <- leaflet::colorNumeric(
      palette  = "RdYlGn",
      domain   = color_domain,
      na.color = "#888888"
    )
    track_color <- pal(color_var)
  } else {
    message("Note: no elevation or speed data found; track will be drawn in a fixed colour.")
    pal         <- NULL
    track_color <- rep("#e76f51", n)
  }

  # ── 5. Build the interactive map ----
  mid_lat <- mean(track$lat, na.rm = TRUE)
  mid_lon <- mean(track$lon, na.rm = TRUE)

  map <- leaflet::leaflet(track) |>
    leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron, options = leaflet::providerTileOptions(opacity = 0.85)) |>
    leaflet::fitBounds(
      lng1 = min(track$lon, na.rm = TRUE),
      lat1 = min(track$lat, na.rm = TRUE),
      lng2 = max(track$lon, na.rm = TRUE),
      lat2 = max(track$lat, na.rm = TRUE)
    )

  # Draw each segment individually so each gets its own colour
  for (i in seq_len(n - 1)) {
    map <- leaflet::addPolylines(
      map,
      lng     = track$lon[c(i, i + 1)],
      lat     = track$lat[c(i, i + 1)],
      color   = track_color[i],
      weight  = 4,
      opacity = 0.9,
      popup   = paste0(
        "<b>Point</b> ", i, "<br>",
        "Lat: ",   round(track$lat[i], 5), "<br>",
        "Lon: ",   round(track$lon[i], 5), "<br>",
        if (!is.na(track$ele[i]))   paste0("Elevation: ", round(track$ele[i],   1), " m<br>") else "",
        if (!is.na(track$speed[i])) paste0("Speed: ",     round(track$speed[i], 3), " km/h")  else ""
      )
    )
  }

  map <- map |>
    leaflet::addCircleMarkers(
      data = track[1, ],
      lng = ~lon, lat = ~lat,
      color = "green", radius = 8, stroke = TRUE,
      fillOpacity = 0.9,
      popup = "Start"
    ) |>
    leaflet::addCircleMarkers(
      data = track[n, ],
      lng = ~lon, lat = ~lat,
      color = "red", radius = 8, stroke = TRUE,
      fillOpacity = 0.9,
      popup = "End"
    )

  # ── 6. Elevation profile as leaflet control ──────────────────────────────────
  if (!all(is.na(track$ele))) {
    elev_plot <- ggplot2::ggplot(track,
                                 ggplot2::aes(x = dist_cumulative_km, y = ele)) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = min(ele, na.rm = TRUE), ymax = ele),
        fill = "#f4a261", alpha = 0.5
      ) +
      ggplot2::geom_line(colour = "#e76f51", linewidth = 1) +
      ggplot2::labs(
        title    = paste0("Elevation Profile — ", basename(file_path)),
        subtitle = sprintf(
          "Total distance: %.2f km  |  Gain: %+.0f m  |  Loss: %+.0f m",
          max(track$dist_cumulative_km, na.rm = TRUE),
          sum(pmax(diff(stats::na.omit(track$ele)), 0)),
          sum(pmin(diff(stats::na.omit(track$ele)), 0))
        ),
        x = "Distance (km)",
        y = "Elevation (m)"
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(face = "bold"),
        plot.subtitle = ggplot2::element_text(colour = "grey40")
      )

    # Render ggplot to a base64 PNG and embed as a leaflet control
    tmp_png <- tempfile(fileext = ".png")
    ggplot2::ggsave(tmp_png, elev_plot, width = 6, height = 1.5, dpi = 120)
    img_base64 <- base64enc::base64encode(tmp_png)
    elev_html <- htmltools::HTML(sprintf(
      '<style>
         .leaflet-bottom.leaflet-left { width: 100%%; pointer-events: none; }
         .leaflet-bottom.leaflet-left .leaflet-control {
           pointer-events: auto;
           position: absolute;
           left: 50%%;
           transform: translateX(-50%%);
           bottom: 0;
         }
       </style>
       <div style="background:white;padding:8px;border-radius:4px;
                   box-shadow:0 1px 5px rgba(0,0,0,0.4);">
         <img src="data:image/png;base64,%s" width="420"/>
       </div>', img_base64
    ))
    map <- leaflet::addControl(map, html = elev_html, position = "bottomleft")
  }

  print(map)

  # ── 7. Console summary ----
  message("\n──────── GPX Summary ────────")
  message(sprintf("  Track points   : %d", n))
  message(sprintf("  Total distance : %.3f km", max(track$dist_cumulative_km, na.rm = TRUE)))
  if (!all(is.na(track$ele))) {
    message(sprintf("  Min elevation  : %.1f m", min(track$ele, na.rm = TRUE)))
    message(sprintf("  Max elevation  : %.1f m", max(track$ele, na.rm = TRUE)))
  }
  if (!all(is.na(track$speed))) {
    message(sprintf("  Avg speed      : %.3f km/h", mean(track$speed, na.rm = TRUE)))
    message(sprintf("  Max speed      : %.3f km/h", max(track$speed,  na.rm = TRUE)))
  }
  message("─────────────────────────────\n")

  invisible(list(track = track, map = map))
}

