.resolveIndexConflicts <- function(name, cityIndex, cities, bot) {
  if (length(cityIndex) == 0) {
    stop(paste0("There is no city called '", name, "' in the available data.\nCreate an issue including lat/long coordinates at https://github.com/koenderks/rcityviews/issues."))
  } else if (length(cityIndex) > 1) {
    if (bot) {
      index <- sample(1:length(cityIndex), size = 1)
    } else {
      index <- utils::menu(
        choices = paste0(cities[cityIndex, 1], ", ", cities[cityIndex, 2], " | Lat: ", round(cities[cityIndex, 3], 3), " | Long: ", round(cities[cityIndex, 4], 3)),
        title = "More than one city matched to this name, which one to pick?"
      )
      if (index == 0) {
        return()
      }
    }
    cityIndex <- cityIndex[index]
  }
  return(cityIndex)
}

.get_features <- function(osmbox, cropped, border, features) {
  feat <- osmdata::add_osm_features(opq = osmbox, features = features)
  spl <- strsplit(features, split = "=")[[1]]
  key <- gsub(pattern = '["\"]', replacement = "", x = spl[1])
  value <- gsub(pattern = '["\"]', replacement = "", x = spl[2])
  query <- osmdata::osmdata_sf(q = feat)
  if (!is.na(value) && value == "water") {
    # Remove unwanted water polygons (not exact match)
    query$osm_multipolygons <- subset(query$osm_multipolygons, query$osm_multipolygons[[key]] == value)
    query$osm_polygons <- subset(query$osm_polygons, query$osm_polygons[[key]] == value)
  }
  multipolygons <- .checkAndCrop(query$osm_multipolygons$geometry, cropped, border)
  polygons <- .checkAndCrop(query$osm_polygons$geometry, cropped, border)
  if (length(multipolygons) > 0 && length(polygons) > 0) {
    pp <- c(multipolygons, polygons)
  } else if (length(multipolygons) == 0) {
    pp <- polygons
  } else if (length(polygons) == 0) {
    pp <- multipolygons
  } else {
    pp <- NULL
  }
  lines <- .checkAndCrop(query$osm_lines$geometry, cropped, border)
  result <- list()
  result[["polygons"]] <- pp
  result[["lines"]] <- if (length(lines) > 0) lines else NULL
  return(result)
}

.tick <- function(progBar, verbose) {
  if (verbose) {
    progBar$tick()
  }
}

.checkAndCrop <- function(object, crop, border) {
  if (!is.null(object)) {
    object <- sf::st_make_valid(object)
    if (border != "none") {
      sf::st_crs(object) <- sf::st_crs(crop)
      object <- object |> sf::st_intersection(crop)
    }
  }
  return(object)
}

.make_circle <- function(long, lat, rlong, rlat, npoints = 10000) {
  tt <- seq(0, 2 * pi, length.out = npoints)
  xx <- long + rlong * cos(tt)
  yy <- lat + rlat * sin(tt)
  return(data.frame(x = xx, y = yy))
}

.theme_options <- function(theme) {
  lines.col <- switch(theme,
    "original" = "#32130f",
    "light" = "#000000",
    "dark" = "#ffffff",
    "colored" = "#eff0db",
    "rouge" = "#f2deb8",
    "verde" = "#284566",
    "neon" = "#0be8ed",
    "antique" = "#32130f",
    "atlantis" = "#ffffff",
    "lichtenstein" = "#2f3737"
  )
  background.col <- switch(theme,
    "original" = "#fdf9f5",
    "light" = "#fafafa",
    "dark" = "#000000",
    "colored" = lines.col,
    "rouge" = "#a25543",
    "verde" = "#6ca67a",
    "neon" = "#000000",
    "antique" = "#fff7d8",
    "atlantis" = lines.col,
    "lichtenstein" = "#ffffff"
  )
  water.col <- switch(theme,
    "original" = background.col,
    "light" = background.col,
    "dark" = "#fafafa",
    "colored" = "#b0e3cf",
    "rouge" = lines.col,
    "verde" = lines.col,
    "neon" = "#ec3b8d",
    "antique" = "#9ebfaa",
    "atlantis" = lines.col,
    "lichtenstein" = "#607ba4"
  )
  waterlines.col <- switch(theme,
    "original" = lines.col,
    "light" = lines.col,
    "dark" = water.col,
    "colored" = water.col,
    "rouge" = lines.col,
    "verde" = lines.col,
    "neon" = water.col,
    "antique" = water.col,
    "atlantis" = lines.col,
    "lichtenstein" = water.col
  )
  landuse.col <- switch(theme,
    "original" = background.col,
    "light" = background.col,
    "dark" = background.col,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "rouge" = background.col,
    "verde" = lines.col,
    "neon" = background.col,
    "antique" = background.col,
    "atlantis" = c("#8fbbe2", "#0072fe", "#0b448b", "#5699b5", "#485270"),
    "lichtenstein" = "#478f70"
  )
  text.col <- switch(theme,
    "original" = lines.col,
    "light" = lines.col,
    "dark" = lines.col,
    "colored" = "#000000",
    "rouge" = lines.col,
    "verde" = lines.col,
    "neon" = "#e7d073",
    "antique" = lines.col,
    "atlantis" = "#000000",
    "lichtenstein" = lines.col
  )
  rails.col <- switch(theme,
    "original" = lines.col,
    "light" = lines.col,
    "dark" = lines.col,
    "colored" = lines.col,
    "rouge" = lines.col,
    "verde" = lines.col,
    "neon" = text.col,
    "antique" = lines.col,
    "atlantis" = lines.col,
    "lichtenstein" = lines.col
  )
  buildings.col <- switch(theme,
    "original" = background.col,
    "light" = background.col,
    "dark" = background.col,
    "colored" = c("#8e76a4", "#a193b1", "#db9b33", "#e8c51e", "#ed6c2e"),
    "rouge" = background.col,
    "verde" = lines.col,
    "neon" = background.col,
    "atlantis" = landuse.col,
    "antique" = c("#facc87", "#f39848", "#f8c98c", "#f58762"),
    "lichtenstein" = "#f4d849"
  )
  font <- switch(theme,
    "original" = "Caveat",
    "light" = "Imbue",
    "dark" = "Imbue",
    "colored" = "Damion",
    "rouge" = "Oswald",
    "verde" = "Righteous",
    "neon" = "Neonderthaw",
    "atlantis" = "Ubuntu Mono",
    "antique" = "Fredericka the Great",
    "lichtenstein" = "Rampart One"
  )
  face <- if (theme %in% c("original", "verde", "rouge", "neon")) "bold" else "plain"
  neighborhood.font.col <- switch(theme,
    "original" = "#000000",
    "light" = "#000000",
    "dark" = "#ffffff",
    "colored" = "#32130f",
    "rouge" = "#32130f",
    "verde" = "#ffffff",
    "neon" = "#ffffff",
    "atlantis" = "#ffffff",
    "antique" = "#000000",
    "lichtenstein" = "#000000"
  )
  halftone.col <- switch(theme,
    "original" = "#000000",
    "light" = "#000000",
    "dark" = "#ffffff",
    "colored" = "#000000",
    "rouge" = "#000000",
    "verde" = "#ffffff",
    "neon" = "#ffffff",
    "atlantis" = "#000000",
    "antique" = "#000000",
    "lichtenstein" = "#000000"
  )
  opts <- list(
    lines = lines.col,
    background = background.col,
    water = water.col,
    water.line = waterlines.col,
    landuse = landuse.col,
    text = text.col,
    rails = rails.col,
    buildings = buildings.col,
    font = font,
    face = face,
    neighborhood = neighborhood.font.col,
    halftone = halftone.col
  )
  return(opts)
}

.with_halftone <- function(p, opts) {
  xseq1 <- seq(0, 1, length = 101)
  xseq2 <- xseq1 + xseq1[2] / 2
  xseq2 <- xseq2[xseq2 > 0 & xseq2 < 1]
  xseq <- c(xseq1, xseq2)
  xseq <- rep(xseq, times = 100)
  yseq <- rep(0, length(xseq1))
  for (i in 1:199) {
    if (i %% 2 == 0) {
      yseq <- c(yseq, rep(yseq[length(yseq)] + 0.005, length(xseq1)))
    } else {
      yseq <- c(yseq, rep(yseq[length(yseq)] + 0.005, length(xseq2)))
    }
  }
  hlist <- list("x" = xseq, "y" = yseq)
  p <- p + ggplot2::geom_point(data = data.frame(x = hlist[["x"]], y = hlist[["y"]]), mapping = ggplot2::aes(x = x, y = y), col = opts[["halftone"]], alpha = 0.1, size = 2, shape = 19)
  return(p)
}

.with_places <- function(p, box, border, crop, opts) {
  suppressWarnings({
    obj <- osmdata::osmdata_sf(q = osmdata::add_osm_feature(opq = box, key = "place", value = c("suburb", "quarter", "neighbourhood")))$osm_points
    obj <- sf::st_make_valid(obj)
    if (border != "none") {
      sf::st_crs(obj) <- sf::st_crs(crop)
      obj <- obj |> sf::st_intersection(crop)
    }
    obj <- obj[which(!is.na(obj$name)), ]
    df <- data.frame(name = obj$name, place = obj$place, x = unlist(lapply(obj$geometry, `[[`, 1)), y = unlist(lapply(obj$geometry, `[[`, 2)))
    df <- df[!is.na(df$place), ]
    df <- df[!duplicated(df$name), , drop = FALSE]
    if (nrow(df) > 0) {
      df <- df[rev(order(df$place)), ]
      p <- p + shadowtext::geom_shadowtext(data = df, mapping = ggplot2::aes(x = x, y = y, label = name), col = opts[["neighborhood"]], size = 10, check_overlap = TRUE, fontface = "bold.italic", bg.colour=opts[["background"]])
    }
  })
  return(p)
}

# Fix for 'osmplotr' package bug
# The following functions are taken over from 'osmplotr'
.line2poly <- function(obj, bbox) {
  if (!methods::is(obj$geometry, "sfc_LINESTRING")) {
    stop("obj must be class 'sf' with fields of class 'sfc_LINESTRING'")
  }
  g <- obj$geom
  bbox <- matrix(bbox, nrow = 2)
  colnames(bbox) <- c("min", "max")
  rownames(bbox) <- c("x", "y")
  head_tail <- t(sapply(g, function(x) rownames(x)[c(1, nrow(x))]))
  m2 <- match(head_tail[, 2], head_tail[, 1])
  m1 <- match(head_tail[, 1], head_tail[, 2])
  startidx <- which(is.na(m1))
  if (nrow(head_tail) > 1 & length(startidx) >= 1) {
    linkorders <- lapply(startidx, function(x, v) .unroll(x, v), v = m2)
    linkorders <- lapply(linkorders, function(x) x[!is.na(x)])
    links <- lapply(linkorders, function(x) head_tail[x, , drop = FALSE])
    head_tail <- head_tail[-unlist(linkorders), , drop = FALSE]
    links <- lapply(links, function(x, g) .lookup_ways(x, g), g = g)
  } else {
    links <- list()
  }
  to_become_polygons <- list()
  lidx <- 1
  while (nrow(head_tail) > 0) {
    m2 <- match(head_tail[, 2], head_tail[, 1])
    if (any(!is.na(m2))) {
      l1 <- .unroll_loop(1, m2)
      to_become_polygons[[lidx]] <- head_tail[l1, ]
      lidx <- lidx + 1
      head_tail <- head_tail[-l1, ]
    } else {
      head_tail <- head_tail[-1, ]
    }
  }
  to_become_polygons <- lapply(to_become_polygons, .lookup_ways, g = g)
  to_become_polygons <- lapply(to_become_polygons, .make_sf, g = g)
  to_become_polygons <- do.call(rbind, to_become_polygons)
  bbxcorners_rh <- c("NE", "SE", "SW", "NW")
  bbxcoords <- rbind(
    c(bbox[1, 2], bbox[2, 2]),
    c(bbox[1, 2], bbox[2, 1]),
    c(bbox[1, 1], bbox[2, 1]),
    c(bbox[1, 1], bbox[2, 2])
  )
  rownames(bbxcoords) <- bbxcorners_rh
  p1 <- p2 <- NULL
  if (length(links) >= 1) {
    links <- lapply(links, function(x, bbox) .clip_one(x, bbox), bbox = bbox)
    linkpoly <- lapply(links, .make_poly, bbox = bbox, g = g)
    p1 <- lapply(linkpoly, "[[", "p1")
    p2 <- lapply(linkpoly, "[[", "p2")
  }
  res <- NULL
  if (!is.null(p1) & !is.null(p2)) {
    res <- list(sea = do.call(rbind, p1), land = do.call(
      rbind,
      p2
    ))
  }
  if (length(to_become_polygons) >= 1) {
    res$islands <- to_become_polygons
  }
  return(res)
}

.unroll <- function(firstpos, v) {
  res <- firstpos
  a <- v[firstpos]
  while (!is.na(a)) {
    res <- c(res, a)
    a <- v[a]
  }
  return(res)
}

.lookup_ways <- function(ll, g) {
  gg <- g[rownames(ll)]
  gg <- do.call(rbind, lapply(gg, as.matrix))
  rr <- duplicated(rownames(gg))
  gg <- gg[!rr, ]
  return(gg)
}

.make_sf <- function(x, g) {
  x <- list(x)
  class(x) <- c("XY", "POLYGON", "sfg")
  x <- list(x)
  attr(x, "n_empty") <- 0
  class(x) <- c("sfc_POLYGON", "sfc")
  attr(x, "precision") <- 0
  attr(x, "bbox") <- attr(g, "bbox")
  attr(x, "crs") <- attr(g, "crs")
  df <- data.frame(row.names = "1")
  df[["geometry"]] <- x
  attr(df, "sf_column") <- "geometry"
  f <- factor(rep(NA_character_, length.out = ncol(df) - 1),
    levels = c("constant", "aggregate", "identity")
  )
  names(f) <- names(df)[-ncol(df)]
  attr(df, "agr") <- f
  class(df) <- c("sf", class(df))
  return(df)
}

.make_poly <- function(out, bbox, g) {
  p1 <- p2 <- NULL
  n <- nrow(out)
  first_pt <- out[1, ]
  last_pt <- out[n, ]
  first_pt_dir <- .classify_pt_dir(first_pt, bbox)
  last_pt_dir <- .classify_pt_dir(last_pt, bbox)
  bb <- bbox
  bb["x", "min"] <- min(c(bb["x", "min"], out[, 1]))
  bb["x", "max"] <- max(c(bb["x", "max"], out[, 1]))
  bb["y", "min"] <- min(c(bb["y", "min"], out[, 2]))
  bb["y", "max"] <- max(c(bb["y", "max"], out[, 2]))
  bb21 <- bb[2, 1]
  bb12 <- bb[1, 2]
  bb22 <- bb[2, 2]
  bb11 <- bb[1, 1]
  ext_corners <- rbind(c(bb12, bb22), c(bb12, bb21), c(
    bb11,
    bb21
  ), c(bb11, bb22))
  if (last_pt_dir == first_pt_dir) {
    v_first_last <- last_pt - first_pt
    v_edge <- ext_corners[first_pt_dir, ] - ext_corners[.wrp(first_pt_dir - 1), ]
    dp <- sign(sum(v_first_last * v_edge))
    if (dp < 0) {
      cw_indx <- c(.wrp(last_pt_dir - 1), last_pt_dir)
      ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
      ccw_indx <- .wrp(ccw_indx)
      ccw_indx <- ccw_indx[1:which.max(ccw_indx == first_pt_dir)]
    } else {
      cw_indx <- last_pt_dir:(last_pt_dir + 4)
      cw_indx <- .wrp(cw_indx)
      cw_indx <- cw_indx[1:which.max(cw_indx == .wrp(first_pt_dir -
        1))]
      ccw_indx <- c(last_pt_dir, .wrp(last_pt_dir - 1))
    }
  } else {
    cw_indx <- last_pt_dir:(last_pt_dir + 4)
    cw_indx <- .wrp(cw_indx)
    cw_indx <- cw_indx[1:which.max(cw_indx == first_pt_dir)]
    ccw_indx <- (last_pt_dir - 1):(last_pt_dir - 4)
    ccw_indx <- .wrp(ccw_indx)
    ccw_indx <- ccw_indx[1:which.max(ccw_indx == first_pt_dir)]
  }
  p1 <- rbind(out, ext_corners[cw_indx, ], out[1, ])
  p2 <- rbind(out, ext_corners[ccw_indx, ], out[1, ])
  return(list(p1 = .make_sf(p1, g), p2 = .make_sf(p2, g)))
}

.unroll_loop <- function(firstpos, v) {
  res <- firstpos
  a <- v[firstpos]
  visted <- rep(FALSE, length(v))
  visted[firstpos] <- TRUE
  while (!visted[a]) {
    res <- c(res, a)
    visted[a] <- TRUE
    a <- v[a]
  }
  return(res)
}

.clip_one <- function(out, bbox) {
  indx <- (out[, 1] >= bbox[1, 1] & out[, 1] <= bbox[1, 2] &
    out[, 2] >= bbox[2, 1] & out[, 2] <= bbox[2, 2])
  indx <- as.logical(pmax(indx, c(indx[-1], FALSE), c(FALSE, indx[-length(indx)])))
  out[indx, ]
}

.classify_pt_dir <- function(pt, bbox) {
  directions <- 1:4
  names(directions) <- c("N", "E", "S", "W")
  compass <- c("W", "S", "E", "N")
  dim(compass) <- c(2, 2)
  lt <- pt < bbox[, "min"]
  gt <- pt > bbox[, "max"]
  td <- cbind(lt, gt)
  return(directions[compass[td]])
}

.wrp <- function(idxs) {
  (idxs - 1) %% 4 + 1
}
