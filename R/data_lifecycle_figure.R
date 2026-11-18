# Data life cycle figure
#
# Edit the tables in this file, then re-run it:
#   Rscript R/data_lifecycle_figure.R
#
# The script writes quarto_webpage/images/data_lifecycle.svg.
# Inline that file on a Quarto page so the links work
# (an ordinary image include ignores links inside an SVG):
#
#   ```{=html}
#   {{< include images/data_lifecycle.svg >}}
#   ```
#
# href values are copied into the SVG unchanged. Quarto does not rewrite
# links inside raw HTML, so use the published address: collect.html or
# collect.html#plan, not collect.qmd. A value such as #plan jumps to an
# element with id="plan" on the same page.
#
# Angles: 0 is the top of the wheel and degrees increase clockwise.
# Leave every stage angle as NA to space the stages evenly from start_angle.

start_angle <- 330

stages <- data.frame(
  id = c(
    "design", "plan", "collect", "digitize",
    "processing", "analyse", "visualise", "document", "disseminate", "archive",
    "find-reuse"
  ),
  label = c(
    "Design", "Plan", "Collect", "Digitize",
    "Process", "Analyse", "Visualise", "Document", "Disseminate", "Archive",
    "Find &\nReuse"
  ),
  phase = c(
    "create", "create", "create",
    "process", "process", "process", "process",
    "share", "share", "share",
    "reuse"
  ),
  icon = c(
    "pencil", "clipboard", "notebook", "scanner",
    "gears", "analyse", "visualise", "document", "share", "archive",
    "reuse"
  ),
  # Signac, from the Met palettes (palettes::met_palettes / MetBrewer),
  # mixed toward a warm umber so the pigments sit quieter than the
  # screen primaries. A phase shares one family of colours; the wash
  # shifts slowly along the block instead of changing at every circle.
  color = c(
    "#3D7463", "#45816D", "#5E8A61",
    "#819751", "#BA992B", "#C08122", "#C48A4A",
    "#B36F72", "#A06574", "#895977",
    "#3F677D"
  ),
  href = c(
    "5_trait_collection.html",
    "plan_logistics.html",
    "5_trait_collection.html#collecting-leaves-in-the-field",
    "5_trait_collection.html#data-entry",
    "12_traits.html#prepare-data",
    "10_community.html",
    "8_reproducible.html",
    "5_trait_collection.html#data-documentation",
    "8_reproducible.html",
    "6_pftc_data.html#pftc-data-repositories",
    "6_pftc_data.html"
  ),
  angle = NA_real_,
  highlight = c(
    FALSE, FALSE, FALSE, FALSE,
    FALSE, TRUE, FALSE, FALSE, FALSE, FALSE,
    FALSE
  ),
  stringsAsFactors = FALSE
)

phases <- data.frame(
  id = c("create", "process", "share", "reuse"),
  label = c("CREATE", "PROCESS", "SHARE", "RE-USE"),
  href = c(
    "step_plan.html",
    "step_digitize.html",
    "step_document.html",
    "reuse.html"
  ),
  stringsAsFactors = FALSE
)

# Learning-outcome skills on the outer grey track.
# near = stage id whose angle centres the chip; span is chip width in degrees.
# Only some lifecycle steps have a matching skill, so the outer ring stays gappy.
skills <- data.frame(
  id = c("formulate", "observe", "test", "illustrate", "write"),
  label = c(
    "Formulate questions",
    "Observe & use equipment",
    "Test hypotheses",
    "Illustrate",
    "Write & interpret"
  ),
  near = c("design", "collect", "analyse", "visualise", "disseminate"),
  span = c(36, 48, 34, 28, 36),
  # Darker cousins of the nearby phase wash so labels stay readable.
  color = c("#2C5A4C", "#355F48", "#865E27", "#8A6428", "#7E545B"),
  text_color = c("#f6f1e6", "#f6f1e6", "#f6f1e6", "#f6f1e6", "#f6f1e6"),
  href = c(
    "5_trait_collection.html",
    "5_trait_collection.html#collecting-leaves-in-the-field",
    "10_community.html",
    "8_reproducible.html",
    "8_reproducible.html"
  ),
  stringsAsFactors = FALSE
)

layout <- list(
  canvas = 1000,
  outer_r = 452,
  track_inner = 404,
  phase_outer = 392,
  # Colour behind the step circles. Centred on the circle orbit.
  step_band_inner = 232,
  # Inner ring. Its arc length is the size of each block, with a gap between blocks.
  block_outer = 214,
  block_inner = 174,
  block_gap = 3.2,
  bubble_orbit = 312,
  bubble_r = 52,
  phase_label_r = 194,
  skill_label_r = 428,
  stage_font = 15,
  phase_font = 22,
  skill_font = 13,
  # Warm grey-brown keyline on every step circle.
  ring = "#7d6a5c",
  icon_ink = "#4a4038",
  label_ink = "#3e342c",
  paper = "#f3eee6",
  well = "#efe6d8",
  arrow = "#f6f1e6",
  center_fill = "#3c2e38",
  track_fill = "#d4cdc2",
  bubble_fill = "#fbf7f1"
)

# --- geometry and SVG helpers -------------------------------------------------

xml_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

mix_hex <- function(a, b, t) {
  ca <- grDevices::col2rgb(a)
  cb <- grDevices::col2rgb(b)
  v <- (ca + (cb - ca) * t) / 255
  grDevices::rgb(v[1], v[2], v[3])
}

rel_lum <- function(hex) {
  v <- grDevices::col2rgb(hex) / 255
  lin <- ifelse(v <= 0.03928, v / 12.92, ((v + 0.055) / 1.055)^2.4)
  sum(c(0.2126, 0.7152, 0.0722) * lin[, 1])
}

polar <- function(deg, r, cx, cy) {
  a <- deg * pi / 180
  c(cx + r * sin(a), cy - r * cos(a))
}

cw_mid <- function(a, b) {
  span <- (b - a) %% 360
  (a + span / 2) %% 360
}

norm_span <- function(a0, a1) {
  a0 <- a0 %% 360
  span <- (a1 - a0) %% 360
  if (span < 1e-6) span <- 360
  list(a0 = a0, span = span, a1 = (a0 + span) %% 360)
}

annulus_d <- function(a0, a1, r_in, r_out, cx, cy) {
  s <- norm_span(a0, a1)
  large <- as.integer(s$span > 180)
  p0o <- polar(s$a0, r_out, cx, cy)
  p1o <- polar(s$a0 + s$span, r_out, cx, cy)
  p1i <- polar(s$a0 + s$span, r_in, cx, cy)
  p0i <- polar(s$a0, r_in, cx, cy)
  sprintf(
    "M %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f L %.2f %.2f A %.2f %.2f 0 %d 0 %.2f %.2f Z",
    p0o[1], p0o[2], r_out, r_out, large, p1o[1], p1o[2],
    p1i[1], p1i[2], r_in, r_in, large, p0i[1], p0i[2]
  )
}

# Glyphs sit on the left of the path direction. A clockwise arc keeps text
# upright on the top half and upside down on the bottom half, so bottom
# labels travel back along the same arc.
text_arc_d <- function(a0, a1, r, flip, cx, cy) {
  s <- norm_span(a0, a1)
  large <- as.integer(s$span > 180)
  if (!flip) {
    p0 <- polar(s$a0, r, cx, cy)
    p1 <- polar(s$a0 + s$span, r, cx, cy)
    sprintf(
      "M %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f",
      p0[1], p0[2], r, r, large, p1[1], p1[2]
    )
  } else {
    p0 <- polar(s$a0 + s$span, r, cx, cy)
    p1 <- polar(s$a0, r, cx, cy)
    sprintf(
      "M %.2f %.2f A %.2f %.2f 0 %d 0 %.2f %.2f",
      p0[1], p0[2], r, r, large, p1[1], p1[2]
    )
  }
}

label_baseline_r <- function(center_r, font_size, flip) {
  # Keep the letters inside the band whether they grow outward or inward.
  shift <- font_size * 0.36
  if (flip) center_r + shift else center_r - shift
}

should_flip <- function(mid) {
  # Keep top labels upright. Flip the right and lower labels so PROCESS
  # reads in the same direction along the ring as SHARE.
  m <- mid %% 360
  m > 90 && m < 270
}

arrow_path <- function(deg, r, size, fill, cx, cy) {
  a <- deg * pi / 180
  x <- cx + r * sin(a)
  y <- cy - r * cos(a)
  tx <- cos(a)
  ty <- sin(a)
  nx <- sin(a)
  ny <- -cos(a)
  tip <- c(x + tx * size, y + ty * size)
  b1 <- c(x - tx * size * 0.7 + nx * size * 0.52, y - ty * size * 0.7 + ny * size * 0.52)
  b2 <- c(x - tx * size * 0.7 - nx * size * 0.52, y - ty * size * 0.7 - ny * size * 0.52)
  sprintf(
    '<path d="M %.2f %.2f L %.2f %.2f L %.2f %.2f Z" fill="%s" stroke="rgba(0,0,0,0.16)" stroke-width="0.6" stroke-linejoin="round" pointer-events="none"/>',
    tip[1], tip[2], b1[1], b1[2], b2[1], b2[2], fill
  )
}

# Curved arrow along the circle, sitting in the open gap between two bubbles.
flow_arrow <- function(a_from, a_to, r, bubble_r, fill, cx, cy) {
  span <- (a_to - a_from) %% 360
  if (span < 1e-3) span <- 360
  # Angle from a bubble's centre to where this circle leaves the bubble.
  half <- acos(max(-1, min(1, 1 - (bubble_r^2) / (2 * r^2)))) * 180 / pi
  free <- span - 2 * half
  if (free < 4) {
    return(arrow_path(a_from + span / 2, r, 9, fill, cx, cy))
  }
  pad <- min(1.7, free * 0.14)
  usable <- free - 2 * pad
  head_deg <- min(2.6, usable * 0.48)
  start_deg <- a_from + half + pad
  tip_deg <- a_from + span - half - pad
  base_deg <- tip_deg - head_deg
  head_half_w <- 5.2
  shaft_w <- 1.8
  edge <- mix_hex(fill, "#3e342c", 0.62)

  shaft <- text_arc_d(start_deg, base_deg, r, FALSE, cx, cy)
  tip <- polar(tip_deg, r, cx, cy)
  base <- polar(base_deg, r, cx, cy)
  b <- base_deg * pi / 180
  nx <- sin(b)
  ny <- -cos(b)
  b1 <- c(base[1] + nx * head_half_w, base[2] + ny * head_half_w)
  b2 <- c(base[1] - nx * head_half_w, base[2] - ny * head_half_w)
  head <- sprintf(
    "M %.2f %.2f L %.2f %.2f L %.2f %.2f Z",
    tip[1], tip[2], b1[1], b1[2], b2[1], b2[2]
  )
  sprintf(
    paste0(
      '<g pointer-events="none">',
      '<path d="%s" fill="none" stroke="%s" stroke-width="%.1f" stroke-linecap="round" stroke-opacity="0.3"/>',
      '<path d="%s" fill="none" stroke="%s" stroke-width="%.1f" stroke-linecap="round"/>',
      '<path d="%s" fill="%s" stroke="%s" stroke-width="0.45" stroke-linejoin="round"/>',
      "</g>"
    ),
    shaft, edge, shaft_w + 1.2,
    shaft, fill, shaft_w,
    head, fill, edge
  )
}

donut_d <- function(r_in, r_out, cx, cy) {
  sprintf(
    paste0(
      "M %.2f %.2f m -%.2f 0 a %.2f %.2f 0 1 0 %.2f 0 a %.2f %.2f 0 1 0 -%.2f 0 ",
      "M %.2f %.2f m -%.2f 0 a %.2f %.2f 0 1 1 %.2f 0 a %.2f %.2f 0 1 1 -%.2f 0 Z"
    ),
    cx, cy, r_out, r_out, r_out, 2 * r_out, r_out, r_out, 2 * r_out,
    cx, cy, r_in, r_in, r_in, 2 * r_in, r_in, r_in, 2 * r_in
  )
}

phase_runs <- function(stages) {
  o <- order(stages$angle)
  s <- stages[o, , drop = FALSE]
  n <- nrow(s)
  prev_phase <- s$phase[c(n, seq_len(n - 1))]
  boundary <- which(s$phase != prev_phase)
  if (!length(boundary)) {
    return(list(list(
      phase = s$phase[1], a0 = 0, a1 = 360, span = 360, ids = s$id
    )))
  }
  start <- boundary[1]
  idx <- c(seq(start, n), if (start > 1) seq_len(start - 1) else integer(0))
  s <- s[idx, , drop = FALSE]
  runs <- list()
  run_start <- 1
  for (i in seq_len(n)) {
    nxt <- if (i == n) 1 else i + 1
    if (s$phase[i] != s$phase[nxt] || i == n) {
      chunk <- s[run_start:i, , drop = FALSE]
      prev_ang <- if (run_start == 1) s$angle[n] else s$angle[run_start - 1]
      next_ang <- if (i == n) s$angle[1] else s$angle[i + 1]
      a0 <- cw_mid(prev_ang, chunk$angle[1])
      a1 <- cw_mid(chunk$angle[nrow(chunk)], next_ang)
      span <- (a1 - a0) %% 360
      if (span < 1e-6) span <- 360
      runs[[length(runs) + 1]] <- list(
        phase = chunk$phase[1],
        a0 = a0,
        a1 = (a0 + span) %% 360,
        span = span,
        ids = chunk$id
      )
      run_start <- i + 1
    }
  }
  runs
}

color_at <- function(deg, stages) {
  o <- order(stages$angle)
  angs <- stages$angle[o]
  cols <- stages$color[o]
  deg <- deg %% 360
  n <- length(angs)
  for (i in seq_len(n)) {
    a0 <- angs[i]
    j <- if (i == n) 1 else i + 1
    span <- (angs[j] - a0) %% 360
    if (span < 1e-6) return(cols[i])
    pos <- (deg - a0) %% 360
    if (pos <= span + 1e-4) return(mix_hex(cols[i], cols[j], pos / span))
  }
  cols[1]
}

# Smooth colour band. bleed lets neighbouring bands meet without a hairline gap.
band_slices <- function(a0, span, r_in, r_out, stages, cx, cy, bleed = 0) {
  n_slice <- max(4, round(span / 3))
  edges <- seq(0, span, length.out = n_slice + 1)
  vapply(seq_len(n_slice), function(k) {
    d0 <- edges[k]
    d1 <- edges[k + 1]
    left <- if (k == 1) d0 - bleed else d0 - 0.55
    right <- if (k == n_slice) d1 + bleed else d1 + 0.55
    col <- color_at(a0 + (d0 + d1) / 2, stages)
    sprintf(
      '<path d="%s" fill="%s" stroke="%s" stroke-width="0.8" stroke-linejoin="round" pointer-events="none"/>',
      annulus_d(a0 + left, a0 + right, r_in, r_out, cx, cy),
      col, col
    )
  }, character(1))
}

# --- icons, drawn in a 32x32 box ----------------------------------------------

gear_markup <- function(cx, cy, r, col) {
  teeth <- 8
  segs <- vapply(0:(teeth - 1), function(i) {
    ang <- (i / teeth) * 2 * pi - pi / 2
    sprintf(
      '<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" stroke="%s" stroke-width="%.2f" stroke-linecap="butt"/>',
      cx + cos(ang) * r * 0.55, cy + sin(ang) * r * 0.55,
      cx + cos(ang) * r, cy + sin(ang) * r,
      col, r * 0.42
    )
  }, character(1))
  paste0(
    paste(segs, collapse = ""),
    sprintf('<circle cx="%.2f" cy="%.2f" r="%.2f" fill="%s"/>', cx, cy, r * 0.58, col),
    sprintf('<circle cx="%.2f" cy="%.2f" r="%.2f" fill="#ffffff"/>', cx, cy, r * 0.26)
  )
}

icon_markup <- function(name, col) {
  stroke <- sprintf(
    'fill="none" stroke="%s" stroke-width="1.55" stroke-linecap="round" stroke-linejoin="round"',
    col
  )
  switch(name,
    clipboard = sprintf(
      '<g %s><rect x="9" y="7" width="14" height="19" rx="2"/><path d="M13 7.2 v-1.2 a2 2 0 0 1 2 -2 h2 a2 2 0 0 1 2 2 v1.2"/><path d="M13 14 h6 M13 18 h6 M13 22 h4"/></g>',
      stroke
    ),
    pencil = sprintf(
      '<g %s><path d="M7 9 h9 v15 h-9 z"/><path d="M16 7 l7 7 -8 8 h-4 v-4 z"/><path d="M18.2 9.2 l4.6 4.6"/></g>',
      stroke
    ),
    notebook = sprintf(
      '<g %s><rect x="8" y="5" width="16" height="22" rx="2"/><path d="M13 5 v22"/><path d="M16.5 11 h5 M16.5 15 h5 M16.5 19 h4"/></g>',
      stroke
    ),
    scanner = sprintf(
      '<g %s><path d="M6 20 h20 v5 a2 2 0 0 1 -2 2 h-16 a2 2 0 0 1 -2 -2 z"/><path d="M10 20 v-9 h12 v9"/><path d="M9 23.5 h14"/></g>',
      stroke
    ),
    "folder-in" = sprintf(
      '<g %s><path d="M4 12 h8 l2 2 h14 v12 h-24 z"/><path d="M16 15 v8"/><path d="M12.8 19.5 l3.2 3.2 l3.2 -3.2"/></g>',
      stroke
    ),
    broom = sprintf(
      '<g %s><path d="M20 4 L10.5 17"/><path d="M7 20 c2.2 -3.2 4.2 -4.2 6.2 -2.2 c1.6 1.6 4.2 1.2 6.6 -1.6"/><path d="M6.5 23.5 h15"/><path d="M9 19.5 v4 M12.2 18 v5.5 M15.4 17.8 v5.7 M18.4 18.8 v4.7"/></g>',
      stroke
    ),
    gears = paste0(
      '<g>',
      gear_markup(12, 13, 8.2, col),
      gear_markup(22.5, 22, 6.4, col),
      "</g>"
    ),
    analyse = sprintf(
      '<g %s><path d="M5 26 V13"/><path d="M5 26 H18"/><path d="M8 22 l4 -6 l3.2 2.4 l5 -7"/><circle cx="22.5" cy="11" r="5"/><path d="M26 14.6 L29.5 18.2"/></g>',
      stroke
    ),
    visualise = sprintf(
      '<g %s><path d="M5 26 V7"/><path d="M5 26 H27"/><path d="M9 22 v-7"/><path d="M14 22 v-12"/><path d="M19 22 v-9"/><path d="M24 22 v-14"/></g>',
      stroke
    ),
    document = sprintf(
      '<g %s><path d="M9 4 h9 l6 6 v16 a2 2 0 0 1 -2 2 h-13 a2 2 0 0 1 -2 -2 v-20 a2 2 0 0 1 2 -2 z"/><path d="M18 4 v6 h6"/><path d="M12 16 h8 M12 20 h8 M12 24 h5"/></g>',
      stroke
    ),
    share = sprintf(
      '<g %s><circle cx="8" cy="16" r="3.1"/><circle cx="24" cy="8" r="3.1"/><circle cx="24" cy="24" r="3.1"/><path d="M10.8 14.6 L21.1 9.5"/><path d="M10.8 17.4 L21.1 22.5"/></g>',
      stroke
    ),
    archive = sprintf(
      '<g %s><path d="M6 13 h20 v12 a2 2 0 0 1 -2 2 h-16 a2 2 0 0 1 -2 -2 z"/><path d="M4 8 h24 v6 h-24 z"/><path d="M13 18 h6"/></g>',
      stroke
    ),
    reuse = sprintf(
      '<g %s><circle cx="13.5" cy="16" r="5.6"/><path d="M17.6 20.1 L22 24.6"/><path d="M23.5 8.5 A10 10 0 0 0 10.5 6.2"/><path d="M10.2 3.2 v4.2 h4"/></g>',
      stroke
    ),
    stop("Unknown icon: ", name, call. = FALSE)
  )
}

stage_label_svg <- function(label, font_size, fill) {
  lines <- strsplit(label, "\n", fixed = TRUE)[[1]]
  n <- length(lines)
  if (n == 1) {
    return(sprintf(
      '<text y="20" text-anchor="middle" font-size="%s" font-weight="500" letter-spacing="0.2" fill="%s">%s</text>',
      font_size, fill, xml_esc(lines)
    ))
  }
  gap <- font_size + 1
  y0 <- 14
  paste(vapply(seq_len(n), function(i) {
    sprintf(
      '<text y="%.1f" text-anchor="middle" font-size="%.1f" font-weight="500" letter-spacing="0.15" fill="%s">%s</text>',
      y0 + (i - 1) * gap, font_size - 1.5, fill, xml_esc(lines[i])
    )
  }, character(1)), collapse = "")
}

curved_text <- function(path_id, path_d, text, font_size, fill, tracking) {
  paste0(
    sprintf('<path id="%s" d="%s" fill="none" stroke="none"/>', path_id, path_d),
    sprintf(
      '<text font-size="%s" font-weight="600" fill="%s" letter-spacing="%s" text-anchor="middle">',
      font_size, fill, tracking
    ),
    sprintf(
      '<textPath xlink:href="#%s" href="#%s" startOffset="50%%">%s</textPath></text>',
      path_id, path_id, xml_esc(text)
    )
  )
}

# --- build ---------------------------------------------------------------------

assign_angles <- function(stages, start_angle) {
  n <- nrow(stages)
  if (all(is.na(stages$angle))) {
    stages$angle <- (start_angle + (seq_len(n) - 1) * (360 / n)) %% 360
  } else if (anyNA(stages$angle)) {
    stop("Set every stage angle, or leave them all as NA.", call. = FALSE)
  } else {
    stages$angle <- stages$angle %% 360
  }
  if (any(duplicated(round(stages$angle, 4)))) {
    stop("Stage angles must be unique.", call. = FALSE)
  }
  stages
}

validate_inputs <- function(stages, phases, skills) {
  if (!all(stages$phase %in% phases$id)) {
    stop("Every stage phase must match a phases$id.", call. = FALSE)
  }
  if (!all(skills$near %in% stages$id)) {
    stop("Every skills$near must match a stages$id.", call. = FALSE)
  }
  if (anyDuplicated(stages$id) || anyDuplicated(phases$id) || anyDuplicated(skills$id)) {
    stop("ids must be unique within each table.", call. = FALSE)
  }
  if (any(!nzchar(c(stages$href, phases$href, skills$href)))) {
    stop("Every href must be filled in.", call. = FALSE)
  }
  known <- c(
    "clipboard", "pencil", "notebook", "scanner", "folder-in", "broom",
    "gears", "analyse", "visualise", "document", "share", "archive", "reuse"
  )
  if (!all(stages$icon %in% known)) {
    stop("Unknown icon name in stages$icon.", call. = FALSE)
  }
}

build_svg <- function(stages, phases, skills, layout) {
  stages <- assign_angles(stages, start_angle)
  validate_inputs(stages, phases, skills)
  skills$angle <- stages$angle[match(skills$near, stages$id)]
  L <- layout
  cx <- L$canvas / 2
  cy <- L$canvas / 2
  runs <- phase_runs(stages)

  css <- paste(
    "<style>",
    ".dlc-stage,.dlc-phase,.dlc-skill{cursor:pointer;text-decoration:none;}",
    ".dlc-stage:hover,.dlc-phase:hover,.dlc-skill:hover,",
    ".dlc-stage:focus,.dlc-phase:focus,.dlc-skill:focus{text-decoration:none;outline:none;}",
    ".dlc-hit{fill:transparent;}",
    ".dlc-phase:hover .dlc-hit,.dlc-phase:focus .dlc-hit,",
    ".dlc-skill:hover .dlc-hit,.dlc-skill:focus .dlc-hit{fill:rgba(255,248,236,0.22);}",
    ".dlc-stage:hover .dlc-bubble,.dlc-stage:focus .dlc-bubble,",
    ".dlc-stage:hover .dlc-keyline,.dlc-stage:focus .dlc-keyline{stroke:#3e342c;}",
    "</style>",
    sep = ""
  )

  defs <- paste(
    "<defs>",
    '<filter id="dlc-shadow" x="-55%" y="-55%" width="210%" height="210%" color-interpolation-filters="sRGB">',
    '<feDropShadow dx="0" dy="1.5" stdDeviation="2.1" flood-color="#3a3028" flood-opacity="0.22"/>',
    "</filter>",
    '<filter id="dlc-grain" x="0" y="0" width="100%" height="100%">',
    '<feTurbulence type="fractalNoise" baseFrequency="0.85" numOctaves="2" seed="4" stitchTiles="stitch"/>',
    '<feColorMatrix type="matrix" values="0 0 0 0 0.30  0 0 0 0 0.26  0 0 0 0 0.20  0.16 0.16 0.16 0 -0.03"/>',
    "</filter>",
    "</defs>",
    sep = ""
  )

  parts <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="dlc" viewBox="0 0 %d %d" width="860" height="860" style="max-width:100%%;height:auto" role="group" aria-label="Reproducible science" font-family="Avenir Next, Gill Sans, Gill Sans MT, Helvetica Neue, Arial, sans-serif">',
      L$canvas, L$canvas
    ),
    "<!-- Generated by R/data_lifecycle_figure.R. Edit that script and re-run. -->",
    css,
    defs,
    sprintf(
      '<circle cx="%.2f" cy="%.2f" r="%.2f" fill="%s"/>',
      cx, cy, L$outer_r, L$paper
    ),
    sprintf(
      '<path d="%s" fill="%s" fill-rule="evenodd"/>',
      donut_d(L$track_inner, L$outer_r, cx, cy), L$track_fill
    ),
    sprintf(
      '<circle cx="%.2f" cy="%.2f" r="%.2f" fill="none" stroke="#b7ad9f" stroke-width="1.15"/>',
      cx, cy, L$outer_r
    ),
    sprintf(
      '<circle cx="%.2f" cy="%.2f" r="%.2f" fill="none" stroke="%s" stroke-width="2.2"/>',
      cx, cy, L$track_inner, L$paper
    )
  )

  # Discrete skill chips on the outer track (gaps show incomplete overlap).
  for (i in seq_len(nrow(skills))) {
    skill <- skills[i, ]
    half <- skill$span / 2
    a0 <- skill$angle - half
    a1 <- skill$angle + half
    s <- norm_span(a0, a1)
    mid <- (s$a0 + s$span / 2) %% 360
    flip <- should_flip(mid)
    baseline <- label_baseline_r(L$skill_label_r, L$skill_font, flip)
    chip <- annulus_d(s$a0, s$a0 + s$span, L$track_inner, L$outer_r, cx, cy)
    parts <- c(
      parts,
      sprintf(
        '<a class="dlc-skill" href="%s" aria-label="%s"><title>%s</title>',
        xml_esc(skill$href), xml_esc(skill$label), xml_esc(skill$label)
      ),
      sprintf('<path d="%s" fill="%s" pointer-events="none"/>', chip, skill$color),
      sprintf('<path class="dlc-hit" d="%s"/>', chip),
      curved_text(
        paste0("dlc-skill-path-", skill$id),
        text_arc_d(s$a0, s$a0 + s$span, baseline, flip, cx, cy),
        skill$label, L$skill_font, skill$text_color, "0.4"
      ),
      "</a>"
    )
  }

  # Each block: a colour band behind its steps, plus a shorter inner arc
  # whose length shows how much of the cycle that block covers.
  for (i in seq_along(runs)) {
    run <- runs[[i]]
    phase <- phases[match(run$phase, phases$id), ]
    inner_a0 <- run$a0 + L$block_gap
    inner_span <- run$span - 2 * L$block_gap
    mid <- (run$a0 + run$span / 2) %% 360
    outer_slices <- band_slices(
      run$a0, run$span, L$step_band_inner, L$phase_outer,
      stages, cx, cy, bleed = 0.4
    )
    inner_col <- color_at(mid, stages)
    inner_edge <- mix_hex(inner_col, L$label_ink, 0.28)
    inner_d <- annulus_d(
      inner_a0, inner_a0 + inner_span, L$block_inner, L$block_outer, cx, cy
    )
    flip <- should_flip(mid)
    label_size <- if (inner_span < 45) L$phase_font - 4 else L$phase_font
    tracking <- if (inner_span < 45) "0.5" else "2.2"
    baseline <- label_baseline_r(L$phase_label_r, label_size, flip)
    label_col <- if (rel_lum(inner_col) > 0.42) L$label_ink else "#f6f1e6"
    parts <- c(
      parts,
      sprintf(
        '<a class="dlc-phase" href="%s" aria-label="%s"><title>%s</title>',
        xml_esc(phase$href), xml_esc(phase$label), xml_esc(phase$label)
      ),
      outer_slices,
      sprintf(
        '<path d="%s" fill="%s" stroke="%s" stroke-width="1.15" stroke-linejoin="round" pointer-events="none"/>',
        inner_d, inner_col, inner_edge
      ),
      sprintf(
        '<path class="dlc-hit" d="%s"/>',
        annulus_d(run$a0, run$a0 + run$span, L$step_band_inner, L$phase_outer, cx, cy)
      ),
      sprintf(
        '<path class="dlc-hit" d="%s"/>',
        annulus_d(inner_a0, inner_a0 + inner_span, L$block_inner, L$block_outer, cx, cy)
      ),
      curved_text(
        sprintf("dlc-phase-path-%s-%d", phase$id, i),
        text_arc_d(inner_a0, inner_a0 + inner_span, baseline, flip, cx, cy),
        phase$label, label_size, label_col, tracking
      ),
      "</a>"
    )
  }

  # Clockwise arrows in the gap between one step and the next.
  o <- order(stages$angle)
  angs <- stages$angle[o]
  for (i in seq_along(angs)) {
    j <- if (i == length(angs)) 1 else i + 1
    parts <- c(
      parts,
      flow_arrow(angs[i], angs[j], L$bubble_orbit, L$bubble_r, L$arrow, cx, cy)
    )
  }

  # Stage bubbles
  icon_scale <- 1.08
  for (i in seq_len(nrow(stages))) {
    st <- stages[i, ]
    p <- polar(st$angle, L$bubble_orbit, cx, cy)
    icon_col <- L$icon_ink
    keyline <- sprintf(
      '<circle class="dlc-keyline" r="%.2f" fill="none" stroke="%s" stroke-width="0.9" pointer-events="none"/>',
      L$bubble_r - 5.5, L$ring
    )
    tip <- gsub("\n", " ", st$label, fixed = TRUE)
    parts <- c(
      parts,
      sprintf(
        '<a class="dlc-stage" href="%s" aria-label="%s"><title>%s</title>',
        xml_esc(st$href), xml_esc(tip), xml_esc(tip)
      ),
      sprintf('<g transform="translate(%.2f %.2f)">', p[1], p[2]),
      sprintf(
        '<circle class="dlc-bubble" r="%.2f" fill="%s" stroke="%s" stroke-width="2.15" filter="url(#dlc-shadow)"/>',
        L$bubble_r, L$bubble_fill, L$ring
      ),
      keyline,
      sprintf(
        '<g transform="translate(%.2f %.2f) scale(%.3f)" pointer-events="none">%s</g>',
        -16 * icon_scale, -16 * icon_scale - 14, icon_scale, icon_markup(st$icon, icon_col)
      ),
      stage_label_svg(st$label, L$stage_font, L$label_ink),
      "</g></a>"
    )
  }

  # Title well in the centre.
  parts <- c(
    parts,
    sprintf(
      '<circle cx="%.2f" cy="%.2f" r="%.2f" fill="%s" pointer-events="none"/>',
      cx, cy, L$block_inner - 8, L$well
    ),
    sprintf(
      '<g text-anchor="middle" fill="%s" font-family="Iowan Old Style, Palatino Linotype, Palatino, Georgia, serif" font-weight="500" pointer-events="none">',
      L$center_fill
    ),
    sprintf('<text x="%.2f" y="%.2f" font-size="24" letter-spacing="2.4">REPRODUCIBLE</text>', cx, cy - 10),
    sprintf('<text x="%.2f" y="%.2f" font-size="24" letter-spacing="2.4">SCIENCE</text>', cx, cy + 20),
    "</g>",
    sprintf(
      '<rect width="%d" height="%d" filter="url(#dlc-grain)" pointer-events="none"/>',
      L$canvas, L$canvas
    ),
    "</svg>"
  )

  list(svg = paste(parts, collapse = "\n"), stages = stages, runs = runs)
}

project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..")))
  }
  if (file.exists("R/data_lifecycle_figure.R")) return(normalizePath("."))
  if (file.exists("data_lifecycle_figure.R")) return(normalizePath(".."))
  normalizePath(".")
}

figure <- build_svg(stages, phases, skills, layout)
root <- project_root()
out_dir <- file.path(root, "quarto_webpage", "images")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_dir, "data_lifecycle.svg")
writeLines(figure$svg, out, useBytes = TRUE)

message("Wrote ", out)
message("\nStage positions (0 = top, clockwise):")
ord <- order(figure$stages$angle)
for (i in ord) {
  st <- figure$stages[i, ]
  message(sprintf("  %3.0f  %-14s  %-8s  %s", st$angle, gsub("\n", " ", st$label), st$phase, st$href))
}
message("\nCycle links:")
for (i in seq_len(nrow(phases))) {
  message(sprintf("  %-8s  %s", phases$label[i], phases$href[i]))
}
message("\nSkill links:")
for (i in seq_len(nrow(skills))) {
  message(sprintf("  %-28s  near %-12s  %s", skills$label[i], skills$near[i], skills$href[i]))
}
