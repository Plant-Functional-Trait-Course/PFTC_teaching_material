# Trait wheel
#
# A semicircle of the leaf-trait stations. The first part is blue because
# the leaf is still wet. The second part is brown because the leaf is dry.
#
#   Rscript R/trait_wheel_figure.R
#
# Writes quarto_webpage/images/trait_wheel/trait_wheel.svg.

xml_esc <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

polar <- function(deg, r, cx, cy) {
  a <- deg * pi / 180
  c(cx + r * sin(a), cy - r * cos(a))
}

annulus_d <- function(a0, a1, r_in, r_out, cx, cy) {
  span <- (a1 - a0) %% 360
  large <- as.integer(span > 180)
  p0o <- polar(a0, r_out, cx, cy)
  p1o <- polar(a0 + span, r_out, cx, cy)
  p1i <- polar(a0 + span, r_in, cx, cy)
  p0i <- polar(a0, r_in, cx, cy)
  sprintf(
    "M %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f L %.2f %.2f A %.2f %.2f 0 %d 0 %.2f %.2f Z",
    p0o[1], p0o[2], r_out, r_out, large, p1o[1], p1o[2],
    p1i[1], p1i[2], r_in, r_in, large, p0i[1], p0i[2]
  )
}

# A simple ovate leaf with a petiole. Not a palmate leaf.
leaf_markup <- function(fill) {
  sprintf(
    paste0(
      '<path fill="%s" d="M16 3 C25 6 32 15 29 23 C26 30 20 32 16 32 C12 32 6 30 3 23 C0 15 7 6 16 3 Z"/>',
      '<path fill="none" stroke="#3e342c" stroke-width="1.15" stroke-linecap="round" d="M16 31 V11"/>',
      '<path fill="none" stroke="#3e342c" stroke-width="1.4" stroke-linecap="round" d="M16 31.5 V34"/>'
    ),
    fill
  )
}

icon_markup <- function(name) {
  stroke <- 'fill="none" stroke="#3e342c" stroke-width="1.55" stroke-linecap="round" stroke-linejoin="round"'
  switch(name,
    leaf = leaf_markup("#7D9A72"),
    "leaf-dry" = leaf_markup("#C4A15A"),
    envelope = sprintf(
      '<g %s><path d="M6 11 h20 v14 H6 z"/><path d="M6 11 l10 8 l10 -8"/></g>',
      stroke
    ),
    balance = sprintf(
      '<g %s><path d="M16 4 v6"/><path d="M7 10 h18"/><path d="M7 10 l-4 8 h8 z"/><path d="M25 10 l-4 8 h8 z"/><path d="M16 10 v13"/><path d="M10 26 h12"/><path d="M8 29 h16"/></g>',
      stroke
    ),
    scanner = sprintf(
      '<g %s><rect x="2.5" y="2.5" width="27" height="27" rx="2.2"/><path d="M4.5 10.5 H27.5" stroke="#5d7f90" stroke-width="1.7"/></g><path fill="#7D9A72" d="M16 13 C21.2 14 24.6 18.2 23.6 23 C22.8 26.6 19.4 28.6 16 28.6 C12.6 28.6 9.2 26.6 8.4 23 C7.4 18.2 10.8 14 16 13 Z"/><path fill="none" stroke="#3e342c" stroke-width="0.9" stroke-linecap="round" d="M16 27.6 V16.2"/>',
      stroke
    ),
    caliper = paste0(
      '<path fill="#7D9A72" d="M3.5 16 C8 9.5 24 9.5 28.5 16 C24 22.5 8 22.5 3.5 16 Z"/>',
      '<path fill="none" stroke="#3e342c" stroke-width="1.35" stroke-linecap="round" d="M16 4.5 V9.2 M16 22.8 V27.5"/>',
      '<path fill="none" stroke="#3e342c" stroke-width="1.35" stroke-linecap="round" stroke-linejoin="round" d="M16 4.5 L13.1 8 M16 4.5 L18.9 8 M16 27.5 L13.1 24 M16 27.5 L18.9 24"/>'
    ),
    flask = sprintf(
      '<path d="M10 22 h12 v5 a2.4 2.4 0 0 1 -2.4 2.4 H12.4 A2.4 2.4 0 0 1 10 27 z" fill="#C4B3D4"/>%s',
      sprintf('<g %s><path d="M13 4 h6 v7 L26 25 a4 4 0 0 1 -3.4 6 H9.4 A4 4 0 0 1 6 25 L13 11 z"/></g>', stroke)
    ),
    document = sprintf(
      '<g %s><path d="M9 5 h9 l6 6 v16 H9 z"/><path d="M18 5 v6 h6"/><path d="M12 16 h8 M12 20 h8 M12 24 h5"/></g>',
      stroke
    ),
    scatter = sprintf(
      '<g %s><path d="M7 26 V8"/><path d="M7 26 H26"/><path d="M10 22 L24 10" stroke-dasharray="2.2 2.4"/></g><circle cx="12" cy="21" r="1.6" fill="#3e342c"/><circle cx="16" cy="16" r="1.6" fill="#3e342c"/><circle cx="21" cy="13" r="1.6" fill="#3e342c"/><circle cx="24" cy="10" r="1.6" fill="#3e342c"/>',
      stroke
    ),
    laptop = sprintf(
      '<g %s><rect x="7" y="6" width="18" height="12" rx="1.3"/><path d="M4 21 h24 l-2 3 H6 z"/></g>',
      stroke
    ),
    height = sprintf(
      '<g %s><path d="M4 29 h12"/><path d="M10 29 V13"/><path d="M10 22 C6.5 20 4.5 17 4.5 15 C8 16 10 19 10 22"/><path d="M10 16 C14 14 16.5 11.5 17.5 9 C14 10.5 10 13.5 10 16"/><path d="M21 5 V29"/><path d="M21 7 h4 M21 13 h4 M21 19 h4 M21 25 h4"/></g>',
      stroke
    ),
    stop("Unknown icon: ", name, call. = FALSE)
  )
}

# Clockwise from the left, over the top, to the right.
# Blue while the leaf is still wet. Thickness is the last blue step.
# The brown steps follow as data entry, data check, data doc, dry mass, chemical traits.
steps <- data.frame(
  id = c(
    "height", "collect", "prepare", "wet", "area", "thickness",
    "entry", "check", "doc", "dry", "chem"
  ),
  label = c(
    "Plant height", "Collect", "Prepare", "Wet mass", "Leaf area", "Thickness",
    "Data entry", "Data check", "Data doc", "Dry mass", "Chemical traits"
  ),
  icon = c(
    "height", "leaf", "envelope", "balance", "scanner", "caliper",
    "laptop", "scatter", "document", "leaf-dry", "flask"
  ),
  phase = c(rep("wet", 6), rep("dry", 5)),
  href = c(
    "#plant-height",
    "#collecting-leaves-in-the-field",
    "#preparing-leaves-for-measuring-traits-in-lab",
    "#wet-mass",
    "#measure-leaf-area-using-a-scanner",
    "#leaf-thickness",
    "#data-entry",
    "#data-checking",
    "#data-documentation",
    "#dry-mass",
    "#chemical-traits"
  ),
  stringsAsFactors = FALSE
)

phase_fill <- c(wet = "#C5D8E6", dry = "#E7D3B4")
paper <- "#f3eee6"
ink <- "#3e342c"

cx <- 760
cy <- 680
r_in <- 180
r_out <- 520
icon_r <- 355
label_r <- 575
n <- nrow(steps)
wedge <- 180 / n
# 270° is the left. Clockwise from there runs over the top and ends on the right.
arc0 <- 270

build_svg <- function() {
  font <- 26
  pieces <- character()
  min_x <- cx - r_out
  max_x <- cx + r_out
  min_y <- cy - r_out
  max_y <- cy + 8

  for (i in seq_len(n)) {
    step <- steps[i, ]
    a0 <- arc0 + (i - 1) * wedge + 0.7
    a1 <- arc0 + i * wedge - 0.7
    mid <- (a0 + a1) / 2
    ang <- mid %% 360
    d <- annulus_d(a0, a1, r_in, r_out, cx, cy)
    fill <- unname(phase_fill[step$phase])
    open <- if (nzchar(step$href)) {
      sprintf('<a class="tw-step" href="%s" aria-label="%s">', xml_esc(step$href), xml_esc(step$label))
    } else {
      sprintf('<g class="tw-step" aria-label="%s">', xml_esc(step$label))
    }
    close <- if (nzchar(step$href)) "</a>" else "</g>"
    ip <- polar(mid, icon_r, cx, cy)
    lp <- polar(mid, label_r, cx, cy)
    # Names stay upright. Side labels sit outside the arc.
    anchor <- "middle"
    lx <- lp[1]
    ly <- lp[2] + 5
    text_w <- nchar(step$label) * font * 0.70
    if (ang > 200 && ang < 330) {
      anchor <- "end"
      lx <- lp[1] - 6
      min_x <- min(min_x, lx - text_w)
      max_x <- max(max_x, lx)
    } else if (ang > 30 && ang < 160) {
      anchor <- "start"
      lx <- lp[1] + 6
      min_x <- min(min_x, lx)
      max_x <- max(max_x, lx + text_w)
    } else {
      min_x <- min(min_x, lx - text_w / 2)
      max_x <- max(max_x, lx + text_w / 2)
    }
    min_y <- min(min_y, ly - font)
    max_y <- max(max_y, ly + 4)
    pieces <- c(
      pieces,
      open,
      sprintf('<path d="%s" fill="%s"/>', d, fill),
      sprintf('<path class="tw-hit" d="%s"/>', d),
      sprintf(
        '<g transform="translate(%.2f %.2f) scale(2.35)" pointer-events="none">%s</g>',
        ip[1] - 37.6, ip[2] - 37.6, icon_markup(step$icon)
      ),
      close,
      sprintf(
        '<text x="%.2f" y="%.2f" text-anchor="%s" font-size="%d" font-weight="600" fill="%s">%s</text>',
        lx, ly, anchor, font, ink, xml_esc(step$label)
      )
    )
  }

  pad <- 16
  x0 <- floor(min_x - pad)
  y0 <- floor(min_y - pad)
  x1 <- ceiling(max_x + pad)
  y1 <- ceiling(max_y + pad)
  width <- x1 - x0
  height <- y1 - y0
  parts <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" class="trait-wheel" viewBox="%d %d %d %d" width="%d" style="max-width:100%%;height:auto" role="img" aria-label="Trait wheel. The blue steps are done while the leaf is wet. The brown steps are done after it is dry." font-family="Avenir Next, Gill Sans, Gill Sans MT, Helvetica Neue, Arial, sans-serif">',
      x0, y0, width, height, width
    ),
    "<!-- Generated by R/trait_wheel_figure.R. Edit that script and re-run. -->",
    paste(
      "<style>",
      ".tw-step{cursor:pointer;text-decoration:none;}",
      ".tw-step:hover .tw-hit,.tw-step:focus .tw-hit{fill:rgba(255,248,236,0.28);}",
      ".tw-hit{fill:transparent;}",
      "</style>",
      sep = ""
    ),
    sprintf('<rect x="%d" y="%d" width="%d" height="%d" fill="%s"/>', x0, y0, width, height, paper),
    pieces,
    "</svg>"
  )
  paste(parts, collapse = "\n")
}

project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..")))
  }
  if (file.exists("R/trait_wheel_figure.R")) return(normalizePath("."))
  if (file.exists("trait_wheel_figure.R")) return(normalizePath(".."))
  normalizePath(".")
}

root <- project_root()
icon_dir <- file.path(root, "quarto_webpage", "images", "trait_wheel", "icons")
dir.create(icon_dir, recursive = TRUE, showWarnings = FALSE)
for (name in c(
  "leaf", "envelope", "balance", "scanner", "caliper",
  "laptop", "leaf-dry", "scatter", "document", "height", "flask"
)) {
  svg <- sprintf(
    '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 32 36">%s</svg>',
    icon_markup(name)
  )
  writeLines(svg, file.path(icon_dir, paste0(name, ".svg")), useBytes = TRUE)
}

out <- file.path(root, "quarto_webpage", "images", "trait_wheel", "trait_wheel.svg")
writeLines(build_svg(), out, useBytes = TRUE)
message("Wrote ", out)
