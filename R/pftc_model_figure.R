# PFTC model figure
#
# A second wheel, drawn from the best-practice table.
# The scientific process is the inner ring. Documents, methods, and
# community sit outside it. The four sectors are the same groups as
# the data life cycle: Create, Process, Share, Re-use.
#
#   Rscript R/pftc_model_figure.R
#
# Writes quarto_webpage/images/pftc_model.svg.

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

norm_span <- function(a0, a1) {
  a0 <- a0 %% 360
  span <- (a1 - a0) %% 360
  if (span < 1e-6) span <- 360
  list(a0 = a0, span = span, a1 = (a0 + span) %% 360)
}

# A full ring. A single 360° arc collapses because its start and end coincide.
full_ring_d <- function(r_in, r_out, cx, cy) {
  sprintf(
    paste0(
      "M %.2f %.2f A %.2f %.2f 0 1 1 %.2f %.2f A %.2f %.2f 0 1 1 %.2f %.2f ",
      "L %.2f %.2f A %.2f %.2f 0 1 0 %.2f %.2f A %.2f %.2f 0 1 0 %.2f %.2f Z"
    ),
    cx, cy - r_out,
    r_out, r_out, cx, cy + r_out,
    r_out, r_out, cx, cy - r_out,
    cx, cy - r_in,
    r_in, r_in, cx, cy + r_in,
    r_in, r_in, cx, cy - r_in
  )
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

should_flip <- function(mid) {
  m <- mid %% 360
  m > 90 && m < 270
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

# Glyphs in a 32x32 box, same stroke weight as the life-cycle wheel.
icon_markup <- function(name, col) {
  stroke <- sprintf(
    'fill="none" stroke="%s" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"',
    col
  )
  switch(name,
    clipboard = sprintf(
      '<g %s><rect x="9" y="6" width="14" height="20" rx="2"/><path d="M13 6.5 v-1.4 a2 2 0 0 1 2 -2 h2 a2 2 0 0 1 2 2 v1.4"/><path d="M13 13 h6 M13 17 h6 M13 21 h4"/></g>',
      stroke
    ),
    sprig = sprintf(
      '<g %s><path d="M16 28 V10"/><path d="M16 20 C10 18 7 12 8 8 C14 9 16 14 16 18"/><path d="M16 16 C22 13 26 8 25 4 C18 6 16 11 16 15"/><path d="M16 24 C12 22 10 20 9 17"/><path d="M16 23 C20 21 23 19 24 16"/></g><circle cx="11" cy="11" r="1.5" fill="%s"/><circle cx="22" cy="8" r="1.5" fill="%s"/>',
      stroke, col, col
    ),
    laptop = sprintf(
      '<g %s><rect x="7" y="6" width="18" height="13" rx="1.4"/><path d="M4 22 h24 l-2 3 H6 z"/><path d="M13 22 v-3 h6 v3"/></g>',
      stroke
    ),
    curate = sprintf(
      '<g %s><rect x="5" y="6" width="22" height="16" rx="1.6"/><path d="M5 10 h22"/><path d="M14 25 h4"/></g><text x="16" y="21" text-anchor="middle" font-size="9" font-weight="700" fill="%s" font-family="Georgia, serif">R</text>',
      stroke, col
    ),
    scatter = sprintf(
      '<g %s><path d="M6 26 V8"/><path d="M6 26 H26"/><path d="M9 22 L22 11"/></g><circle cx="11" cy="18" r="1.6" fill="%s"/><circle cx="16" cy="20" r="1.6" fill="%s"/><circle cx="15" cy="14" r="1.6" fill="%s"/><circle cx="21" cy="12" r="1.6" fill="%s"/>',
      stroke, col, col, col, col
    ),
    database = sprintf(
      '<g %s><ellipse cx="16" cy="8" rx="8" ry="3.2"/><path d="M8 8 v12 c0 1.8 3.6 3.2 8 3.2 s8 -1.4 8 -3.2 V8"/><path d="M8 14 c0 1.8 3.6 3.2 8 3.2 s8 -1.4 8 -3.2"/></g>',
      stroke
    ),
    book = sprintf(
      '<g %s><path d="M7 6 h8 a4 4 0 0 1 4 4 v16 H11 a4 4 0 0 0 -4 4 z"/><path d="M25 6 h-8 a4 4 0 0 0 -4 4 v16 h8 a4 4 0 0 1 4 4 z"/></g>',
      stroke
    ),
    voucher = sprintf(
      '<g %s><rect x="8" y="5" width="16" height="22" rx="1.5"/><path d="M16 11 c-3 0 -5 2.4 -5 5 c0 3.4 5 8 5 8 s5 -4.6 5 -8 c0 -2.6 -2 -5 -5 -5 z"/></g>',
      stroke
    ),
    folder = sprintf(
      '<g %s><path d="M4 12 h8 l2 2 h14 v12 H4 z"/><path d="M4 12 V9 h7 l2 3"/></g>',
      stroke
    ),
    code = sprintf(
      '<g %s><path d="M12 8 L6 16 L12 24"/><path d="M20 8 L26 16 L20 24"/><path d="M18 6 L14 26"/></g>',
      stroke
    ),
    document = sprintf(
      '<g %s><path d="M9 4 h9 l6 6 v16 a2 2 0 0 1 -2 2 H11 a2 2 0 0 1 -2 -2 V6 a2 2 0 0 1 2 -2 z"/><path d="M18 4 v6 h6"/><path d="M12 16 h8 M12 20 h8 M12 24 h5"/></g>',
      stroke
    ),
    archive = sprintf(
      '<g %s><path d="M6 13 h20 v12 a2 2 0 0 1 -2 2 H8 a2 2 0 0 1 -2 -2 z"/><path d="M4 8 h24 v6 H4 z"/><path d="M13 18 h6"/></g>',
      stroke
    ),
    list = sprintf(
      '<g %s><path d="M8 8 h16 M8 14 h16 M8 20 h16 M8 26 h10"/><circle cx="5" cy="8" r="1" fill="%s"/><circle cx="5" cy="14" r="1" fill="%s"/><circle cx="5" cy="20" r="1" fill="%s"/></g>',
      stroke, col, col, col
    ),
    fair = sprintf(
      '<g %s><circle cx="16" cy="16" r="10"/><path d="M11 16.5 l3.2 3.2 L22 12"/></g>',
      stroke
    ),
    clock = sprintf(
      '<g %s><circle cx="16" cy="16" r="10"/><path d="M16 10 v6 l4 2"/></g>',
      stroke
    ),
    grid = sprintf(
      '<g %s><rect x="6" y="6" width="8" height="8" rx="1"/><rect x="18" y="6" width="8" height="8" rx="1"/><rect x="6" y="18" width="8" height="8" rx="1"/><rect x="18" y="18" width="8" height="8" rx="1"/></g>',
      stroke
    ),
    terminal = sprintf(
      '<g %s><rect x="4" y="6" width="24" height="18" rx="2"/><path d="M8 12 l4 4 l-4 4"/><path d="M15 20 h8"/></g>',
      stroke
    ),
    branch = sprintf(
      '<g %s><circle cx="10" cy="8" r="2.3"/><circle cx="10" cy="24" r="2.3"/><circle cx="22" cy="14" r="2.3"/><path d="M10 10 v12"/><path d="M10 14 c0 0 4 -2 10 -1"/></g>',
      stroke
    ),
    github = sprintf(
      '<g %s><circle cx="16" cy="16" r="8"/><circle cx="16" cy="16" r="2.2" fill="%s" stroke="none"/><path d="M16 8 v-3 M22.5 11.5 l2.4 -2 M9.5 11.5 l-2.4 -2"/></g>',
      stroke, col
    ),
    share = sprintf(
      '<g %s><circle cx="8" cy="16" r="2.6"/><circle cx="23" cy="8" r="2.6"/><circle cx="23" cy="24" r="2.6"/><path d="M10.4 14.8 L20.6 9.4"/><path d="M10.4 17.2 L20.6 22.6"/></g>',
      stroke
    ),
    tools = sprintf(
      '<g %s><path d="M20 6 a5 5 0 0 0 -6.2 6.6 L6 20.4 l4.2 4.2 L18 16.8 A5 5 0 0 0 26 12 L22 14 l-4 -1.2 L16.8 9 z"/></g>',
      stroke
    ),
    person = sprintf(
      '<g %s><circle cx="16" cy="10" r="4"/><path d="M8 26 c1.2 -6 4.2 -8 8 -8 s6.8 2 8 8"/></g>',
      stroke
    ),
    people = sprintf(
      '<g %s><circle cx="12" cy="11" r="3.2"/><circle cx="21" cy="11" r="3.2"/><path d="M5 26 c1 -5 3.4 -7 7 -7"/><path d="M27 26 c-1 -5 -3.4 -7 -7 -7"/><path d="M12 19 c1.2 0 2.2 .4 4 1.4 c1.8 -1 2.8 -1.4 4 -1.4"/></g>',
      stroke
    ),
    ribbon = sprintf(
      '<g %s><circle cx="16" cy="12" r="6"/><path d="M12.2 17.2 L10 27 l6 -3.2 L22 27 l-2.2 -9.8"/><path d="M13.2 12 l1.8 1.8 l3.6 -3.6"/></g>',
      stroke
    ),
    nodes = sprintf(
      '<g %s><circle cx="10" cy="20" r="3"/><circle cx="22" cy="10" r="3"/><circle cx="22" cy="22" r="3"/><path d="M12.6 18.4 L19.4 12"/><path d="M13 20.6 L19.2 21.4"/></g>',
      stroke
    ),
    cap = sprintf(
      '<g %s><path d="M4 14 L16 8 l12 6 l-12 6 z"/><path d="M10 16.5 v5 c2 2 10 2 12 0 v-5"/><path d="M28 14 v7"/></g>',
      stroke
    ),
    flow = sprintf(
      '<g %s><path d="M10 20 a7 7 0 1 1 2 5.2"/><path d="M10.2 20.2 l0.4 -5.4 l5.2 1.6"/></g>',
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
    gears = paste0("<g>", gear_markup(12, 13, 8.2, col), gear_markup(22.5, 22, 6.4, col), "</g>"),
    analyse = sprintf(
      '<g %s><path d="M5 26 V13"/><path d="M5 26 H18"/><path d="M8 22 l4 -6 l3.2 2.4 l5 -7"/><circle cx="22.5" cy="11" r="5"/><path d="M26 14.6 L29.5 18.2"/></g>',
      stroke
    ),
    reuse = sprintf(
      '<g %s><circle cx="13.5" cy="16" r="5.6"/><path d="M17.6 20.1 L22 24.6"/><path d="M23.5 8.5 A10 10 0 0 0 10.5 6.2"/><path d="M10.2 3.2 v4.2 h4"/></g>',
      stroke
    ),
    stop("Unknown icon: ", name, call. = FALSE)
  )
}

# Same wedges as the life-cycle wheel. 0° is the top, clockwise.
# Create 3 steps (90°), Process 5 steps (150°), Share 3 steps (90°), Re-use 1 step (30°).
sectors <- data.frame(
  id = c("create", "process", "share", "reuse"),
  label = c("CREATE", "PROCESS", "SHARE", "RE-USE"),
  a0 = c(315, 45, 195, 285),
  span = c(90, 150, 90, 30),
  href = c("step_plan.html", "step_digitize.html", "step_document.html", "reuse.html"),
  stringsAsFactors = FALSE
)

# Inner ring: the same four colours, washed toward the paper so they step back.
# Outer rings use those pigments again, mixed and deepened: cedar, olive, and sea.
paper <- "#f3eee6"
ink <- "#3e342c"
cream <- "#f6f1e6"
sector_colour <- c(
  create = "#2C5A4C",
  process = "#865E27",
  share = "#7E545B",
  reuse = "#3F677D"
)
sector_colour <- setNames(
  vapply(sector_colour, function(h) mix_hex(h, paper, 0.66), character(1)),
  names(sector_colour)
)

rings <- data.frame(
  id = c("scientific", "data", "methods", "community"),
  title = c("", "Data documentation", "Methods and technology", "Community of practice"),
  # Cedar from rose and ochre, olive from the green, sea from the slate.
  fill = c(unname(sector_colour["create"]), "#BC9D8A", "#99AC8F", "#8CABAE"),
  r_in = c(114, 256, 388, 520),
  r_out = c(248, 380, 512, 644),
  title_r = c(NA, 286, 374, 462),
  text_r = c(198, 314, 400, 484),
  icon_r = c(218, 328, 414, 500),
  font = c(18, 12, 12, 12),
  stringsAsFactors = FALSE
)

items <- data.frame(
  sector = c(
    "create", "create", "create",
    "process", "process", "process", "process", "process",
    "share", "share", "share",
    "reuse",
    "create", "create", "process", "process", "share", "share", "share", "reuse",
    "create", "create", "process", "process", "process", "share", "reuse",
    "create", "create", "create", "process", "process", "process", "share", "share", "reuse"
  ),
  ring = c(
    rep("scientific", 12),
    rep("data", 8),
    rep("methods", 7),
    rep("community", 9)
  ),
  # Inner ring uses the life-cycle steps, at the same angles as that wheel.
  angle = c(
    330, 0, 30, 60, 90, 120, 150, 180, 210, 240, 270, 300,
    # Data documentation, lined up with the inner steps.
    330, 30, 75, 120, 214, 232, 270, 300,
    rep(NA_real_, 16)
  ),
  label = c(
    "Plan", "Design", "Collect", "Digitize", "Import", "Clean",
    "Process", "Analyse", "Document", "Communicate", "Archive", "Find &\nReuse",
    "Method\nprotocols", "Herbarium\nvouchers", "Raw\ndata\nrepository", "Well\ncommented\ncode",
    "Data\ndictionary", "Data\npaper", "Final\ndata\nrepository", "FAIR (?)\ndata",
    "Contingency\nplanning", "Systematic\ndata\ncollection", "Repro-\nducible\nworkflow", "Code\nbased\nscripts", "Version\ncontrol", "Open\ncode", "Open\nscience\ntools",
    "Student\nselection", "Real\ndata\nreal science\nresponsibility", "Peer learning\nand certification",
    "Problem\nsolving", "Collaboration\nskills", "Quality\ncontrol", "Networking\nopportunities", "Dissemi-\nnation\nof Open\nScience", "Learning\noutcomes"
  ),
  icon = c(
    "clipboard", "pencil", "notebook", "scanner", "folder-in", "broom",
    "gears", "analyse", "document", "share", "archive", "reuse",
    "book", "voucher", "folder", "code", "list", "document", "archive", "fair",
    "clock", "grid", "flow", "terminal", "branch", "github", "tools",
    "person", "document", "ribbon", "people", "people", "fair", "nodes", "share", "cap"
  ),
  stringsAsFactors = FALSE
)

layout <- list(
  canvas_w = 1352,
  canvas_h = 1352,
  cx = 676,
  cy = 676,
  sector_gap = 2.2,
  sector_label_r = 142,
  sector_font = 17,
  well_r = 102,
  paper = paper,
  well = "#efe6d8",
  center = "#3c2e38",
  cream = cream,
  ink = ink
)

item_angles <- function(a0, span, n) {
  pad <- if (n >= 3) span * 0.14 else span * 0.22
  if (n == 1) return((a0 + span / 2) %% 360)
  seq(a0 + pad, a0 + span - pad, length.out = n) %% 360
}

ink_for <- function(hex) {
  if (rel_lum(hex) > 0.42) ink else cream
}

build_svg <- function(outer_items = TRUE, id_prefix = "pm", aria = "PFTC model") {
  L <- layout
  svg_width <- 860
  # The layers figure keeps the life-cycle wheel and draws three thin name rings around it.
  if (!outer_items) {
    rings <- rings
    thin <- 46
    gap <- 8
    r <- 256
    for (id in c("data", "methods", "community")) {
      rings$r_in[rings$id == id] <- r
      rings$r_out[rings$id == id] <- r + thin
      r <- r + thin + gap
    }
    outer_r <- r - gap
    pad <- 28
    side <- as.integer(ceiling((outer_r + pad) * 2))
    L$canvas_w <- side
    L$canvas_h <- side
    L$cx <- side / 2
    L$cy <- side / 2
    svg_width <- 520
  }
  cx <- L$cx
  cy <- L$cy
  sid <- function(name) paste0(id_prefix, "-", name)
  parts <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" class="dlc" viewBox="0 0 %d %d" width="%d" style="max-width:100%%;height:auto" role="group" aria-label="%s" font-family="Avenir Next, Gill Sans, Gill Sans MT, Helvetica Neue, Arial, sans-serif">',
      L$canvas_w, L$canvas_h, svg_width, xml_esc(aria)
    ),
    "<!-- Generated by R/pftc_model_figure.R. Edit that script and re-run. -->",
    paste(
      "<style>",
      ".pm-sector{cursor:pointer;text-decoration:none;}",
      ".pm-sector:hover,.pm-sector:focus{text-decoration:none;outline:none;}",
      ".pm-hit{fill:transparent;}",
      ".pm-sector:hover .pm-hit,.pm-sector:focus .pm-hit{fill:rgba(255,248,236,0.22);}",
      "</style>",
      sep = ""
    ),
    paste(
      "<defs>",
      sprintf('<filter id="%s" x="-20%%" y="-20%%" width="140%%" height="140%%" color-interpolation-filters="sRGB">', sid("shadow")),
      '<feDropShadow dx="0" dy="1.5" stdDeviation="2.2" flood-color="#3a3028" flood-opacity="0.18"/>',
      "</filter>",
      sprintf('<filter id="%s" x="0" y="0" width="100%%" height="100%%">', sid("grain")),
      '<feTurbulence type="fractalNoise" baseFrequency="0.85" numOctaves="2" seed="4" stitchTiles="stitch"/>',
      '<feColorMatrix type="matrix" values="0 0 0 0 0.30  0 0 0 0 0.26  0 0 0 0 0.20  0.16 0.16 0.16 0 -0.03"/>',
      "</filter>",
      sprintf('<marker id="%s" viewBox="0 0 10 10" refX="8.2" refY="5" markerWidth="11" markerHeight="11" orient="auto" markerUnits="userSpaceOnUse">', sid("arrow")),
      '<path d="M0.2 0.8 L9.2 5 L0.2 9.2 Z" fill="#3c2e38"/>',
      "</marker>",
      "</defs>",
      sep = ""
    ),
    sprintf(
      '<circle cx="%.2f" cy="%.2f" r="%.2f" fill="%s"/>',
      cx, cy, min(cx, cy) - 18, L$paper
    )
  )

  # Outer layers are full circles. One quiet colour each, named on the ring.
  outer <- rings[rings$id != "scientific", , drop = FALSE]
  for (k in seq_len(nrow(outer))) {
    ring <- outer[k, ]
    d <- full_ring_d(ring$r_in, ring$r_out, cx, cy)
    parts <- c(
      parts,
      sprintf(
        '<path d="%s" fill="%s" stroke="%s" stroke-width="0.6"/>',
        d, ring$fill, ring$fill
      )
    )
  }

  # Outer-ring words are horizontal. The icon sits above, below, or beside
  # the words, whichever keeps the pair centred in the band.
  draw_ring_labels <- function(chunk, r_in, r_out, font) {
    out <- character(0)
    scale <- 1.15
    gap <- 5
    icon_r <- 16 * scale
    icon_d <- icon_r * 2
    glyph_above <- font * 0.76
    glyph_below <- font * 0.22
    line_step <- font + 5
    layouts_for <- function(lines) {
      n <- length(lines)
      word_w <- max(nchar(lines)) * font * 0.52
      text_h <- glyph_above + (n - 1) * line_step + glyph_below
      above_h <- icon_d + gap + text_h
      beside_w <- word_w + gap + icon_d
      list(
        above = list(
          icon = c(0, -above_h / 2 + icon_r),
          text = c(0, -above_h / 2 + icon_d + gap + glyph_above),
          word_w = word_w, n = n, text_h = text_h
        ),
        below = list(
          icon = c(0, above_h / 2 - icon_r),
          text = c(0, -above_h / 2 + glyph_above),
          word_w = word_w, n = n, text_h = text_h
        ),
        left = list(
          icon = c(-beside_w / 2 + icon_r, 0),
          text = c(-beside_w / 2 + icon_d + gap + word_w / 2, glyph_above - text_h / 2),
          word_w = word_w, n = n, text_h = text_h
        ),
        right = list(
          icon = c(beside_w / 2 - icon_r, 0),
          text = c(beside_w / 2 - icon_d - gap - word_w / 2, glyph_above - text_h / 2),
          word_w = word_w, n = n, text_h = text_h
        )
      )
    }
    corners <- function(lay) {
      tw <- lay$word_w / 2
      top <- lay$text[2] - glyph_above
      bot <- lay$text[2] + (lay$n - 1) * line_step + glyph_below
      rbind(
        c(lay$text[1] - tw, top), c(lay$text[1] + tw, top),
        c(lay$text[1] - tw, bot), c(lay$text[1] + tw, bot),
        c(lay$icon[1] - icon_r, lay$icon[2] - icon_r),
        c(lay$icon[1] + icon_r, lay$icon[2] - icon_r),
        c(lay$icon[1] - icon_r, lay$icon[2] + icon_r),
        c(lay$icon[1] + icon_r, lay$icon[2] + icon_r)
      )
    }
    for (j in seq_len(nrow(chunk))) {
      ang <- chunk$angle[j] %% 360
      lines <- strsplit(chunk$label[j], "\n", fixed = TRUE)[[1]]
      layouts <- layouts_for(lines)
      a <- ang * pi / 180
      outward <- if (abs(cos(a)) >= abs(sin(a))) {
        if (cos(a) > 0) "above" else "below"
      } else if (sin(a) > 0) "right" else "left"
      best <- NULL
      for (name in names(layouts)) {
        lay <- layouts[[name]]
        pts <- corners(lay)
        for (r in seq(r_in + 12, r_out - 12, by = 2)) {
          p <- polar(ang, r, cx, cy)
          rad <- sqrt((p[1] + pts[, 1] - cx)^2 + (p[2] + pts[, 2] - cy)^2)
          pad <- min(min(rad) - r_in, r_out - max(rad))
          # Prefer the outward icon when it sits in the band as comfortably.
          better <- is.null(best) || pad > best$pad + 0.3 ||
            (pad > 2 && best$pad > 2 && pad > best$pad - 2 && name == outward && best$name != outward)
          if (better) best <- list(name = name, r = r, pad = pad, lay = lay)
        }
      }
      lay <- best$lay
      p <- polar(ang, best$r, cx, cy)
      if (best$pad < 2) {
        message(sprintf(
          "%s  %s  r %.0f  pad %.1f",
          gsub("\n", " ", chunk$label[j]), best$name, best$r, best$pad
        ))
      }
      icon_x <- p[1] + lay$icon[1]
      icon_y <- p[2] + lay$icon[2]
      text_x <- p[1] + lay$text[1]
      text_y0 <- p[2] + lay$text[2]
      out <- c(
        out,
        sprintf(
          '<g transform="translate(%.2f %.2f) scale(%.2f)" pointer-events="none">%s</g>',
          icon_x - icon_r, icon_y - icon_r, scale,
          icon_markup(chunk$icon[j], ink)
        )
      )
      for (k in seq_len(lay$n)) {
        out <- c(
          out,
          sprintf(
            '<text x="%.2f" y="%.2f" text-anchor="middle" font-size="%.1f" font-weight="600" fill="%s">%s</text>',
            text_x, text_y0 + (k - 1) * line_step, font, ink, xml_esc(lines[k])
          )
        )
      }
    }
    out
  }

  # Spread a ring's labels around its sectors, leaving the top open for the title.
  angles_for <- function(chunk) {
    angs <- numeric(nrow(chunk))
    pos <- 1
    for (i in seq_len(nrow(sectors))) {
      sec <- sectors[i, ]
      n <- sum(chunk$sector == sec$id)
      if (!n) next
      a0 <- sec$a0 + L$sector_gap / 2
      span <- sec$span - L$sector_gap
      these <- item_angles(a0, span, n)
      if (sec$id == "create") {
        these <- c((a0 + span * 0.22) %% 360, (a0 + span * 0.78) %% 360)[seq_len(n)]
      }
      if (span < 40 && n > 1) {
        mid <- (a0 + span / 2) %% 360
        these <- (mid + c(-18, 18))[seq_len(n)] %% 360
      }
      angs[pos:(pos + n - 1)] <- these
      pos <- pos + n
    }
    angs
  }

  # Life-cycle steps: a horizontal word, with no icon.
  draw_flat <- function(chunk, angs, label_col, font) {
    out <- character(0)
    for (j in seq_len(nrow(chunk))) {
      ang <- angs[j] %% 360
      p <- polar(ang, 208, cx, cy)
      lines <- strsplit(chunk$label[j], "\n", fixed = TRUE)[[1]]
      n <- length(lines)
      # Long words on the sides grow away from the centre, so they clear the wedge name.
      wide <- max(nchar(lines)) > 9
      anchor <- "middle"
      text_x <- p[1]
      if (wide && ang > 180 && ang < 360) {
        anchor <- "end"
        # Communicate sits on the left of Share. Keep the word inside that wedge.
        text_x <- p[1] + 78
      } else if (wide && ang > 0 && ang < 180) {
        anchor <- "start"
        text_x <- p[1] + 6
      }
      drop <- if (chunk$label[j] == "Find &\nReuse") {
        -6
      } else if (chunk$label[j] == "Communicate") {
        4
      } else if (chunk$label[j] %in% c(
        "Plan", "Design", "Collect", "Digitize", "Archive"
      )) 14 else 0
      text_y0 <- p[2] + 4 + drop
      for (k in seq_len(n)) {
        out <- c(
          out,
          sprintf(
            '<text x="%.2f" y="%.2f" text-anchor="%s" font-size="%.1f" font-weight="600" fill="%s">%s</text>',
            text_x, text_y0 + (k - 1) * (font + 5), anchor, font, label_col, xml_esc(lines[k])
          )
        )
      }
    }
    out
  }


  sci <- rings[rings$id == "scientific", ]
  for (i in seq_len(nrow(sectors))) {
    sec <- sectors[i, ]
    a0 <- sec$a0 + L$sector_gap / 2
    span <- sec$span - L$sector_gap
    col <- unname(sector_colour[sec$id])
    d <- annulus_d(a0, a0 + span, sci$r_in, sci$r_out, cx, cy)
    parts <- c(
      parts,
      sprintf(
        '<a class="pm-sector" href="%s" aria-label="%s"><title>%s</title>',
        xml_esc(sec$href), xml_esc(sec$label), xml_esc(sec$label)
      ),
      sprintf('<path d="%s" fill="%s"/>', d, col),
      sprintf('<path class="pm-hit" d="%s"/>', d),
      "</a>"
    )

    mid <- (a0 + span / 2) %% 360
    # Share sits low on the left. Nudge it clockwise so the word rides higher.
    if (sec$id == "share") mid <- (mid + 8) %% 360
    flip <- should_flip(mid)
    if (sec$id == "reuse") {
      # The wedge is narrow, so the name is two short lines at the usual size.
      reuse_size <- L$sector_font
      reuse_shift <- reuse_size * 0.36
      # Outer line first, so the name reads RE- then USE toward the centre.
      reuse_lines <- c("RE-", "USE")
      for (k in seq_along(reuse_lines)) {
        visual_r <- c(156, 128)[k]
        name_r <- if (flip) visual_r + reuse_shift else visual_r - reuse_shift
        parts <- c(
          parts,
          curved_text(
            sid(paste0("sector-reuse-", k)),
            text_arc_d(mid - 12, mid + 12, name_r, flip, cx, cy),
            reuse_lines[k], reuse_size, ink_for(col), "1.0"
          )
        )
      }
    } else {
      name_size <- L$sector_font
      name_half <- min(28, span * 0.42)
      name_shift <- name_size * 0.36
      name_r <- if (flip) L$sector_label_r + name_shift else L$sector_label_r - name_shift
      parts <- c(
        parts,
        curved_text(
          sid(paste0("sector-", sec$id)),
          text_arc_d(mid - name_half, mid + name_half, name_r, flip, cx, cy),
          sec$label, name_size, ink_for(col), "1.6"
        )
      )
    }

    chunk <- items[items$sector == sec$id & items$ring == "scientific", , drop = FALSE]
    if (nrow(chunk)) {
      parts <- c(parts, draw_flat(chunk, chunk$angle, ink_for(col), sci$font))
    }
  }

  # Horizontal labels on every outer ring, drawn after the wedges so they stay visible.
  # The layers-only figure keeps the rings and their names, and leaves the activities off.
  if (outer_items) for (ring_id in c("data", "methods", "community")) {
    chunk <- items[items$ring == ring_id, , drop = FALSE]
    if (anyNA(chunk$angle)) chunk$angle <- angles_for(chunk)
    # The community phrases are long, so place them where the ring can hold them.
    if (ring_id == "methods") {
      # Reproducible workflow sits with Digitize. Version control sits between Process and Analyse.
      chunk$angle <- c(335, 25, 60, 118, 165, 230, 300)
    }
    if (ring_id == "community") {
      # Peer learning sits with real data, near Collect. Problem solving sits with Digitize.
      # Networking and dissemination sit with Communicate.
      chunk$angle <- c(328, 14, 30, 62, 146, 168, 218, 236, 300)
    }
    band <- rings[rings$id == ring_id, ]
    parts <- c(parts, draw_ring_labels(chunk, band$r_in, band$r_out, 18.5))
  }

  # On the full wheel the names sit in a box, clear of the activities.
  # On the layers figure each name is one line, curved along the top of its ring.
  if (!outer_items) {
    title_size <- 18
    for (ring_id in c("data", "methods", "community")) {
      ring <- rings[rings$id == ring_id, ]
      path_r <- (ring$r_in + ring$r_out) / 2 - title_size * 0.32
      text_w <- nchar(ring$title) * title_size * 0.56
      half <- (text_w / path_r) * (180 / pi) / 2 * 1.12
      parts <- c(
        parts,
        curved_text(
          sid(paste0("ring-", ring_id)),
          text_arc_d(-half, half, path_r, FALSE, cx, cy),
          ring$title, title_size, ink, "0.6"
        )
      )
    }
  } else {
    title_w <- c(
      "Data" = 56, "documentation" = 182,
      "Methods and" = 158, "technology" = 136,
      "Community" = 138, "of practice" = 128
    )
    title_dx <- c(data = 12, methods = 0, community = -26)
    for (ring_id in c("data", "methods", "community")) {
      ring <- rings[rings$id == ring_id, ]
      title_lines <- switch(
        ring_id,
        data = c("Data", "documentation"),
        methods = c("Methods and", "technology"),
        community = c("Community", "of practice")
      )
      title_size <- 24
      title_gap <- title_size + 4
      title_x <- cx + unname(title_dx[ring_id])
      title_y <- cy - (ring$r_in + ring$r_out) / 2 - title_gap / 2 + title_size * 0.32
      pad_x <- 14
      pad_y <- 8
      box_w <- max(unname(title_w[title_lines])) + pad_x * 2
      box_h <- 24.2 + title_gap + 8.6 + pad_y * 2
      box_y <- title_y - 24.2 - pad_y
      box_fill <- mix_hex(ring$fill, ink, 0.26)
      box_edge <- mix_hex(ring$fill, ink, 0.42)
      parts <- c(
        parts,
        sprintf(
          '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="10" fill="%s" stroke="%s" stroke-width="1.4"/>',
          title_x - box_w / 2, box_y, box_w, box_h, box_fill, box_edge
        )
      )
      for (line in seq_along(title_lines)) {
        parts <- c(
          parts,
          sprintf(
            '<text x="%.2f" y="%.2f" text-anchor="middle" font-size="%.1f" font-weight="700" letter-spacing="0.2" fill="%s">%s</text>',
            title_x, title_y + (line - 1) * title_gap, title_size, ink, xml_esc(title_lines[line])
          )
        )
      }
    }
  }

  parts <- c(
    parts,
    sprintf(
      '<circle cx="%.2f" cy="%.2f" r="%.2f" fill="%s"/>',
      cx, cy, L$well_r, L$well
    )
  )

  # Timeline in the well. Plan and Design are pre-course, Collect through
  # Document are the field course, and Communicate through Find & Reuse are post-course.
  # Each arrow runs around the part of the circle those steps occupy.
  arrow_r <- 96
  label_font <- 19
  phases <- data.frame(
    label = c("pre-course", "field\ncourse", "post-course"),
    a0 = c(315, 15, 225),
    span = c(60, 210, 90),
    stringsAsFactors = FALSE
  )
  label_box <- function(lines, ang, r, font) {
    p <- polar(ang, r, cx, cy)
    line_step <- font + 3
    n <- length(lines)
    half_w <- max(nchar(lines)) * font * 0.60 / 2
    half_h <- (n - 1) * line_step / 2 + font * 0.72
    list(
      x = p[1], y = p[2],
      x0 = p[1] - half_w, x1 = p[1] + half_w,
      y0 = p[2] - half_h, y1 = p[2] + half_h,
      line_step = line_step, n = n
    )
  }
  inside_ring <- function(b, r_max) {
    corners <- list(
      c(b$x0, b$y0), c(b$x1, b$y0), c(b$x0, b$y1), c(b$x1, b$y1)
    )
    all(vapply(corners, function(pt) {
      sqrt((pt[1] - cx)^2 + (pt[2] - cy)^2) <= r_max
    }, logical(1)))
  }
  choose_place <- function(text, ang) {
    search <- function(lines) {
      for (r in seq(arrow_r - 16, 36, by = -1)) {
        b <- label_box(lines, ang, r, label_font)
        if (inside_ring(b, arrow_r - 10)) {
          return(list(lines = lines, r = r, box = b))
        }
      }
      NULL
    }
    hit <- search(strsplit(text, "\n", fixed = TRUE)[[1]])
    if (is.null(hit) && grepl("-", text, fixed = TRUE)) {
      hit <- search(strsplit(sub("-", "-\n", text), "\n", fixed = TRUE)[[1]])
    }
    if (is.null(hit)) stop("No place for timeline label: ", text, call. = FALSE)
    hit
  }
  for (i in seq_len(nrow(phases))) {
    a0 <- phases$a0[i]
    start <- a0 + 4
    end <- a0 + phases$span[i] - 11
    p0 <- polar(start, arrow_r, cx, cy)
    p1 <- polar(end, arrow_r, cx, cy)
    large <- as.integer((end - start) > 180)
    parts <- c(
      parts,
      sprintf(
        '<path d="M %.2f %.2f A %.2f %.2f 0 %d 1 %.2f %.2f" fill="none" stroke="%s" stroke-width="2.8" stroke-linecap="round" marker-end="url(#%s)"/>',
        p0[1], p0[2], arrow_r, arrow_r, large, p1[1], p1[2], L$center, sid("arrow")
      )
    )
    ang <- (a0 + phases$span[i] / 2) %% 360
    place <- choose_place(phases$label[i], ang)
    b <- place$box
    # pre-course has room above it, toward the arrow at the top of the well.
    y_nudge <- if (phases$label[i] == "pre-course") -14 else 0
    text_y0 <- b$y - (b$n - 1) * b$line_step / 2 + label_font * 0.32 + y_nudge
    for (k in seq_len(b$n)) {
      parts <- c(
        parts,
        sprintf(
          '<text x="%.2f" y="%.2f" text-anchor="middle" font-size="%.1f" font-weight="500" fill="%s" font-family="Iowan Old Style, Palatino Linotype, Palatino, Georgia, serif">%s</text>',
          b$x, text_y0 + (k - 1) * b$line_step, label_font, L$center, xml_esc(place$lines[k])
        )
      )
    }
  }

  parts <- c(
    parts,
    sprintf(
      '<rect width="%d" height="%d" filter="url(#%s)" pointer-events="none"/>',
      L$canvas_w, L$canvas_h, sid("grain")
    ),
    "</svg>"
  )
  paste(parts, collapse = "\n")
}

# The wheel unrolled. The data life cycle is the top band. Documentation,
# methods, and community sit under it, each activity with its symbol.
build_bands_svg <- function() {
  gutter <- 78
  left <- 22
  right <- 22
  top <- 22
  col_gap <- 12
  band_gap <- 14
  weights <- c(create = 2.5, process = 3.7, share = 2.5, reuse = 1.9)
  content_w <- 900
  usable <- content_w - col_gap * (length(weights) - 1)
  col_w <- usable * weights / sum(weights)
  names(col_w) <- names(weights)
  W <- left + gutter + content_w + right

  # Hyphens in the wheel labels are only there to fit the ring. Join them back.
  lines_of <- function(text) {
    text <- gsub("-\n", "", text, fixed = TRUE)
    strsplit(text, "\n", fixed = TRUE)[[1]]
  }
  font <- 15
  line_h <- 19
  item_gap <- 12
  pad_y <- 12
  icon_scale <- 0.88
  icon_px <- 32 * icon_scale
  # Same weight and tracking as the curved Create and Process names.
  title_size <- 17
  title_track <- 1.6

  # Wide enough for Avenir Next at this size and tracking, plus a little air at each end.
  title_px <- function(text) {
    n <- nchar(text)
    n * title_size * 0.70 + max(0, n - 1) * title_track + title_size
  }
  sector_rows <- function(ring_id, sec) {
    items[items$ring == ring_id & items$sector == sec, , drop = FALSE]
  }
  activity_h <- function(label) {
    max(icon_px, length(lines_of(label)) * line_h)
  }
  stack_h <- function(ring_id, sec) {
    chunk <- sector_rows(ring_id, sec)
    n <- nrow(chunk)
    if (!n) return(0)
    sum(vapply(chunk$label, activity_h, numeric(1))) + (n - 1) * item_gap
  }

  text_block <- function(x, y, lines, size, weight, fill, anchor = "middle") {
    vapply(seq_along(lines), function(k) {
      sprintf(
        '<text x="%.2f" y="%.2f" text-anchor="%s" font-size="%.1f" font-weight="%s" fill="%s">%s</text>',
        x, y + (k - 1) * line_h, anchor, size, weight, fill, xml_esc(lines[k])
      )
    }, character(1))
  }
  draw_icon <- function(name, icon_left, icon_top) {
    sprintf(
      '<g transform="translate(%.2f %.2f) scale(%.3f)" pointer-events="none">%s</g>',
      icon_left, icon_top, icon_scale, icon_markup(name, ink)
    )
  }
  # Icon to the left of the words, the pair centred in the column.
  draw_activity <- function(x, w, row_top, label, icon_name) {
    lines <- lines_of(label)
    row_h <- activity_h(label)
    text_w <- max(nchar(lines)) * font * 0.54
    group_w <- icon_px + 7 + text_w
    max_w <- w - 10
    if (group_w > max_w) {
      text_w <- max_w - icon_px - 7
      group_w <- max_w
    }
    group_left <- x + (w - group_w) / 2
    icon_top <- row_top + (row_h - icon_px) / 2
    text_h <- length(lines) * line_h
    text_y <- row_top + (row_h - text_h) / 2 + font * 0.78
    text_x <- group_left + icon_px + 7 + text_w / 2
    c(
      draw_icon(icon_name, group_left, icon_top),
      text_block(text_x, text_y, lines, font, "600", ink)
    )
  }
  draw_stack <- function(x, w, stack_top, chunk) {
    out <- character(0)
    cursor <- stack_top
    if (!nrow(chunk)) return(out)
    for (j in seq_len(nrow(chunk))) {
      out <- c(out, draw_activity(x, w, cursor, chunk$label[j], chunk$icon[j]))
      cursor <- cursor + activity_h(chunk$label[j]) + item_gap
    }
    out
  }
  # Two short vertical lines, in the Create / Process lettering.
  draw_vertical_title <- function(band_y, band_h, fill, lines) {
    box_fill <- mix_hex(fill, ink, 0.26)
    box_edge <- mix_hex(fill, ink, 0.42)
    line_gap <- title_size + 4
    box_w <- 18 + length(lines) * line_gap
    inset <- 10
    box_h <- max(36, band_h - inset * 2)
    box_x <- left + 8
    box_y <- band_y + (band_h - box_h) / 2
    tx <- box_x + box_w / 2
    ty <- box_y + box_h / 2
    out <- sprintf(
      '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="10" fill="%s" stroke="%s" stroke-width="1.4"/>',
      box_x, box_y, box_w, box_h, box_fill, box_edge
    )
    n <- length(lines)
    for (k in seq_len(n)) {
      x_off <- (k - (n + 1) / 2) * line_gap
      out <- c(
        out,
        sprintf(
          '<text text-anchor="middle" dominant-baseline="middle" font-size="%.0f" font-weight="600" letter-spacing="%.1f" fill="%s" transform="translate(%.2f %.2f) rotate(-90)">%s</text>',
          title_size, title_track, ink, tx + x_off, ty, xml_esc(lines[k])
        )
      )
    }
    out
  }

  life_lines <- c("Data life", "cycle")
  life_h <- max(72, title_px(life_lines[1]) + 28)
  ring_titles <- list(
    data = c("Data", "documentation"),
    methods = c("Methods and", "technology"),
    community = c("Community", "of practice")
  )
  ring_order <- c("data", "methods", "community")
  ring_h <- vapply(ring_order, function(id) {
    body <- max(vapply(names(weights), function(sec) stack_h(id, sec), numeric(1)))
    longest <- max(vapply(ring_titles[[id]], title_px, numeric(1)))
    max(body + pad_y * 2, longest + 28)
  }, numeric(1))
  H <- top + life_h + sum(ring_h) + band_gap * 3 + top

  parts <- c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    sprintf(
      '<svg xmlns="http://www.w3.org/2000/svg" class="dlc" viewBox="0 0 %.0f %.0f" width="960" style="max-width:100%%;height:auto" role="group" aria-label="PFTC model as bands" font-family="Avenir Next, Gill Sans, Gill Sans MT, Helvetica Neue, Arial, sans-serif">',
      W, H
    ),
    "<!-- Generated by R/pftc_model_figure.R. Edit that script and re-run. -->",
    sprintf('<rect width="%.0f" height="%.0f" fill="%s"/>', W, H, paper)
  )

  content_x <- left + gutter
  xs <- numeric(length(weights))
  names(xs) <- names(weights)
  xcursor <- content_x
  for (id in names(weights)) {
    xs[id] <- xcursor
    xcursor <- xcursor + col_w[id] + col_gap
  }

  y <- top
  parts <- c(parts, draw_vertical_title(y, life_h, "#cbbba8", life_lines))
  for (i in seq_len(nrow(sectors))) {
    sec <- sectors[i, ]
    col <- unname(sector_colour[sec$id])
    x <- xs[sec$id]
    w <- col_w[sec$id]
    parts <- c(
      parts,
      sprintf(
        '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="14" fill="%s"/>',
        x, y, w, life_h, col
      ),
      sprintf(
        '<text x="%.2f" y="%.2f" text-anchor="middle" dominant-baseline="middle" font-size="17" font-weight="600" letter-spacing="1.6" fill="%s">%s</text>',
        x + w / 2, y + life_h / 2, ink, xml_esc(sec$label)
      )
    )
  }
  y <- y + life_h + band_gap

  for (ring_id in ring_order) {
    ring <- rings[rings$id == ring_id, ]
    h <- unname(ring_h[ring_id])
    parts <- c(
      parts,
      sprintf(
        '<rect x="%.2f" y="%.2f" width="%.2f" height="%.2f" rx="16" fill="%s"/>',
        left, y, W - left - right, h, ring$fill
      ),
      draw_vertical_title(y, h, ring$fill, ring_titles[[ring_id]])
    )
    rule <- mix_hex(ring$fill, ink, 0.18)
    body <- max(vapply(names(weights), function(sec) stack_h(ring_id, sec), numeric(1)))
    stack_top <- y + (h - body) / 2
    for (id in names(weights)) {
      if (id != names(weights)[1]) {
        parts <- c(
          parts,
          sprintf(
            '<line x1="%.2f" y1="%.2f" x2="%.2f" y2="%.2f" stroke="%s" stroke-width="1"/>',
            xs[id] - col_gap / 2, y + 14, xs[id] - col_gap / 2, y + h - 14, rule
          )
        )
      }
      parts <- c(parts, draw_stack(xs[id], col_w[id], stack_top, sector_rows(ring_id, id)))
    }
    y <- y + h + band_gap
  }

  paste(c(parts, "</svg>"), collapse = "\n")
}

project_root <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg)) {
    return(normalizePath(file.path(dirname(sub("^--file=", "", file_arg[1])), "..")))
  }
  if (file.exists("R/pftc_model_figure.R")) return(normalizePath("."))
  if (file.exists("pftc_model_figure.R")) return(normalizePath(".."))
  normalizePath(".")
}

root <- project_root()
img <- file.path(root, "quarto_webpage", "images")
dir.create(img, recursive = TRUE, showWarnings = FALSE)
out <- file.path(img, "pftc_model.svg")
writeLines(build_svg(), out, useBytes = TRUE)
message("Wrote ", out)
layers <- file.path(img, "pftc_model_layers.svg")
writeLines(
  build_svg(
    outer_items = FALSE,
    id_prefix = "pml",
    aria = "PFTC model layers"
  ),
  layers,
  useBytes = TRUE
)
message("Wrote ", layers)
bands <- file.path(img, "pftc_model_bands.svg")
writeLines(build_bands_svg(), bands, useBytes = TRUE)
message("Wrote ", bands)
