# Rendering helpers for the Research page.
# Reads entries produced by yaml::read_yaml(publications.yml) and writes
# HTML/markdown via cat() for use in a `results: asis` chunk.
#
# Per entry, up to three buttons appear (each only if there's something to
# show): Abstract, Links (toggle box for Replication Data, PDF, code, OSF,
# pre-registration, ...), and Metrics (toggle box: Altmetric bar badge +
# Dimensions rectangle badge + a Google-Scholar-styled search link, all
# sized to sit neatly in a row).

# Turns "Race, Identity & Politics" into "race-identity-politics" for use
# as an HTML attribute / data-filter value.
pub_slugify <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "-", x)
  gsub("(^-+|-+$)", "", x)
}

pub_categories_of <- function(p) {
  if (is.null(p$categories)) return(character(0))
  unlist(p$categories)
}

pub_link_button <- function(url, label, icon_class) {
  if (is.null(url) || is.na(url) || !nzchar(url)) return("")
  sprintf(
    '<a class="btn btn-sm btn-outline-secondary rounded-pill" href="%s" target="_blank" rel="noopener">
       <i class="%s"></i> %s
     </a>',
    url, icon_class, label
  )
}

pub_toggle_button <- function(target_id, label) {
  sprintf(
    '<button class="btn btn-sm btn-outline-primary rounded-pill" type="button"
       data-bs-toggle="collapse" data-bs-target="#%s"
       aria-expanded="false" aria-controls="%s">
       <i class="bi bi-chevron-down"></i> %s
     </button>',
    target_id, target_id, label
  )
}

# Resolves the bare DOI for an entry: prefers an explicit `doi:` field,
# otherwise extracts it from a https://doi.org/... url. Returns NULL if
# neither is available (e.g. `url` points straight at a journal page with
# no doi.org link and no explicit `doi:` field was set).
pub_doi <- function(p) {
  if (!is.null(p$doi) && nzchar(p$doi)) return(p$doi)
  url <- p$url
  if (is.null(url) || is.na(url) || !nzchar(url)) return(NULL)
  doi <- sub("^https?://(dx\\.)?doi\\.org/", "", url)
  if (identical(doi, url)) return(NULL)
  doi
}

# Altmetric's "classic" badge (type 2): a rectangle with the Altmetric
# logo plus a legible score number, ~88x22px - the same two-part
# logo-block + number-block shape as the Dimensions badge below, so the
# two sit together as a matched pair. (The "bar" style is more compact
# but shows only color, no readable number - not useful as a badge.)
pub_altmetric_widget <- function(doi) {
  if (is.null(doi)) return("")
  sprintf(
    '<div class="altmetric-embed pub-metric-widget" data-badge-type="2" data-badge-popover="right" data-doi="%s" data-hide-no-mentions="true"></div>',
    doi
  )
}

# Dimensions citation badge, small rectangular style to match Altmetric's
# "bar" type. Auto-hides for papers with zero citations so brand-new
# entries don't show an empty/zero badge.
pub_dimensions_widget <- function(doi) {
  if (is.null(doi)) return("")
  sprintf(
    '<span class="__dimensions_badge_embed__ pub-metric-widget" data-doi="%s" data-style="small_rectangle" data-hide-zero-citations="true"></span>',
    doi
  )
}

# Google Scholar's cap-and-"g" mark (extracted from the Academicons icon
# font as SVG path data). This is embedded as a data-URI <img>, not inline
# <svg> markup - Quarto/Pandoc reparses raw HTML blocks and lowercases
# unrecognized attribute names, including the case-sensitive "viewBox",
# which silently breaks the SVG's coordinate scaling (it rendered as an
# empty box, twice). A data URI's contents are just a string attribute
# value, never reparsed as markup, so the casing survives intact.
pub_scholar_icon_svg <- '<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 384 512"><path transform="translate(0,448) scale(1,-1)" fill="#ffffff" d="M343.759 341.338c7.33887 -0.942383 6.56152 -5.35449 6.56152 -10.6309v-128.667c0 -5.95898 -4.875 -10.834 -10.834 -10.834h-3.98828c-5.95898 0 -10.834 4.87598 -10.834 10.834v128.667c0 5.28906 -0.769531 9.70605 6.62402 10.6357v17.1182l-56.9072 -46.6719c0.657227 -1.21875 1.28125 -2 1.875 -3.02246c5 -8.85254 7.54785 -19.8613 7.54785 -33.3135c0 -10.3115 -1.71875 -19.5781 -5.2041 -27.7568c-3.46875 -8.18066 -7.6875 -14.8613 -12.6094 -20.0078c-4.9209 -5.16602 -9.85938 -9.88379 -14.7969 -14.1338c-4.93555 -4.25781 -9.15527 -8.69629 -12.6094 -13.3057c-3.48145 -4.59375 -5.21582 -9.34375 -5.21582 -14.2656c0 -4.92969 2.25 -9.9375 6.73438 -14.9834c4.46875 -5.0625 9.96777 -9.96777 16.4688 -14.8135c6.51562 -4.8125 13.0146 -10.1553 19.5156 -15.9678c6.51465 -5.80273 11.9834 -13.2715 16.4521 -22.3438c4.50098 -9.10254 6.75 -19.125 6.75 -30.1406c0 -14.5312 -3.7041 -27.6562 -11.0938 -39.3359c-7.40723 -11.6318 -17.0625 -20.9141 -28.9219 -27.7188c-11.8916 -6.85156 -24.6094 -12.0078 -38.1719 -15.4922c-13.5928 -3.45312 -27.0781 -5.1875 -40.5469 -5.1875c-8.50098 0 -17.0781 0.65625 -25.7061 1.99805c-8.6543 1.34473 -17.3262 3.71973 -26.0605 7.06348c-8.75 3.36719 -16.501 7.5 -23.2188 12.4688c-6.73438 4.91309 -12.1563 11.2578 -16.3125 18.9834c-4.15723 7.72656 -6.21875 16.4141 -6.21875 26.0625c0 11.4463 3.18652 22.0625 9.57715 31.9619c6.39062 9.82715 14.8594 18.0322 25.3906 24.5313c18.376 11.4297 47.2031 18.4922 86.4365 21.1631c-8.96777 11.2109 -13.4668 21.7666 -13.4668 31.6416c0 5.61914 1.46777 11.6416 4.375 18.1426c-4.68848 -0.65625 -9.51562 -1.02441 -14.4385 -1.02441c-21.0752 0 -38.874 6.85059 -53.3271 20.6475c-14.4521 13.7744 -21.6719 31.0166 -21.6719 51.8613c0 2.17871 0.0644531 4.10059 0.219727 6.22559h-85.6562l129.157 112.273h213.89l-19.7646 -15.4307v-27.2314zM236.398 118.858h0.000976562c-1.125 0.789062 -4.04688 2.92188 -8.75098 6.38477c-4.70215 3.49316 -7.28027 5.34375 -7.73438 5.5625c-2.46875 0.445312 -5.93555 0.696289 -10.4043 0.696289c-9.6416 0 -19.127 -0.852539 -28.4385 -2.55469c-9.3125 -1.65625 -18.5 -4.45215 -27.5615 -8.39844c-9.09473 -3.92383 -16.4395 -9.68945 -22.0322 -17.3154c-5.62598 -7.61621 -8.42285 -16.5859 -8.42285 -26.9053c0 -9.83594 2.46875 -18.6104 7.40625 -26.25c4.92383 -7.59375 11.4229 -13.5391 19.501 -17.7969c8.0791 -4.26562 16.5469 -7.48535 25.3906 -9.60156c8.85938 -2.10254 17.875 -3.19531 27.0625 -3.19531c18.1709 0 33.8125 4.09277 46.9375 12.2734c13.0928 8.17969 19.6562 20.8115 19.6562 37.8594c0 3.58594 -0.499023 7.11719 -1.48535 10.5615c-1.03027 3.48535 -2.0459 6.46875 -3.0459 8.94629c-1 2.42969 -2.90625 5.35938 -5.71875 8.7334c-2.79688 3.38281 -4.9375 5.88379 -6.37598 7.54785c-1.46875 1.71875 -4.21582 4.1709 -8.26562 7.41406c-4.01465 3.25781 -6.57715 5.28809 -7.71875 6.03809zM233.04 219.437h0.000976563c7.37402 8.84473 11.0762 20.0078 11.0762 33.4551c0 11.4541 -1.95117 23.4443 -5.87305 35.9834c-3.90723 12.5547 -10.4229 23.8828 -19.5166 33.9678c-9.06348 10.1191 -19.5625 15.1572 -31.4375 15.1572c-13.4385 0 -23.751 -4.83496 -30.9375 -14.4854c-7.18555 -9.62402 -10.7812 -21.2812 -10.7812 -34.9756c0 -11.6641 1.96875 -23.5391 5.90625 -35.6475c3.9209 -12.1035 10.3438 -22.9082 19.3438 -32.4307c8.96777 -9.54785 19.4062 -14.3125 31.2656 -14.3125c13.2188 0 23.5469 4.42871 30.9531 13.2881z"/></svg>'

pub_scholar_icon_data_uri <- paste0(
  "data:image/svg+xml;charset=utf-8,",
  utils::URLencode(pub_scholar_icon_svg, reserved = TRUE)
)

# Set `scholar:` in publications.yml to point at a specific Google Scholar
# cluster URL; otherwise this falls back to a title search. Styled to look
# like an Altmetric/Dimensions badge (small two-tone rectangle) rather than
# a plain button, since Google Scholar has no embeddable widget of its own.
pub_scholar_button <- function(p) {
  url <- p$scholar
  if (is.null(url)) {
    query <- paste0('"', p$title, '"')
    url <- paste0(
      "https://scholar.google.com/scholar?q=",
      utils::URLencode(query, reserved = TRUE)
    )
  }
  sprintf(
    '<a class="pub-scholar-badge pub-metric-widget" href="%s" target="_blank" rel="noopener">
       <span class="pub-scholar-badge-icon"><img src="%s" width="13" height="13" alt=""></span>
       <span class="pub-scholar-badge-label">Scholar</span>
     </a>',
    url, pub_scholar_icon_data_uri
  )
}

render_pub <- function(p) {
  abs_id <- paste0("abs-", p$id)
  links_id <- paste0("links-", p$id)
  metrics_id <- paste0("metrics-", p$id)

  # Title links to the journal page (`url`) if there is one; otherwise,
  # for a working paper with only a locally-hosted draft (`pdf`), falls
  # back to linking straight to that PDF so the title is always clickable
  # once there's something to read.
  title_href <- if (!is.null(p$url) && nzchar(p$url)) {
    p$url
  } else if (!is.null(p$pdf) && nzchar(p$pdf)) {
    p$pdf
  } else {
    NULL
  }
  title_html <- if (!is.null(title_href)) {
    sprintf('<a href="%s" target="_blank" rel="noopener">%s</a>', title_href, p$title)
  } else {
    p$title
  }

  citation <- if (isTRUE(p$online_first)) {
    sprintf("%s (Online First). %s, %s", p$authors, title_html, p$venue)
  } else if (!is.null(p$year)) {
    sprintf("%s (%d). %s, %s", p$authors, p$year, title_html, p$venue)
  } else {
    # p$authors already ends in an abbreviated initial (e.g. "Neuner, F.G."),
    # so no extra period is needed before the title
    sprintf("%s %s.", p$authors, title_html)
  }

  # --- Abstract ---
  abstract_text <- if (!is.null(p$abstract)) {
    p$abstract
  } else {
    "*No abstract available online for this piece yet — reach out if you'd like a draft.*"
  }
  abstract_btn <- pub_toggle_button(abs_id, "Abstract")

  # --- Links (Replication Data, plus anything else you've added: PDF,
  # code, OSF, pre-registration, ...), ordered PDF, Supplementary
  # Material, Replication Data, then everything else as given in the yaml.
  extra_links <- pub_all_links(p$pdf, p$data, p$links)
  links_btn <- ""
  links_box <- ""
  if (length(extra_links) > 0) {
    links_btn <- pub_toggle_button(links_id, "Links")
    links_box <- sprintf(
      '<div class="collapse pub-box" id="%s">\n<div class="pub-links">%s</div>\n</div>\n\n',
      links_id, paste(extra_links, collapse = "\n")
    )
  }

  # --- Metrics (Altmetric bar + Dimensions rectangle + Google Scholar) ---
  doi <- pub_doi(p)
  metrics_btn <- ""
  metrics_box <- ""
  if (!is.null(doi) || !is.null(p$scholar)) {
    metrics_btn <- pub_toggle_button(metrics_id, "Metrics")
    metrics_content <- paste(
      c(
        pub_altmetric_widget(doi),
        pub_dimensions_widget(doi),
        pub_scholar_button(p)
      ),
      collapse = "\n"
    )
    metrics_box <- sprintf(
      '<div class="collapse pub-box" id="%s">\n<div class="pub-links pub-metrics">%s</div>\n</div>\n\n',
      metrics_id, metrics_content
    )
  }

  cat_slugs <- paste(vapply(pub_categories_of(p), pub_slugify, character(1)), collapse = "|")
  cat(sprintf('<div class="pub-entry" data-category="%s">\n\n', cat_slugs))
  cat(sprintf('<p class="pub-citation">%s</p>\n\n', citation))
  cat(sprintf(
    '<div class="pub-links">%s%s%s</div>\n\n',
    abstract_btn, links_btn, metrics_btn
  ))
  cat(sprintf(
    '<div class="collapse pub-abstract" id="%s">\n<div class="card card-body">\n\n%s\n\n</div>\n</div>\n\n',
    abs_id, abstract_text
  ))
  cat(links_box)
  cat(metrics_box)
  cat('</div>\n\n')
}

# The Links box always sorts to this order regardless of how entries are
# listed in publications.yml: PDF, Supplementary Material, Replication
# Data, then everything else in the order given.
pub_link_priority <- function(label) {
  priority <- c("PDF" = 1, "Supplementary Material" = 2, "Replication Data" = 3)[label]
  if (is.na(priority)) 4 else priority
}

# Combines the `pdf:` (local draft, mainly for working papers - see
# publications.yml's header comment) and `data:` replication link with the
# `links:` list from publications.yml (each item: label, url, and
# optionally icon) into one ordered set of pill buttons. Defaults to a
# generic link icon if none is given for a `links:` entry.
pub_all_links <- function(pdf_url, data_url, links_list) {
  entries <- c(
    list(list(label = "PDF", url = pdf_url, icon = "bi bi-file-earmark-pdf")),
    list(list(label = "Replication Data", url = data_url, icon = "bi bi-box-seam")),
    lapply(links_list, function(l) {
      list(label = l$label, url = l$url, icon = if (!is.null(l$icon)) l$icon else "bi bi-link-45deg")
    })
  )
  html <- vapply(entries, function(e) pub_link_button(e$url, e$label, e$icon), character(1))
  keep <- nzchar(html)
  entries <- entries[keep]
  html <- html[keep]
  if (length(html) == 0) return(character(0))
  priority <- vapply(entries, function(e) pub_link_priority(e$label), numeric(1))
  html[order(priority, seq_along(html))]
}

render_pub_list <- function(pubs) {
  invisible(lapply(pubs, render_pub))
}

# Renders a Categories list - "All (N)" plus one line per category with
# its own count - meant to sit in the page margin next to the "On this
# page" TOC, the way https://drganghe.github.io/quarto-academic-website-template/pub-listing.html
# does it. `container_id` must match the id on the wrapping <div> around
# the corresponding render_pub_list() call - the filter script (see
# research.qmd) uses it to find which .pub-entry elements to show/hide.
# Categories come from each entry's `categories:` field in publications.yml.
render_category_filter <- function(pubs, container_id) {
  cats_per_pub <- lapply(pubs, pub_categories_of)
  all_cats <- sort(unique(unlist(cats_per_pub)))
  if (length(all_cats) == 0) return(invisible(NULL))

  counts <- vapply(all_cats, function(cat_label) {
    sum(vapply(cats_per_pub, function(cs) cat_label %in% cs, logical(1)))
  }, integer(1))

  items <- vapply(seq_along(all_cats), function(i) {
    sprintf(
      '<li><button type="button" class="pub-filter-btn" data-filter="%s">%s <span class="pub-filter-count">(%d)</span></button></li>',
      pub_slugify(all_cats[i]), all_cats[i], counts[i]
    )
  }, character(1))

  cat(sprintf(
    '::: {.column-margin}\n<div class="pub-filter-bar" data-target="#%s">\n<div class="pub-filter-heading">Categories</div>\n<ul class="pub-filter-list">\n<li><button type="button" class="pub-filter-btn active" data-filter="all">All <span class="pub-filter-count">(%d)</span></button></li>\n%s\n</ul>\n</div>\n:::\n\n',
    container_id, length(pubs), paste(items, collapse = "\n")
  ))
}
