suppressPackageStartupMessages({
  library(rvest)
  library(httr)
  library(dplyr)
  library(readr)
  library(stringr)
  library(lubridate)
})

ua       <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36"
today    <- lubridate::today()
csv_path <- file.path("data", "obituaries.csv")

# ── Helpers ────────────────────────────────────────────────────────────────────

html_escape <- function(x) {
  x <- gsub("&",  "&amp;",  x, fixed = TRUE)
  x <- gsub("<",  "&lt;",   x, fixed = TRUE)
  x <- gsub(">",  "&gt;",   x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  x
}

fetch_page <- function(url) {
  resp <- GET(url, add_headers("User-Agent" = ua))
  if (http_error(resp)) stop("HTTP ", status_code(resp), " for ", url)
  read_html(content(resp, "text", encoding = "UTF-8"))
}

# ── Load existing data ─────────────────────────────────────────────────────────

if (file.exists(csv_path)) {
  existing <- read_csv(csv_path, show_col_types = FALSE) |>
    mutate(
      date_added = as_date(date_added),
      birth_year = as.character(birth_year)
    )
} else {
  existing <- tibble(
    source     = character(),
    name       = character(),
    birth_year = character(),
    url        = character(),
    date_added = as_date(character())
  )
}

# ── Scrape The Record ──────────────────────────────────────────────────────────

scrape_record <- function() {
  message("Scraping The Record...")
  page    <- fetch_page("https://obituaries.therecord.com/obituaries/obituaries/search/?limit=125")
  entries <- page |> html_elements("a:has(.name)")

  names <- entries |>
    html_element(".name") |>
    html_text(trim = TRUE) |>
    str_squish() |>
    str_remove("\\s*Obituary.*$") |>
    str_squish()

  dates_txt <- entries |>
    html_element(".dates") |>
    html_text(trim = TRUE) |>
    str_squish()

  birth_years <- str_extract(dates_txt, "\\b(19|20)\\d{2}\\b")

  urls <- paste0("https://obituaries.therecord.com", entries |> html_attr("href"))

  tibble(
    source     = "The Record",
    name       = names,
    birth_year = birth_years,
    url        = urls,
    date_added = today
  ) |>
    filter(!is.na(name), name != "")
}

# ── Scrape ERB & Good ──────────────────────────────────────────────────────────

get_erb_birth_year <- function(url) {
  tryCatch({
    page     <- fetch_page(url)
    headings <- page |> html_elements("h1, h2, h3") |> html_text(trim = TRUE)
    match    <- str_subset(headings, "^\\d{4}\\s*-\\s*\\d{4}$")
    if (length(match) > 0) str_extract(match[1], "^\\d{4}") else NA_character_
  }, error = function(e) NA_character_)
}

scrape_erb_good <- function(n_pages = 2) {
  message("Scraping ERB & Good...")
  base_url    <- "https://erbgood.com"
  all_entries <- list()

  for (p in seq_len(n_pages)) {
    suffix <- if (p == 1) "" else paste0("?page=", p - 1)
    url    <- paste0(base_url, "/tribute/all-services/index.html", suffix)

    page <- tryCatch(fetch_page(url), error = function(e) {
      message("  Error on page ", p, ": ", e$message)
      NULL
    })
    if (is.null(page)) break

    rows <- page |> html_elements(".tribute-row")
    if (length(rows) == 0) break

    link_paths <- rows |>
      html_element(".deceased-name a") |>
      html_attr("href") |>
      str_remove("#.*$")

    all_entries[[p]] <- tibble(
      source = "ERB & Good",
      name   = rows |> html_element(".deceased-name") |> html_text(trim = TRUE) |> str_squish(),
      url    = paste0(base_url, link_paths)
    )
    Sys.sleep(1)
  }

  if (length(all_entries) == 0) {
    return(tibble(
      source = character(), name = character(),
      birth_year = character(), url = character(),
      date_added = as_date(character())
    ))
  }

  erb_df <- bind_rows(all_entries)

  # Carry forward known birth years to avoid re-fetching individual pages
  known <- existing |>
    filter(source == "ERB & Good") |>
    select(url, birth_year)

  erb_df <- erb_df |>
    left_join(known, by = "url") |>
    mutate(birth_year = as.character(birth_year))

  new_urls <- erb_df$url[!erb_df$url %in% existing$url]

  for (i in seq_len(nrow(erb_df))) {
    if (erb_df$url[i] %in% new_urls) {
      message("  Fetching birth year for: ", erb_df$name[i])
      erb_df$birth_year[i] <- get_erb_birth_year(erb_df$url[i])
      Sys.sleep(0.5)
    }
  }

  erb_df |> mutate(date_added = today)
}

# ── Run scrapers ───────────────────────────────────────────────────────────────

empty_df <- function() tibble(
  source = character(), name = character(), birth_year = character(),
  url = character(), date_added = as_date(character())
)

record_data <- tryCatch(scrape_record(), error = function(e) {
  message("Record scraper failed: ", e$message); empty_df()
})

erb_data <- tryCatch(scrape_erb_good(n_pages = 2), error = function(e) {
  message("ERB Good scraper failed: ", e$message); empty_df()
})

# ── Merge and save ─────────────────────────────────────────────────────────────

new_entries <- bind_rows(record_data, erb_data) |>
  filter(!url %in% existing$url)

message("New entries today: ", nrow(new_entries))

all_data <- bind_rows(existing, new_entries) |>
  arrange(desc(date_added), source, name) |>
  distinct(url, .keep_all = TRUE)

dir.create("data", showWarnings = FALSE)
write_csv(all_data, csv_path)

# ── Generate HTML ──────────────────────────────────────────────────────────────

make_rows <- function(df) {
  if (nrow(df) == 0) {
    return('      <tr><td colspan="3" class="empty">No entries.</td></tr>')
  }
  rows <- character(nrow(df))
  for (i in seq_len(nrow(df))) {
    by   <- if (is.na(df$birth_year[i])) "&ndash;" else df$birth_year[i]
    rows[i] <- sprintf(
      '      <tr>\n        <td><a href="%s" target="_blank" rel="noopener">%s</a></td>\n        <td>%s</td>\n        <td>%s</td>\n      </tr>',
      html_escape(df$url[i]),
      html_escape(df$name[i]),
      by,
      html_escape(df$source[i])
    )
  }
  paste(rows, collapse = "\n")
}

today_data  <- all_data |> filter(date_added == today)
prev_data   <- all_data |> filter(date_added <  today)
today_label <- format(today, "%B %d, %Y")
updated_ts  <- format(lubridate::now("UTC"), "%B %d, %Y at %H:%M UTC")

html <- paste0(
'<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Waterloo Region Obituaries</title>
  <style>
    *, *::before, *::after { box-sizing: border-box; margin: 0; padding: 0; }
    body {
      font-family: Georgia, "Times New Roman", serif;
      background: #f9f7f4;
      color: #2c2c2c;
      max-width: 860px;
      margin: 0 auto;
      padding: 2rem 1.5rem 4rem;
      line-height: 1.6;
    }
    header {
      border-bottom: 2px solid #5a7a6a;
      padding-bottom: 1rem;
      margin-bottom: 2rem;
    }
    h1 {
      font-size: 1.75rem;
      font-weight: normal;
      letter-spacing: 0.02em;
    }
    header p {
      color: #666;
      font-size: 0.875rem;
      margin-top: 0.4rem;
    }
    section { margin-bottom: 2.5rem; }
    h2 {
      font-size: 1rem;
      font-weight: normal;
      color: #5a7a6a;
      text-transform: uppercase;
      letter-spacing: 0.08em;
      margin-bottom: 0.6rem;
      padding-bottom: 0.3rem;
      border-bottom: 1px solid #d0ccc7;
    }
    table { width: 100%; border-collapse: collapse; font-size: 0.9rem; }
    thead th {
      text-align: left;
      padding: 0.35rem 0.6rem;
      font-weight: normal;
      font-size: 0.75rem;
      color: #888;
      text-transform: uppercase;
      letter-spacing: 0.05em;
      border-bottom: 1px solid #d0ccc7;
    }
    tbody tr { border-bottom: 1px solid #ebe8e4; }
    tbody td { padding: 0.45rem 0.6rem; vertical-align: top; }
    a { color: #3a5c8a; text-decoration: none; }
    a:hover { text-decoration: underline; }
    .empty { color: #aaa; font-style: italic; }
    footer {
      color: #aaa;
      font-size: 0.8rem;
      margin-top: 3rem;
      border-top: 1px solid #d0ccc7;
      padding-top: 1rem;
    }
  </style>
</head>
<body>

<header>
  <h1>Waterloo Region Obituaries</h1>
  <p>Compiled daily from the Waterloo Region Record and Erb &amp; Good Family Funeral Home &mdash; ',
  nrow(all_data),
  ' entries total.</p>
</header>

<section>
  <h2>Added ', today_label, '</h2>
  <table>
    <thead><tr><th>Name</th><th>Born</th><th>Source</th></tr></thead>
    <tbody>
',
  make_rows(today_data),
'
    </tbody>
  </table>
</section>

<section>
  <h2>Previous Entries</h2>
  <table>
    <thead><tr><th>Name</th><th>Born</th><th>Source</th></tr></thead>
    <tbody>
',
  make_rows(prev_data),
'
    </tbody>
  </table>
</section>

<footer>Last updated: ', updated_ts, '</footer>

</body>
</html>'
)

writeLines(html, "index.html")
message("Done. HTML written to index.html. Total entries: ", nrow(all_data))
