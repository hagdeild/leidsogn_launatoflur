library(shiny)
library(bslib)
library(readxl)
library(dplyr)
library(fontawesome)

# --- Data Loading ---
excel_file <- "leidsogn_launatoflur_fyrir_grid.xlsx"

extract_data <- function(sheet_name) {
  raw <- read_excel(excel_file, sheet = sheet_name, col_names = FALSE)

  # Wage group names from column 9 (rows 7-17)
  flokkur_names <- as.character(raw[[9]])[7:17]

  # Year headers from row 6, columns 10-16
  year_headers <- as.character(raw[6, 10:16])

  # Monthly salaries (rows 7-17, columns 10-16)
  salary_rows <- raw[7:17, 10:16]

  salary_tbl <- do.call(
    rbind,
    lapply(seq_along(flokkur_names), function(i) {
      tibble(
        flokkur = flokkur_names[i],
        ar = year_headers,
        manadarlaun = as.numeric(salary_rows[i, , drop = TRUE])
      )
    })
  )

  # Des/orlofsuppbót: row 7, columns 18 onwards (column 17 is an empty spacer).
  # Leiðsögn has 7 year columns (18-24), Ökuleiðsögn has 6 (18-23). Beyond those
  # the sheet contains orlof lookup data, so keep only headers that look like a year.
  uppbot_headers <- as.character(raw[6, 18:ncol(raw)])
  uppbot_values <- suppressWarnings(as.numeric(raw[7, 18:ncol(raw)]))

  is_year <- grepl("^\\d{4}", uppbot_headers)
  valid <- is_year & !is.na(uppbot_values)
  uppbot_tbl <- tibble(
    ar = uppbot_headers[valid],
    uppbot = uppbot_values[valid]
  )

  # If "2025 apríl" is missing from uppbot (Ökuleiðsögn), duplicate "2025" value
  if (!"2025 apríl" %in% uppbot_tbl$ar && "2025" %in% uppbot_tbl$ar) {
    row_2025 <- uppbot_tbl |> filter(ar == "2025")
    uppbot_tbl <- bind_rows(
      uppbot_tbl,
      tibble(ar = "2025 apríl", uppbot = row_2025$uppbot[1])
    )
  }

  # If "2026 apríl" is missing from uppbot, duplicate "2026" value
  if (!"2026 apríl" %in% uppbot_tbl$ar && "2026" %in% uppbot_tbl$ar) {
    row_2026 <- uppbot_tbl |> filter(ar == "2026")
    uppbot_tbl <- bind_rows(
      uppbot_tbl,
      tibble(ar = "2026 apríl", uppbot = row_2026$uppbot[1])
    )
  }

  # If plain "2026" is missing from uppbot, duplicate "2026 apríl" value
  if (!"2026" %in% uppbot_tbl$ar && "2026 apríl" %in% uppbot_tbl$ar) {
    row_2026a <- uppbot_tbl |> filter(ar == "2026 apríl")
    uppbot_tbl <- bind_rows(
      uppbot_tbl,
      tibble(ar = "2026", uppbot = row_2026a$uppbot[1])
    )
  }

  list(salary = salary_tbl, uppbot = uppbot_tbl)
}

leidsogn_data <- extract_data("Leiðsögn")
okuleid_data <- extract_data("Ökuleiðsögn")

# Combine into lookup tables
salary_data <- bind_rows(
  leidsogn_data$salary |> mutate(tegund = "Leiðsögumaður"),
  okuleid_data$salary |> mutate(tegund = "Ökuleiðsögumaður")
)

uppbot_data <- bind_rows(
  leidsogn_data$uppbot |> mutate(tegund = "Leiðsögumaður"),
  okuleid_data$uppbot |> mutate(tegund = "Ökuleiðsögumaður")
)

# Filter out "Núverandi" (redundant with the most recent year)
salary_data <- salary_data |> filter(!ar %in% c("Núverandi"))
uppbot_data <- uppbot_data |> filter(!ar %in% c("Núverandi"))

# Backfill missing uppbot years from the most recent available year, per tegund.
# The salary table carries years (e.g. 2027) that the uppbot section doesn't
# always publish yet — reuse the latest known rate so every dropdown year works.
salary_years <- unique(salary_data$ar)
uppbot_data <- bind_rows(lapply(unique(uppbot_data$tegund), function(t) {
  existing <- uppbot_data |> filter(tegund == t)
  missing <- setdiff(salary_years, existing$ar)
  if (length(missing) == 0) {
    return(existing)
  }
  # Pick the latest existing year by lexicographic order (works for YYYY / "YYYY apríl")
  fallback <- existing$uppbot[order(existing$ar, decreasing = TRUE)][1]
  bind_rows(existing, tibble(ar = missing, uppbot = fallback, tegund = t))
}))

# Available years
ar_choices <- unique(salary_data$ar)

# Wage group choices
flokkur_choices <- unique(salary_data$flokkur)

# Wage group descriptions
flokkur_skyring <- read_excel(
  excel_file,
  sheet = "Launaflokkur - skyring",
  col_names = FALSE
)
flokkur_desc <- setNames(
  as.character(flokkur_skyring[[2]]),
  as.character(flokkur_skyring[[1]])
)

# Vacation options
orlof_choices <- c(
  "Án orlofs" = 0,
  "24 dagar (10,17% orlof)" = 0.1017,
  "27 dagar (5 ára starfsreynsla)" = 0.1159,
  "30 dagar (10 ára starfsreynsla)" = 0.1304
)

# Long trip predefined splits: list of (virkir, fridagar) per duration
long_trip_splits <- list(
  "2" = list(c(2, 0), c(1, 1), c(0, 2)),
  "4" = list(c(4, 0), c(3, 1), c(2, 2)),
  "6" = list(c(5, 1), c(4, 2)),
  "8" = list(c(6, 2), c(5, 3)),
  "10" = list(c(8, 2), c(7, 3), c(6, 4)),
  "12" = list(c(10, 2), c(9, 3), c(8, 4)),
  "14" = list(c(10, 4))
)

# --- Helper: Icelandic number formatting ---
fmt_kr <- function(x) {
  paste0(format(round(x), big.mark = ".", decimal.mark = ","), " kr.")
}

fmt_num <- function(x) {
  # Format number with Icelandic separators (for non-currency display)
  if (x == round(x)) {
    format(x, big.mark = ".", decimal.mark = ",")
  } else {
    format(x, big.mark = ".", decimal.mark = ",", nsmall = 1)
  }
}

# --- UI ---
ui <- page_fluid(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#013766",
    secondary = "#0d8bb7",
    success = "#f0d526",
    "enable-rounded" = TRUE,
    "border-radius" = "0.5rem"
  ),

  tags$head(tags$style(HTML(
    "
    .card { height: auto !important; overflow: visible !important; }
    .card-body { height: auto !important; max-height: none !important; overflow: visible !important; flex: 0 0 auto !important; }
  "
  ))),

  # Header bar
  div(
    class = "container-fluid py-3 mb-4",
    style = "background-color: #013766; color: white;",
    div(
      class = "container",
      h2(
        "Reiknivél fyrir verkefna-/ferðaráðið leiðsögufólk",
        style = "margin: 0; font-size: 1.5rem;"
      ),
      p(
        "Gildir frá 1. febrúar 2024",
        style = "margin: 4px 0 0 0; font-size: 0.9rem; opacity: 0.8;"
      )
    )
  ),

  # Main content
  div(
    class = "container",
    fluidRow(
      # Left column: inputs
      column(
        width = 4,
        card(
          full_screen = FALSE,
          fill = FALSE,
          card_header(class = "bg-light", h5("Stillingar", class = "m-0")),
          card_body(
            fillable = FALSE,
            selectInput(
              "ar",
              span(icon("calendar-alt"), "Veldu ár"),
              choices = ar_choices,
              selected = "2026",
              width = "100%"
            ),
            selectInput(
              "tegund",
              span(icon("compass"), "Veldu töflu"),
              choices = c("Leiðsögumaður", "Ökuleiðsögumaður"),
              width = "100%"
            ),
            selectInput(
              "orlof",
              span(icon("umbrella-beach"), "Veldu orlofsréttindi"),
              choices = names(orlof_choices),
              width = "100%"
            ),
            selectInput(
              "flokkur",
              span(icon("layer-group"), "Veldu launaflokk"),
              choices = flokkur_choices,
              width = "100%"
            )
          )
        ),
        card(
          full_screen = FALSE,
          fill = FALSE,
          card_header(class = "bg-light", h5("Tegund ferðar", class = "m-0")),
          card_body(
            fillable = FALSE,
            radioButtons(
              "ferd_tegund",
              span(icon("route"), "Tegund ferðar"),
              choices = c(
                "Dagsferð" = "dagsferð",
                "Langferð (11 klst./dag)" = "langferd_11",
                "Langferð - Tjald og skálaferðir (12 klst./dag)" = "langferd_12"
              )
            ),
            conditionalPanel(
              "input.ferd_tegund == 'dagsferð'",
              selectInput(
                "dag_klst",
                span(icon("clock"), "Lengd ferðar"),
                choices = setNames(4:11, paste0(4:11, " klst.")),
                width = "100%"
              ),
              radioButtons(
                "dag_tegund",
                span(icon("calendar-week"), "Dagur vikunnar"),
                choices = c(
                  "Mánudagur - föstudagur" = "weekday",
                  "Laugardagur - sunnudagur" = "weekend"
                )
              )
            ),
            conditionalPanel(
              "input.ferd_tegund == 'langferd_11' || input.ferd_tegund == 'langferd_12'",
              selectInput(
                "lang_dagar",
                span(icon("clock"), "Lengd ferðar"),
                choices = setNames(
                  c("2", "4", "6", "8", "10", "12", "14"),
                  paste0(c(2, 4, 6, 8, 10, 12, 14), " dagar")
                ),
                width = "100%"
              ),
              uiOutput("skipting_ui")
            )
          )
        )
      ),

      # Right column: results
      column(
        width = 8,
        card(
          card_header(
            style = "background-color: #013766; color: white;",
            "Launatafla"
          ),
          tableOutput("launatafla")
        ),
        card(
          card_header(
            style = "background-color: #013766; color: white;",
            "Útreikningur"
          ),
          uiOutput("utreikningur")
        ),
        card(
          card_header(class = "bg-light", "Lýsing launaflokks"),
          card_body(textOutput("flokkur_lysing"))
        )
      )
    )
  ),

  # Footer
  div(
    class = "container-fluid mt-4 py-3",
    style = "background-color: #0d8bb7; color: white;",
    div(
      class = "container text-center",
      p("© 2025 - Reiknivél fyrir leiðsögufólk")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # Dynamic split selector for long trips
  output$skipting_ui <- renderUI({
    req(input$lang_dagar)
    splits <- long_trip_splits[[input$lang_dagar]]
    choices <- sapply(splits, function(s) {
      paste0(s[1], " virkir + ", s[2], " frídagar")
    })
    selectInput(
      "skipting",
      span(icon("calendar-check"), "Skipting virkra daga / frídaga"),
      choices = setNames(seq_along(choices), choices)
    )
  })

  # Lookup monthly salary
  manadarlaun <- reactive({
    row <- salary_data |>
      filter(tegund == input$tegund, flokkur == input$flokkur, ar == input$ar)
    req(nrow(row) > 0)
    row$manadarlaun[1]
  })

  # Lookup des/orlofsuppbót per hour
  uppbot_per_hour <- reactive({
    row <- uppbot_data |> filter(tegund == input$tegund, ar == input$ar)
    req(nrow(row) > 0)
    row$uppbot[1]
  })

  # Hourly rates
  dagvinnukaup <- reactive(round(manadarlaun() / 162.5))
  yfirvinnukaup <- reactive(round(manadarlaun() * 0.010385))
  storhatidarkaup <- reactive(round(manadarlaun() * 0.01375))

  # Vacation percentage
  orlof_pct <- reactive(as.numeric(orlof_choices[input$orlof]))

  # Launatafla output
  output$launatafla <- renderTable(
    {
      data.frame(
        ` ` = c(
          "Mánaðarlaun",
          "Dagvinnukaup",
          "Yfirvinnukaup",
          "Stórhátíðakaup",
          "Des/orlofsuppbót á klst."
        ),
        `Upphæð` = c(
          fmt_kr(manadarlaun()),
          fmt_kr(dagvinnukaup()),
          fmt_kr(yfirvinnukaup()),
          fmt_kr(storhatidarkaup()),
          fmt_kr(uppbot_per_hour())
        ),
        check.names = FALSE
      )
    },
    striped = TRUE,
    hover = TRUE,
    width = "100%",
    align = "lr"
  )

  # Build utreikningur data + bold flags
  utreikningur_data <- reactive({
    orlof <- orlof_pct()
    uppbot <- uppbot_per_hour()
    dagv <- dagvinnukaup()
    yfirv <- yfirvinnukaup()

    if (input$ferd_tegund == "dagsferð") {
      hours <- as.numeric(input$dag_klst)
      is_weekday <- input$dag_tegund == "weekday"

      rows <- list()
      bold <- logical(0)

      if (is_weekday) {
        dagv_klst <- min(hours, 7.5)
        yfirv_klst <- max(hours - 7.5, 0)
        dagv_total <- dagv_klst * dagv
        yfirv_total <- yfirv_klst * yfirv
        grunnlaun <- dagv_total + yfirv_total
        uppbot_klst <- min(hours, 7.5)

        rows[[length(rows) + 1]] <- c(
          "Dagvinna",
          fmt_num(dagv_klst),
          fmt_kr(dagv),
          fmt_kr(dagv_total)
        )
        bold <- c(bold, FALSE)
        if (yfirv_klst > 0) {
          rows[[length(rows) + 1]] <- c(
            "Yfirvinna",
            fmt_num(yfirv_klst),
            fmt_kr(yfirv),
            fmt_kr(yfirv_total)
          )
          bold <- c(bold, FALSE)
        }
      } else {
        yfirv_klst <- hours
        yfirv_total <- yfirv_klst * yfirv
        grunnlaun <- yfirv_total
        uppbot_klst <- min(hours, 7.5)

        rows[[length(rows) + 1]] <- c(
          "Yfirvinna",
          fmt_num(yfirv_klst),
          fmt_kr(yfirv),
          fmt_kr(yfirv_total)
        )
        bold <- c(bold, FALSE)
      }

      rows[[length(rows) + 1]] <- c("Grunnlaun", "", "", fmt_kr(grunnlaun))
      bold <- c(bold, TRUE)

      if (orlof > 0) {
        orlof_kr <- grunnlaun * orlof
        rows[[length(rows) + 1]] <- c(
          paste0("Orlof (", sub("\\.", ",", as.character(orlof * 100)), "%)"),
          "",
          "",
          fmt_kr(orlof_kr)
        )
        bold <- c(bold, FALSE)
      } else {
        orlof_kr <- 0
      }

      uppbot_total <- uppbot_klst * uppbot
      rows[[length(rows) + 1]] <- c(
        "Des/orlofsuppbót",
        fmt_num(uppbot_klst),
        fmt_kr(uppbot),
        fmt_kr(uppbot_total)
      )
      bold <- c(bold, FALSE)

      samtals <- grunnlaun + orlof_kr + uppbot_total
      rows[[length(rows) + 1]] <- c("Samtals", "", "", fmt_kr(samtals))
      bold <- c(bold, TRUE)

      df <- do.call(rbind, rows) |> as.data.frame()
      names(df) <- c("Liður", "Klst.", "Kaup/klst.", "Samtals")
      list(df = df, bold = bold)
    } else {
      # Long trip
      req(input$skipting)
      hours_per_day <- if (input$ferd_tegund == "langferd_11") 11 else 12
      splits <- long_trip_splits[[input$lang_dagar]]
      idx <- as.numeric(input$skipting)
      req(idx >= 1, idx <= length(splits))
      split <- splits[[idx]]
      virkir <- split[1]
      fridagar <- split[2]

      if (hours_per_day == 11) {
        daglaun_virkur <- 7.5 * dagv + 3.5 * yfirv
        daglaun_fridagur <- 11 * yfirv
      } else {
        daglaun_virkur <- 7.5 * dagv + 4.5 * yfirv
        daglaun_fridagur <- 12 * yfirv
      }

      virkir_total <- virkir * daglaun_virkur
      fridagar_total <- fridagar * daglaun_fridagur
      grunnlaun <- virkir_total + fridagar_total

      rows <- list()
      bold <- logical(0)

      if (virkir > 0) {
        rows[[length(rows) + 1]] <- c(
          "Virkir dagar",
          as.character(virkir),
          fmt_kr(daglaun_virkur),
          fmt_kr(virkir_total)
        )
        bold <- c(bold, FALSE)
      }
      if (fridagar > 0) {
        rows[[length(rows) + 1]] <- c(
          "Almennir frídagar",
          as.character(fridagar),
          fmt_kr(daglaun_fridagur),
          fmt_kr(fridagar_total)
        )
        bold <- c(bold, FALSE)
      }

      rows[[length(rows) + 1]] <- c("Grunnlaun", "", "", fmt_kr(grunnlaun))
      bold <- c(bold, TRUE)

      if (orlof > 0) {
        orlof_kr <- grunnlaun * orlof
        rows[[length(rows) + 1]] <- c(
          paste0("Orlof (", sub("\\.", ",", as.character(orlof * 100)), "%)"),
          "",
          "",
          fmt_kr(orlof_kr)
        )
        bold <- c(bold, FALSE)
      } else {
        orlof_kr <- 0
      }

      uppbot_total <- virkir * 7.5 * uppbot
      if (uppbot_total > 0) {
        rows[[length(rows) + 1]] <- c(
          "Des/orlofsuppbót",
          paste0(virkir, " virkir × 7,5 klst."),
          fmt_kr(uppbot),
          fmt_kr(uppbot_total)
        )
        bold <- c(bold, FALSE)
      }

      samtals <- grunnlaun + orlof_kr + uppbot_total
      rows[[length(rows) + 1]] <- c("Samtals", "", "", fmt_kr(samtals))
      bold <- c(bold, TRUE)

      df <- do.call(rbind, rows) |> as.data.frame()
      names(df) <- c("Liður", "Dagar", "Daglaun", "Samtals")
      list(df = df, bold = bold)
    }
  })

  # Main calculation output
  output$utreikningur <- renderUI({
    res <- utreikningur_data()
    df <- res$df
    bold <- res$bold
    col_align <- c("left", "right", "right", "right")

    header_cells <- lapply(seq_along(names(df)), function(j) {
      tags$th(style = paste0("text-align:", col_align[j], ";"), names(df)[j])
    })

    body_rows <- lapply(seq_len(nrow(df)), function(i) {
      cells <- lapply(seq_along(names(df)), function(j) {
        style <- paste0(
          "text-align:",
          col_align[j],
          ";",
          if (bold[i]) "font-weight:bold;" else ""
        )
        tags$td(style = style, df[i, j])
      })
      do.call(tags$tr, cells)
    })

    tags$table(
      class = "table table-striped table-hover",
      style = "width:100%;",
      tags$thead(do.call(tags$tr, header_cells)),
      do.call(tags$tbody, body_rows)
    )
  })

  # Wage group description
  output$flokkur_lysing <- renderText({
    # Extract the launaflokkur number (1-4) from the selection
    flokkur_num <- sub("Flokkur (\\d+).*", "\\1", input$flokkur)
    key <- paste("Launaflokkur", flokkur_num)
    if (key %in% names(flokkur_desc)) {
      flokkur_desc[[key]]
    } else {
      ""
    }
  })
}

shinyApp(ui, server)
