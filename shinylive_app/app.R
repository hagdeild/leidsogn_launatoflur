library(shiny)
library(bslib)

# --- Data Loading (from pre-processed CSV files) ---
salary_data <- read.csv("salary_data.csv", stringsAsFactors = FALSE)
uppbot_data <- read.csv("uppbot_data.csv", stringsAsFactors = FALSE)
flokkur_desc_raw <- read.csv("flokkur_desc.csv", stringsAsFactors = FALSE)
flokkur_desc <- setNames(flokkur_desc_raw$value, flokkur_desc_raw$key)

# Available years
ar_choices <- unique(salary_data$ar)

# Wage group choices
flokkur_choices <- unique(salary_data$flokkur)

# Vacation options
orlof_choices <- c(
  "Án orlofs" = 0,
  "24 dagar (10,17% orlof)" = 0.1017,
  "27 dagar (5 ára starfsreynsla)" = 0.1159,
  "30 dagar (10 ára starfsreynsla)" = 0.1304
)

# Long trip predefined splits: list of (virkir, fridagar) per duration
long_trip_splits <- list(
  "2"  = list(c(2, 0), c(1, 1), c(0, 2)),
  "4"  = list(c(4, 0), c(3, 1), c(2, 2)),
  "6"  = list(c(5, 1), c(4, 2)),
  "8"  = list(c(6, 2), c(5, 3)),
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
ui <- page_sidebar(
  title = "Launatafla og útreikningar fyrir dags- og langferðir",
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#013766",
    secondary = "#0d8bb7",
    success = "#f0d526",
    "enable-rounded" = TRUE,
    "border-radius" = "0.5rem",
    "navbar-bg" = "#013766"
  ),

  sidebar = sidebar(
    width = 320,

    selectInput("ar", "Veldu ár",
      choices = ar_choices, selected = "2026"
    ),

    selectInput("tegund", "Veldu töflu",
      choices = c("Leiðsögumaður", "Ökuleiðsögumaður")
    ),

    selectInput("orlof", "Veldu orlofsréttindi",
      choices = names(orlof_choices)
    ),

    selectInput("flokkur", "Veldu launaflokk",
      choices = flokkur_choices
    ),

    hr(),

    radioButtons("ferd_tegund", "Tegund ferðar",
      choices = c(
        "Dagsferð" = "dagsferd",
        "Langferð (11 klst./dag)" = "langferd_11",
        "Langferð - Tjald og skálaferðir (12 klst./dag)" = "langferd_12"
      )
    ),

    conditionalPanel(
      "input.ferd_tegund == 'dagsferd'",
      selectInput("dag_klst", "Lengd ferðar",
        choices = setNames(4:11, paste0(4:11, " klst."))
      ),
      radioButtons("dag_tegund", "Dagur vikunnar",
        choices = c("Mánudagur - föstudagur" = "weekday", "Laugardagur - sunnudagur" = "weekend")
      )
    ),

    conditionalPanel(
      "input.ferd_tegund == 'langferd_11' || input.ferd_tegund == 'langferd_12'",
      selectInput("lang_dagar", "Lengd ferðar",
        choices = setNames(
          c("2", "4", "6", "8", "10", "12", "14"),
          paste0(c(2, 4, 6, 8, 10, 12, 14), " dagar")
        )
      ),
      uiOutput("skipting_ui")
    )
  ),

  # Main panel
  layout_columns(
    col_widths = c(12),

    card(
      card_header(class = "bg-primary text-white", "Launatafla"),
      tableOutput("launatafla")
    ),

    card(
      card_header(class = "bg-primary text-white", "Útreikningur"),
      tableOutput("utreikningur")
    ),

    card(
      card_header("Lýsing launaflokks"),
      textOutput("flokkur_lysing")
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
    selectInput("skipting", "Skipting virkra daga / frídaga",
      choices = setNames(seq_along(choices), choices)
    )
  })

  # Lookup monthly salary
  manadarlaun <- reactive({
    row <- salary_data[salary_data$tegund == input$tegund &
                        salary_data$flokkur == input$flokkur &
                        salary_data$ar == input$ar, ]
    req(nrow(row) > 0)
    row$manadarlaun[1]
  })

  # Lookup des/orlofsuppbót per hour
  uppbot_per_hour <- reactive({
    row <- uppbot_data[uppbot_data$tegund == input$tegund &
                        uppbot_data$ar == input$ar, ]
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
  output$launatafla <- renderTable({
    data.frame(
      ` ` = c("Mánaðarlaun", "Dagvinnukaup", "Yfirvinnukaup",
              "Stórhátíðakaup", "Des/orlofsuppbót á klst."),
      `Upphæð` = c(
        fmt_kr(manadarlaun()),
        fmt_kr(dagvinnukaup()),
        fmt_kr(yfirvinnukaup()),
        fmt_kr(storhatidarkaup()),
        fmt_kr(uppbot_per_hour())
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lr")

  # Main calculation
  output$utreikningur <- renderTable({
    orlof <- orlof_pct()
    uppbot <- uppbot_per_hour()
    dagv <- dagvinnukaup()
    yfirv <- yfirvinnukaup()

    if (input$ferd_tegund == "dagsferd") {
      hours <- as.numeric(input$dag_klst)
      is_weekday <- input$dag_tegund == "weekday"

      if (is_weekday) {
        dagv_klst <- min(hours, 7.5)
        yfirv_klst <- max(hours - 7.5, 0)
        dagv_total <- dagv_klst * dagv
        yfirv_total <- yfirv_klst * yfirv
        grunnlaun <- dagv_total + yfirv_total
        uppbot_klst <- min(hours, 7.5)

        rows <- list()
        rows[[length(rows) + 1]] <- c("Dagvinna", fmt_num(dagv_klst), fmt_kr(dagv), fmt_kr(dagv_total))
        if (yfirv_klst > 0) {
          rows[[length(rows) + 1]] <- c("Yfirvinna", fmt_num(yfirv_klst), fmt_kr(yfirv), fmt_kr(yfirv_total))
        }
      } else {
        yfirv_klst <- hours
        yfirv_total <- yfirv_klst * yfirv
        grunnlaun <- yfirv_total
        uppbot_klst <- min(hours, 7.5)

        rows <- list()
        rows[[length(rows) + 1]] <- c("Yfirvinna", fmt_num(yfirv_klst), fmt_kr(yfirv), fmt_kr(yfirv_total))
      }

      rows[[length(rows) + 1]] <- c("Grunnlaun", "", "", fmt_kr(grunnlaun))

      if (orlof > 0) {
        orlof_kr <- grunnlaun * orlof
        rows[[length(rows) + 1]] <- c(
          paste0("Orlof (", sub("\\.", ",", as.character(orlof * 100)), "%)"),
          "", "", fmt_kr(orlof_kr)
        )
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

      samtals <- grunnlaun + orlof_kr + uppbot_total
      rows[[length(rows) + 1]] <- c("Samtals", "", "", fmt_kr(samtals))

      df <- do.call(rbind, rows) |> as.data.frame()
      names(df) <- c("Liður", "Klst.", "Kaup/klst.", "Samtals")
      df

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

      if (virkir > 0) {
        rows[[length(rows) + 1]] <- c(
          "Virkir dagar", as.character(virkir),
          fmt_kr(daglaun_virkur), fmt_kr(virkir_total)
        )
      }
      if (fridagar > 0) {
        rows[[length(rows) + 1]] <- c(
          "Almennir frídagar", as.character(fridagar),
          fmt_kr(daglaun_fridagur), fmt_kr(fridagar_total)
        )
      }

      rows[[length(rows) + 1]] <- c("Grunnlaun", "", "", fmt_kr(grunnlaun))

      if (orlof > 0) {
        orlof_kr <- grunnlaun * orlof
        rows[[length(rows) + 1]] <- c(
          paste0("Orlof (", sub("\\.", ",", as.character(orlof * 100)), "%)"),
          "", "", fmt_kr(orlof_kr)
        )
      } else {
        orlof_kr <- 0
      }

      uppbot_total <- virkir * 7.5 * uppbot
      if (uppbot_total > 0) {
        rows[[length(rows) + 1]] <- c(
          "Des/orlofsuppbót",
          paste0(virkir, " virkir \u00d7 7,5 klst."),
          fmt_kr(uppbot),
          fmt_kr(uppbot_total)
        )
      }

      samtals <- grunnlaun + orlof_kr + uppbot_total
      rows[[length(rows) + 1]] <- c("Samtals", "", "", fmt_kr(samtals))

      df <- do.call(rbind, rows) |> as.data.frame()
      names(df) <- c("Liður", "Dagar", "Daglaun", "Samtals")
      df
    }
  }, striped = TRUE, hover = TRUE, width = "100%", align = "lrrr")

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
