# SMS Payment Reminder Test: results dashboard
#
#   Overview        the recommendation, headline numbers, who it helps, and the trend over time
#   Explore         the effect broken down by any user or loan attribute
#   Dollar impact   annual savings under adjustable assumptions
#   Data quality    the seven data problems found, and how each fix changed the result
#   Method          metric definitions and who's included
#
# All data is fictional. Build the data first with 01_build_dashboard_data.R, then:
#   shiny::runApp("dashboard")   (from the repo folder)

library(shiny)
library(bslib)
library(ggplot2)
library(DT)

d   <- readRDS("data/dashboard_data.rds")
fct <- d$fct

navy  <- "#1f4e79"
grey  <- "#8a96a3"
green <- "#2e7d4f"

fct$Autopay             <- ifelse(fct$has_autopay, "Autopay", "No autopay")
fct$`Customer type`     <- ifelse(fct$first_time_customer, "First-time customer", "Repeat customer")
fct$`Merchant category` <- fct$merchant_category
fct$`Installment size`  <- cut(fct$amount_due, c(0, 50, 100, 200, Inf),
                               labels = c("Under $50", "$50-100", "$100-200", "Over $200"))
fct$`Payment number`    <- paste("Payment", fct$installment_number, "of 4")
fct$week                <- as.Date(cut(fct$original_due_date, "week"))

dimensions <- c("Autopay", "Customer type", "Merchant category", "Installment size", "Payment number")
metrics    <- c("Paid on time" = "paid_on_time", "30+ days late" = "dpd30")

# ---- Helpers --------------------------------------------------------------------

# Test vs. control for one metric: both rates, the difference in points, and its 95% interval
compare <- function(df, metric) {
  ctl <- df[df$variant == "control", ]
  tst <- df[df$variant == "test", ]
  x <- c(sum(ctl[[metric]]), sum(tst[[metric]]))
  n <- c(nrow(ctl), nrow(tst))
  ci <- suppressWarnings(prop.test(x, n)$conf.int)
  data.frame(control = x[1] / n[1], test = x[2] / n[2],
             effect = 100 * (x[2] / n[2] - x[1] / n[1]),
             low = -100 * ci[2], high = -100 * ci[1],
             n_control = n[1], n_test = n[2])
}

by_group <- function(df, dim, metric) {
  out <- do.call(rbind, lapply(split(df, df[[dim]], drop = TRUE), function(g) {
    cbind(group = as.character(g[[dim]][1]), compare(g, metric))
  }))
  rownames(out) <- NULL
  out
}

pct  <- function(x, digits = 1) sprintf(paste0("%.", digits, "f%%"), 100 * x)
pts  <- function(x) sprintf("%+.1f pts", x)
usd  <- function(x) ifelse(abs(x) >= 1e6, sprintf("$%.1fM", x / 1e6), sprintf("$%.0fK", x / 1e3))

theme_dash <- function() {
  theme_minimal(base_size = 13) +
    theme(panel.grid.minor = element_blank(), plot.title.position = "plot",
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(colour = "grey35"), legend.position = "top",
          legend.title = element_blank())
}

effect_chart <- function(eff, title, subtitle, better_lower = FALSE) {
  eff$group <- factor(eff$group, levels = eff$group[order(if (better_lower) -eff$effect else eff$effect)])
  eff$clear <- if (better_lower) eff$high < 0 else eff$low > 0
  ggplot(eff, aes(x = effect, y = group, colour = clear)) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_errorbar(aes(xmin = low, xmax = high), width = 0.15, orientation = "y", linewidth = 0.8) +
    geom_point(size = 3.5) +
    geom_label(aes(label = sprintf(if (better_lower) "%+.2f pts" else "%+.1f pts", effect)),
               vjust = -0.6, size = 3.8, fill = "white", linewidth = 0, show.legend = FALSE) +
    scale_colour_manual(values = c(`TRUE` = navy, `FALSE` = grey), guide = "none") +
    labs(title = title, subtitle = subtitle, x = "Change, test minus control (percentage points)", y = NULL) +
    theme_dash() +
    theme(panel.grid.major.y = element_blank())
}

# ---- Headline numbers (users without autopay) -----------------------------------

na        <- fct[!fct$has_autopay, ]
on_time   <- compare(na, "paid_on_time")
late      <- compare(na, "dpd30")
late_rel  <- late$test / late$control - 1
bal       <- tapply(na$dpd30_balance, na$variant, mean)
saved_per <- unname(bal["control"] - bal["test"])
opt       <- setNames(d$optouts$opted_out / d$optouts$users, d$optouts$variant)
resched   <- setNames(d$reschedules$rescheduled / d$reschedules$installments, d$reschedules$variant)

# Autopay balance change, for the "send to everyone" option on the dollar tab
ap        <- fct[fct$has_autopay, ]
ap_bal    <- tapply(ap$dpd30_balance, ap$variant, mean)
ap_saved  <- unname(ap_bal["control"] - ap_bal["test"])
ap_ratio  <- nrow(ap) / nrow(na)

net_benefit <- function(installments, writeoff, sms_cost, everyone = FALSE) {
  n_ap   <- if (everyone) installments * ap_ratio else 0
  losses <- (installments * saved_per + n_ap * ap_saved) * writeoff
  cost   <- (installments + n_ap) * sms_cost
  c(losses = losses, cost = cost, net = losses - cost, texts = installments + n_ap)
}
default <- net_benefit(12e6, 0.35, 0.008)

# ---- UI -------------------------------------------------------------------------

ui <- page_navbar(
  title = "SMS Payment Reminder Test",
  theme = bs_theme(version = 5, primary = navy, "navbar-bg" = navy,
                   base_font = font_collection("Segoe UI", "Helvetica Neue", "Arial", "sans-serif")),
  fillable = FALSE,
  header = tags$style(HTML("
    .rec { border-left: 5px solid #1f4e79; background: #f3f7fb; padding: 16px 20px; border-radius: 6px; font-size: 1.05rem; }
    .intro { background: #fff; border: 1px solid #dde3ea; border-radius: 6px; padding: 14px 16px; height: 100%; }
    .intro-label { color: #1f4e79; font-weight: 700; font-size: 0.8rem; text-transform: uppercase;
                   letter-spacing: 0.04em; margin-bottom: 4px; }
    .tab-intro { color: #3d4852; margin: 4px 0 14px; max-width: 900px; }
    .note { color: #5b6773; font-size: 0.9rem; }
    .bslib-value-box .value-box-title { font-size: 0.95rem; }
  ")),

  nav_panel("Overview",
    layout_columns(
      col_widths = c(4, 4, 4), class = "mb-1",
      div(class = "intro",
          div(class = "intro-label", "1 · The situation"),
          "A buy-now-pay-later lender lets shoppers split a purchase into 4 payments, one every 2 weeks.",
          "Some payments come in late, and those that run 30+ days late often turn into losses."),
      div(class = "intro",
          div(class = "intro-label", "2 · The test"),
          "From June to August 2026, about 20,000 customers were randomly split in half. One half got a",
          "text 3 days before each payment was due. The other half got nothing."),
      div(class = "intro",
          div(class = "intro-label", "3 · The question"),
          "The product team asked: ", em("\"Did the reminder help? Should we send it to everyone?\""),
          " This dashboard is the answer.")
    ),
    div(class = "rec mb-3",
        div(class = "intro-label", "The answer"),
        strong("Roll out the 3-day SMS reminder to all users without autopay."),
        sprintf(" It raised on-time payments by %.1f points and cut payments 30+ days late by %.0f%%,",
                on_time$effect, -100 * late_rel),
        sprintf(" worth about %s a year in avoided losses after text costs.", usd(default["net"])),
        " Autopay users don't benefit, so leave them out.",
        div(class = "note mt-2",
            "Use the tabs above to break the result down by customer group (Explore), change the cost",
            "assumptions (Dollar impact), or see how the raw data was cleaned (Data quality).")),
    layout_columns(
      col_widths = c(3, 3, 3, 3),
      value_box("On-time payment lift", pts(on_time$effect),
                p(sprintf("%s → %s, users without autopay", pct(on_time$control), pct(on_time$test)))),
      value_box("Payments 30+ days late", sprintf("%.0f%%", 100 * late_rel),
                p(sprintf("%s → %s", pct(late$control, 2), pct(late$test, 2)))),
      value_box("Net benefit per year", usd(default["net"]),
                p("At default assumptions; adjust on the Dollar impact tab")),
      value_box("SMS opt-outs (guardrail)", pct(opt["test"]),
                p(sprintf("vs. %s in control; limit is 2%%", pct(opt["control"]))))
    ),
    layout_columns(
      col_widths = c(6, 6),
      card(card_header("Who it helps"), plotOutput("overview_groups", height = 330)),
      card(card_header("Is the effect steady over time?"), plotOutput("overview_trend", height = 330))
    ),
    p(class = "note", "Practice case built on fictional data. Snapshot taken 2026-10-15.")
  ),

  nav_panel("Explore",
    layout_sidebar(
      sidebar = sidebar(
        width = 280,
        selectInput("dim", "Break down by", dimensions, selected = "Customer type"),
        radioButtons("metric", "Metric", metrics),
        checkboxInput("no_autopay", "Users without autopay only", TRUE),
        p(class = "note", "Points in navy are clearly different from zero. Grey intervals cross zero, so the",
          "data can't rule out no effect for that group.")
      ),
      p(class = "tab-intro",
        "Did the reminder work better for some customers than others? Pick a way to split customers on the",
        "left. Each dot shows how much the reminder changed the rate for that group; the line through it is",
        "the range the true effect likely falls in."),
      card(plotOutput("explore_chart", height = 380)),
      card(card_header("Rates by group"), DTOutput("explore_table"))
    )
  ),

  nav_panel("Dollar impact",
    layout_sidebar(
      sidebar = sidebar(
        width = 300,
        sliderInput("installments", "Installments per year, users without autopay (millions)",
                    min = 1, max = 30, value = 12, step = 1),
        sliderInput("writeoff", "Share of a 30+ day late balance written off",
                    min = 0.10, max = 0.60, value = 0.35, step = 0.05),
        sliderInput("sms_cost", "Cost per text ($)", min = 0.002, max = 0.05, value = 0.008, step = 0.002),
        radioButtons("audience", "Send the reminder to",
                     c("Users without autopay" = "targeted", "Everyone" = "everyone"))
      ),
      p(class = "tab-intro",
        "What is the reminder worth in dollars? Fewer payments going 30+ days late means fewer losses,",
        "minus the cost of sending the texts. The sliders on the left change the assumptions behind",
        "the estimate."),
      layout_columns(
        col_widths = c(4, 4, 4),
        value_box("Losses avoided per year", textOutput("d_losses")),
        value_box("Text cost per year", textOutput("d_cost")),
        value_box("Net benefit per year", textOutput("d_net"), theme = "primary")
      ),
      card(card_header("How the estimate is built"), tableOutput("d_table"),
           p(class = "note", textOutput("d_note", inline = TRUE)))
    )
  ),

  nav_panel("Data quality",
    p(class = "tab-intro",
      "Before any analysis, the raw data had to be cleaned. It contained seven problems, and none of them",
      "caused an error; each one just made the answer quietly wrong. This chart shows how the measured",
      "effect changed as each problem was fixed."),
    card(plotOutput("fix_chart", height = 400)),
    card(card_header("The seven problems found in the raw data"), DTOutput("fix_table")),
    p(class = "note", "Effects are test minus control on-time rate for users without autopay, with each fix",
      "applied on top of the ones before it. No query errored at any step: each problem silently gave a",
      "confident, wrong answer.")
  ),

  nav_panel("Method",
    p(class = "tab-intro", "The exact definitions behind every number in this dashboard."),
    card(
      card_header("How the test was measured"),
      tags$dl(
        tags$dt("The question"),
        tags$dd("Half of users were randomly sent an SMS 3 days before each payment was due, from June to",
                "August 2026. Did it improve repayment, and should it go to everyone?"),
        tags$dt("Paid on time (primary metric)"),
        tags$dd("Completed payments cover the full amount by 11:59 pm Pacific on the original due date."),
        tags$dt("30+ days late (secondary metric)"),
        tags$dd("The installment still isn't fully paid 30 days after the original due date. This is the metric",
                "closest to credit losses."),
        tags$dt("SMS opt-out rate (guardrail)"),
        tags$dd("Share of users who opted out of texts. Agreed limit: 2%."),
        tags$dt("Who's included"),
        tags$dd("Every user randomly assigned to the sms_reminder_3day experiment (intent to treat), except",
                "the users assigned to both groups. Installments count if they were due at least 3 days after",
                "the user was assigned and on or before Sept 15, so each one had 30 days to go late."),
        tags$dt("Significance"),
        tags$dd("Two-proportion tests with 95% confidence intervals (R's prop.test)."),
        tags$dt("Rescheduled due dates"),
        tags$dd(sprintf("Judged against the original due date. Test and control rescheduled at the same rate (%s vs. %s),",
                        pct(resched["test"]), pct(resched["control"])),
                "so the choice doesn't change the result."),
        tags$dt("Tools"),
        tags$dd("SQL in DuckDB for cleaning and modeling, R for analysis, Shiny and ggplot2 for this dashboard.")
      )
    )
  )
)

# ---- Server ---------------------------------------------------------------------

server <- function(input, output, session) {

  output$overview_groups <- renderPlot({
    df <- fct
    df$group <- ifelse(df$has_autopay, "Autopay",
                       ifelse(df$first_time_customer, "No autopay, first-time", "No autopay, repeat"))
    effect_chart(by_group(df, "group", "paid_on_time"),
                 "The reminder works only without autopay",
                 "Change in on-time payment rate, with 95% confidence interval")
  })

  output$overview_trend <- renderPlot({
    weekly <- aggregate(paid_on_time ~ week + variant, data = na, FUN = mean)
    sizes  <- aggregate(paid_on_time ~ week + variant, data = na, FUN = length)
    small  <- unique(sizes$week[sizes$paid_on_time < 250])   # early weeks are too small to read
    weekly <- weekly[!weekly$week %in% small, ]
    weekly$variant <- ifelse(weekly$variant == "test", "Test (got reminder)", "Control")
    ggplot(weekly, aes(week, paid_on_time, colour = variant)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      scale_colour_manual(values = c("Test (got reminder)" = navy, "Control" = grey)) +
      scale_y_continuous(labels = function(x) paste0(round(100 * x), "%")) +
      labs(title = "Test stays ahead of control every week",
           subtitle = "On-time payment rate by week due, users without autopay",
           caption = "Weeks with 250+ installments per group", x = NULL, y = NULL) +
      theme_dash()
  })

  explore_data <- reactive({
    df <- if (input$no_autopay) fct[!fct$has_autopay, ] else fct
    by_group(df, input$dim, input$metric)
  })

  output$explore_chart <- renderPlot({
    lower <- input$metric == "dpd30"
    label <- names(metrics)[metrics == input$metric]
    effect_chart(explore_data(),
                 sprintf("%s by %s", label, tolower(input$dim)),
                 paste0("Change, test minus control, with 95% confidence interval",
                        if (lower) " (below zero is better)" else ""),
                 better_lower = lower)
  })

  output$explore_table <- renderDT({
    e <- explore_data()
    digits <- if (input$metric == "dpd30") 2 else 1
    datatable(data.frame(Group = e$group,
                         Installments = format(e$n_control + e$n_test, big.mark = ","),
                         Control = pct(e$control, digits), Test = pct(e$test, digits),
                         Change = sprintf("%+.2f pts", e$effect),
                         `95% interval` = sprintf("%+.2f to %+.2f", e$low, e$high), check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })

  dollars <- reactive(net_benefit(input$installments * 1e6, input$writeoff, input$sms_cost,
                                  input$audience == "everyone"))
  output$d_losses <- renderText(usd(dollars()["losses"]))
  output$d_cost   <- renderText(usd(dollars()["cost"]))
  output$d_net    <- renderText(usd(dollars()["net"]))

  output$d_table <- renderTable({
    x <- dollars()
    data.frame(
      Step = c("30+ day late balance per installment (control → test)", "Late balance avoided per year",
               "Losses avoided", "Texts sent per year", "Text cost", "Net benefit"),
      Value = c(sprintf("$%.2f → $%.2f (%.2f less)", bal["control"], bal["test"], saved_per),
                usd(x["losses"] / input$writeoff),
                sprintf("%s (%.0f%% written off)", usd(x["losses"]), 100 * input$writeoff),
                sprintf("%.1fM", x["texts"] / 1e6),
                sprintf("%s ($%.3f each)", usd(x["cost"]), input$sms_cost),
                usd(x["net"])))
  }, striped = TRUE)

  output$d_note <- renderText({
    if (input$audience == "everyone") {
      extra <- input$installments * 1e6 * ap_ratio
      drag  <- extra * input$sms_cost - extra * ap_saved * input$writeoff
      sprintf("Texting autopay users adds %.1fM texts a year but avoids no real losses, so it lowers the net benefit by %s.",
              extra / 1e6, usd(drag))
    } else {
      "Balances are the amount still unpaid 30 days after the due date, measured in the test."
    }
  })

  output$fix_chart <- renderPlot({
    f <- d$fixes
    f$label <- factor(paste0(f$step, ". ", f$fix), levels = rev(paste0(f$step, ". ", f$fix)))
    f$end <- f$step %in% c(0, 7)
    ggplot(f, aes(x = 100 * effect, y = label, fill = end)) +
      geom_col(width = 0.65) +
      geom_text(aes(label = sprintf("%.1f pts", 100 * effect)), hjust = -0.15, size = 3.9) +
      scale_fill_manual(values = c(`TRUE` = navy, `FALSE` = "#a9bfd6"), guide = "none") +
      scale_x_continuous(expand = expansion(mult = c(0, 0.12))) +
      labs(title = sprintf("Cleaning the data moved the measured lift from %.1f to %.1f points",
                           100 * f$effect[1], 100 * f$effect[8]),
           subtitle = "On-time payment lift for users without autopay, as each fix is applied",
           x = "Measured lift (percentage points)", y = NULL) +
      theme_dash() +
      theme(panel.grid.major.y = element_blank())
  })

  output$fix_table <- renderDT({
    f <- d$fixes[-1, ]
    datatable(data.frame(`#` = f$step, Fix = f$fix, Problem = f$problem,
                         `Rows affected` = paste(format(f$rows_affected, big.mark = ","), f$unit),
                         `Lift after fix` = sprintf("%.1f pts", 100 * f$effect), check.names = FALSE),
              rownames = FALSE, options = list(dom = "t", paging = FALSE))
  })
}

shinyApp(ui, server)
