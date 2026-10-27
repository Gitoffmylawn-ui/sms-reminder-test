# Generates a fake buy-now-pay-later dataset for the SMS reminder practice case.
# Everything here is invented. The data has realistic problems planted in it on purpose.
#
# Run once from this folder:  Rscript 00_generate_data.R
# Writes data/*.csv and loans.duckdb (the same tables, ready for SQL).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(DBI)
  library(duckdb)
})
set.seed(2026)

snapshot <- as.Date("2026-10-15")   # "today" in the warehouse
tz_local <- "America/Los_Angeles"   # due dates are Pacific time; payment timestamps are UTC
n_users  <- 20000

# ---- Users ----------------------------------------------------------------------

users <- tibble(
  user_id      = sprintf("U%05d", seq_len(n_users)),
  signup_date  = as.Date("2023-01-01") + sample(0:1240, n_users, TRUE),
  prior_loans  = rpois(n_users, 1.3),
  has_autopay  = rbinom(n_users, 1, 0.45) == 1,
  phone_valid  = rbinom(n_users, 1, 0.96) == 1,
  risk         = rnorm(n_users)            # hidden; not written out
)

# ---- Experiment assignments -----------------------------------------------------

assign <- users |>
  transmute(user_id,
            experiment_name = "sms_reminder_3day",
            variant = sample(c("control", "test"), n_users, TRUE),
            assigned_at = as.POSIXct("2026-06-01 00:00:00", tz = "UTC") +
              runif(n_users, 0, 30 * 86400))

# Planted: 150 users were assigned twice, once to each variant
dupes <- assign |>
  slice_sample(n = 150) |>
  mutate(variant = if_else(variant == "test", "control", "test"),
         assigned_at = assigned_at + runif(n(), 3600, 10 * 86400))

# Planted: an unrelated experiment shares the table
other_exp <- users |>
  slice_sample(prop = 0.6) |>
  transmute(user_id, experiment_name = "checkout_button_color",
            variant = sample(c("control", "blue", "green"), n(), TRUE),
            assigned_at = as.POSIXct("2026-05-10", tz = "UTC") + runif(n(), 0, 40 * 86400))

experiment_assignments <- bind_rows(assign, dupes, other_exp) |>
  arrange(assigned_at) |>
  mutate(assigned_at = format(assigned_at, "%Y-%m-%d %H:%M:%S", tz = "UTC"))

first_assign <- assign |> select(user_id, variant, assigned_at)

# ---- Loans ----------------------------------------------------------------------

categories <- c("Electronics", "Home & Furniture", "Fashion", "Travel", "Fitness", "Beauty")

loans_post <- first_assign |>
  mutate(n_loans = 1 + rpois(n(), 0.4)) |>
  tidyr::uncount(n_loans) |>
  mutate(origination_date = pmin(as.Date(assigned_at) + sample(0:75, n(), TRUE), as.Date("2026-08-15")))

# Planted: some loans started before the experiment, so early installments predate assignment
loans_pre <- first_assign |>
  slice_sample(prop = 0.15) |>
  mutate(origination_date = as.Date("2026-05-01") + sample(0:30, n(), TRUE))

loans <- bind_rows(loans_post, loans_pre) |>
  select(user_id, origination_date) |>
  arrange(origination_date) |>
  mutate(loan_id = sprintf("L%06d", row_number()),
         merchant_category = sample(categories, n(), TRUE, prob = c(.25, .2, .2, .1, .15, .1)),
         loan_amount = round(pmin(pmax(rlnorm(n(), log(260), 0.7), 50), 2500), 2)) |>
  select(loan_id, user_id, origination_date, merchant_category, loan_amount)

# ---- Installments (pay in 4, every 2 weeks) -------------------------------------

installments <- loans |>
  tidyr::crossing(installment_number = 1:4) |>
  arrange(loan_id, installment_number) |>
  group_by(loan_id) |>
  mutate(amount_due = if_else(installment_number < 4,
                              floor(loan_amount / 4 * 100) / 100,
                              round(loan_amount - 3 * floor(loan_amount / 4 * 100) / 100, 2))) |>
  ungroup() |>
  mutate(installment_id = sprintf("I%07d", row_number()),
         original_due_date = origination_date + 14 * installment_number) |>
  left_join(users |> select(user_id, risk, has_autopay, prior_loans, phone_valid), by = "user_id") |>
  left_join(first_assign, by = "user_id")

# Planted: about 3% of installments were rescheduled 1-2 weeks later (more often for riskier users)
resched <- rbinom(nrow(installments), 1, plogis(-3.8 + 0.6 * installments$risk)) == 1
installments$due_date <- installments$original_due_date + ifelse(resched, sample(7:14, nrow(installments), TRUE), 0)

# ---- SMS reminders (test group only, 3 days before the due date) ----------------

installments <- installments |>
  mutate(sms_sent = variant == "test" & due_date - 3 >= as.Date(assigned_at),
         sms_delivered = sms_sent & phone_valid)

sms_events <- installments |>
  filter(sms_sent) |>
  transmute(sms_id = sprintf("S%07d", row_number()), user_id, installment_id,
            sent_at = format(as.POSIXct(paste(due_date - 3, "17:00:00"), tz = "UTC"), "%Y-%m-%d %H:%M:%S"),
            status = if_else(sms_delivered, "delivered", "failed"))

# ---- Payment behavior -----------------------------------------------------------
# The reminder lowers the chance of paying late, but only for users without autopay
# who actually received it. Lateness is measured against the date the user is working to.

n_inst <- nrow(installments)
base <- with(installments, case_when(has_autopay ~ 0.015, prior_loans == 0 ~ 0.17, TRUE ~ 0.11))
logit <- qlogis(base) + 0.8 * installments$risk + 0.25 * (installments$amount_due > 150) -
  0.42 * (installments$sms_delivered & !installments$has_autopay)
is_late <- rbinom(n_inst, 1, plogis(logit)) == 1

days_early <- sample(0:10, n_inst, TRUE, prob = c(.30, .18, .12, .08, .06, .06, .05, .05, .04, .03, .03))
days_late  <- pmax(1, ceiling(rexp(n_inst, 1 / 11)))
never_pays <- is_late & rbinom(n_inst, 1, 0.10) == 1

pay_date <- installments$due_date + ifelse(is_late, days_late, -days_early)
# Local time of day: on-time payers who pay on the due date itself often pay in the evening
hour <- ifelse(!is_late & days_early == 0, sample(c(8:23), n_inst, TRUE, prob = c(rep(1, 9), rep(2.2, 7))),
               sample(7:23, n_inst, TRUE))
local_ts <- as.POSIXct(paste(pay_date, sprintf("%02d:%02d:%02d", hour, sample(0:59, n_inst, TRUE),
                                               sample(0:59, n_inst, TRUE))), tz = tz_local)

pay <- tibble(installment_id = installments$installment_id, user_id = installments$user_id,
              amount_due = installments$amount_due, ts = local_ts, never = never_pays)

# Planted: 5% of installments are paid in two parts: a first chunk days earlier, the rest at the usual time
split <- rbinom(n_inst, 1, 0.05) == 1
first_share <- round(runif(n_inst, 0.5, 0.8), 2)
part1 <- pay |> mutate(amount = if_else(split, round(amount_due * first_share, 2), amount_due),
                       ts = ts - split * sample(3:10, n_inst, TRUE) * 86400)
part2 <- pay |> filter(split) |>
  mutate(amount = round(amount_due - round(amount_due * first_share[split], 2), 2))

payments <- bind_rows(part1, part2) |>
  filter(!never, as.Date(ts, tz = tz_local) <= snapshot) |>
  mutate(status = "completed")

# Planted: 1.5% of payments bounced (status "reversed"); most are retried 5-25 days later
bounced <- rbinom(nrow(payments), 1, 0.015) == 1
retries <- payments[bounced, ] |>
  filter(rbinom(n(), 1, 0.85) == 1) |>
  mutate(ts = ts + sample(5:25, n(), TRUE) * 86400) |>
  filter(as.Date(ts, tz = tz_local) <= snapshot)
payments$status[bounced] <- "reversed"

payments <- bind_rows(payments, retries) |>
  arrange(ts) |>
  mutate(payment_id = sprintf("P%07d", row_number()),
         paid_at = format(ts, "%Y-%m-%d %H:%M:%S", tz = "UTC")) |>   # stored in UTC
  select(payment_id, installment_id, user_id, paid_at, amount, status)

# ---- SMS opt-outs (guardrail) ---------------------------------------------------

opt_p <- ifelse(first_assign$variant == "test", 0.013, 0.006)
sms_optouts <- first_assign |>
  filter(rbinom(n(), 1, opt_p) == 1) |>
  transmute(user_id, opted_out_at = format(as.POSIXct(assigned_at, tz = "UTC") + runif(n(), 86400, 100 * 86400),
                                           "%Y-%m-%d %H:%M:%S", tz = "UTC"))

# ---- Write ----------------------------------------------------------------------

tables <- list(
  users = users |> select(user_id, signup_date, prior_loans, has_autopay),
  experiment_assignments = experiment_assignments,
  loans = loans,
  installments = installments |>
    select(installment_id, loan_id, user_id, installment_number, original_due_date, due_date, amount_due),
  payments = payments,
  sms_events = sms_events,
  sms_optouts = sms_optouts
)

dir.create("data", showWarnings = FALSE)
for (nm in names(tables)) write_csv(tables[[nm]], file.path("data", paste0(nm, ".csv")))

if (file.exists("loans.duckdb")) file.remove("loans.duckdb")
con <- dbConnect(duckdb(), "loans.duckdb")
for (nm in names(tables)) dbWriteTable(con, nm, tables[[nm]])
invisible(dbExecute(con, "alter table experiment_assignments alter assigned_at type timestamp"))
invisible(dbExecute(con, "alter table payments alter paid_at type timestamp"))
invisible(dbExecute(con, "alter table sms_events alter sent_at type timestamp"))
invisible(dbExecute(con, "alter table sms_optouts alter opted_out_at type timestamp"))
dbDisconnect(con, shutdown = TRUE)

cat("Rows written:\n")
print(sapply(tables, nrow))
