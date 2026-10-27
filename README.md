# SMS Reminder Test

**Live dashboard: [bonnyandclaude.shinyapps.io/sms-reminder-test](https://bonnyandclaude.shinyapps.io/sms-reminder-test/)**. It runs on a free plan and sleeps when idle, so the first load can take 20–30 seconds.

An end-to-end analysis of a randomized A/B test at a buy-now-pay-later lender, from raw warehouse tables to a recommendation and dashboard. Built with SQL (DuckDB), R and Shiny. **All data is simulated.**

## The question

A lender lets shoppers split a purchase into 4 payments, one every 2 weeks. From June to August 2026, about 20,000 customers were randomly split in half: one half got a text 3 days before each payment was due, the other half got nothing.

The product team asked: *"Did the reminder help? Should we send it to everyone?"*

## The answer

**Send the reminder to every user without autopay, and leave autopay users out.**

| Users without autopay | Control | Test | Change |
|---|---|---|---|
| Paid on time | 81.1% | 85.9% | **+4.8 pts** (95% CI 4.2 to 5.5) |
| 30+ days late | 2.83% | 2.05% | **−28%** |

- Autopay users show no effect (+0.2 pts, not significant).
- First-time customers respond most (+7.1 pts), but repeat customers produce more extra on-time payments in total because there are almost three times as many of them.
- Estimated net benefit: **about $3.1M a year** in avoided losses, for under $100K in text costs.
- SMS opt-outs rose from 0.7% to 1.2%, within the agreed 2% limit.

## The data problems

The raw tables had seven problems planted in them, like real warehouse data. None caused an error; each one quietly made the answer wrong. Fixing them moved the measured lift from **2.8 to 4.8 points**.

| # | Problem | Fix |
|---|---|---|
| 1 | A second experiment shares the assignments table, with its own "control" group | Filter to the SMS experiment |
| 2 | About 150 users were assigned to both test and control | Drop them |
| 3 | Bounced payments (status `reversed`) never brought in money | Count only completed payments |
| 4 | Some due dates were rescheduled | Judge against the original date; reschedule rates were equal in both groups |
| 5 | Installments due before a user joined couldn't have received a reminder | Keep only installments due 3+ days after assignment |
| 6 | Recent installments haven't had 30 days to go late | Keep only installments due by Sept 15 |
| 7 | Payment times are stored in UTC, so evening payments look a day late | Convert to Pacific time |

## What's here

| File | What it does |
|---|---|
| `00_generate_data.R` | Builds the simulated dataset (`loans.duckdb`): 20,000 users, 31,000 loans, 124,000 installments, 129,000 payments |
| `01_build_dashboard_data.R` | Cleans the data in SQL into one row per installment, and records how each fix changed the result |
| `dashboard/app.R` | The Shiny dashboard: overview, breakdowns by customer group, dollar impact with adjustable assumptions, data quality, and method |
| `step2_explore.R`, `step5_chart.R` | Working scripts from the analysis |
| `solutions/answer_key.R` | The full analysis in one script: cleaning, results by segment, significance tests and guardrail |

### Tables

| Table | One row per | Columns |
|---|---|---|
| `users` | user | `user_id`, `signup_date`, `prior_loans`, `has_autopay` |
| `experiment_assignments` | user × experiment assignment | `user_id`, `experiment_name`, `variant`, `assigned_at` (UTC) |
| `loans` | loan | `loan_id`, `user_id`, `origination_date`, `merchant_category`, `loan_amount` |
| `installments` | scheduled payment | `installment_id`, `loan_id`, `user_id`, `installment_number`, `original_due_date`, `due_date`, `amount_due` |
| `payments` | payment attempt | `payment_id`, `installment_id`, `user_id`, `paid_at` (UTC), `amount`, `status` |
| `sms_events` | reminder sent | `sms_id`, `user_id`, `installment_id`, `sent_at` (UTC), `status` |
| `sms_optouts` | user who opted out of texts | `user_id`, `opted_out_at` (UTC) |

## Run it

In R 4.4 or later, from this folder:

```r
install.packages(c("DBI", "duckdb", "dplyr", "readr", "tidyr", "ggplot2", "shiny", "bslib", "DT"))

source("00_generate_data.R")            # optional: rebuilds loans.duckdb (about a minute)
source("01_build_dashboard_data.R")     # optional: rebuilds dashboard/data/dashboard_data.rds
shiny::runApp("dashboard")
```

The database and dashboard data are included, so the app runs straight after cloning. `00_generate_data.R` also writes each table as a CSV in `data/`.

## Assumptions behind the dollar estimate

- About 12 million installments a year from users without autopay.
- 35% of a balance that's 30+ days late is eventually written off.
- A text costs $0.008.

All three can be changed on the dashboard's Dollar impact tab.
