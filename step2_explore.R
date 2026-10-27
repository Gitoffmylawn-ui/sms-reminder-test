# Step 2: explore the data.
# Open this file in RStudio and click "Source" (top right of the editor), or press Ctrl+Shift+S.
# Set the working directory to this folder first, e.g. setwd("path/to/sms-reminder-test").
# Results print in the Console pane below.

library(DBI)
library(duckdb)

con <- dbConnect(duckdb(), "loans.duckdb", read_only = TRUE)

# Runs a SQL query and prints the result (print() is needed when a script is sourced)
run_sql <- function(title, sql) {
  cat("\n====", title, "====\n")
  print(dbGetQuery(con, sql))
}

run_sql("a) What's in the experiment table?", "
  select experiment_name, variant, count(*) as n
  from experiment_assignments
  group by 1, 2
  order by 1, 2")

run_sql("b) Users assigned to the SMS experiment more than once (first 10)", "
  select user_id, count(*) as n
  from experiment_assignments
  where experiment_name = 'sms_reminder_3day'
  group by 1
  having count(*) > 1
  limit 10")

run_sql("c) Payment statuses", "
  select status, count(*) as n
  from payments
  group by 1")

run_sql("d) Installments where due_date differs from original_due_date", "
  select (due_date <> original_due_date) as rescheduled, count(*) as n
  from installments
  group by 1")

dbDisconnect(con, shutdown = TRUE)
