# Builds the data behind the dashboard: the clean installment table from Step 3, plus a record
# of how much each data fix changed the measured effect.
#
# Run from this folder:  Rscript 01_build_dashboard_data.R
# Writes dashboard/data/dashboard_data.rds

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})
con <- dbConnect(duckdb(), "loans.duckdb", read_only = TRUE)
run_sql <- function(sql) dbGetQuery(con, sql)

# ---- Clean installment-level table (all seven fixes) ----------------------------

fct <- run_sql("
with assign as (
  select user_id,
         min(variant)            as variant,
         count(distinct variant) as n_variants,
         min(assigned_at)        as assigned_at
  from experiment_assignments
  where experiment_name = 'sms_reminder_3day'                    -- #1 only our experiment
  group by user_id
),
pay as (
  select installment_id,
         cast(paid_at - interval 7 hour as date) as paid_date_pacific,  -- #7 UTC to Pacific
         amount
  from payments
  where status = 'completed'                                     -- #3 only completed payments
),
paid as (
  select i.installment_id, i.user_id, a.variant, u.has_autopay,
         u.prior_loans = 0 as first_time_customer,
         l.merchant_category, i.installment_number, i.amount_due, i.original_due_date,
         coalesce(sum(p.amount) filter (where p.paid_date_pacific <= i.original_due_date), 0)      as paid_by_due,
         coalesce(sum(p.amount) filter (where p.paid_date_pacific <= i.original_due_date + 30), 0) as paid_by_due_30
  from installments i
  join assign a on a.user_id = i.user_id
  join users  u on u.user_id = i.user_id
  join loans  l on l.loan_id = i.loan_id
  left join pay p on p.installment_id = i.installment_id
  where a.n_variants = 1                                         -- #2 drop users in both groups
    and i.original_due_date >= cast(a.assigned_at as date) + 3   -- #4, #5 original date, due after assignment
    and i.original_due_date <= date '2026-09-15'                 -- #6 old enough to judge
  group by all
)
select *,
       paid_by_due    >= amount_due - 0.01 as paid_on_time,
       paid_by_due_30 <  amount_due - 0.01 as dpd30,
       case when paid_by_due_30 < amount_due - 0.01 then amount_due - paid_by_due_30 else 0 end as dpd30_balance
from paid
")

# ---- How the measured effect changes as each fix is applied ---------------------
# Effect = test minus control on-time rate, users without autopay. Fixes are applied cumulatively.

effect_with_fixes <- function(k) {
  on <- function(i) i <= k
  due <- if (on(4)) "i.original_due_date" else "i.due_date"
  run_sql(sprintf("
    with assign as (%s),
    pay as (
      select installment_id, cast(%s as date) as paid_date, amount
      from payments %s
    ),
    per_inst as (
      select a.variant, i.installment_id, i.amount_due,
             coalesce(sum(p.amount) filter (where p.paid_date <= %s), 0) as paid_by_due
      from installments i
      join assign a on a.user_id = i.user_id
      join users u on u.user_id = i.user_id
      left join pay p on p.installment_id = i.installment_id
      where not u.has_autopay %s %s
      group by all
    )
    select avg((paid_by_due >= amount_due - 0.01)::int) filter (where variant = 'test') -
           avg((paid_by_due >= amount_due - 0.01)::int) filter (where variant = 'control') as effect
    from per_inst",
    if (on(2)) "select user_id, min(variant) variant, min(assigned_at) assigned_at from experiment_assignments
                where experiment_name = 'sms_reminder_3day' group by 1 having count(distinct variant) = 1"
    else if (on(1)) "select user_id, variant, assigned_at from experiment_assignments
                     where experiment_name = 'sms_reminder_3day'"
    else "select user_id, variant, assigned_at from experiment_assignments where variant in ('test', 'control')",
    if (on(7)) "paid_at - interval 7 hour" else "paid_at",
    if (on(3)) "where status = 'completed'" else "",
    due,
    if (on(5)) sprintf("and %s >= cast(a.assigned_at as date) + 3", due) else "",
    if (on(6)) sprintf("and %s <= date '2026-09-15'", due) else ""
  ))$effect
}

fixes <- data.frame(
  step = 0:7,
  fix = c("No cleaning", "Only the SMS experiment", "Drop users in both groups", "Only completed payments",
          "Judge against original due date", "Only installments due after assignment",
          "Only installments old enough to judge", "Convert UTC to Pacific time"),
  problem = c("",
              "A second experiment (checkout button color) shares the assignments table and also has a 'control' group",
              "Some users were assigned to both test and control",
              "Bounced payments (status 'reversed') never brought in money",
              "Some due dates were rescheduled; reschedule rates were equal in both groups (2.5% vs. 2.6%)",
              "Installments due before a user joined could never have received a reminder",
              "Installments due after Sept 15 haven't had 30 days to go late",
              "Payment times are stored in UTC, so evening payments on the due date look a day late"),
  rows_affected = c(NA,
    run_sql("select count(*) n from experiment_assignments where experiment_name <> 'sms_reminder_3day'")$n,
    run_sql("select count(*) n from (select user_id from experiment_assignments where experiment_name = 'sms_reminder_3day'
             group by 1 having count(distinct variant) > 1)")$n,
    run_sql("select count(*) n from payments where status = 'reversed'")$n,
    run_sql("select count(*) n from installments where due_date <> original_due_date")$n,
    run_sql("select count(*) n from installments i join experiment_assignments a on a.user_id = i.user_id
             where a.experiment_name = 'sms_reminder_3day'
               and i.original_due_date < cast(a.assigned_at as date) + 3")$n,
    run_sql("select count(*) n from installments where original_due_date > date '2026-09-15'")$n,
    run_sql("select count(*) n from payments p join installments i on i.installment_id = p.installment_id
             where p.status = 'completed' and cast(p.paid_at as date) = i.original_due_date + 1
               and cast(p.paid_at - interval 7 hour as date) = i.original_due_date")$n),
  unit = c("", "assignment rows", "users", "payments", "installments", "installments", "installments", "payments")
)
fixes$effect <- vapply(fixes$step, effect_with_fixes, numeric(1))

# ---- Guardrail and side checks --------------------------------------------------

optouts <- run_sql("
  select a.variant, count(*) as users, count(o.user_id) as opted_out
  from experiment_assignments a
  left join sms_optouts o on o.user_id = a.user_id
  where a.experiment_name = 'sms_reminder_3day'
  group by 1")

reschedules <- run_sql("
  select a.variant, count(*) as installments, sum((i.due_date <> i.original_due_date)::int) as rescheduled
  from installments i
  join experiment_assignments a on a.user_id = i.user_id
  where a.experiment_name = 'sms_reminder_3day'
  group by 1")

dbDisconnect(con, shutdown = TRUE)

dir.create("dashboard/data", recursive = TRUE, showWarnings = FALSE)
saveRDS(list(fct = fct, fixes = fixes, optouts = optouts, reschedules = reschedules,
             snapshot = as.Date("2026-10-15")),
        "dashboard/data/dashboard_data.rds")

cat("Clean installments:", nrow(fct), "\n")
print(fixes[, c("step", "fix", "rows_affected", "effect")])
