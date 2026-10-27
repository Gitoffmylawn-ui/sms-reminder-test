# Answer key for the SMS reminder practice case. Don't open until you've done the steps yourself.
# Run from the repo folder:  Rscript solutions/answer_key.R

suppressPackageStartupMessages({
  library(DBI)
  library(duckdb)
})
con <- dbConnect(duckdb(), "loans.duckdb", read_only = TRUE)
q <- function(sql) dbGetQuery(con, sql)

# ---- Clean installment-level model (what Step 3 builds) -------------------------

invisible(dbExecute(con, "
create or replace temp view fct_installment_outcomes as
with assign as (
  select user_id,
         min(variant)               as variant,
         count(distinct variant)    as n_variants,
         min(assigned_at)           as assigned_at
  from experiment_assignments
  where experiment_name = 'sms_reminder_3day'
  group by user_id
),
pay as (
  select installment_id,
         cast(paid_at - interval 7 hour as date) as paid_date_local,   -- UTC to Pacific daylight time
         amount
  from payments
  where status = 'completed'
),
paid as (
  select i.installment_id, i.user_id, a.variant, u.has_autopay,
         u.prior_loans = 0 as first_time_customer,
         i.amount_due, i.original_due_date,
         coalesce(sum(p.amount) filter (where p.paid_date_local <= i.original_due_date), 0)      as paid_by_due,
         coalesce(sum(p.amount) filter (where p.paid_date_local <= i.original_due_date + 30), 0) as paid_by_due_30
  from installments i
  join assign a on a.user_id = i.user_id
  join users  u on u.user_id = i.user_id
  left join pay p on p.installment_id = i.installment_id
  where a.n_variants = 1                                           -- drop users assigned to both arms
    and i.original_due_date >= cast(a.assigned_at as date) + 3     -- due after the reminder could be sent
    and i.original_due_date <= date '2026-10-15' - 30              -- mature: 30 days have passed
  group by 1, 2, 3, 4, 5, 6, 7
)
select *,
       paid_by_due    >= amount_due - 0.01 as paid_on_time,
       paid_by_due_30 <  amount_due - 0.01 as dpd30
from paid
"))

cat("\n== Overall (intent to treat) ==\n")
print(q("select variant, count(*) n, avg(paid_on_time::int) on_time, avg(dpd30::int) dpd30
         from fct_installment_outcomes group by 1 order by 1"))

cat("\n== By autopay ==\n")
print(q("select has_autopay, variant, count(*) n, avg(paid_on_time::int) on_time, avg(dpd30::int) dpd30
         from fct_installment_outcomes group by 1, 2 order by 1, 2"))

cat("\n== No autopay, by first-time vs repeat ==\n")
print(q("select first_time_customer, variant, count(*) n, avg(paid_on_time::int) on_time, avg(dpd30::int) dpd30
         from fct_installment_outcomes where not has_autopay group by 1, 2 order by 1, 2"))

# Two-proportion z-tests, no-autopay segment
na <- q("select variant, count(*) n, sum(paid_on_time::int) on_time, sum(dpd30::int) dpd30
         from fct_installment_outcomes where not has_autopay group by 1 order by 1")
cat("\n== Significance, no-autopay users ==\n")
print(prop.test(na$on_time, na$n))
print(prop.test(na$dpd30, na$n))

cat("\n== Guardrail: SMS opt-out rate by variant ==\n")
print(q("select a.variant, count(*) users, count(o.user_id) opted_out, count(o.user_id) / count(*) rate
         from experiment_assignments a left join sms_optouts o using (user_id)
         where a.experiment_name = 'sms_reminder_3day'
         group by 1 order by 1"))

cat("\n== Naive version (no cleaning), for comparison ==\n")
print(q("
  select a.variant, count(distinct i.installment_id) n,
         avg((p.paid_at::date <= i.due_date)::int) naive_on_time
  from experiment_assignments a
  join installments i on i.user_id = a.user_id
  left join payments p on p.installment_id = i.installment_id
  group by 1 order by 1"))

dbDisconnect(con, shutdown = TRUE)
