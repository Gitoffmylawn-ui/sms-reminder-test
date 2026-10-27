# Step 5: the memo chart.
# Needs the fct_installment_outcomes view from Step 3 (run that first in the same R session).

library(ggplot2)

seg <- run_sql("
  select case when has_autopay then 'Autopay'
              when first_time_customer then 'No autopay, first-time customer'
              else 'No autopay, repeat customer' end as segment,
         variant,
         count(*)               as n,
         sum(paid_on_time::int) as on_time
  from fct_installment_outcomes
  group by 1, 2
")

# Effect = test rate minus control rate, with a 95% confidence interval, for each segment
effects <- do.call(rbind, lapply(split(seg, seg$segment), function(d) {
  d <- d[order(d$variant), ]                      # control first, then test
  t <- prop.test(d$on_time, d$n)
  data.frame(segment = d$segment[1],
             effect  = 100 * (d$on_time[2] / d$n[2] - d$on_time[1] / d$n[1]),
             low     = -100 * t$conf.int[2],
             high    = -100 * t$conf.int[1])
}))
effects$segment <- factor(effects$segment, levels = effects$segment[order(effects$effect)])
print(effects)

chart <- ggplot(effects, aes(x = effect, y = segment)) +
  geom_vline(xintercept = 0, colour = "grey60") +
  geom_errorbar(aes(xmin = low, xmax = high), width = 0.15, orientation = "y", colour = "#1f4e79") +
  geom_point(size = 3.5, colour = "#1f4e79") +
  geom_label(aes(label = sprintf("%+.1f pts", effect)), vjust = -0.6, size = 3.8,
             fill = "white", linewidth = 0) +
  labs(title = "Reminders help users without autopay, not autopay users",
       subtitle = "Change in on-time payment rate, test minus control, with 95% confidence interval",
       x = "Percentage points", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.major.y = element_blank(), plot.title = element_text(face = "bold"),
        plot.title.position = "plot")

print(chart)
ggsave("sms_reminder_effect.png", chart,
       width = 8, height = 4, dpi = 150, bg = "white")
