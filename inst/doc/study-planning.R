## ----pilot-----------------------------------------------------------------------------------
library(Gtheory4LLM)
set.seed(2718)
panel <- expand.grid(item = 1:24, rater = 1:4, run = 1:3,
                     KEEP.OUT.ATTRS = FALSE)
item_rater <- as.integer(interaction(panel$item, panel$rater, drop = TRUE))
panel$quality <- rnorm(24)[panel$item] + rnorm(4, sd = 0.5)[panel$rater] +
  rnorm(96, sd = 0.4)[item_rater] + rnorm(nrow(panel), sd = 0.8)
design <- gt_design("item", c("rater", "run"),
                    random = ~ item + rater + item:rater)
control <- gt_control(gaussian = list(extra_tries = 2L, retry_seed = 439L))
fit <- gt_fit(panel, "quality", design, control = control)
stopifnot(isTRUE(fit$numerically_accepted))
gt_reliability(fit)$per_trait

## ----monetary-plan---------------------------------------------------------------------------
plan <- gt_plan(fit,
  candidates = list(rater = c(2, 4, 6), run = c(1, 3, 5)),
  cost = function(d) list(
    costs = data.frame(annotation = 0.01 * d$total_annotations,
                       setup = 2 * d$allocation_rater),
    resources = data.frame(requests = d$total_annotations),
    assumptions = "Illustrative prices; one item per request."),
  currency = "USD", cost_date = "2026-10-09", study_items = 1000,
  target = 0.80, coefficient = "Phi", baseline = 1L,
  constraints = function(d) data.frame(budget = d$total_cost <= 180))
as.data.frame(plan)[, c("design_id", "total_cost", "screening_value",
                       "feasible", "minimum_cost", "pareto")]
head(plan$source_contributions)

## ----batch-fit-------------------------------------------------------------------------------
batched <- panel
group <- (batched$item - 1L) %/% 6L + 1L
call_index <- as.integer(interaction(group, batched$rater, batched$run, drop = TRUE))
batched$call_id <- paste0("call_", call_index)
batched$quality <- batched$quality + rnorm(max(call_index), sd = 0.7)[call_index]
batch_design <- gt_design("item", c("rater", "run"),
  random = ~ item + rater + item:rater, batch = gt_batch(6, id = "call_id"))
batch_fit <- gt_fit(batched, "quality", batch_design, control = control)
stopifnot(isTRUE(batch_fit$numerically_accepted))
within <- c("1" = 1, "2" = -1)
between <- c("1" = 1, "7" = -1)
corpus <- setNames(rep(1 / 24, 24), as.character(1:24))
batch_projection <- gt_batch_reliability(batch_fit,
  contrasts = list(within = within, between = between), aggregate = list(corpus = corpus))
subset(as.data.frame(batch_projection), target_kind != "absolute_item")
subset(batch_projection$source_contributions,
       source == "Call" & target_kind != "absolute_item")

## ----batch-design-study----------------------------------------------------------------------
batch_study <- gt_batch_dstudy(batch_fit,
  expand.grid(rater = c(2, 4), run = c(1, 3)),
  contrasts = list(within = within, between = between), aggregate = list(corpus = corpus))
subset(as.data.frame(batch_study), target_kind == "contrast")

## ----pilot-precision-------------------------------------------------------------------------
precision <- gt_pilot_plan(fit,
  grid = data.frame(n_items = c(16, 24), rater = c(3, 4)),
  target_counts = c(rater = 4, run = 3),
  nsim = 2, seed = 2026, coefficient = "Phi", width_target = 0.25)
precision$summary
precision$replicates[, c("design_id", "replicate_id", "status", "accepted",
                         "interval_available", "interval_conditional", "width",
                         "acceptance_failures", "selected_attempt")]

