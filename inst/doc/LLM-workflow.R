## ----library--------------------------------------------------------------------------------------
# This vignette is also installed as a runnable script:
#   source(system.file("doc", "LLM-workflow.R", package = "Gtheory4LLM"))
library(Gtheory4LLM)

## ----design---------------------------------------------------------------------------------------
set.seed(20260912)
llm_data <- expand.grid(item = factor(seq_len(24)),
  evaluator = factor(paste0("model_", seq_len(4))),
  prompt = factor(paste0("prompt_", seq_len(3))),
  run = factor(seq_len(2)))

llm_design <- gt_design("item", c("evaluator", "prompt", "run"),
  crossed = c("evaluator", "prompt"),
  nested = list(run = c("evaluator", "prompt")),
  item_interactions = "additive", instrument_interactions = "additive")
llm_design$terms_requested

## ----simulate-------------------------------------------------------------------------------------
source_sd <- c(item = 1.2, evaluator = 0.4, prompt = 0.3,
  "item:evaluator" = 0.5, "item:prompt" = 0.4,
  "evaluator:prompt:run" = 0.25)
llm_data$quality <- 0
for (source in names(source_sd)) {
  members <- strsplit(source, ":", fixed = TRUE)[[1L]]
  group <- do.call(interaction, c(llm_data[members], list(drop = TRUE)))
  effects <- rnorm(nlevels(group), sd = source_sd[[source]])
  llm_data$quality <- llm_data$quality + effects[as.integer(group)]
}
llm_data$quality <- llm_data$quality + rnorm(nrow(llm_data), sd = 0.6)

## ----preflight------------------------------------------------------------------------------------
preflight <- gt_preflight(llm_data, "quality", llm_design)
print(preflight)
preflight$sources
stopifnot(preflight$fitting_feasible)

## ----panel-audit, fig.width=7, fig.height=4-------------------------------------------------------
preflight$panel_audit$summary
plot(preflight, type = "cells")
incomplete <- gt_preflight(llm_data[-1, ], "quality", llm_design)
incomplete$panel_audit$missing_cells

## ----fit------------------------------------------------------------------------------------------
fit <- gt_fit(llm_data, "quality", llm_design,
  control = gt_control(gaussian = list(optimizer = "CSOLNP",
    extra_tries = 2L, retry_seed = 415L, threads = 1L)))
print(summary(fit))
diagnostics <- gt_diagnostics(fit)
print(diagnostics[c("numerically_accepted", "selected_attempt",
                    "acceptance_failures", "approximation_adequacy")])
stopifnot(isTRUE(diagnostics$numerically_accepted))
gt_components(fit)

## ----variances------------------------------------------------------------------------------------
summary(fit)$variances

## ----reliability----------------------------------------------------------------------------------
reliability <- gt_reliability(fit)
reliability
reliability$interpretation

## ----reliability-figure, fig.width=8, fig.height=4.5----------------------------------------------
plot(reliability, target = 0.80,
  main = "Reliability of the declared mean quality score")
reliability_table <- as.data.frame(reliability)

## ----dstudy---------------------------------------------------------------------------------------
grid <- expand.grid(evaluator = c(2L, 4L, 6L),
  prompt = c(2L, 3L, 4L), run = c(1L, 2L))
dstudy <- gt_dstudy(fit, grid)
planning <- as.data.frame(dstudy)
# Prefixed allocation columns avoid collisions with outcome/estimate names.
# These readable aliases are just for this tutorial's known facet names.
planning$evaluator <- planning$allocation_evaluator
planning$prompt <- planning$allocation_prompt
planning$run <- planning$allocation_run
planning$measurements_per_item <- planning$measurements_per_object
planning <- planning[order(planning$measurements_per_item, -planning$Phi), ]
rownames(planning) <- NULL
print(planning)

## ----threshold------------------------------------------------------------------------------------
screen <- gt_dstudy_target(dstudy, target = 0.80, coefficient = "Phi",
  outcome = "quality")
screen[screen$meets_target %in% TRUE, ]
screen[screen$fewest_measurements %in% TRUE, ]

## ----dstudy-figure, fig.width=7, fig.height=5-----------------------------------------------------
plot(dstudy, coefficient = "Phi", outcome = "quality", target = 0.80,
  main = "Candidate measurement allocations")

## ----analysis-report------------------------------------------------------------------------------
analysis_report <- gt_report(fit, preflight = preflight,
  reliability = reliability, dstudy = dstudy)
print(analysis_report)

## ----fixed----------------------------------------------------------------------------------------
fixed_prompt <- gt_reliability(fit, fixed = "prompt")
fixed_prompt
fixed_prompt$source_roles

## ----discrete-design------------------------------------------------------------------------------
gt_design("item", "rater", full_cell = FALSE)$terms_requested

## ----outcome-information, fig.width=7, fig.height=4-----------------------------------------------
information_data <- expand.grid(item = seq_len(6), rater = seq_len(3))
information_data$label <- as.integer(information_data$item == 1 &
  information_data$rater == 1)
information_design <- gt_design("item", "rater", random = ~ item + rater)
information <- gt_preflight(information_data, "label", information_design,
  gt_family("binary"), max_examples = 0)
information$outcome_profile$label$categories
information$outcome_profile$label$by_variable
plot(information, type = "outcomes", outcome = "label")

