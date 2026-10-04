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

## ----dstudy---------------------------------------------------------------------------------------
grid <- expand.grid(evaluator = c(2L, 4L, 6L),
  prompt = c(2L, 3L, 4L), run = c(1L, 2L))
dstudy <- gt_dstudy(fit, grid)
allocation <- dstudy$allocations
allocation$measurements_per_item <- dstudy$measurements_per_object
allocation$extrapolated <- dstudy$extrapolated
planning <- cbind(allocation,
  dstudy$results[, c("outcome", "Erho2", "Erho2_lower", "Erho2_upper",
                     "Phi", "Phi_lower", "Phi_upper")])
planning <- planning[order(planning$measurements_per_item, -planning$Phi), ]
rownames(planning) <- NULL
print(planning)

## ----threshold------------------------------------------------------------------------------------
planning[planning$Phi >= 0.80, ]

## ----fixed----------------------------------------------------------------------------------------
fixed_prompt <- gt_reliability(fit, fixed = "prompt")
fixed_prompt
fixed_prompt$source_roles

## ----discrete-design------------------------------------------------------------------------------
gt_design("item", "rater", full_cell = FALSE)$terms_requested

