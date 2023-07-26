library(gadget3)
library(gadgetutils)
library(gadgetplots)

# Create time/area definitions ################################################

area_names <- list("IXa" = 1)

actions_time <- list(
  g3a_time(
    1979L, 2023L,
    #final_year_steps = 2L
    step_lengths = c(3L, 3L, 3L, 3L)),
  NULL)

actions <- actions_time


# Create stock definition for anch ############################################
anch <- g3_stock("anch", seq(3, 22 - 0.5, 0.5)) |> 
  g3s_livesonareas(area_names["IXa"]) |>
  g3s_age(0L, 3L)

actions_anch <- list(
  g3a_growmature(anch, g3a_grow_impl_bbinom(
    maxlengthgroupgrowth = 5L)),
  g3a_naturalmortality(anch,
    g3a_naturalmortality_exp(by_age = TRUE)),
  g3a_initialconditions_normalparam(anch,
    factor_f = g3a_renewal_initabund(
      scalar = g3_parameterized("init.scalar", by_stock = TRUE, by_age = TRUE),
      M = g3_parameterized('M', by_stock = TRUE, by_age = TRUE)),
    by_age = TRUE),
  g3a_renewal_normalparam(anch,
    factor_f = g3_parameterized('anch.rec', by_year = TRUE, by_step = TRUE, ifmissing = NaN),
    stddev_f = g3_parameterized('rec.sd', by_stock = TRUE, by_step = TRUE),
    run_f = ~age == stock__minage && !cur_year_projection),
  g3a_age(anch),
  NULL)

actions_likelihood_anch <- list(
  g3l_understocking(list(anch), weight = 1e+08, nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_anch, actions_likelihood_anch)


# Create fleet definition for seine ###########################################
seine_observations <- readRDS('Data_catchdistribution_ldist_seine_sumofsquares.rds')
seine_data <- readRDS('Data_fleet_seine_data.rds')

seine <- g3_fleet("seine") |> g3s_livesonareas(area_names["IXa"])
actions_seine <- list(
  g3a_predate_fleet(
    seine,
    list(anch),
    suitabilities = list(
      anch = g3_suitability_exponentiall50(
        g3_formula(
          if (cur_year < 2001) alpha_early else alpha_late,
          alpha_early = g3_parameterized('seine.alpha.1998', by_stock = TRUE),
          alpha_late = g3_parameterized('seine.alpha.2001', by_stock = TRUE)),
        g3_formula(
          if (cur_year < 2001) l50_early else l50_late,
          l50_early = g3_parameterized('seine.l50.1998', by_stock = TRUE),
          l50_late = g3_parameterized('seine.l50.2001', by_stock = TRUE)))),
    catchability_f = g3a_predate_catchability_numberfleet(
      g3_timeareadata("seine_data", seine_data, "number", areas = area_names))),
  NULL)
actions_likelihood_seine <- list(
  g3l_catchdistribution(
    "ldist.seine",
    seine_observations,
    fleets = list(seine),
    stocks = list(anch),
    function_f = g3l_distribution_sumofsquares(),
    area_group = list(IXa = 1),
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_seine, actions_likelihood_seine)


# Create fleet definition for pelago ##########################################
pelago_observations <- readRDS('Data_catchdistribution_ldist_pelago_noage_sumofsquares.rds')
pelago_data <- readRDS('Data_catchdistribution_aldist_pelago_sumofsquares.rds')

pelago <- g3_fleet("pelago") |> g3s_livesonareas(area_names["IXa"])
actions_pelago <- list(
  g3a_predate_fleet(
    pelago,
    list(anch),
    suitabilities = list(
      anch = g3_suitability_exponentiall50(
        g3_parameterized('pelago.alpha', by_stock = TRUE),
        g3_parameterized('pelago.l50', by_stock = TRUE))),
    catchability_f = g3a_predate_catchability_numberfleet(
      g3_timeareadata("pelago_data", pelago_data, "number", areas = area_names))),
  NULL)
actions_likelihood_pelago <- list(
  g3l_catchdistribution(
    "ldist.pelago",
    pelago_observations,
    fleets = list(pelago),
    stocks = list(anch),
    function_f = g3l_distribution_sumofsquares(),
    area_group = area_names,
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_pelago, actions_likelihood_pelago)


# Create survey definition for pelagonumber ###################################
pelagonumber_observations <- readRDS('Data_surveyindices_pelagonumber_survey_lengths.rds')

actions_pelagonumber <- list()
actions_likelihood_pelagonumber <- list(
  g3l_abundancedistribution(
    "pelagonumber.survey",
    pelagonumber_observations,
    stocks = list(anch),
    function_f = g3l_distribution_surveyindices_log(beta = 1L),
    area_group = area_names, 
    report = TRUE,
    nll_breakdown = TRUE),
  NULL)

actions <- c(actions, actions_pelagonumber, actions_likelihood_pelagonumber)


# Reporting & debugging actions for entire model ##############################

actions_reporting <- list(
  g3a_report_detail(actions),
  # g3a_report_history(actions),
  # g3experiments:::g3a_trace_nan(actions,  var_re = c("anch__num$", "anch__wgt$")),
  NULL)

actions <- c(actions, actions_reporting)


# Create model objective function #############################################

model_code <- g3_to_tmb(actions)
# model_code <- edit(model_code)
params.in <- attr(model_code, "parameter_template")
params.in['report_detail', 'value'] <- 1

# g3a_renewal_initabund()
params.in[grepl('^anch.init.scalar\\.\\d+', params.in$switch), 'value'] <- c(60000, 60000, 60000, 0.000001)
params.in[grepl('^anch.init.scalar\\.\\d+', params.in$switch), 'optimise'] <- FALSE
params.in[grepl('^anch\\.init\\.\\d+', params.in$switch), 'value'] <- 1  # #init0..3
params.in[grepl('^anch\\.init\\.sd\\.\\d+', params.in$switch), 'value'] <- c(0.5, 0.531, 1, 1.2)  # #init0..3
params.in['init.F', 'value'] <- 0  # NB: No equivalent in gadget2, see g3a_renewal_initabund()

# NB: This is as close as we can get with the default _vonb()
# all.equal(c(9.76, 13.6, 15.2, 16.1), sapply(0:3, function (a) g3_eval(g3a_renewal_vonb(Linf = 19, K = 0.89, recage = 0, recl = 9.759), age = a)))
# [1] "Mean relative difference: 0.1117414"
params.in["anch.recl", 'value'] <- 9.759
params.in["anch.recl", 'optimise'] <- FALSE
params.in["recage", 'value'] <- 0
params.in["recage", 'optimise'] <- FALSE

# g3a_naturalmortality()
params.in[grepl('^anch\\.M\\.\\d+', params.in$switch), 'value'] <- c(2.21, 1.3, 1.3, 1.3)

# g3a_renewal_normalparam()
params.in[grepl('^anch\\.rec\\.[0-9]+', params.in$switch), 'value'] <- 1000 * 0.5 * 100
# TODO: anch.rec.sd.0 should be 0, but that results in a div/0?
params.in[grepl('^anch\\.rec\\.sd\\.[0-9]+', params.in$switch), 'value'] <- c(1, 1, 1, 1)  # 0 #sdrecl2..4

# g3a_predate_fleet()
params.in["anch.pelago.alpha", 'value'] <- 0.2  # #constantpel
params.in["anch.pelago.l50", 'value'] <- 10  # #l50pel
params.in["anch.seine.alpha.1998", 'value'] <- 1  # #anchalpha88
params.in["anch.seine.l50.1998", 'value'] <- 8  # #anchL5088
params.in["anch.seine.alpha.2001", 'value'] <- 1  # #anchalpha01
params.in["anch.seine.l50.2001", 'value'] <- 10  # #anchL5001

# g3a_growmature() 
params.in["anch.Linf", 'value'] <- 19  # #Linf
params.in["anch.K", 'value'] <- 0.89  # #kappa
params.in["anch.walpha", 'value'] <- 3.128958e-06  # #alpha
params.in["anch.wbeta", 'value'] <- 3.277667619  # #beta
params.in["anch.bbin", 'value'] <- 1  # #bbeta

obj.fn <- gadget3::g3_tmb_adfun(model_code, params.in)
obj.fn$report()$anch__num

fit <- gadgetutils::g3_fit(model_code, params.in)
# gadgetplots::plot_ldist(fit)
