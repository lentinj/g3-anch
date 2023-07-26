Modelfiles_area <- readRDS('Modelfiles_area.rds')
Data_fleet_seine_data <- readRDS('Data_fleet_seine_data.rds')
Data_fleet_ECO_data <- readRDS('Data_fleet_ECO_data.rds')
Data_fleet_PEL_data <- readRDS('Data_fleet_PEL_data.rds')
Data_catchdistribution_ldist_seine_sumofsquares <- readRDS('Data_catchdistribution_ldist_seine_sumofsquares.rds')
Data_catchdistribution_ldist_pelago_noage_sumofsquares <- readRDS('Data_catchdistribution_ldist_pelago_noage_sumofsquares.rds')
Data_catchdistribution_ldist_ecocadiz_noage_sumofsquares <- readRDS('Data_catchdistribution_ldist_ecocadiz_noage_sumofsquares.rds')
Data_catchdistribution_ldist_alkseine_sumofsquares <- readRDS('Data_catchdistribution_ldist_alkseine_sumofsquares.rds')
Data_catchdistribution_aldist_pelago_sumofsquares <- readRDS('Data_catchdistribution_aldist_pelago_sumofsquares.rds')
Data_catchdistribution_aldist_ecocadiz_sumofsquares <- readRDS('Data_catchdistribution_aldist_ecocadiz_sumofsquares.rds')
Data_surveyindices_pelagonumber_survey_lengths <- readRDS('Data_surveyindices_pelagonumber_survey_lengths.rds')
Data_surveyindices_ecocadiz_survey_lengths <- readRDS('Data_surveyindices_ecocadiz_survey_lengths.rds')

actions_time <- list(g3a_time(1979L, 2023L, step_lengths = c(3L, 3L, 3L, 3L), final_year_steps = 2L))

area_names <- structure(seq_len(1), names = c("1"))
area_size <- structure(13000L, names = c("1"))
area_temperature <- Modelfiles_area

comment("Create stock definition for anch")
anch <- g3_stock("anch", seq(3, 22 - 0.5, 0.5))
anch <- g3s_livesonareas(anch, area_names["1"])
anch <- g3s_age(anch, 0L, 3L)
anch_refweight <- NULL

actions_anch <- list(
  g3a_growmature(anch, impl_f = g3a_grow_impl_bbinom(g3a_grow_lengthvbsimple(~g3_param("Linf"), ~g3_param("kappa")), g3a_grow_weightsimple(~g3_param("alpha"), ~g3_param("beta")), beta_f = ~g3_param("bbeta"), maxlengthgroupgrowth = 5L)),
  g3a_naturalmortality(anch, g3a_naturalmortality_exp(~if (age == 0L) 2.21 else if (age == 1L) 1.3 else if (age == 2L) 1.3 else if (age == 3L) 1.3 else 0)),
  g3a_initialconditions_normalparam(anch, factor_f = ~(if (area == area_1 && age == 0L) (60000 * 
    g3_param("init0")) else if (area == area_1 && age == 1L) (60000 * g3_param("init1")) else if (area == area_1 && age == 2L) (60000 * g3_param("init2")) else if (area == area_1 && age == 3L) (1e-06 * g3_param("init3")) else 0) * area, mean_f = ~if (area == area_1 && age == 0L) 9.76 else if (area == area_1 && age == 1L) 13.6 else if (area == area_1 && age == 2L) 15.2 else if (area == area_1 && age == 3L) 16.1 else 0, stddev_f = ~if (area == area_1 && age == 0L) 0.5 else if (area == area_1 && age == 
    1L) 0.531 else if (area == area_1 && age == 2L) 1 else if (area == area_1 && age == 3L) 1.2 else 0, alpha_f = ~anch_refweight * 1, beta_f = 0),
  g3a_age(anch),
  g3a_renewal_normalparam(anch, factor_f = ~if (cur_year == 1979L) (1000 * g3_param("rec79")) else if (cur_year == 1980L) (1000 * g3_param("rec80")) else if (cur_year == 1981L) (1000 * g3_param("rec81")) else if (cur_year == 1982L) (1000 * g3_param("rec82")) else if (cur_year == 1983L) (1000 * g3_param("rec83")) else if (cur_year == 1984L) (1000 * 
    g3_param("rec84")) else if (cur_year == 1985L) (1000 * g3_param("rec85")) else if (cur_year == 1986L) (1000 * g3_param("rec86")) else if (cur_year == 1987L) (1000 * g3_param("rec87")) else if (cur_year == 1988L) (1000 * (g3_param("P388") * g3_param("rec88"))) else if (cur_year == 1989L) (1000 * (g3_param("P389") * g3_param("rec89"))) else if (cur_year == 1990L) (1000 * (g3_param("P390") * g3_param("rec90"))) else if (cur_year == 1991L) (1000 * (g3_param("P391") * g3_param("rec91"))) else if (cur_year == 
    1992L) (1000 * (g3_param("P392") * g3_param("rec92"))) else if (cur_year == 1993L) (1000 * (g3_param("P393") * g3_param("rec93"))) else if (cur_year == 1994L) (1000 * (g3_param("P394") * g3_param("rec94"))) else if (cur_year == 1995L) (1000 * (g3_param("P395") * g3_param("rec95"))) else if (cur_year == 1996L) (1000 * (g3_param("P396") * g3_param("rec96"))) else if (cur_year == 1997L) (1000 * (g3_param("P397") * g3_param("rec97"))) else if (cur_year == 1998L) (1000 * (g3_param("P398") * g3_param("rec98"))) else if (cur_year == 
    1999L) (1000 * (g3_param("P399") * g3_param("rec99"))) else if (cur_year == 2000L) (1000 * (g3_param("P300") * g3_param("rec00"))) else if (cur_year == 2001L) (1000 * (g3_param("P301") * g3_param("rec01"))) else if (cur_year == 2002L) (1000 * (g3_param("P302") * g3_param("rec02"))) else if (cur_year == 2003L) (1000 * (g3_param("P303") * g3_param("rec03"))) else if (cur_year == 2004L) (1000 * (g3_param("P304") * g3_param("rec04"))) else if (cur_year == 2005L) (1000 * (g3_param("P305") * g3_param("rec05"))) else if (cur_year == 
    2006L) (1000 * (g3_param("P306") * g3_param("rec06"))) else if (cur_year == 2007L) (1000 * (g3_param("P307") * g3_param("rec07"))) else if (cur_year == 2008L) (1000 * (g3_param("P308") * g3_param("rec08"))) else if (cur_year == 2009L) (1000 * (g3_param("P309") * g3_param("rec09"))) else if (cur_year == 2010L) (1000 * (g3_param("P310") * g3_param("rec10"))) else if (cur_year == 2011L) (1000 * (g3_param("P311") * g3_param("rec11"))) else if (cur_year == 2012L) (1000 * (g3_param("P312") * g3_param("rec12"))) else if (cur_year == 
    2013L) (1000 * (g3_param("P313") * g3_param("rec13"))) else if (cur_year == 2014L) (1000 * (g3_param("P314") * g3_param("rec14"))) else if (cur_year == 1988L) (1000 * (g3_param("P488") * g3_param("rec88"))) else if (cur_year == 1989L) (1000 * (g3_param("P489") * g3_param("rec89"))) else if (cur_year == 1990L) (1000 * (g3_param("P490") * g3_param("rec90"))) else if (cur_year == 1991L) (1000 * (g3_param("P491") * g3_param("rec91"))) else if (cur_year == 1992L) (1000 * (g3_param("P492") * g3_param("rec92"))) else if (cur_year == 
    1993L) (1000 * (g3_param("P493") * g3_param("rec93"))) else if (cur_year == 1994L) (1000 * (g3_param("P494") * g3_param("rec94"))) else if (cur_year == 1995L) (1000 * (g3_param("P495") * g3_param("rec95"))) else if (cur_year == 1996L) (1000 * (g3_param("P496") * g3_param("rec96"))) else if (cur_year == 1997L) (1000 * (g3_param("P497") * g3_param("rec97"))) else if (cur_year == 1998L) (1000 * (g3_param("P498") * g3_param("rec98"))) else if (cur_year == 1999L) (1000 * (g3_param("P499") * g3_param("rec99"))) else if (cur_year == 
    2000L) (1000 * (g3_param("P400") * g3_param("rec00"))) else if (cur_year == 2001L) (1000 * (g3_param("P401") * g3_param("rec01"))) else if (cur_year == 2002L) (1000 * (g3_param("P402") * g3_param("rec02"))) else if (cur_year == 2003L) (1000 * (g3_param("P403") * g3_param("rec03"))) else if (cur_year == 2004L) (1000 * (g3_param("P404") * g3_param("rec04"))) else if (cur_year == 2005L) (1000 * (g3_param("P405") * g3_param("rec05"))) else if (cur_year == 2006L) (1000 * (g3_param("P406") * g3_param("rec06"))) else if (cur_year == 
    2007L) (1000 * (g3_param("P407") * g3_param("rec07"))) else if (cur_year == 2008L) (1000 * (g3_param("P408") * g3_param("rec08"))) else if (cur_year == 2009L) (1000 * (g3_param("P409") * g3_param("rec09"))) else if (cur_year == 2010L) (1000 * (g3_param("P410") * g3_param("rec10"))) else if (cur_year == 2011L) (1000 * (g3_param("P411") * g3_param("rec11"))) else if (cur_year == 2012L) (1000 * (g3_param("P412") * g3_param("rec12"))) else if (cur_year == 2013L) (1000 * (g3_param("P413") * g3_param("rec13"))) else if (cur_year == 
    2014L) (1000 * (g3_param("P414") * g3_param("rec14"))) else if (cur_year == 2015L) (1000 * (g3_param("P315") * g3_param("rec15"))) else if (cur_year == 2015L) (1000 * (g3_param("P415") * g3_param("rec15"))) else if (cur_year == 2016L) (1000 * (g3_param("P316") * g3_param("rec16"))) else if (cur_year == 2016L) (1000 * (g3_param("P416") * g3_param("rec16"))) else if (cur_year == 1989L) (1000 * (g3_param("P288") * g3_param("rec89"))) else if (cur_year == 1990L) (1000 * (g3_param("P289") * g3_param("rec90"))) else if (cur_year == 
    1991L) (1000 * (g3_param("P290") * g3_param("rec91"))) else if (cur_year == 1992L) (1000 * (g3_param("P291") * g3_param("rec92"))) else if (cur_year == 1993L) (1000 * (g3_param("P292") * g3_param("rec93"))) else if (cur_year == 1994L) (1000 * (g3_param("P293") * g3_param("rec94"))) else if (cur_year == 1995L) (1000 * (g3_param("P294") * g3_param("rec95"))) else if (cur_year == 1996L) (1000 * (g3_param("P295") * g3_param("rec96"))) else if (cur_year == 1997L) (1000 * (g3_param("P296") * g3_param("rec97"))) else if (cur_year == 
    1998L) (1000 * (g3_param("P297") * g3_param("rec98"))) else if (cur_year == 1999L) (1000 * (g3_param("P298") * g3_param("rec99"))) else if (cur_year == 2000L) (1000 * (g3_param("P299") * g3_param("rec00"))) else if (cur_year == 2001L) (1000 * (g3_param("P200") * g3_param("rec01"))) else if (cur_year == 2002L) (1000 * (g3_param("P201") * g3_param("rec02"))) else if (cur_year == 2003L) (1000 * (g3_param("P202") * g3_param("rec03"))) else if (cur_year == 2004L) (1000 * (g3_param("P203") * g3_param("rec04"))) else if (cur_year == 
    2005L) (1000 * (g3_param("P204") * g3_param("rec05"))) else if (cur_year == 2006L) (1000 * (g3_param("P205") * g3_param("rec06"))) else if (cur_year == 2007L) (1000 * (g3_param("P206") * g3_param("rec07"))) else if (cur_year == 2008L) (1000 * (g3_param("P207") * g3_param("rec08"))) else if (cur_year == 2009L) (1000 * (g3_param("P208") * g3_param("rec09"))) else if (cur_year == 2010L) (1000 * (g3_param("P209") * g3_param("rec10"))) else if (cur_year == 2011L) (1000 * (g3_param("P210") * g3_param("rec11"))) else if (cur_year == 
    2012L) (1000 * (g3_param("P211") * g3_param("rec12"))) else if (cur_year == 2013L) (1000 * (g3_param("P212") * g3_param("rec13"))) else if (cur_year == 2014L) (1000 * (g3_param("P213") * g3_param("rec14"))) else if (cur_year == 2015L) (1000 * (g3_param("P214") * g3_param("rec15"))) else if (cur_year == 2016L) (1000 * (g3_param("P215") * g3_param("rec16"))) else if (cur_year == 2017L) (1000 * (g3_param("P216") * g3_param("rec17"))) else if (cur_year == 2017L) (1000 * (g3_param("P317") * g3_param("rec17"))) else if (cur_year == 
    2017L) (1000 * (g3_param("P417") * g3_param("rec17"))) else if (cur_year == 2018L) (1000 * (g3_param("P217") * g3_param("rec18"))) else if (cur_year == 2018L) (1000 * (g3_param("P318") * g3_param("rec18"))) else if (cur_year == 2018L) (1000 * (g3_param("P418") * g3_param("rec18"))) else if (cur_year == 2019L) (1000 * (g3_param("P218") * g3_param("rec19"))) else if (cur_year == 2019L) (1000 * (g3_param("P319") * g3_param("rec19"))) else if (cur_year == 2019L) (1000 * (g3_param("P419") * g3_param("rec19"))) else if (cur_year == 
    2020L) (1000 * (g3_param("P219") * g3_param("rec20"))) else if (cur_year == 2020L) (1000 * (g3_param("P320") * g3_param("rec20"))) else if (cur_year == 2020L) (1000 * (g3_param("P420") * g3_param("rec20"))) else if (cur_year == 2021L) (1000 * (g3_param("P220") * g3_param("rec21"))) else if (cur_year == 2021L) (1000 * (g3_param("P321") * g3_param("rec21"))) else if (cur_year == 2021L) (1000 * (g3_param("P421") * g3_param("rec21"))) else if (cur_year == 2022L) (1000 * (g3_param("P221") * g3_param("rec22"))) else if (cur_year == 
    2022L) (1000 * (g3_param("P322") * g3_param("rec22"))) else if (cur_year == 2022L) (1000 * (g3_param("P422") * g3_param("rec22"))) else 0, mean_f = ~if (cur_year == 1979L) g3_param("reclm") else if (cur_year == 1980L) g3_param("reclm") else if (cur_year == 1981L) g3_param("reclm") else if (cur_year == 1982L) g3_param("reclm") else if (cur_year == 1983L) g3_param("reclm") else if (cur_year == 1984L) g3_param("reclm") else if (cur_year == 1985L) g3_param("reclm") else if (cur_year == 1986L) g3_param("reclm") else if (cur_year == 
    1987L) g3_param("reclm") else if (cur_year == 1988L) g3_param("recl") else if (cur_year == 1989L) g3_param("recl") else if (cur_year == 1990L) g3_param("recl") else if (cur_year == 1991L) g3_param("recl") else if (cur_year == 1992L) g3_param("recl") else if (cur_year == 1993L) g3_param("recl") else if (cur_year == 1994L) g3_param("recl") else if (cur_year == 1995L) g3_param("recl") else if (cur_year == 1996L) g3_param("recl") else if (cur_year == 1997L) g3_param("recl") else if (cur_year == 1998L) g3_param("recl") else if (cur_year == 
    1999L) g3_param("recl") else if (cur_year == 2000L) g3_param("recl") else if (cur_year == 2001L) g3_param("recl") else if (cur_year == 2002L) g3_param("recl") else if (cur_year == 2003L) g3_param("recl") else if (cur_year == 2004L) g3_param("recl") else if (cur_year == 2005L) g3_param("recl") else if (cur_year == 2006L) g3_param("recl") else if (cur_year == 2007L) g3_param("recl") else if (cur_year == 2008L) g3_param("recl") else if (cur_year == 2009L) g3_param("recl") else if (cur_year == 2010L) g3_param("recl") else if (cur_year == 
    2011L) g3_param("recl") else if (cur_year == 2012L) g3_param("recl") else if (cur_year == 2013L) g3_param("recl") else if (cur_year == 2014L) g3_param("recl") else if (cur_year == 1988L) g3_param("recl") else if (cur_year == 1989L) g3_param("recl") else if (cur_year == 1990L) g3_param("recl") else if (cur_year == 1991L) g3_param("recl") else if (cur_year == 1992L) g3_param("recl") else if (cur_year == 1993L) g3_param("recl") else if (cur_year == 1994L) g3_param("recl") else if (cur_year == 1995L) g3_param("recl") else if (cur_year == 
    1996L) g3_param("recl") else if (cur_year == 1997L) g3_param("recl") else if (cur_year == 1998L) g3_param("recl") else if (cur_year == 1999L) g3_param("recl") else if (cur_year == 2000L) g3_param("recl") else if (cur_year == 2001L) g3_param("recl") else if (cur_year == 2002L) g3_param("recl") else if (cur_year == 2003L) g3_param("recl") else if (cur_year == 2004L) g3_param("recl") else if (cur_year == 2005L) g3_param("recl") else if (cur_year == 2006L) g3_param("recl") else if (cur_year == 2007L) g3_param("recl") else if (cur_year == 
    2008L) g3_param("recl") else if (cur_year == 2009L) g3_param("recl") else if (cur_year == 2010L) g3_param("recl") else if (cur_year == 2011L) g3_param("recl") else if (cur_year == 2012L) g3_param("recl") else if (cur_year == 2013L) g3_param("recl") else if (cur_year == 2014L) g3_param("recl") else if (cur_year == 2015L) g3_param("recl") else if (cur_year == 2015L) g3_param("recl") else if (cur_year == 2016L) g3_param("recl") else if (cur_year == 2016L) g3_param("recl") else if (cur_year == 1989L) g3_param("recl") else if (cur_year == 
    1990L) g3_param("recl") else if (cur_year == 1991L) g3_param("recl") else if (cur_year == 1992L) g3_param("recl") else if (cur_year == 1993L) g3_param("recl") else if (cur_year == 1994L) g3_param("recl") else if (cur_year == 1995L) g3_param("recl") else if (cur_year == 1996L) g3_param("recl") else if (cur_year == 1997L) g3_param("recl") else if (cur_year == 1998L) g3_param("recl") else if (cur_year == 1999L) g3_param("recl") else if (cur_year == 2000L) g3_param("recl") else if (cur_year == 2001L) g3_param("recl") else if (cur_year == 
    2002L) g3_param("recl") else if (cur_year == 2003L) g3_param("recl") else if (cur_year == 2004L) g3_param("recl") else if (cur_year == 2005L) g3_param("recl") else if (cur_year == 2006L) g3_param("recl") else if (cur_year == 2007L) g3_param("recl") else if (cur_year == 2008L) g3_param("recl") else if (cur_year == 2009L) g3_param("recl") else if (cur_year == 2010L) g3_param("recl") else if (cur_year == 2011L) g3_param("recl") else if (cur_year == 2012L) g3_param("recl") else if (cur_year == 2013L) g3_param("recl") else if (cur_year == 
    2014L) g3_param("recl") else if (cur_year == 2015L) g3_param("recl") else if (cur_year == 2016L) g3_param("recl") else if (cur_year == 2017L) g3_param("recl") else if (cur_year == 2017L) g3_param("recl") else if (cur_year == 2017L) g3_param("recl") else if (cur_year == 2018L) g3_param("recl") else if (cur_year == 2018L) g3_param("recl") else if (cur_year == 2018L) g3_param("recl") else if (cur_year == 2019L) g3_param("recl") else if (cur_year == 2019L) g3_param("recl") else if (cur_year == 2019L) g3_param("recl") else if (cur_year == 
    2020L) g3_param("recl") else if (cur_year == 2020L) g3_param("recl") else if (cur_year == 2020L) g3_param("recl") else if (cur_year == 2021L) g3_param("recl") else if (cur_year == 2021L) g3_param("recl") else if (cur_year == 2021L) g3_param("recl") else if (cur_year == 2022L) g3_param("recl") else if (cur_year == 2022L) g3_param("recl") else if (cur_year == 2022L) g3_param("recl") else 0, stddev_f = ~if (cur_year == 1979L) g3_param("sdreclm") else if (cur_year == 1980L) g3_param("sdreclm") else if (cur_year == 
    1981L) g3_param("sdreclm") else if (cur_year == 1982L) g3_param("sdreclm") else if (cur_year == 1983L) g3_param("sdreclm") else if (cur_year == 1984L) g3_param("sdreclm") else if (cur_year == 1985L) g3_param("sdreclm") else if (cur_year == 1986L) g3_param("sdreclm") else if (cur_year == 1987L) g3_param("sdreclm") else if (cur_year == 1988L) g3_param("sdrecl") else if (cur_year == 1989L) g3_param("sdrecl") else if (cur_year == 1990L) g3_param("sdrecl") else if (cur_year == 1991L) g3_param("sdrecl") else if (cur_year == 
    1992L) g3_param("sdrecl") else if (cur_year == 1993L) g3_param("sdrecl") else if (cur_year == 1994L) g3_param("sdrecl") else if (cur_year == 1995L) g3_param("sdrecl") else if (cur_year == 1996L) g3_param("sdrecl") else if (cur_year == 1997L) g3_param("sdrecl") else if (cur_year == 1998L) g3_param("sdrecl") else if (cur_year == 1999L) g3_param("sdrecl") else if (cur_year == 2000L) g3_param("sdrecl") else if (cur_year == 2001L) g3_param("sdrecl") else if (cur_year == 2002L) g3_param("sdrecl") else if (cur_year == 
    2003L) g3_param("sdrecl") else if (cur_year == 2004L) g3_param("sdrecl") else if (cur_year == 2005L) g3_param("sdrecl") else if (cur_year == 2006L) g3_param("sdrecl") else if (cur_year == 2007L) g3_param("sdrecl") else if (cur_year == 2008L) g3_param("sdrecl") else if (cur_year == 2009L) g3_param("sdrecl") else if (cur_year == 2010L) g3_param("sdrecl") else if (cur_year == 2011L) g3_param("sdrecl") else if (cur_year == 2012L) g3_param("sdrecl") else if (cur_year == 2013L) g3_param("sdrecl") else if (cur_year == 
    2014L) g3_param("sdrecl") else if (cur_year == 1988L) g3_param("sdrecl4") else if (cur_year == 1989L) g3_param("sdrecl4") else if (cur_year == 1990L) g3_param("sdrecl4") else if (cur_year == 1991L) g3_param("sdrecl4") else if (cur_year == 1992L) g3_param("sdrecl4") else if (cur_year == 1993L) g3_param("sdrecl4") else if (cur_year == 1994L) g3_param("sdrecl4") else if (cur_year == 1995L) g3_param("sdrecl4") else if (cur_year == 1996L) g3_param("sdrecl4") else if (cur_year == 1997L) g3_param("sdrecl4") else if (cur_year == 
    1998L) g3_param("sdrecl4") else if (cur_year == 1999L) g3_param("sdrecl4") else if (cur_year == 2000L) g3_param("sdrecl4") else if (cur_year == 2001L) g3_param("sdrecl4") else if (cur_year == 2002L) g3_param("sdrecl4") else if (cur_year == 2003L) g3_param("sdrecl4") else if (cur_year == 2004L) g3_param("sdrecl4") else if (cur_year == 2005L) g3_param("sdrecl4") else if (cur_year == 2006L) g3_param("sdrecl4") else if (cur_year == 2007L) g3_param("sdrecl4") else if (cur_year == 2008L) g3_param("sdrecl4") else if (cur_year == 
    2009L) g3_param("sdrecl4") else if (cur_year == 2010L) g3_param("sdrecl4") else if (cur_year == 2011L) g3_param("sdrecl4") else if (cur_year == 2012L) g3_param("sdrecl4") else if (cur_year == 2013L) g3_param("sdrecl4") else if (cur_year == 2014L) g3_param("sdrecl4") else if (cur_year == 2015L) g3_param("sdrecl") else if (cur_year == 2015L) g3_param("sdrecl4") else if (cur_year == 2016L) g3_param("sdrecl") else if (cur_year == 2016L) g3_param("sdrecl4") else if (cur_year == 1989L) g3_param("sdrecl2") else if (cur_year == 
    1990L) g3_param("sdrecl2") else if (cur_year == 1991L) g3_param("sdrecl2") else if (cur_year == 1992L) g3_param("sdrecl2") else if (cur_year == 1993L) g3_param("sdrecl2") else if (cur_year == 1994L) g3_param("sdrecl2") else if (cur_year == 1995L) g3_param("sdrecl2") else if (cur_year == 1996L) g3_param("sdrecl2") else if (cur_year == 1997L) g3_param("sdrecl2") else if (cur_year == 1998L) g3_param("sdrecl2") else if (cur_year == 1999L) g3_param("sdrecl2") else if (cur_year == 2000L) g3_param("sdrecl2") else if (cur_year == 
    2001L) g3_param("sdrecl2") else if (cur_year == 2002L) g3_param("sdrecl2") else if (cur_year == 2003L) g3_param("sdrecl2") else if (cur_year == 2004L) g3_param("sdrecl2") else if (cur_year == 2005L) g3_param("sdrecl2") else if (cur_year == 2006L) g3_param("sdrecl2") else if (cur_year == 2007L) g3_param("sdrecl2") else if (cur_year == 2008L) g3_param("sdrecl2") else if (cur_year == 2009L) g3_param("sdrecl2") else if (cur_year == 2010L) g3_param("sdrecl2") else if (cur_year == 2011L) g3_param("sdrecl2") else if (cur_year == 
    2012L) g3_param("sdrecl2") else if (cur_year == 2013L) g3_param("sdrecl2") else if (cur_year == 2014L) g3_param("sdrecl2") else if (cur_year == 2015L) g3_param("sdrecl2") else if (cur_year == 2016L) g3_param("sdrecl2") else if (cur_year == 2017L) g3_param("sdrecl2") else if (cur_year == 2017L) g3_param("sdrecl") else if (cur_year == 2017L) g3_param("sdrecl4") else if (cur_year == 2018L) g3_param("sdrecl2") else if (cur_year == 2018L) g3_param("sdrecl") else if (cur_year == 2018L) g3_param("sdrecl4") else if (cur_year == 
    2019L) g3_param("sdrecl2") else if (cur_year == 2019L) g3_param("sdrecl") else if (cur_year == 2019L) g3_param("sdrecl4") else if (cur_year == 2020L) g3_param("sdrecl2") else if (cur_year == 2020L) g3_param("sdrecl") else if (cur_year == 2020L) g3_param("sdrecl4") else if (cur_year == 2021L) g3_param("sdrecl2") else if (cur_year == 2021L) g3_param("sdrecl") else if (cur_year == 2021L) g3_param("sdrecl4") else if (cur_year == 2022L) g3_param("sdrecl2") else if (cur_year == 2022L) g3_param("sdrecl") else if (cur_year == 
    2022L) g3_param("sdrecl4") else 0, alpha_f = ~anch_refweight * 1, beta_f = 0, run_f = quote(stop("Can't translate multi-step renewals") && area == area_1 && age == 0L)))

comment("Create fleet definition for seine")
seine <- g3_fleet("seine")
seine <- g3s_livesonareas(seine, area_names["1"])
actions_seine <- list(g3a_predate_fleet(seine, list(anch), suitabilities = list(anch = g3_suitability_exponentiall50(~tv_anch, ~tv_anch)), catchability_f = g3a_predate_catchability_numberfleet(g3_timeareadata("Data_fleet_seine_data", Data_fleet_seine_data, "number", areas = area_names))))

comment("Create fleet definition for ECO")
ECO <- g3_fleet("ECO")
ECO <- g3s_livesonareas(ECO, area_names["1"])
actions_ECO <- list(g3a_predate_fleet(ECO, list(anch), suitabilities = list(anch = g3_suitability_exponentiall50(~g3_param("constanteco"), ~g3_param("l50eco"))), catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata("Data_fleet_ECO_data", Data_fleet_ECO_data, "number", areas = area_names))))

comment("Create fleet definition for PEL")
PEL <- g3_fleet("PEL")
PEL <- g3s_livesonareas(PEL, area_names["1"])
actions_PEL <- list(g3a_predate_fleet(PEL, list(anch), suitabilities = list(anch = g3_suitability_exponentiall50(~g3_param("constantpel"), ~g3_param("l50pel"))), catchability_f = g3a_predate_catchability_totalfleet(g3_timeareadata("Data_fleet_PEL_data", Data_fleet_PEL_data, "number", areas = area_names))))

actions_lik_bounds <- list(NULL)

actions_lik_understocking <- list(g3l_understocking(weight = as.double(1e+08), nll_breakdown = TRUE, list(anch)))

actions_lik_ldist.seine <- list(g3l_catchdistribution("ldist.seine", Data_catchdistribution_ldist_seine_sumofsquares, fleets = list(seine), stocks = list(anch), function_f = g3l_distribution_sumofsquares(), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_ldist.pelago.noage <- list(g3l_catchdistribution("ldist.pelago.noage", Data_catchdistribution_ldist_pelago_noage_sumofsquares, fleets = list(PEL), stocks = list(anch), function_f = g3l_distribution_sumofsquares(), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_ldist.ecocadiz.noage <- list(g3l_catchdistribution("ldist.ecocadiz.noage", Data_catchdistribution_ldist_ecocadiz_noage_sumofsquares, fleets = list(ECO), stocks = list(anch), function_f = g3l_distribution_sumofsquares(), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_ldist.alkseine <- list(g3l_catchdistribution("ldist.alkseine", Data_catchdistribution_ldist_alkseine_sumofsquares, fleets = list(seine), stocks = list(anch), function_f = g3l_distribution_sumofsquares(), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_aldist.pelago <- list(g3l_catchdistribution("aldist.pelago", Data_catchdistribution_aldist_pelago_sumofsquares, fleets = list(PEL), stocks = list(anch), function_f = g3l_distribution_sumofsquares(), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_aldist.ecocadiz <- list(g3l_catchdistribution("aldist.ecocadiz", Data_catchdistribution_aldist_ecocadiz_sumofsquares, fleets = list(ECO), stocks = list(anch), function_f = g3l_distribution_sumofsquares(), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_pelagonumber.survey <- list(g3l_abundancedistribution("pelagonumber.survey", Data_surveyindices_pelagonumber_survey_lengths, stocks = list(anch), function_f = g3l_distribution_surveyindices_log(beta = 1L), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions_lik_ecocadiz.survey <- list(g3l_abundancedistribution("ecocadiz.survey", Data_surveyindices_ecocadiz_survey_lengths, stocks = list(anch), function_f = g3l_distribution_surveyindices_log(beta = 1L), area_group = area_names, report = TRUE, nll_breakdown = TRUE, weight = 1))

actions <- c(actions_time, actions_anch, actions_seine, actions_ECO, actions_PEL, actions_lik_bounds, actions_lik_understocking, actions_lik_ldist.seine, actions_lik_ldist.pelago.noage, actions_lik_ldist.ecocadiz.noage, actions_lik_ldist.alkseine, actions_lik_aldist.pelago, actions_lik_aldist.ecocadiz, actions_lik_pelagonumber.survey, actions_lik_ecocadiz.survey)



model_fn <- g3_to_r(actions)
params.in <- attr(model_fn, "parameter_template")
params.in[["Linf"]] <- "19"
params.in[["kappa"]] <- "0.89"
params.in[["alpha"]] <- "3.128958e-06"
params.in[["beta"]] <- "3.277667619"
params.in[["bbeta"]] <- "1"
params.in[["anchalpha88"]] <- "1"
params.in[["anchalpha01"]] <- "1"
params.in[["anchL5088"]] <- "8"
params.in[["anchL5001"]] <- "10"
params.in[["constant"]] <- "random"
params.in[["l50"]] <- "random"
params.in[["constanteco"]] <- "random"
params.in[["l50eco"]] <- "10"
params.in[["constantpel"]] <- "random"
params.in[["l50pel"]] <- "10"
params.in[["constantecorec"]] <- "random"
params.in[["l50ecorec"]] <- "10"
params.in[["constantarsa"]] <- "random"
params.in[["l50arsa"]] <- "random"
params.in[["recl"]] <- "8"
params.in[["sdrecl"]] <- "1"
params.in[["reclm"]] <- "5"
params.in[["sdreclm"]] <- "1"
params.in[["sdrecl4"]] <- "1"
params.in[["rec79"]] <- "100"
params.in[["rec80"]] <- "100"
params.in[["rec81"]] <- "100"
params.in[["rec82"]] <- "100"
params.in[["rec83"]] <- "100"
params.in[["rec84"]] <- "100"
params.in[["rec85"]] <- "100"
params.in[["rec86"]] <- "100"
params.in[["rec87"]] <- "100"
params.in[["rec88"]] <- "100"
params.in[["rec89"]] <- "100"
params.in[["rec90"]] <- "100"
params.in[["rec91"]] <- "100"
params.in[["rec92"]] <- "100"
params.in[["rec93"]] <- "100"
params.in[["rec94"]] <- "100"
params.in[["rec95"]] <- "100"
params.in[["rec96"]] <- "100"
params.in[["rec97"]] <- "100"
params.in[["rec98"]] <- "100"
params.in[["rec99"]] <- "100"
params.in[["rec00"]] <- "100"
params.in[["rec01"]] <- "100"
params.in[["rec02"]] <- "100"
params.in[["rec03"]] <- "100"
params.in[["rec04"]] <- "100"
params.in[["rec05"]] <- "100"
params.in[["rec06"]] <- "100"
params.in[["rec07"]] <- "100"
params.in[["rec08"]] <- "100"
params.in[["rec09"]] <- "100"
params.in[["rec10"]] <- "100"
params.in[["rec11"]] <- "100"
params.in[["rec12"]] <- "100"
params.in[["rec13"]] <- "100"
params.in[["rec14"]] <- "100"
params.in[["rec15"]] <- "100"
params.in[["rec16"]] <- "100"
params.in[["rec17"]] <- "100"
params.in[["rec18"]] <- "100"
params.in[["rec19"]] <- "100"
params.in[["rec20"]] <- "100"
params.in[["rec21"]] <- "100"
params.in[["rec22"]] <- "100"
params.in[["init0"]] <- "1"
params.in[["init1"]] <- "1"
params.in[["init2"]] <- "1"
params.in[["init3"]] <- "1"
params.in[["P388"]] <- "0.5"
params.in[["P389"]] <- "0.5"
params.in[["P390"]] <- "0.5"
params.in[["P391"]] <- "0.5"
params.in[["P392"]] <- "0.5"
params.in[["P393"]] <- "0.5"
params.in[["P394"]] <- "0.5"
params.in[["P395"]] <- "0.5"
params.in[["P396"]] <- "0.5"
params.in[["P397"]] <- "0.5"
params.in[["P398"]] <- "0.5"
params.in[["P399"]] <- "0.5"
params.in[["P300"]] <- "0.5"
params.in[["P301"]] <- "0.5"
params.in[["P302"]] <- "0.5"
params.in[["P303"]] <- "0.5"
params.in[["P304"]] <- "0.5"
params.in[["P305"]] <- "0.5"
params.in[["P306"]] <- "0.5"
params.in[["P307"]] <- "0.5"
params.in[["P308"]] <- "0.5"
params.in[["P309"]] <- "0.5"
params.in[["P310"]] <- "0.5"
params.in[["P311"]] <- "0.5"
params.in[["P312"]] <- "0.5"
params.in[["P313"]] <- "0.5"
params.in[["P314"]] <- "0.5"
params.in[["P488"]] <- "0.5"
params.in[["P489"]] <- "0.5"
params.in[["P490"]] <- "0.5"
params.in[["P491"]] <- "0.5"
params.in[["P492"]] <- "0.5"
params.in[["P493"]] <- "0.5"
params.in[["P494"]] <- "0.5"
params.in[["P495"]] <- "0.5"
params.in[["P496"]] <- "0.5"
params.in[["P497"]] <- "0.5"
params.in[["P498"]] <- "0.5"
params.in[["P499"]] <- "0.5"
params.in[["P400"]] <- "0.5"
params.in[["P401"]] <- "0.5"
params.in[["P402"]] <- "0.5"
params.in[["P403"]] <- "0.5"
params.in[["P404"]] <- "0.5"
params.in[["P405"]] <- "0.5"
params.in[["P406"]] <- "0.5"
params.in[["P407"]] <- "0.5"
params.in[["P408"]] <- "0.5"
params.in[["P409"]] <- "0.5"
params.in[["P410"]] <- "0.5"
params.in[["P411"]] <- "0.5"
params.in[["P412"]] <- "0.5"
params.in[["P413"]] <- "0.5"
params.in[["P414"]] <- "0.5"
params.in[["P315"]] <- "0.5"
params.in[["P415"]] <- "0.5"
params.in[["P316"]] <- "0.5"
params.in[["P416"]] <- "0.5"
params.in[["P317"]] <- "0.5"
params.in[["P417"]] <- "0.5"
params.in[["P318"]] <- "0.5"
params.in[["P418"]] <- "0.5"
params.in[["P319"]] <- "0.5"
params.in[["P419"]] <- "0.5"
params.in[["P320"]] <- "0.5"
params.in[["P420"]] <- "0.5"
params.in[["P321"]] <- "0.5"
params.in[["P421"]] <- "0.5"
params.in[["P322"]] <- "0.5"
params.in[["P422"]] <- "0.5"
params.in[["P288"]] <- "0.5"
params.in[["sdrecl2"]] <- "1"
params.in[["P289"]] <- "0.5"
params.in[["P290"]] <- "0.5"
params.in[["P291"]] <- "0.5"
params.in[["P292"]] <- "0.5"
params.in[["P293"]] <- "0.5"
params.in[["P294"]] <- "0.5"
params.in[["P295"]] <- "0.5"
params.in[["P296"]] <- "0.5"
params.in[["P297"]] <- "0.5"
params.in[["P298"]] <- "0.5"
params.in[["P299"]] <- "0.5"
params.in[["P200"]] <- "0.5"
params.in[["P201"]] <- "0.5"
params.in[["P202"]] <- "0.5"
params.in[["P203"]] <- "0.5"
params.in[["P204"]] <- "0.5"
params.in[["P205"]] <- "0.5"
params.in[["P206"]] <- "0.5"
params.in[["P207"]] <- "0.5"
params.in[["P208"]] <- "0.5"
params.in[["P209"]] <- "0.5"
params.in[["P210"]] <- "0.5"
params.in[["P211"]] <- "0.5"
params.in[["P212"]] <- "0.5"
params.in[["P213"]] <- "0.5"
params.in[["P214"]] <- "0.5"
params.in[["P215"]] <- "0.5"
params.in[["P216"]] <- "0.5"
params.in[["P217"]] <- "0.5"
params.in[["P218"]] <- "0.5"
params.in[["P219"]] <- "0.5"
params.in[["P220"]] <- "0.5"
params.in[["P221"]] <- "0.5"

result <- model_fn(params.in)


model_cpp <- g3_to_tmb(actions)
params.in <- attr(model_cpp, "parameter_template")
params.in["Linf", c("value", "lower", "upper", "optimise")] <- c("19", 14, 30, TRUE)
params.in["kappa", c("value", "lower", "upper", "optimise")] <- c("0.89", 0.05, 1.2, TRUE)
params.in["alpha", c("value", "lower", "upper", "optimise")] <- c("3.128958e-06", 1e-07, 10, FALSE)
params.in["beta", c("value", "lower", "upper", "optimise")] <- c("3.277667619", 1e-04, 10, FALSE)
params.in["bbeta", c("value", "lower", "upper", "optimise")] <- c("1", 0.01, 5000, TRUE)
params.in["anchalpha88", c("value", "lower", "upper", "optimise")] <- c("1", 0.1, 1.5, TRUE)
params.in["anchalpha01", c("value", "lower", "upper", "optimise")] <- c("1", 0.1, 1.5, TRUE)
params.in["anchL5088", c("value", "lower", "upper", "optimise")] <- c("8", 1, 20, TRUE)
params.in["anchL5001", c("value", "lower", "upper", "optimise")] <- c("10", 6, 15, TRUE)
params.in["constant", c("value", "lower", "upper", "optimise")] <- c("random", 0.2, 1.5, TRUE)
params.in["l50", c("value", "lower", "upper", "optimise")] <- c("random", 2, 20, TRUE)
params.in["constanteco", c("value", "lower", "upper", "optimise")] <- c("random", 0.2, 2.2, TRUE)
params.in["l50eco", c("value", "lower", "upper", "optimise")] <- c("10", 2, 20, TRUE)
params.in["constantpel", c("value", "lower", "upper", "optimise")] <- c("random", 0.2, 1.5, TRUE)
params.in["l50pel", c("value", "lower", "upper", "optimise")] <- c("10", 2, 20, TRUE)
params.in["constantecorec", c("value", "lower", "upper", "optimise")] <- c("random", 0.2, 3, TRUE)
params.in["l50ecorec", c("value", "lower", "upper", "optimise")] <- c("10", 2, 20, TRUE)
params.in["constantarsa", c("value", "lower", "upper", "optimise")] <- c("random", 0.01, 1.5, TRUE)
params.in["l50arsa", c("value", "lower", "upper", "optimise")] <- c("random", 2, 30, TRUE)
params.in["recl", c("value", "lower", "upper", "optimise")] <- c("8", 3, 15, TRUE)
params.in["sdrecl", c("value", "lower", "upper", "optimise")] <- c("1", 0.5, 4, TRUE)
params.in["reclm", c("value", "lower", "upper", "optimise")] <- c("5", 3, 15, TRUE)
params.in["sdreclm", c("value", "lower", "upper", "optimise")] <- c("1", 0.5, 3, TRUE)
params.in["sdrecl4", c("value", "lower", "upper", "optimise")] <- c("1", 0.5, 4, TRUE)
params.in["rec79", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec80", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec81", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec82", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec83", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec84", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec85", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec86", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec87", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec88", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec89", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec90", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec91", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec92", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec93", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec94", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec95", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec96", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec97", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec98", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec99", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec00", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec01", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec02", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec03", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec04", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec05", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec06", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec07", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec08", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec09", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec10", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec11", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec12", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec13", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec14", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec15", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec16", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec17", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec18", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec19", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec20", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec21", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["rec22", c("value", "lower", "upper", "optimise")] <- c("100", 1e-06, 1000, TRUE)
params.in["init0", c("value", "lower", "upper", "optimise")] <- c("1", 1e-06, 2, TRUE)
params.in["init1", c("value", "lower", "upper", "optimise")] <- c("1", 1e-06, 3, TRUE)
params.in["init2", c("value", "lower", "upper", "optimise")] <- c("1", 1e-06, 1.5, TRUE)
params.in["init3", c("value", "lower", "upper", "optimise")] <- c("1", 1e-07, 1.1, TRUE)
params.in["P388", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P389", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P390", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P391", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P392", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P393", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P394", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P395", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P396", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P397", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P398", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P399", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P300", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P301", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P302", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P303", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P304", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P305", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P306", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P307", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P308", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P309", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P310", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P311", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P312", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P313", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P314", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P488", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P489", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P490", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P491", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P492", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P493", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P494", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P495", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P496", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P497", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P498", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P499", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P400", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P401", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P402", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P403", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P404", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P405", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P406", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P407", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P408", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P409", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P410", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P411", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P412", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P413", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P414", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P315", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P415", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P316", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P416", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P317", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P417", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P318", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P418", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P319", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P419", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P320", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P420", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P321", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P421", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P322", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P422", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P288", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["sdrecl2", c("value", "lower", "upper", "optimise")] <- c("1", 0.5, 4, TRUE)
params.in["P289", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P290", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P291", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P292", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P293", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P294", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P295", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P296", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P297", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P298", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P299", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P200", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P201", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P202", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P203", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P204", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P205", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P206", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P207", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P208", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P209", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P210", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P211", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P212", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P213", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P214", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P215", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P216", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P217", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P218", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P219", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P220", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)
params.in["P221", c("value", "lower", "upper", "optimise")] <- c("0.5", 0, 1, TRUE)

obj <- g3_tmb_adfun(model_cpp, params.in)
