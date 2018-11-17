ui <- fluidPage(
  titlePanel( get_session_info()$current_model),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

  # Sidebar panel for inputs ----
  sidebarPanel(
   tabsetPanel( tabPanel(title=' input.global_parameters ',sliderInput(inputId = 'input.global_parameters.age0',
                 label = 'input.global_parameters.age0',
                 min = 20,
                 max = 80,
                 value = 40) 
,sliderInput(inputId = 'input.global_parameters.time_horizon',
                 label = 'input.global_parameters.time_horizon',
                 min = 10,
                 max = 40,
                 value = 20) 
,sliderInput(inputId = 'input.global_parameters.discount_cost',
                 label = 'input.global_parameters.discount_cost',
                 min = 0.015,
                 max = 0.06,
                 value = 0.03) 
,sliderInput(inputId = 'input.global_parameters.discount_qaly',
                 label = 'input.global_parameters.discount_qaly',
                 min = 0.015,
                 max = 0.06,
                 value = 0.03) 
 ), tabPanel(title=' input.agent ',sliderInput(inputId = 'input.agent.p_female',
                 label = 'input.agent.p_female',
                 min = 0.25,
                 max = 1,
                 value = 0.5) 
,textInput(inputId = 'input.agent.height_0_betas',
                 label = 'input.agent.height_0_betas',
                 value = '1.8266,-0.1309,-0.0012,2.31e-06,-2e-04') 
,sliderInput(inputId = 'input.agent.height_0_sd',
                 label = 'input.agent.height_0_sd',
                 min = 0.0337,
                 max = 0.1348,
                 value = 0.0674) 
,textInput(inputId = 'input.agent.weight_0_betas',
                 label = 'input.agent.weight_0_betas',
                 value = '50,-5,0.1,0,0,1,0.01') 
,sliderInput(inputId = 'input.agent.weight_0_sd',
                 label = 'input.agent.weight_0_sd',
                 min = 2.5,
                 max = 10,
                 value = 5) 
,sliderInput(inputId = 'input.agent.height_weight_rho',
                 label = 'input.agent.height_weight_rho',
                 min = 0,
                 max = 0,
                 value = 0) 
,textInput(inputId = 'input.agent.p_prevalence_age',
                 label = 'input.agent.p_prevalence_age',
                 value = '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.0261,0.0255,0.0255,0.0258,0.0269,0.0268,0.0266,0.0263,0.0266,0.028,0.0299,0.0307,0.0309,0.0302,0.0305,0.03,0.0292,0.0288,0.0281,0.0272,0.0268,0.0256,0.0244,0.0234,0.0228,0.0222,0.0217,0.0215,0.0213,0.0182,0.0167,0.0161,0.0153,0.014,0.0133,0.0123,0.0116,0.0109,0.0102,0.0098,0.0092,0.0085,0.0082,0.0077,0.0073,0.0066,0.0058,0.0052,0.0046,0.004,0.0034,0.0029,0.0023,0.0019,0.0015,0.0011,7e-04,5e-04,3e-04,2e-04,4e-04,0,0,0,0,0,0,0,0,0,0') 
,textInput(inputId = 'input.agent.p_incidence_age',
                 label = 'input.agent.p_incidence_age',
                 value = '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0.9006,0.018,0.0147,0.0121,0.0099,0.0081,0.0066,0.0054,0.0044,0.0036,0.003,0.0024,0.002,0.0016,0.0013,0.0011,9e-04,7e-04,6e-04,5e-04,4e-04,3e-04,3e-04,2e-04,2e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,0,0,0,0,0,0,0,0,9.0144e-06,7.3804e-06,6.0426e-06,4.9472e-06,4.0505e-06,3.3162e-06,2.7151e-06,2.2229e-06,1.82e-06,1.4901e-06,1.22e-06,9.9883e-07,8.1777e-07,6.6953e-07,5.4817e-07,4.488e-07,3.6745e-07,3.0084e-07,2.4631e-07,2.0166e-07,1.6511e-07,1.3518e-07,0,0,0,0,0,0,0,0,0,0') 
,textInput(inputId = 'input.agent.p_bgd_by_sex',
                 label = 'input.agent.p_bgd_by_sex',
                 value = '0.0052,3e-04,2e-04,2e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,2e-04,3e-04,4e-04,5e-04,6e-04,7e-04,7e-04,8e-04,8e-04,8e-04,7e-04,7e-04,7e-04,7e-04,7e-04,7e-04,7e-04,8e-04,8e-04,9e-04,9e-04,0.001,0.001,0.0011,0.0012,0.0012,0.0013,0.0014,0.0015,0.0016,0.0018,0.0019,0.0021,0.0023,0.0025,0.0028,0.003,0.0033,0.0036,0.004,0.0044,0.0048,0.0053,0.0059,0.0064,0.0071,0.0078,0.0086,0.0094,0.0104,0.0114,0.0126,0.0139,0.0153,0.0168,0.0185,0.0204,0.0225,0.0248,0.0273,0.03,0.0331,0.0365,0.0402,0.0443,0.0488,0.0538,0.0594,0.0654,0.0722,0.0796,0.0878,0.0968,0.1068,0.1178,0.13,0.1434,0.1579,0.1733,0.1893,0.206,0.2184,0.2354,0.2529,0.2709,0.2893,0.308,0.3269,0.3458,0.3646,0.3832,0.4015,0.4194,0.4367,0.4535,0.4696,1,0.0045,2e-04,2e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,1e-04,2e-04,2e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,3e-04,4e-04,4e-04,4e-04,5e-04,5e-04,6e-04,6e-04,7e-04,7e-04,8e-04,8e-04,9e-04,0.001,0.0011,0.0012,0.0013,0.0014,0.0015,0.0017,0.0018,0.002,0.0022,0.0024,0.0026,0.0028,0.0031,0.0034,0.0037,0.004,0.0044,0.0048,0.0053,0.0059,0.0064,0.0071,0.0078,0.0086,0.0095,0.0105,0.0116,0.0128,0.0142,0.0157,0.0174,0.0193,0.0215,0.0238,0.0265,0.0295,0.0328,0.0365,0.0407,0.0454,0.0507,0.0567,0.0634,0.0709,0.0794,0.089,0.0998,0.112,0.1254,0.1399,0.1554,0.1719,0.1885,0.2065,0.2255,0.2453,0.2657,0.2867,0.3081,0.3297,0.3513,0.3728,0.394,0.4146,0.4346,0.4539,0.4722,1') 
,textInput(inputId = 'input.agent.l_inc_betas',
                 label = 'input.agent.l_inc_betas',
                 value = '-3.55,0.01,0') 
,textInput(inputId = 'input.agent.ln_h_bgd_betas',
                 label = 'input.agent.ln_h_bgd_betas',
                 value = '0,-0.025,0,0,0,0,0,0,0') 
 ), tabPanel(title=' input.smoking ',textInput(inputId = 'input.smoking.logit_p_current_smoker_0_betas',
                 label = 'input.smoking.logit_p_current_smoker_0_betas',
                 value = '-0.2,-1,-0.02,0,0,0,-0.02') 
,textInput(inputId = 'input.smoking.logit_p_never_smoker_con_not_current_0_betas',
                 label = 'input.smoking.logit_p_never_smoker_con_not_current_0_betas',
                 value = '3.7,0,-0.06,0,0,0,-0.02') 
,sliderInput(inputId = 'input.smoking.minimum_smoking_prevalence',
                 label = 'input.smoking.minimum_smoking_prevalence',
                 min = 0.05,
                 max = 0.2,
                 value = 0.1) 
,textInput(inputId = 'input.smoking.mortality_factor_current',
                 label = 'input.smoking.mortality_factor_current',
                 value = '1,1,1.94,1.86,1.66') 
,textInput(inputId = 'input.smoking.mortality_factor_former',
                 label = 'input.smoking.mortality_factor_former',
                 value = '1,1,1.54,1.36,1.27') 
,textInput(inputId = 'input.smoking.pack_years_0_betas',
                 label = 'input.smoking.pack_years_0_betas',
                 value = '22,-4,0,-0.6,10') 
,sliderInput(inputId = 'input.smoking.pack_years_0_sd',
                 label = 'input.smoking.pack_years_0_sd',
                 min = 2.5,
                 max = 10,
                 value = 5) 
,textInput(inputId = 'input.smoking.ln_h_inc_betas',
                 label = 'input.smoking.ln_h_inc_betas',
                 value = '-4,-0.15,-0.02,0,-0.01') 
,textInput(inputId = 'input.smoking.ln_h_ces_betas',
                 label = 'input.smoking.ln_h_ces_betas',
                 value = '-3.7,0,0.02,0,-0.01') 
 ), tabPanel(title=' input.COPD ',textInput(inputId = 'input.COPD.logit_p_COPD_betas_by_sex',
                 label = 'input.COPD.logit_p_COPD_betas_by_sex',
                 value = '-4.5222,0.0331,0,0.025,0,0,0,-4.0749,0.0274,0,0.0304,0,0,0') 
,textInput(inputId = 'input.COPD.ln_h_COPD_betas_by_sex',
                 label = 'input.COPD.ln_h_COPD_betas_by_sex',
                 value = '-7.8656,0.0326,0,0.0308,0,0,0,-7.7588,0.0279,0,0.0418,0,0,0') 
 ), tabPanel(title=' input.lung_function ',textInput(inputId = 'input.lung_function.fev1_0_prev_betas_by_sex',
                 label = 'input.lung_function.fev1_0_prev_betas_by_sex',
                 value = '1.5462,-0.0313,1.0126,-0.0069,0,0,1.3298,-0.0313,1.0126,-0.0069,0,0') 
,textInput(inputId = 'input.lung_function.fev1_0_prev_sd_by_sex',
                 label = 'input.lung_function.fev1_0_prev_sd_by_sex',
                 value = '0.6148,0.4242') 
,textInput(inputId = 'input.lung_function.fev1_0_inc_betas_by_sex',
                 label = 'input.lung_function.fev1_0_inc_betas_by_sex',
                 value = '0.8064,-0.0322,1.27,-0.0043,0,0,1.1487,-0.0275,0.97,-0.0046,0,0') 
,textInput(inputId = 'input.lung_function.fev1_0_inc_sd_by_sex',
                 label = 'input.lung_function.fev1_0_inc_sd_by_sex',
                 value = '0.54,0.36') 
,textInput(inputId = 'input.lung_function.fev1_0_ZafarCMAJ_by_sex',
                 label = 'input.lung_function.fev1_0_ZafarCMAJ_by_sex',
                 value = '1.91,-0.0051,-5e-04,-1.8725,1.9513,-0.0922,-0.0083,0,1.4275,-0.0051,-5e-04,-1.8725,1.9513,-0.0922,-0.0083,0') 
,textInput(inputId = 'input.lung_function.pred_fev1_betas_by_sex',
                 label = 'input.lung_function.pred_fev1_betas_by_sex',
                 value = '0.5536,-0.013,-2e-04,1.4098,0.4333,-0.0036,-2e-04,1.1496') 
,textInput(inputId = 'input.lung_function.fev1_betas_by_sex',
                 label = 'input.lung_function.fev1_betas_by_sex',
                 value = '-0.1619,0.0023,1e-04,0.0584,0.0181,-0.0307,-9e-04,-0.0015,-0.1543,0.0023,1e-04,0.0584,0.0181,-0.0307,-9e-04,-0.0015') 
,textInput(inputId = 'input.lung_function.dfev1_sigmas',
                 label = 'input.lung_function.dfev1_sigmas',
                 value = '0.3172,0.0275') 
,sliderInput(inputId = 'input.lung_function.dfev1_re_rho',
                 label = 'input.lung_function.dfev1_re_rho',
                 min = 0.0498,
                 max = 0.1992,
                 value = 0.0996) 
 ), tabPanel(title=' input.exacerbation ',textInput(inputId = 'input.exacerbation.ln_rate_betas',
                 label = 'input.exacerbation.ln_rate_betas',
                 value = '-2.1,0,0.0041,0,0,1.1,1.9,2.4') 
,sliderInput(inputId = 'input.exacerbation.ln_rate_intercept_sd',
                 label = 'input.exacerbation.ln_rate_intercept_sd',
                 min = 0.3708,
                 max = 1.4832,
                 value = 0.7416) 
,textInput(inputId = 'input.exacerbation.logit_severity_betas',
                 label = 'input.exacerbation.logit_severity_betas',
                 value = '1.091,1.902,5.208,-0.764,-0.007,-0.003,0.348,-0.001,0.018') 
,sliderInput(inputId = 'input.exacerbation.logit_severity_intercept_sd',
                 label = 'input.exacerbation.logit_severity_intercept_sd',
                 min = 0.72,
                 max = 2.88,
                 value = 1.44) 
,sliderInput(inputId = 'input.exacerbation.rate_severity_intercept_rho',
                 label = 'input.exacerbation.rate_severity_intercept_rho',
                 min = 0,
                 max = 0,
                 value = 0) 
,textInput(inputId = 'input.exacerbation.exac_end_rate',
                 label = 'input.exacerbation.exac_end_rate',
                 value = '73,73,73,73') 
,textInput(inputId = 'input.exacerbation.logit_p_death_by_sex',
                 label = 'input.exacerbation.logit_p_death_by_sex',
                 value = '-13,0.0488,0,0,7.4,8,0,-13,0.0488,0,0,7.4,8,0') 
 ), tabPanel(title=' input.outpatient ',sliderInput(inputId = 'input.outpatient.rate_doctor_visit',
                 label = 'input.outpatient.rate_doctor_visit',
                 min = 0.05,
                 max = 0.2,
                 value = 0.1) 
,sliderInput(inputId = 'input.outpatient.p_specialist',
                 label = 'input.outpatient.p_specialist',
                 min = 0.05,
                 max = 0.2,
                 value = 0.1) 
 ), tabPanel(title=' input.medication ',textInput(inputId = 'input.medication.ln_h_start_betas_by_class',
                 label = 'input.medication.ln_h_start_betas_by_class',
                 value = '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0') 
,textInput(inputId = 'input.medication.ln_h_stop_betas_by_class',
                 label = 'input.medication.ln_h_stop_betas_by_class',
                 value = '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0') 
,textInput(inputId = 'input.medication.ln_rr_exac_by_class',
                 label = 'input.medication.ln_rr_exac_by_class',
                 value = '0,0,0,0,0') 
 ), tabPanel(title=' input.comorbidity ',textInput(inputId = 'input.comorbidity.logit_p_mi_betas_by_sex',
                 label = 'input.comorbidity.logit_p_mi_betas_by_sex',
                 value = '-3000,0.001,0,0.01,0.001,0,0,0.05,-3000,0.001,0,0.01,0.001,0,0,0.05') 
,textInput(inputId = 'input.comorbidity.ln_h_mi_betas_by_sex',
                 label = 'input.comorbidity.ln_h_mi_betas_by_sex',
                 value = '-10000.748,0.1133,-4e-04,0.01,0.7095,0,0.01,0.05,0.5,0,-30000,0.001,0,0.01,0.6187,0,0.01,0.05,0,0.01') 
,sliderInput(inputId = 'input.comorbidity.p_mi_death',
                 label = 'input.comorbidity.p_mi_death',
                 min = 0.025,
                 max = 0.1,
                 value = 0.05) 
,textInput(inputId = 'input.comorbidity.logit_p_stroke_betas_by_sex',
                 label = 'input.comorbidity.logit_p_stroke_betas_by_sex',
                 value = '-3000,0.001,0,0.01,0.001,0,0,0.05,0,0,-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0') 
,textInput(inputId = 'input.comorbidity.ln_h_stroke_betas_by_sex',
                 label = 'input.comorbidity.ln_h_stroke_betas_by_sex',
                 value = '-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0.01,0,0,-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0.01,0,0') 
,sliderInput(inputId = 'input.comorbidity.p_stroke_death',
                 label = 'input.comorbidity.p_stroke_death',
                 min = 0.09,
                 max = 0.36,
                 value = 0.18) 
,textInput(inputId = 'input.comorbidity.logit_p_hf_betas_by_sex',
                 label = 'input.comorbidity.logit_p_hf_betas_by_sex',
                 value = '-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0.01,0,0,-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0.01,0,0') 
,textInput(inputId = 'input.comorbidity.ln_h_hf_betas_by_sex',
                 label = 'input.comorbidity.ln_h_hf_betas_by_sex',
                 value = '-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0.01,0,0,-3000,0.001,0,0.01,0.001,0,0.01,0.05,0,0.01,0,0') 
 ), tabPanel(title=' input.cost ',textInput(inputId = 'input.cost.bg_cost_by_stage',
                 label = 'input.cost.bg_cost_by_stage',
                 value = '0,615,1831,2619,3021') 
,textInput(inputId = 'input.cost.exac_dcost',
                 label = 'input.cost.exac_dcost',
                 value = '29,726,9212,20170') 
 ), tabPanel(title=' input.utility ',textInput(inputId = 'input.utility.bg_util_by_stage',
                 label = 'input.utility.bg_util_by_stage',
                 value = '0.85,0.81,0.72,0.68,0.58') 
,textInput(inputId = 'input.utility.exac_dutil',
                 label = 'input.utility.exac_dutil',
                 value = '-0.0225,-0.0225,-0.0728,-0.0728,-0.0155,-0.0155,-0.0683,-0.0683,-0.0488,-0.0488,-0.0655,-0.0655,-0.0488,-0.0488,-0.0655,-0.0655') 
 ), tabPanel(title=' input.manual ',sliderInput(inputId = 'input.manual.MORT_COEFF',
                 label = 'input.manual.MORT_COEFF',
                 min = 0.5,
                 max = 2,
                 value = 1) 
, tabPanel(title=' input.manual.smoking ',sliderInput(inputId = 'input.manual.smoking.intercept_k',
                 label = 'input.manual.smoking.intercept_k',
                 min = 0.5,
                 max = 2,
                 value = 1) 
 ),textInput(inputId = 'input.manual.explicit_mortality_by_age_sex',
                 label = 'input.manual.explicit_mortality_by_age_sex',
                 value = '0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,2e-04,3e-04,1e-04,0,1e-04,1e-04,0,1e-04,0,0,0,0,-1e-04,0,-2e-04,-2e-04,-3e-04,-4e-04,-5e-04,6e-04,0.0017,0.0018,0.002,0.0021,0.0024,0.0026,0.0029,0.0031,0.0033,0.0029,0.0024,0.0027,0.003,0.0032,0.0036,0.0041,0.0045,0.005,0.0056,0.0051,0.0041,0.0046,0.0055,0.0071,0.0086,0.0096,0.0112,0.0131,0.0166,0.0198,0.0229,0.0276,0.0305,0.0375,0.0395,0.0469,0.0518,0.0588,0.0652,0.0724,0.0838,0.0878,0.0804,0.09,0.0747,0.0774,0.1497,0.1672,0.1257,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,-8e-04,2e-04,1e-04,1e-04,1e-04,0,1e-04,1e-04,1e-04,1e-04,1e-04,-1e-04,1e-04,0,0,-1e-04,-1e-04,-1e-04,-1e-04,-2e-04,2e-04,9e-04,9e-04,0.001,0.0012,0.0013,0.0014,0.0017,0.0018,0.0019,0.0019,0.0014,0.0015,0.0017,0.0019,0.002,0.0026,0.0025,0.003,0.0031,0.0031,0.0025,0.0026,0.003,0.004,0.0043,0.0052,0.0069,0.0076,0.0093,0.0113,0.0128,0.0175,0.0188,0.0219,0.0265,0.0302,0.033,0.0374,0.0433,0.0531,0.0624,0.0592,0.0737,0.0753,0.1047,0.1086,0.1568,0.114,0.1915,0,0') 
 ))), mainPanel( tabPanel(title=' output ',, ))))


                 server <- function(input, output)
                 {
                    invoke<-reactive(
                    {
                      for(nm in names(input))
                      {
                        p_input<-eval(parse(text=paste('thisSession$default_',gsub('[.]', '$', nm),sep='')))
                        s_input<-eval(parse(text=paste('input$',nm,sep='')))
                        value<-''
                        if(canbe_prism_input(p_input))
                        {
                          if(substring(p_input$type,1,7)=='numeric')
                            value<-as.numeric(unlist(strsplit(s_input )))
                          else
                            value<-s_input
                        }
                        else
                        {
                          if(is.numeric(s_input))
                            value<-s_input
                          else
                          {
                            value<-as.numeric(unlist(strsplit(s_input,',')))
                            if(sum(is.na(value))>0) value<-s_input
                          }
                        }
                        eval(parse(text=paste('thisSession$',gsub('[.]', '$', nm),'<-value',sep='')))
                      }
                      model_run()
                  })
 output$output.n_agents<-renderText({invoke(); 'salam:'; show_output(thisSession$output$n_agents)});output$output.cumul_time<-renderText({invoke(); 'salam:'; show_output(thisSession$output$cumul_time)});output$output.n_deaths<-renderText({invoke(); 'salam:'; show_output(thisSession$output$n_deaths)});output$output.n_COPD<-renderText({invoke(); 'salam:'; show_output(thisSession$output$n_COPD)});output$output.total_exac<-renderText({invoke(); 'salam:'; show_output(thisSession$output$total_exac)});output$output.total_exac_time<-renderText({invoke(); 'salam:'; show_output(thisSession$output$total_exac_time)});output$output.total_pack_years<-renderText({invoke(); 'salam:'; show_output(thisSession$output$total_pack_years)});output$output.total_doctor_visit<-renderText({invoke(); 'salam:'; show_output(thisSession$output$total_doctor_visit)});output$output.total_cost<-renderText({invoke(); 'salam:'; show_output(thisSession$output$total_cost)});output$output.total_qaly<-renderText({invoke(); 'salam:'; show_output(thisSession$output$total_qaly)}); }
