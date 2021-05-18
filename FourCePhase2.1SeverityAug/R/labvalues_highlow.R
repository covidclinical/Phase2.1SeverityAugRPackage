lab_highlow <- function(df){
  
  #alt#
  df_alt = subset(df, df$concept_code == '1742-6')
  df_alt[df_alt$value <= 0, 'labname'] = 'alt_low'
  df_alt[(df_alt$value > 0) & (df_alt$value < 40), 'labname'] = 'alt_standard'
  df_alt[df_alt$value >= 40, 'labname'] = 'alt_elevated'
  
  #albumin#
  df_albumin = subset(df, df$concept_code == '1751-7')
  df_albumin[df_albumin$value <= 3.5, 'labname'] = 'albumin_low'
  df_albumin[(df_albumin$value > 3.5) & (df_albumin$value < 5.2), 'labname'] = 'albumin_standard'
  df_albumin[df_albumin$value >= 5.2, 'labname'] = 'albumin_elevated'

  #ast#
  df_ast = subset(df, df$concept_code == '1920-8')
  df_ast[df_ast$value <= 0, 'labname'] = 'ast_low'
  df_ast[(df_ast$value > 0) & (df_ast$value < 40), 'labname'] = 'ast_standard'
  df_ast[df_ast$value >= 40, 'labname'] = 'ast_elevated'

  #troponin_high#
  df_troponin_high = subset(df, df$concept_code == '49563-0')
  df_troponin_high[df_troponin_high$value <= 0, 'labname'] = 'troponin_high_low'
  df_troponin_high[(df_troponin_high$value > 0) & (df_troponin_high$value < 0.01), 'labname'] = 'troponin_high_standard'
  df_troponin_high[df_troponin_high$value >= 0.01, 'labname'] = 'troponin_high_elevated'

  #troponin_normal#
  df_troponin_normal = subset(df, df$concept_code == '6598-7')
  df_troponin_normal[df_troponin_normal$value <= 0, 'labname'] = 'troponin_normal_low'
  df_troponin_normal[(df_troponin_normal$value > 0) & (df_troponin_normal$value < 0.01), 'labname'] = 'troponin_normal_standard'
  df_troponin_normal[df_troponin_normal$value >= 0.01, 'labname'] = 'troponin_normal_elevated'
  
  #crp#
  df_crp = subset(df, df$concept_code == '1988-5')
  df_crp[df_crp$value <= 0, 'labname'] = 'crp_low'
  df_crp[(df_crp$value > 0) & (df_crp$value < 5), 'labname'] = 'crp_standard'
  df_crp[df_crp$value >= 5, 'labname'] = 'crp_elevated'
  
  #creatinine#
  df_creatinine = subset(df, df$concept_code == '2160-0')
  df_creatinine[df_creatinine$value <= 0.4, 'labname'] = 'creatinine_low'
  df_creatinine[(df_creatinine$value > 0.4) & (df_creatinine$value < 1.1), 'labname'] = 'creatinine_standard'
  df_creatinine[df_creatinine$value >= 1.1, 'labname'] = 'creatinine_elevated'
  
  #ddimer_ddu#
  df_ddimer_ddu = subset(df, df$concept_code == '48066-5')
  df_ddimer_ddu[df_ddimer_ddu$value <= 0, 'labname'] = 'ddimer_ddu_low'
  df_ddimer_ddu[(df_ddimer_ddu$value > 0) & (df_ddimer_ddu$value < 250), 'labname'] = 'ddimer_ddu_standard'
  df_ddimer_ddu[df_ddimer_ddu$value >= 250, 'labname'] = 'ddimer_ddu_elevated'
  
  #ddimer_feu#
  df_ddimer_feu = subset(df, df$concept_code == '48066-5')
  df_ddimer_feu[df_ddimer_feu$value <= 0, 'labname'] = 'ddimer_feu_low'
  df_ddimer_feu[(df_ddimer_feu$value > 0) & (df_ddimer_feu$value < 500), 'labname'] = 'ddimer_feu_standard'
  df_ddimer_feu[df_ddimer_feu$value >= 500, 'labname'] = 'ddimer_feu_elevated'
  
  #ferritin#
  df_ferritin = subset(df, df$concept_code == '2276-4')
  df_ferritin[df_ferritin$value <= 13, 'labname'] = 'ferritin_low'
  df_ferritin[(df_ferritin$value > 13) & (df_ferritin$value < 400), 'labname'] = 'ferritin_standard'
  df_ferritin[df_ferritin$value >= 400, 'labname'] = 'ferritin_elevated'
  
  #fibrinogen#
  df_fibrinogen = subset(df, df$concept_code == '3255-7')
  df_fibrinogen[df_fibrinogen$value <= 180, 'labname'] = 'fibrinogen_low'
  df_fibrinogen[(df_fibrinogen$value > 180) & (df_fibrinogen$value < 400), 'labname'] = 'fibrinogen_standard'
  df_fibrinogen[df_fibrinogen$value >= 400, 'labname'] = 'fibrinogen_elevated'
  
  #ldh#
  df_ldh = subset(df, df$concept_code == '2532-0')
  df_ldh[df_ldh$value <= 94, 'labname'] = 'ldh_low'
  df_ldh[(df_ldh$value > 94) & (df_ldh$value < 250), 'labname'] = 'ldh_standard'
  df_ldh[df_ldh$value >= 250, 'labname'] = 'ldh_elevated'
  
  #lymphocyte#
  df_lymphocyte = subset(df, df$concept_code == '731-0')
  df_lymphocyte[df_lymphocyte$value <= 1.2, 'labname'] = 'lymphocyte_low'
  df_lymphocyte[(df_lymphocyte$value > 1.2) & (df_lymphocyte$value < 3.7), 'labname'] = 'lymphocyte_standard'
  df_lymphocyte[df_lymphocyte$value >= 3.7, 'labname'] = 'lymphocyte_elevated'
  
  #neutrophil#
  df_neutrophil = subset(df, df$concept_code == '751-8')
  df_neutrophil[df_neutrophil$value <= 1.6, 'labname'] = 'neutrophil_low'
  df_neutrophil[(df_neutrophil$value > 1.6) & (df_neutrophil$value < 6.1), 'labname'] = 'neutrophil_standard'
  df_neutrophil[df_neutrophil$value >= 6.1, 'labname'] = 'neutrophil_elevated'
  
  #procalcitonin#
  df_procalcitonin = subset(df, df$concept_code == '33959-8')
  df_procalcitonin[df_procalcitonin$value <= 0, 'labname'] = 'procalcitonin_low'
  df_procalcitonin[(df_procalcitonin$value > 0) & (df_procalcitonin$value < .15), 'labname'] = 'procalcitonin_standard'
  df_procalcitonin[df_procalcitonin$value >= .15, 'labname'] = 'procalcitonin_elevated'
  
  #prothrombin_time#
  df_prothrombin_time = subset(df, df$concept_code == '5902-2')
  df_prothrombin_time[df_prothrombin_time$value <= 9.4, 'labname'] = 'prothrombin_time_low'
  df_prothrombin_time[(df_prothrombin_time$value > 9.4) & (df_prothrombin_time$value < 12.5), 'labname'] = 'prothrombin_time_standard'
  df_prothrombin_time[df_prothrombin_time$value >= 12.5, 'labname'] = 'prothrombin_time_elevated'
  
  #bilirubin#
  df_bilirubin = subset(df, df$concept_code == '1975-2')
  df_bilirubin[df_bilirubin$value <= 0, 'labname'] = 'bilirubin_low'
  df_bilirubin[(df_bilirubin$value > 0) & (df_bilirubin$value < 1.5), 'labname'] = 'bilirubin_standard'
  df_bilirubin[df_bilirubin$value >= 1.5, 'labname'] = 'bilirubin_elevated'
  
  #wbc#
  df_wbc = subset(df, df$concept_code == '6690-2')
  df_wbc[df_wbc$value <= 4, 'labname'] = 'wbc_low'
  df_wbc[(df_wbc$value > 4) & (df_wbc$value < 10), 'labname'] = 'wbc_standard'
  df_wbc[df_wbc$value >= 10, 'labname'] = 'wbc_elevated'
  
  df_labvalues = rbind(df_alt, df_albumin,df_ast,df_troponin_high, df_troponin_normal, df_crp, df_creatinine, df_ddimer_feu, df_ddimer_ddu, df_ferritin, df_fibrinogen, df_ldh, df_lymphocyte, df_neutrophil, df_procalcitonin, df_prothrombin_time, df_bilirubin, df_wbc)
  
  return(df_labvalues)
  
  
}
  