
#' Runs the analytic workflow for the Phase2.1SeverityAugPackage project
#'
#' @keywords 4CE
#' @export

runAnalysis <- function(chartReview_path = '/4ceData/LocalPatientChartReview_wpids.csv',maxdays = 30,mindays = -2, outcome = 'Severe?') {
    
    library(readr)
    library(ggplot2)
    library(dplyr)
    library(FourCePhase2.1Data)
    library(tidyr)
    library(stringr)
    library(data.table)
    library(eRm)
    library(splines)
    library(Rmisc)
    
    ## make sure this instance has the latest version of the quality control and data wrangling code available
    devtools::install_github("https://github.com/covidclinical/Phase2.1DataRPackage", subdir="FourCePhase2.1Data", upgrade=FALSE)

    ## get the site identifier assocaited with the files stored in the /4ceData/Input directory that 
    ## is mounted to the container
    currSiteId = FourCePhase2.1Data::getSiteId()

    ## run the quality control
    FourCePhase2.1Data::runQC(currSiteId)

    LocalPatientSummary = FourCePhase2.1Data::getLocalPatientSummary(currSiteId)
    LocalPatientObservations = FourCePhase2.1Data::getLocalPatientObservations(currSiteId)
    LocalPatientClinicalCourse = FourCePhase2.1Data::getLocalPatientClinicalCourse(currSiteId)
    labnames <- read_csv(system.file("extdata", "labnames.csv", package = "FourCePhase2.1Phase2.1SeverityAugPackage"))
    LocalPatientChartReview <- read_csv(chartReview_path ) %>% select(c('patient_num', 'Severe?', 'COVID-19 Hospitalization?'))
    LocalPatientSummary <- merge(LocalPatientSummary, LocalPatientChartReview, all.x = TRUE)
    
    LocalPatientSummary$severeDays = as.integer(as.Date(LocalPatientSummary$severe_date) - as.Date(LocalPatientSummary$admission_date))
    LocalPatientSummary$deathDays = as.integer(as.Date(LocalPatientSummary$death_date) - as.Date(LocalPatientSummary$admission_date))
    LocalPatientSummary$exitDay = as.integer(as.Date(LocalPatientSummary$last_discharge_date) - as.Date(LocalPatientSummary$admission_date))
    
    if ("icu_date" %in% colnames(LocalPatientSummary)){
        LocalPatientSummary$icuDays = as.integer(as.Date(LocalPatientSummary$icu_date) - as.Date(LocalPatientSummary$admission_date))
        }else {
            print('No ICU Data Available')
        }
    LocalPatientSummary_study = LocalPatientSummary

    LocalPatientObservations_study = subset(LocalPatientObservations,LocalPatientObservations$patient_num %in% LocalPatientSummary_study$patient_num)
    LocalPatientObservations_study = subset(LocalPatientObservations_study,(LocalPatientObservations_study$concept_type %in% c('SEVERE-DIAG','LAB-LOINC')) | (LocalPatientObservations_study$concept_code %in% c('J96.01', 'R65.21','SIANES','SupplementalOxygenSevere', 'N17.9', 'Z66', 'J12.89')))
    LocalPatientObservations_study <- merge(LocalPatientObservations_study, labnames, all.x = TRUE)
    LocalPatientObservations_study[is.na(LocalPatientObservations_study$labname), 'labname'] <- 'SEVEREDIAG'
    LocalPatientObservations_study$wideval = 1
    LocalPatientClinicalCourse_study = subset(LocalPatientClinicalCourse, LocalPatientClinicalCourse$patient_num %in% LocalPatientSummary_study$patient_num)
    LocalPatientSummary_study$endDate = pmin(abs(LocalPatientSummary_study$exitDay),abs(LocalPatientSummary_study$deathDays))
    LocalPatientClinicalCourse_study = merge(LocalPatientClinicalCourse_study, LocalPatientSummary_study %>% select(patient_num, endDate))
    LocalPatientClinicalCourse_study[LocalPatientClinicalCourse_study$endDate >=maxdays, 'endDate'] = maxdays
    LocalPatientClinicalCourse_study = subset(LocalPatientClinicalCourse_study, LocalPatientClinicalCourse_study$days_since_admission <= LocalPatientClinicalCourse_study$endDate)
    LocalPatientClinicalCourse_study = subset(LocalPatientClinicalCourse_study, LocalPatientClinicalCourse_study$in_hospital == 1)
    
    LocalPatientObservations_study_values = lab_highlow(LocalPatientObservations_study)
    
    LocalPatientObservations_study_values[grepl("elevated", LocalPatientObservations_study_values$labname),'wideval'] <- 2
    LocalPatientObservations_study_values[grepl("low", LocalPatientObservations_study_values$labname),'wideval'] <- 0
    
    LocalPatientObservations_study_values$labname = gsub("elevated","val",LocalPatientObservations_study_values$labname)
    LocalPatientObservations_study_values$labname = gsub("low","val",LocalPatientObservations_study_values$labname)
    LocalPatientObservations_study_values$labname = gsub("standard","val",LocalPatientObservations_study_values$labname)
    
    
    
    LocalPatientObservations_wide_val = LocalPatientObservations_study_values %>%
        tidyr::pivot_wider(id_cols = c("patient_num", "days_since_admission"),
                           names_from = c("labname"),
                           values_from = "wideval", values_fn = max
        )
    
    LocalPatientObservations_wide = LocalPatientObservations_study %>%
        tidyr::pivot_wider(id_cols = c("patient_num", "days_since_admission"),
                           names_from = c("labname"),
                           values_from = "wideval", values_fn = length
        )
    
    LocalPatientObservations_wide = merge(LocalPatientObservations_wide, LocalPatientObservations_wide_val, by = c('patient_num', 'days_since_admission'))
    
    
    #filter the wide table to only look at observation period
    LocalPatientObservations_wide = subset(LocalPatientObservations_wide,(LocalPatientObservations_wide$days_since_admission >= mindays) & (LocalPatientObservations_wide$days_since_admission <= maxdays))
    LocalPatientObservations_widesplit <- split( LocalPatientObservations_wide , f = LocalPatientObservations_wide$patient_num )
    
    #generate the person-day matrix
    labnames = colnames(LocalPatientObservations_wide)[!colnames(LocalPatientObservations_wide) %in% c('patient_num', 'days_since_admission')]
    for(i in labnames){
        LocalPatientClinicalCourse_study[[i]] = 0
    }
    
    labvals = labnames[grepl('_val', labnames)]
    lab_counts = labnames[!labnames %in% labvals]
    
    op = list()
    
    for (ln in unique(LocalPatientObservations_study$labname)){
        dfln = subset(LocalPatientObservations_study, LocalPatientObservations_study$labname == ln)
        q = quantile(unname(table(dfln$patient_num)), probs = c(0,.25,.5,.75,1))
        if (q[1] == q[2]){
            q = q[2:5]
        }
        op[[ln]] = q
        
    }
    
    for(j in 1:nrow(LocalPatientClinicalCourse_study)){
        patnum = LocalPatientClinicalCourse_study$patient_num[j]
        patday = LocalPatientClinicalCourse_study$days_since_admission[j]
        labdf = LocalPatientObservations_widesplit[[paste(patnum)]]
        labdf = subset(labdf, labdf$days_since_admission <= patday)
        rownames(labdf) = NULL
        if(!is.null(labdf) ){
            labdf = labdf[order(labdf$days_since_admission),]
            
            x_counts = colSums(labdf, na.rm = TRUE)[lab_counts]
            x_quants = quantize_counts(op, x_counts)
            
            
            LocalPatientClinicalCourse_study[j,lab_counts] = x_quants
            
            y = sapply(labdf, function(x) x[max(which(!is.na(x)))])[labvals]     
            LocalPatientClinicalCourse_study[j,labvals] = y
            
            
        }
        print(j)
        
    }
    
    LocalPatientClinicalCourse_study = merge(LocalPatientClinicalCourse_study,LocalPatientSummary_study %>% select(patient_num, age_group, sex, race, 'COVID-19 Hospitalization?', 'Severe?'))
    LocalPatientClinicalCourse_study[ 7 >= LocalPatientClinicalCourse_study$days_since_admission, 'days_since_admission'] <- 0
    LocalPatientClinicalCourse_study[ (14 >= LocalPatientClinicalCourse_study$days_since_admission) & (LocalPatientClinicalCourse_study$days_since_admission >7), 'days_since_admission'] <-1
    LocalPatientClinicalCourse_study[ (21 >= LocalPatientClinicalCourse_study$days_since_admission) & (LocalPatientClinicalCourse_study$days_since_admission >14), 'days_since_admission'] <-2
    LocalPatientClinicalCourse_study[ (LocalPatientClinicalCourse_study$days_since_admission >21), 'days_since_admission'] <-3
    
    racemat = data.frame(model.matrix(~(LocalPatientClinicalCourse_study$race)+0))
    
    if ('LocalPatientClinicalCourse_study.raceno_information' %in% colnames(racemat)){
        racemat[racemat$LocalPatientClinicalCourse_study.raceno_information == 1,] <- NA
        racemat = select(racemat, -c('LocalPatientClinicalCourse_study.raceno_information'))
    }
    
    if ('LocalPatientClinicalCourse_study.raceother' %in% colnames(racemat)){
        racemat[racemat$LocalPatientClinicalCourse_study.raceother == 1,] <- NA
        racemat = select(racemat, -c('LocalPatientClinicalCourse_study.raceother'))
    }
    PatientDayMatrix = cbind(LocalPatientClinicalCourse_study,racemat)
    
    PatientDayMatrix$age_group = gsub("18to20",0,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = gsub("18to25",1,PatientDayMatrix$age_group)
    
    PatientDayMatrix$age_group = gsub("21to25",1,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = gsub("26to49",2,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = gsub("50to69",3,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = gsub("70to79",4,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = gsub("80plus",5,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = gsub("other",NA,PatientDayMatrix$age_group)
    PatientDayMatrix$age_group = as.integer(PatientDayMatrix$age_group)
    PatientDayMatrix$sex_bin <- ifelse(grepl("female", PatientDayMatrix$sex), 0, 1)
    
    PatientDayMatrix_abr = select(PatientDayMatrix, -c('in_hospital','alt_val', 'endDate','race','sex','calendar_date'))  
    
    
    pat_data <- select(PatientDayMatrix_abr, -c("patient_num",'Severe?','COVID-19 Hospitalization?'))
    pat_labels <- select(PatientDayMatrix_abr, c("patient_num", "days_since_admission",'Severe?','COVID-19 Hospitalization?', 'severe'))
    BIDMC_PCM_model <- readRDS(system.file("extdata", "BIDMC_PCM_model.rds", package = "FourCePhase2.1Phase2.1SeverityAugPackage"))
    if (!("in_icu" %in% colnames(pat_data))){
        pat_data$in_icu = NA
        }
    if (length(BIDMC_PCM_model$X_labels[!(BIDMC_PCM_model$X_labels %in% colnames(pat_data))]) > 0){
        for(i in BIDMC_PCM_model$X_labels[!(BIDMC_PCM_model$X_labels %in% colnames(pat_data))]){
            pat_data[[i]] = 0
        }
    }
    
    pat_data = select(pat_data, BIDMC_PCM_model$X_labels)
    pat_data = rbind(pat_data, rep(1, ncol(pat_data)))
    pat_data = rbind(pat_data, rep(0, ncol(pat_data)))

    pat_labels = rbind(pat_labels, rep(1, ncol(pat_labels)))
    pat_labels = rbind(pat_labels, rep(0, ncol(pat_labels)))
    
        
    pat_dataX01 = datprep_PCM(pat_data,BIDMC_PCM_model)
    thissite_PCM_model = BIDMC_PCM_model
    thissite_PCM_model$X = as.matrix(pat_data)
    thissite_PCM_model$X01 = as.matrix(pat_dataX01)
    
    pprtt = person.parameter.eRm(thissite_PCM_model,BIDMC_PCM_model)
    irtY = pprtt$theta.table
    irtY[['Severe?']] = pat_labels$`Severe?`
    irtY[['COVID-19 Hospitalization?']] = pat_labels$`COVID-19 Hospitalization?`
    
    irtY$patient_num = pat_labels$patient_num
    irtY$days_since_admission = pat_labels$days_since_admission
    
    irtY = irtY[!is.na(irtY$`Person Parameter`),]
    sev1 = data.frame(subset(irtY, irtY[[outcome]] == 1)$'Person Parameter')
    sev0 = data.frame(subset(irtY, irtY[[outcome]] == 0)$'Person Parameter')
    colnames(sev0) = colnames(sev1) = 'val'
    sev1$sev = '1'
    sev0$sev = '0'
    tptsne = rbind(sev1, sev0)
    ggplot(tptsne, aes(val, fill = sev)) + geom_density(alpha = 0.2)
    t.test(val ~ sev, data = tptsne)
    aucs = c()
    print('calculating AUCS')
    for(i in 1:1000){
        aucs = c(aucs, mean(sample(sev1$val,1000,replace=T) > sample(sev0$val,1000,replace=T)))
        
    }
    
    print(CI(aucs, 0.05))
    
    irt_thresholds = data.frame(unique(tptsne$val))
    irt_thresholds$FP = irt_thresholds$TP = irt_thresholds$TN = irt_thresholds$FN = 0
    print('calculating thresholds')
    for(i in 1:nrow(irt_thresholds)){
        threshold = irt_thresholds$unique.tptsne.val.[i]
        
        
        
        try(irt_thresholds$TP[i] <- table(subset(tptsne, tptsne$val >=threshold )$sev)[['1']], silent = T)
        try(irt_thresholds$TN[i] <- table(subset(tptsne, tptsne$val < threshold )$sev)[['0']], silent = T)
        try(irt_thresholds$FN[i] <- table(subset(tptsne, tptsne$val < threshold )$sev)[['1']], silent = T)
        try(irt_thresholds$FP[i] <- table(subset(tptsne, tptsne$val >=threshold )$sev)[['0']], silent = T)
    }
    
    patient_level_output = aggregate(irtY$`Person Parameter`, by = list(irtY$patient_num), max)
    colnames(patient_level_output) = c('patient_num', 'val')
    patient_level_output = merge(patient_level_output, distinct(select(LocalPatientSummary_study, c('patient_num', 'admission_date',outcome))))
    
    patient_level_output_1 = subset(patient_level_output, patient_level_output[[outcome]] == 1)
    patient_level_output_0 = subset(patient_level_output, patient_level_output[[outcome]] == 0)
    plauc = c()
    print('calculating AUCS')
    for(i in 1:1000){
        plauc = c(plauc, mean(sample(patient_level_output_1$val,1000,replace=T) > sample(patient_level_output_0$val,1000,replace=T)))
        
    }  
    
    
    CI(plauc)
    patient_level_output$sev = patient_level_output[[outcome]]
    tptsnepl = select(patient_level_output, c('val', 'sev'))
    tptsnepl =subset(tptsnepl, !is.na(tptsnepl$sev))
    tptsnepl$sev = as.factor(tptsnepl$sev)
    ggplot(tptsnepl, aes(val, fill = sev)) + geom_density(alpha = 0.2)
    
    irt_thresholds_PL = data.frame(unique(patient_level_output$val))
    irt_thresholds_PL$FP = irt_thresholds_PL$TP = irt_thresholds_PL$TN = irt_thresholds_PL$FN = 0
    print('calculating PL thresholds')
    for(i in 1:nrow(irt_thresholds_PL)){
        threshold = irt_thresholds_PL$unique.patient_level_output.val.[i]
        
        
        
        try(irt_thresholds_PL$TP[i] <- table(subset(patient_level_output, patient_level_output$val >=threshold )[[outcome]])[['1']], silent = T)
        try(irt_thresholds_PL$TN[i] <- table(subset(patient_level_output, patient_level_output$val < threshold )[[outcome]])[['0']], silent = T)
        try(irt_thresholds_PL$FN[i] <- table(subset(patient_level_output, patient_level_output$val < threshold )[[outcome]])[['1']], silent = T)
        try(irt_thresholds_PL$FP[i] <- table(subset(patient_level_output, patient_level_output$val >=threshold )[[outcome]])[['0']], silent = T)
    }
    
    output = list(aucs, plauc, irt_thresholds,irt_thresholds_PL,tptsne, tptsnepl)
    saveRDS(output, paste(getProjectOutputDirectory() ,'/', currSiteId,'_PCMOutput.RDS', sep = ''))
    
}

