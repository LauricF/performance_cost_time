`Cost categories` <- c("Physician time","Secretary time","Lab test","Patient time ","Travel cost")
`AUC glucose` <- c('3/4 visits',"1/12h", "4 glucose samples", "2.25h", "1 trip")
`AUC Cpeptide` <- c('3/4 visits',"1/12h", "4 C-peptide samples", "2.25h", "1 trip")
Hba1c <- c('3/4 visits',"1/12h", "1 Hba1c", "1/4h", "1 trip")
Index60 <- c('3/4 visits',"1/12h", "1 glucose sample + 2  C-peptide samples", "1.25h", "1 trip")
Cpep30 <- c('3/4 visits',"1/12h", "2 glucose samples + 2 C-peptide samples", "1.25h", "1 trip")
`Beta2 score` <- c('3/4 visits',"1/12h", "1 glucose sample + C-peptide sample + 1 Hba1c", "0.25h", "1 trip")
`Unit cost` <- c("$66.08/visit","$19.84","$16.79/4 glucose samples; $9.71/HbA1c; $20.81/Cpeptide sample","$14.88/h","$7.00/trip")
`source of cost` <- c("CMS code 99213", "Bureau of Labor Statistics", "UPMC","half the mean hourly wage for all occupations in 2022, Bureau of Labor Statistics","Wu E, Kazzi NG, Lee JM. Cost-effectiveness of Screening Strategies for Identifying Pediatric Diabetes Mellitus and Dysglycemia. JAMA Pediatr. 2013;167(1):32–39. doi:10.1001/jamapediatrics.2013.419")
# secretaria https://www.bls.gov/oes/current/oes436013.htm

res <- data.frame(`Cost categories`,`AUC glucose`,`AUC Cpeptide`,Hba1c, Index60, Cpep30,`Beta2 score`,`Unit cost`,`source of cost`)
res[dim(res)[1] + 1,] <- c("total cost",
                           round(3/4*66.08 + 1/12*19.84 + 16.79 + 2.25*14.88 + 7,1), # AUC glucose
                           round(3/4*66.08 + 1/12*19.84 + 4*20.81 + 2.25*14.88 + 7), # AUC Cpeptide
                           round(3/4*66.08 + 1/12*19.84 + 9.71 + 0.25*14.88 + 7), # Hba1c
                           round(3/4*66.08 + 1/12*19.84 + 16.79/4 + 2 * 20.81 + 1.25*14.88 + 7), # Index60
                           round(3/4*66.08 + 1/12*19.84 + 16.79/4/2 + 1 * 20.81 + 0.45*14.88 + 7), # Cpep30
                           round(3/4*66.08 + 1/12*19.84 + 16.79/4 + 1 * 20.81 + 0.25*14.88 + 7), # Beta2 score
                           "",
                           "")
res <- res %>% rename_all(str_replace_all, "\\.", " ")
knitr::kable(res, digits = 4, escape = TRUE)

# https://www.niddk.nih.gov/health-information/professionals/clinical-tools-patient-management/diabetes/game-plan-preventing-type-2-diabetes/reimbursement-coding
# 82951 Glucose Tolerance Test (GTT); three specimens (includes glucose)
# 83036  	Hemoglobin A1C
# 82952 Blood glucose (sugar) tolerance test, each additional beyond 3 specimens
# 
# https://www.upmc.com/-/media/upmc/healthcare-professionals/physicians/documents/lab-fee-schedule.pdf

