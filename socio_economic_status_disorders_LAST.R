#Socioecnomic  status and its Subjective self-perception in Mental health 
library(TwoSampleMR)
library(MRPRESSO)
library(MRInstruments)
library(tidyverse)
library(rlang)
library(MendelianRandomization)
library(jsonlite)
library(purrr)
library(data.table)
library(xlsx)
vignette("MVMR")


#para instalar el MVMR packager
install.packages("remotes")
library(remotes)
install_github("WSpiller/MVMR", build_opts = c("--no-resave-data", "--no-manual"), build_vignettes = TRUE)
vignette("MVMR")

library(MVMR)

setwd("~/work/MENDELIAN PDF/subjective_satisfaction/SES_IN_MENTAL_HEALTH")

#traits to assess

#Socioeconomic and health status: main: household income ukbb (ukb-b-7408), Educational attainment (ieu-a-1239 years of schooling) full 700k(Lee),

#townsend deprivation index ukb-b-10011
#Posible sensitivity.. #MTAGS: EA 1M MTAG (Lee), Household income MTAG with EA (Hill)

#Subjective self-perception: Financial satisfaction(ukb-b-2830)

#Mental disorders: Schizophrenia(ieu-a-22), Bipolar disorder(ieu-b-41), Major depression (ieu-b-102), autism (ieu-a-1185), ADHD (ieu-a-1183)


#UNIVARIANT MR HOUSEHOLD INCOME BEFORE TAXES UKBB
exp_income_ukbb <- TwoSampleMR::extract_instruments("ukb-b-7408")
head(exp_income_ukbb)
#calculo F statistic
F_income <- qf(exp_income_ukbb$pval.exposure, df1=1, df2= exp_income_ukbb$samplesize.exposure-1, lower.tail=FALSE)
F_income
range(F_income)



#SCZ
out_inc_uk_scz <- TwoSampleMR::extract_outcome_data(exp_income_ukbb$SNP, "ieu-a-22")
# Harmonise the exposure and outcome data
dat_inc_scz_uk <- TwoSampleMR::harmonise_data(exp_income_ukbb, out_inc_uk_scz)

# Perform MR
res_inc_scz_uk<- TwoSampleMR::mr(dat_inc_scz_uk )
res_inc_scz_uk
#sensitiviy
het2 = mr_heterogeneity(dat_inc_scz_uk)
het2

pleo2<- mr_pleiotropy_test(dat_inc_scz_uk)
pleo2

res_inc_scz_odds <- generate_odds_ratios(res_inc_scz_uk)
res_inc_scz_odds

#MRPRESSO
res_presso_inc_scz <-  TwoSampleMR::run_mr_presso(dat_inc_scz_uk, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_inc_scz
presso_inc_scz <- map(res_presso_inc_scz, as.data.table)
presso_inc_scz
# Convert nested list to data frame by column
presso <- as.data.frame(do.call(cbind,presso_inc_scz))
presso 

#intento  combine_all_results function pero debe haber un bug porque me da error todo el tiempo, busco algun merge alternativo
#put all data frames into list
df_list <- list(res_inc_scz_odds, het2, pleo2)   

#merge all data frames together
inc_scz_df<- df_list %>% reduce(full_join, by='id.exposure')
inc_scz_df <- filter(inc_scz_df, method.y == "Inverse variance weighted")
head(inc_scz_df)


#bip
out_inc_uk_bip <- TwoSampleMR::extract_outcome_data(exp_income_ukbb$SNP, "ieu-b-41", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_inc_uk_bip <- TwoSampleMR::harmonise_data(exp_income_ukbb, out_inc_uk_bip)

# Perform MR
res_inc_bip_uk<- TwoSampleMR::mr(dat_inc_uk_bip )
res_inc_bip_uk

res_inc_bip_odds <- generate_odds_ratios(res_inc_bip_uk)
res_inc_bip_odds
#sensitiviy
het3 = mr_heterogeneity(dat_inc_uk_bip)
het3

pleo3<- mr_pleiotropy_test(dat_inc_uk_bip)
pleo3

#put all data frames into list
bip_list <- list(res_inc_bip_odds, het3, pleo3)   

#merge all data frames together
inc_bip_df<- bip_list %>% reduce(full_join, by='id.exposure')
inc_bip_df <- filter(inc_bip_df, method.y == "Inverse variance weighted")


#MRPRESSO
res_presso_inc_bip <-  TwoSampleMR::run_mr_presso(dat_inc_uk_bip, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_inc_bip


#dep
out_inc_uk_dep <- TwoSampleMR::extract_outcome_data(exp_income_ukbb$SNP, "ieu-b-102", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_inc_uk_dep <- TwoSampleMR::harmonise_data(exp_income_ukbb, out_inc_uk_dep)

# Perform MR
res_inc_dep_uk<- TwoSampleMR::mr(dat_inc_uk_dep )
res_inc_dep_uk

res_inc_dep_odds <- generate_odds_ratios(res_inc_dep_uk)
res_inc_dep_odds
#sensitiviy
het4 = mr_heterogeneity(dat_inc_uk_dep)
het4

pleo4<- mr_pleiotropy_test(dat_inc_uk_dep)
pleo4



#put all data frames into list
dep_list <- list(res_inc_dep_odds, het4, pleo4)   

#merge all data frames together
inc_dep_df<- dep_list %>% reduce(full_join, by='id.exposure')
inc_dep_df <- filter(inc_dep_df, method.y == "Inverse variance weighted")

#MRPRESSO
res_presso_inc_dep <-  TwoSampleMR::run_mr_presso(dat_inc_uk_dep, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_inc_dep



#autism ieu-a-1185
out_inc_uk_au <- TwoSampleMR::extract_outcome_data(exp_income_ukbb$SNP, "ieu-a-1185")

dat_inc_uk_au <- TwoSampleMR::harmonise_data(exp_income_ukbb, out_inc_uk_au)

# Perform MR
res_inc_uk_au<- TwoSampleMR::mr(dat_inc_uk_au )
res_inc_uk_au
#sensitiviy
het5 = mr_heterogeneity(dat_inc_uk_au)
het5

pleo5<- mr_pleiotropy_test(dat_inc_uk_au)
pleo5

res_inc_au_odds <- generate_odds_ratios(res_inc_uk_au)
res_inc_au_odds

#put all data frames into list
au_list <- list(res_inc_au_odds, het5, pleo5)   

#merge all data frames together
inc_au_df<- au_list %>% reduce(full_join, by='id.exposure')
inc_au_df <- filter(inc_au_df, method.y == "Inverse variance weighted")

#MRPRESSO

res_presso_inc_au <-  TwoSampleMR::run_mr_presso(dat_inc_uk_au, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_inc_au

#ADHD ieu-a-1183
out_inc_uk_adhd <- TwoSampleMR::extract_outcome_data(exp_income_ukbb$SNP, "ieu-a-1183", proxies=FALSE)

dat_inc_uk_adhd <- TwoSampleMR::harmonise_data(exp_income_ukbb, out_inc_uk_adhd)

res_inc_uk_adhd<- TwoSampleMR::mr(dat_inc_uk_adhd)
res_inc_uk_adhd
#odds
res_inc_adhd_odds <- generate_odds_ratios(res_inc_uk_adhd)
res_inc_adhd_odds

#sensitiviy
het6 = mr_heterogeneity(dat_inc_uk_adhd)
het6

pleo6<- mr_pleiotropy_test(dat_inc_uk_adhd)
pleo6

#put all data frames into list
adhd_list <- list(res_inc_adhd_odds, het6, pleo6)   

#merge all data frames together
inc_adhd_df<- adhd_list %>% reduce(full_join, by='id.exposure')
inc_adhd_df <- filter(inc_adhd_df, method.y == "Inverse variance weighted")

#MR_PRESSO tardas mas cuanta mas nbdistribution subas pero conn 1000 en muchos casos no es suficiente
res_presso_inc_adhd <-  TwoSampleMR::run_mr_presso(dat_inc_uk_adhd, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_inc_adhd

#combino todas los dataframes de todos los resultados income to Mental disorders
income_mentaldisorders <- rbind(inc_scz_df,inc_bip_df, inc_dep_df, inc_au_df, inc_adhd_df)
write.xlsx(income_mentaldisorders, "UnivariantMR_income_MD.xlsx")


#UNIVARIANT EA  ieu-a-1239#########################################
expys <- TwoSampleMR::extract_instruments("ieu-a-1239")

#calculo F statistic
F_ea <- qf(expys$pval.exposure, df1=1, df2= expys$samplesize.exposure-1, lower.tail=FALSE)
F_ea
range(F_income)

#EA on SCZ
out_ys_scz <- TwoSampleMR::extract_outcome_data(expys$SNP, "ieu-a-22")

dat_ys_scz <- TwoSampleMR::harmonise_data(expys, out_ys_scz)
dat_ys_scz$exposure =  "Educational Attainment"
head(dat_ys_scz)

res_ys_scz <-mr(dat_ys_scz)
res_ys_scz

#sensitiviy
het_ys = mr_heterogeneity(dat_ys_scz)
het_ys

pleo_sz<- mr_pleiotropy_test(dat_ys_scz)
pleo_sz

#odds
res_ys_scz_odds <- generate_odds_ratios(res_ys_scz)
res_ys_scz_odds

#put all data frames into list
ea_list <- list(res_ys_scz_odds, het_ys, pleo_sz)   

#merge all data frames together
ea_scz_df<- ea_list %>% reduce(full_join, by='id.exposure')
ea_scz_df <- filter(ea_scz_df, method.y == "Inverse variance weighted")
head(ea_scz_df)

#MR_PRESSO tardas mas cuanta mas nbdistribution subas pero conn 1000 en muchos casos no es suficiente
res_presso_ea_scz <-  TwoSampleMR::run_mr_presso(dat_ys_scz, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_ea_scz



#EA ON BIPOLAR
out_ys_bip <- TwoSampleMR::extract_outcome_data(expys$SNP, "ieu-b-41", proxies=FALSE)

dat_ys_bip <- TwoSampleMR::harmonise_data(expys, out_ys_bip)

head(dat_ys_bip)
dat_ys_bip$exposure =  "Educational Attainment"

res_ys_bip<-mr(dat_ys_bip)
res_ys_bip
#sensitiviy
het_ea_b = mr_heterogeneity(dat_ys_bip)
het_ea_b

pleo_ea_b<- mr_pleiotropy_test(dat_ys_bip)
pleo_ea_b

#MR_PRESSO tardas mas cuanta mas nbdistribution subas pero conn 1000 en muchos casos no es suficiente
res_presso_ea_b <-  TwoSampleMR::run_mr_presso(dat_ys_bip, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_ea_b
#odds
res_ys_bip_odds <- generate_odds_ratios(res_ys_bip)
res_ys_bip_odds

#put all data frames into list
ea_bip_list <- list(res_ys_bip_odds, het_ea_b, pleo_ea_b)  
ea_bip_list

#merge all data frames together
ea_bip_df<- ea_bip_list %>% reduce(full_join, by='id.exposure')
ea_bip_df <- filter(ea_bip_df, method.y == "Inverse variance weighted")
head(ea_bip_df)


#EA ON depression
out_ys_dep <- TwoSampleMR::extract_outcome_data(expys$SNP, "ieu-b-102", proxies=FALSE)
dat_ys_dep <- TwoSampleMR::harmonise_data(expys, out_ys_dep)

head(dat_ys_dep)
dat_ys_dep$exposure = "Educational Attainment"

res_ys_dep<-mr(dat_ys_dep)
res_ys_dep

#sensitiviy
het_ea_d = mr_heterogeneity(dat_ys_dep)
het_ea_d

pleo_ea_d<- mr_pleiotropy_test(dat_ys_dep)
pleo_ea_d
#odds
res_ys_dep_odds <- generate_odds_ratios(res_ys_dep)
res_ys_dep_odds


#put all data frames into list
ea_dep_list <- list(res_ys_dep_odds, het_ea_d, pleo_ea_d)   

#merge all data frames together
ea_dep_df<- ea_dep_list %>% reduce(full_join, by='id.exposure')
ea_dep_df <- filter(ea_dep_df, method.y == "Inverse variance weighted")
head(ea_dep_df)
#mrpresso
res_presso_ys_dep <-  TwoSampleMR::run_mr_presso(dat_ys_dep, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_ys_dep

#AUTISM
out_ys_au <- TwoSampleMR::extract_outcome_data(expys$SNP, "ieu-a-1185", proxies=FALSE)

dat_ys_au <- TwoSampleMR::harmonise_data(expys, out_ys_au)

head(dat_ys_au)
dat_ys_au$exposure = "Educational Attainment"

res_ys_au <-mr(dat_ys_au)
res_ys_au 
#odds
res_ys_au_odds <- generate_odds_ratios(res_ys_au)
res_ys_au_odds
#sensitiviy
het_ea_au = mr_heterogeneity(dat_ys_au)
het_ea_au

pleo_ea_au<- mr_pleiotropy_test(dat_ys_au)
pleo_ea_au

#put all data frames into list
ea_au_list <- list(res_ys_au_odds, het_ea_au, pleo_ea_au)   

#merge all data frames together
ea_au_df<- ea_au_list %>% reduce(full_join, by='id.exposure')
ea_au_df <- filter(ea_au_df, method.y == "Inverse variance weighted")
head(ea_au_df)

res_presso_ys_au <-  TwoSampleMR::run_mr_presso(dat_ys_au, NbDistribution = 10000, SignifThreshold = 0.05)
res_presso_ys_au

#ADHD
out_ys_adhd <- TwoSampleMR::extract_outcome_data(expys$SNP, "ieu-a-1183", proxies=FALSE)

dat_ys_adhd <- TwoSampleMR::harmonise_data(expys, out_ys_adhd)

head(dat_ys_adhd)
dat_ys_adhd$exposure = "Educational Attainment"

res_ea_adhd <-mr(dat_ys_adhd)
res_ea_adhd 

#odds
res_ys_ad_odds <- generate_odds_ratios(res_ea_adhd)
res_ys_ad_odds

#sensitiviy
het_ea_ad = mr_heterogeneity(dat_ys_adhd)
het_ea_ad

pleo_ea_ad<- mr_pleiotropy_test(dat_ys_adhd)
pleo_ea_ad

#put all data frames into list
ea_ad_list <- list(res_ys_ad_odds, het_ea_ad, pleo_ea_ad)   

#merge all data frames together
ea_ad_df<- ea_ad_list %>% reduce(full_join, by='id.exposure')
ea_ad_df <- filter(ea_ad_df, method.y == "Inverse variance weighted")
head(ea_ad_df)

res_presso_ea_adhd <-  TwoSampleMR::run_mr_presso(dat_ys_adhd, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_ea_adhd

#combino todas los dataframes de todos los resultados income to Mental disorders
EA_mentaldisorders <- rbind(ea_scz_df,ea_bip_df, ea_dep_df, ea_au_df, ea_ad_df)
write.xlsx(EA_mentaldisorders, "UnivariantMR_EA_MentalD.xlsx")

#MULTIVARIANT MR with TWOSAMPLE + MVMR PACKAGE

#hago MVMR INCOME "ukb-b-7408" + EA(LEE) ieu-a-1239 Con TWOSAMPLEMR
#primero conseguimos los exposures combinados

id_exposure <- c("ukb-b-7408", "ieu-a-1239")
id_outcome_scz <- "ieu-a-22"

exposure_dat <- mv_extract_exposures(id_exposure)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, id_outcome_scz)
head(exposure_dat)
mvdat <- mv_harmonise_data(exposure_dat, outcome_dat)
head(mvdat)
library (plyr)

res_MV <- mv_multiple(mvdat)
res_MV

library (plyr)
res_df <- ldply (res_MV, data.frame)
res_inc_scz_mvmr <- generate_odds_ratios(res_df)
res_inc_scz_mvmr


#hago un multivariant metiendo tambien intelligence ebi-a-GCST006250

id_exposure <- c("ukb-b-7408", "ieu-a-1239", "ebi-a-GCST006250")
id_outcome_scz <- "ieu-a-22"

exposure_dat <- mv_extract_exposures(id_exposure)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, id_outcome_scz)
head(exposure_dat)
mvdat_i <- mv_harmonise_data(exposure_dat, outcome_dat)
head(mvdat_i)
library (plyr)

res_MV_i <- mv_multiple(mvdat_i)
res_MV_i

library (plyr)
res_df_i <- ldply (res_MV_i, data.frame)
res_inc_scz_mvmr_i <- generate_odds_ratios(res_df_i)
res_inc_scz_mvmr_i


#la mismma prueba pero con bipolar 
id_outcome_bip<- "ieu-b-41"
mvdat_bip <- mv_harmonise_data(exposure_dat, outcome_bip)

res_mv_bip_i <- mv_multiple(mvdat_bip)
res_mv_bip_i

#genero odds
res_mv_bip_i <- ldply (res_mv_bip_i, data.frame)
res_inc_bip_mvmr <- generate_odds_ratios(res_mv_bip2)
res_inc_bip_mvmr


#MULTVARIANT WITH MVMR PACKAGE
#INCOME AND EA TO SCZ
id_exposure <- c("ukb-b-7408", "ieu-a-1239")
id_outcome_scz <- "ieu-a-22"
exposure_dat <- mv_extract_exposures(id_exposure)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, id_outcome_scz)
head(exposure_dat)
mvdat <- mv_harmonise_data(exposure_dat, outcome_dat)
#Nos proporciona una lista de 8 con nested columns exposure.beta (double)
mvdat

#mapea para quitar los doubles
dt_list <- map(mvdat, as.data.table)
# Convert nested list to data frame by column
my_list_data_cbind <- as.data.frame(do.call(cbind,dt_list))
class(my_list_data_cbind)
dat <-my_list_data_cbind[,1:9]
#hago un df con mejores nombres
colnames(dat) <- c("EA_beta", "income_beta", "P_ea", "P_income", "EA_se", "income_se", "scz_beta", "P_scz", "scz_se")
head(dat)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat[,c("EA_beta", "income_beta")])
bxse <- as.matrix(dat[,c("EA_se", "income_se")])
mrmv <-mr_mvinput(bx = bx,
                  bxse = bxse,
                  by = dat$scz_beta,
                  byse = dat$scz_se)

mrvm <- mrmvinput_to_mvmr_format(mrmv)
mrvm 
class(mrvm)

#si distitnas muestras
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)

Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])

sres2 <- strength_mvmr(r_input = mrmv, gencov = Xcovmat)

pres <- pleiotropy_mvmr(r_input = mrmv, gencov = Xcovmat)

res <- ivw_mvmr(r_input = mrmv)
res
res1 <- qhet_mvmr(mrmv, mvmrcovmatrix, CI = T, iterations = 1000)
res1 
res_het_odd <- mutate(res1, OR = exp(as.numeric("Effect Estimates")))
res_het_odd <-  exp(as.numeric(unlist(res_het_odd)))
res_het_odd

#MVMR BIPOLAR
id_outcome_bip<- "ieu-b-41"
outcome_bip <- extract_outcome_data(exposure_dat$SNP, id_outcome_bip)

mvdat_bip <- mv_harmonise_data(exposure_dat, outcome_bip)

res_mv_bip <- mv_multiple(mvdat_bip)
res_mv_bip

#genero odds
res_mv_bip2 <- ldply (res_mv_bip, data.frame)
res_inc_bip_mvmr <- generate_odds_ratios(res_mv_bip2)
res_inc_bip_mvmr

#MVMR PACKAGE BIPOLAR
#MVMR BIPOLAR
id_outcome_bip<- "ieu-b-41"
outcome_bip <- extract_outcome_data(exposure_dat$SNP, id_outcome_bip)

mvdat_bip <- mv_harmonise_data(exposure_dat, outcome_bip)

#mapea para quitar los doubles
bip_list <- map(mvdat_bip, as.data.table)
# Convert nested list to data frame by column
bip_mvmr <- as.data.frame(do.call(cbind,bip_list))
class(bip_mvmr)
dat_bip <-bip_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_bip) <- c("EA_beta", "income_beta", "P_ea", "P_income", "EA_se", "income_se", "bip_beta", "P_bip", "bip_se")
head(dat_bip)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_bip[,c("EA_beta", "income_beta")])
bxse <- as.matrix(dat_bip[,c("EA_se", "income_se")])
mrmv_bip <-mr_mvinput(bx = bx,
                  bxse = bxse,
                  by = dat_bip$bip_beta,
                  byse = dat_bip$bip_se)

mrmv_bip <- mrmvinput_to_mvmr_format(mrmv_bip)
class(mrmv_bip)
mrmv_bip 

#si distitnas muestras
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures
mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrmv_bip[,6:7])
Xcovmat
#strenght calculation
sres_bip <- strength_mvmr(r_input = mrmv_bip, gencov = Xcovmat)
sres_bip
#het_mvmr
pres_bip <- pleiotropy_mvmr(r_input = mrmv_bip, gencov = Xcovmat)
pres_bip 
#res
res_bip <- ivw_mvmr(r_input = mrmv_bip)
res_bip
#resultado controlando heterogeneicidad 
res_het_bip <- qhet_mvmr(mrmv_bip, mvmrcovmatrix, CI = T, iterations = 1000)
res_het_bip 

#MVMR Depresion
id_outcome_dep<- "ieu-b-102"
outcome_dep <- extract_outcome_data(exposure_dat$SNP, id_outcome_dep)

mvdat_dep <- mv_harmonise_data(exposure_dat, outcome_dep)

res_mv_dep <- mv_multiple(mvdat_dep)
res_mv_dep

res_mv_dep2 <- ldply (res_mv_dep, data.frame)
res_mv_dep_odd <- generate_odds_ratios(res_mv_dep2)
res_mv_dep_odd

#MVMR PACKAGE DEPRESSION
#MVMR DEP
id_outcome_dep<- "ieu-b-102"
outcome_dep <- extract_outcome_data(exposure_dat$SNP, id_outcome_dep)

mvdat_dep <- mv_harmonise_data(exposure_dat, outcome_dep)

#mapea para quitar los doubles
dep_list <- map(mvdat_dep, as.data.table)
# Convert nested list to data frame by column
dep_mvmr <- as.data.frame(do.call(cbind,dep_list))
class(dep_mvmr)
dat_dep <-dep_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_dep) <- c("EA_beta", "income_beta", "P_ea", "P_income", "EA_se", "income_se", "dep_beta", "p_dep", "dep_se")
head(dat_dep)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_dep[,c("EA_beta", "income_beta")])
bxse <- as.matrix(dat_dep[,c("EA_se", "income_se")])
mrmv_dep <-mr_mvinput(bx = bx,
                      bxse = bxse,
                      by = dat_dep$dep_beta,
                      byse = dat_dep$dep_se)

mrmv_dep <- mrmvinput_to_mvmr_format(mrmv_dep)
class(mrmv_dep)

#si distitnas muestras
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures
mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrmv_dep[,6:7])

#strenght calculation
sres_dep <- strength_mvmr(r_input = mrmv_dep, gencov = Xcovmat)
sres_dep
#het_mvmr
pres_dep <- pleiotropy_mvmr(r_input = mrmv_dep, gencov = Xcovmat)
pres_dep
#res
res_dep <- ivw_mvmr(r_input = mrmv_dep)

#resultado controlando heterogeneicidad 
res_het_dep <- qhet_mvmr(mrmv_dep, mvmrcovmatrix, CI = T, iterations = 1000)
res_het_dep 


#MVMR ASD
id_outcome_asd<- "ieu-a-1185"
outcome_asd <- extract_outcome_data(exposure_dat$SNP, id_outcome_asd)

mvdat_asd <- mv_harmonise_data(exposure_dat, outcome_asd)

res_mv_asd <- mv_multiple(mvdat_asd)
res_mv_asd

res_mv_asd2 <- ldply (res_mv_asd, data.frame)
res_mv_asd_odd <- generate_odds_ratios(res_mv_asd2)
res_mv_asd_odd

#MVMR PACKAGE ASD
#MVMR 
id_outcome_asd<- "ieu-a-1185"
outcome_asd <- extract_outcome_data(exposure_dat$SNP, id_outcome_asd)

mvdat_asd <- mv_harmonise_data(exposure_dat, outcome_asd)

#mapea para quitar los doubles
asd_list <- map(mvdat_asd, as.data.table)
# Convert nested list to data frame by column
asd_mvmr <- as.data.frame(do.call(cbind,asd_list))
class(asd_mvmr)
dat_asd <-asd_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_asd) <- c("EA_beta", "income_beta", "P_ea", "P_income", "EA_se", "income_se", "asd_beta", "p_asd", "asd_se")
head(dat_asd)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_asd[,c("EA_beta", "income_beta")])
bxse <- as.matrix(dat_asd[,c("EA_se", "income_se")])
mrmv_asd <-mr_mvinput(bx = bx,
                      bxse = bxse,
                      by = dat_asd$asd_beta,
                      byse = dat_asd$asd_se)

mrmv_asd <- mrmvinput_to_mvmr_format(mrmv_asd)
class(mrmv_asd)

#si distitnas muestras
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures
mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrmv_asd[,6:7])

#strenght calculation
sres_asd <- strength_mvmr(r_input = mrmv_asd, gencov = Xcovmat)
sres_asd
#het_mvmr
pres_asd <- pleiotropy_mvmr(r_input = mrmv_asd, gencov = Xcovmat)
pres_asd
#res
res_asd <- ivw_mvmr(r_input = mrmv_asd)
res_asd
#resultado controlando heterogeneicidad 
res_het_asd <- qhet_mvmr(mrmv_asd, mvmrcovmatrix, CI = T, iterations = 1000)
res_het_asd 

#MVMR ADHD
id_outcome_adhd<- "ieu-a-1183"
outcome_adhd <- extract_outcome_data(exposure_dat$SNP, id_outcome_adhd)

mvdat_adhd <- mv_harmonise_data(exposure_dat, outcome_adhd)

res_mv_adhd <- mv_multiple(mvdat_adhd)
res_mv_adhd

res_mv_adhd2 <- ldply (res_mv_adhd, data.frame)
res_mv_adhd_odd <- generate_odds_ratios(res_mv_adhd2)
res_mv_adhd_odd

#MVMR package adhd
id_outcome_adhd<- "ieu-a-1183"
outcome_adhd <- extract_outcome_data(exposure_dat$SNP, id_outcome_adhd)

mvdat_adhd <- mv_harmonise_data(exposure_dat, outcome_adhd)

#mapea para quitar los doubles
adhd_list <- map(mvdat_adhd, as.data.table)
# Convert nested list to data frame by column
adhd_mvmr <- as.data.frame(do.call(cbind,adhd_list))
class(adhd_mvmr)
dat_adhd <-adhd_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_adhd) <- c("EA_beta", "income_beta", "P_ea", "P_income", "EA_se", "income_se", "adhd_beta", "p_adhd", "adhd_se")
head(dat_adhd)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_adhd[,c("EA_beta", "income_beta")])
bxse <- as.matrix(dat_adhd[,c("EA_se", "income_se")])
mrmv_adhd <-mr_mvinput(bx = bx,
                      bxse = bxse,
                      by = dat_adhd$adhd_beta,
                      byse = dat_adhd$adhd_se)

mrmv_adhd <- mrmvinput_to_mvmr_format(mrmv_adhd)
class(mrmv_adhd)

#si distitnas muestras
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures
mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrmv_adhd[,6:7])

#strenght calculation
sres_adhd <- strength_mvmr(r_input = mrmv_adhd, gencov = Xcovmat)
sres_adhd
#het_mvmr
pres_adhd <- pleiotropy_mvmr(r_input = mrmv_adhd, gencov = Xcovmat)
pres_adhd
#res
res_adhd <- ivw_mvmr(r_input = mrmv_adhd)
res_adhd
#resultado controlando heterogeneicidad 
res_het_adhd <- qhet_mvmr(mrmv_adhd, mvmrcovmatrix, CI = T, iterations = 1000)
res_het_adhd 






# UNIVARIANT MR DISORDERS VS INCOME ukb-b-7408 ##################################3
#INVERSA

#inversa scz
#scz vs income

expscz <- TwoSampleMR::extract_instruments("ieu-a-22")
outin_s <- TwoSampleMR::extract_outcome_data(expscz$SNP, "ukb-b-7408", proxies=FALSE)

#calculo F statistic
F_scz <- qf(expscz$pval.exposure, df1=1, df2=expscz$samplesize.exposure-1, lower.tail=FALSE)
F_scz 
range(F_scz)
# Harmonise the exposure and outcome data
dat_scz_in <- TwoSampleMR::harmonise_data(expscz, outin_s)
# Perform MR
ress_inc <- TwoSampleMR::mr(dat_scz_in)
ress_inc
#odds
res_scz_odds <- generate_odds_ratios(ress_inc)
res_scz_odds

het_scz = mr_heterogeneity(dat_scz_in)
het_scz

pleo_scz<- mr_pleiotropy_test(dat_scz_in)
pleo_scz

#MRPRESSO
res_presso_scz_inc <-  TwoSampleMR::run_mr_presso(dat_scz_in, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_scz_inc

#how to represent burgess say for binary exposure:causal estimate represents the average change in the outcome per 2.72-fold increase 
# It may be more interpretable to think instead about the average change in the outcome per doubling (2-fold increase)
# This can be obtained by multiplying the causal estimate by 0.693 (= loge 2) in de logs_odds
#income SD in uk biobank £33181

#por tanto hacemos la multiplicacion para representarlo
res_scz_odds_pounds <-mutate(res_scz_odds,
                             pounds = b*0.693*33181,
                             lo_ci_p= lo_ci*0.693*33181,
                             up_ci_p = up_ci*0.693*33181
)
res_scz_odds_pounds

#put all data frames into list
sczin_list <- list(res_scz_odds_pounds, het_scz, pleo_scz)   
sczin_list
#merge all data frames together
scz_in_df<- sczin_list %>% reduce(full_join, by='id.exposure')

scz_in_df <- filter(scz_in_df, method.y == "Inverse variance weighted")
scz_in_df

#MULTIVARIANT
#SCZ controlada por EA en income

id_exposure_scz <- c("ieu-a-1239", "ieu-a-22")
id_outcome_in <- "ukb-b-7408"

#primero conseguimos los exposures combinados
exposure_dat_scz <- mv_extract_exposures(id_exposure_scz)
outcome_dat_scz_in <- extract_outcome_data(exposure_dat_scz$SNP, id_outcome_in)

mvdat_scz_in <- mv_harmonise_data(exposure_dat_scz, outcome_dat_scz_in)

res_scz_in <- mv_multiple(mvdat_scz_in)
res_scz_in

res_mv_scz_in <- ldply (res_scz_in, data.frame)
res_mv_scz_in_odd <- generate_odds_ratios(res_mv_scz_in)
res_mv_scz_in_odd
#in pounds
mv_scz_odds_pounds <- res_mv_scz_in_odd[2,]

mv_scz_odds_pounds <-mutate(mv_scz_odds_pounds,
                             pounds = b*0.693*33181,
                             lo_ci_p= lo_ci*0.693*33181,
                             up_ci_p = up_ci*0.693*33181
)
mv_scz_odds_pounds

#lo hacemos ahora con MVMR PACKAGE
#MVMR SCZ INDEPENDIENT OF EA TO INCOME
#MVMR SCZ

id_exposure_scz <- c("ieu-a-1239", "ieu-a-22")
id_outcome_in <- "ukb-b-7408"

#primero conseguimos los exposures combinados
exposure_dat_scz <- mv_extract_exposures(id_exposure_scz)
outcome_dat_scz_in <- extract_outcome_data(exposure_dat_scz$SNP, id_outcome_in)

mvdat_scz_in <- mv_harmonise_data(exposure_dat_scz, outcome_dat_scz_in)

#mapea para quitar los doubles
sczin_list <- map(mvdat_scz_in, as.data.table)
# Convert nested list to data frame by column
sczin_mvmr <- as.data.frame(do.call(cbind,sczin_list))
class(sczin_mvmr)
head(sczin_mvmr)
dat_sczin <-sczin_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_sczin) <- c("EA_beta", "scz_beta", "P_ea", "P_scz", "EA_se", "scz_se", "income_beta", "p_income", "income_se")
head(dat_sczin)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_sczin[,c("EA_beta", "scz_beta")])
bxse <- as.matrix(dat_sczin[,c("EA_se", "scz_beta")])
mrmv_sczin <-mr_mvinput(bx = bx,
                      bxse = bxse,
                      by = dat_sczin$income_beta,
                      byse = dat_sczin$income_se)

mrmv_sczin <- mrmvinput_to_mvmr_format(mrmv_sczin)
class(mrmv_sczin)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_scizn <- strength_mvmr(r_input = mrmv_sczin, gencov = 0)
sres_scizn
#het_mvmr
pres_sczin <- pleiotropy_mvmr(r_input = mrmv_sczin, gencov = 0)
pres_sczin
#res
res_sczin <- ivw_mvmr(r_input = mrmv_sczin)
res_sczin 
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_sczin <- qhet_mvmr(mrmv_sczin, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_sczin 

#inversa bip
#bip  vs income

expb <- TwoSampleMR::extract_instruments("ieu-b-41")
out_bip_income <- TwoSampleMR::extract_outcome_data(expb$SNP, "ukb-b-7408", proxies=FALSE)
#fstatistic
F_bip <- qf(expb$pval.exposure, df1=1, df2=expb$samplesize.exposure-1, lower.tail=FALSE)
F_bip 
range(F_bip)
# Harmonise the exposure and outcome data
dat_bip_in <- TwoSampleMR::harmonise_data(expb, out_bip_income)
# Perform MR
resb_income <- TwoSampleMR::mr(dat_bip_in)
resb_income 
het_b = mr_heterogeneity(dat_bip_in)
het_b

pleo_bip<- mr_pleiotropy_test(dat_bip_in)
pleo_bip

#odds
res_bip_odds <- generate_odds_ratios(resb_income)
res_bip_odds
#pounds
res_bip_odds_pounds <-mutate(res_bip_odds,
                             pounds = b*0.693*33181,
                             lo_ci_p= lo_ci*0.693*33181,
                             up_ci_p = up_ci*0.693*33181
)
res_bip_odds_pounds

#MRPRESSO
res_presso_bip_inc <-  TwoSampleMR::run_mr_presso(dat_bip_in, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_bip_inc


#put all data frames into list
bipin_list <- list(res_bip_odds_pounds, het_b, pleo_bip)   
bipin_list
#merge all data frames together
bip_in_df<- bipin_list %>% reduce(full_join, by='id.exposure')

bip_in_df <- filter(bip_in_df, method.y == "Inverse variance weighted")
bip_in_df

#MULTIVARIANT
#bip controlada por EA en income

id_exposure_bip <- c("ieu-a-1239", "ieu-b-41")
id_outcome_in <- "ukb-b-7408"

#primero conseguimos los exposures combinados
exposure_dat_bip <- mv_extract_exposures(id_exposure_bip)
outcome_dat_bip_in <- extract_outcome_data(exposure_dat_bip$SNP, id_outcome_in)

mvdat_bip_in <- mv_harmonise_data(exposure_dat_bip, outcome_dat_bip_in)

res_bip_in <- mv_multiple(mvdat_bip_in)
res_bip_in

#ODDS
res_mv_bip_in <- ldply (res_bip_in, data.frame)
res_mv_bip_in_odd <- generate_odds_ratios(res_mv_bip_in)
res_mv_bip_in_odd
#in pounds
mv_bip_odds_pounds <- res_mv_bip_in_odd[2,]

mv_bip_odds_pounds <-mutate(mv_bip_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_bip_odds_pounds
#con el MVMR PACKAGE

#mapea para quitar los doubles
bip_in_list <- map(mvdat_bip_in, as.data.table)
# Convert nested list to data frame by column
bip_in_mvmr <- as.data.frame(do.call(cbind,bip_in_list))
class(bip_in_mvmr)
head(bip_in_mvmr)
dat_bip_in <-bip_in_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_bip_in) <- c("EA_beta", "bip_beta", "P_ea", "P_bip", "EA_se", "bip_se", "income_beta", "p_income", "income_se")
head(dat_bip_in)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_bip_in[,c("EA_beta", "bip_beta")])
bxse <- as.matrix(dat_bip_in[,c("EA_se", "bip_beta")])
mrmv_bip_in <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_bip_in$income_beta,
                        byse = dat_bip_in$income_se)

mrmv_bip_in <- mrmvinput_to_mvmr_format(mrmv_bip_in)
class(mrmv_bip_in)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_bip_in <- strength_mvmr(r_input = mrmv_bip_in, gencov = 0)
sres_bip_in
#het_mvmr
pres_bip_in <- pleiotropy_mvmr(r_input = mrmv_bip_in, gencov = 0)
pres_bip_in
#res
res_bip_in <- ivw_mvmr(r_input = mrmv_bip_in)
res_bip_in
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_bip_in <- qhet_mvmr(mrmv_bip_in, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_bip_in 


#DEP vs income
expd <- TwoSampleMR::extract_instruments("ieu-b-102")
out_dep_income <- TwoSampleMR::extract_outcome_data(expd$SNP,"ukb-b-7408", proxies=FALSE)
#f statistic
F_dep <- qf(expd$pval.exposure, df1=1, df2=500199-1, lower.tail=FALSE)
F_dep 
range(F_dep)
# Harmonise the exposure and outcome data
dat_dep_in <- TwoSampleMR::harmonise_data(expd, out_dep_income)
# Perform MR
res_dep_in <- TwoSampleMR::mr(dat_dep_in)
res_dep_in

het_d = mr_heterogeneity(dat_dep_in)
het_d

pleo_d<- mr_pleiotropy_test(dat_dep_in)
pleo_d

#odds
res_dep_odds <- generate_odds_ratios(res_dep_in)
res_dep_odds
#pounds
res_dep_odds_pounds <-mutate(res_dep_odds,
                             pounds = b*0.693*33181,
                             lo_ci_p= lo_ci*0.693*33181,
                             up_ci_p = up_ci*0.693*33181
)
res_dep_odds_pounds

#MRPRESSO
res_presso_dep_inc <-  TwoSampleMR::run_mr_presso(dat_dep_in, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_dep_inc

#put all data frames into list
depin_list <- list(res_dep_odds_pounds, het_d, pleo_d)   
depin_list
#merge all data frames together
dep_in_df<- depin_list %>% reduce(full_join, by='id.exposure')

dep_in_df <- filter(dep_in_df, method.y == "Inverse variance weighted")
dep_in_df
#Depresion controlada por EA en income

id_exposure_dep <- c("ieu-a-1239", "ieu-b-102")
id_outcome_dep <- "ukb-b-7408"

#primero conseguimos los exposures combinados
exposure_dat_dep <- mv_extract_exposures(id_exposure_dep)
outcome_dat_dep_in <- extract_outcome_data(exposure_dat_dep$SNP, id_outcome_in)

mvdat_dep_in <- mv_harmonise_data(exposure_dat_dep, outcome_dat_dep_in)

mv_dep_in <- mv_multiple(mvdat_dep_in)
mv_dep_in

#ODDS
res_mv_dep_in <- ldply (mv_dep_in, data.frame)
res_mv_dep_in_odd <- generate_odds_ratios(res_mv_dep_in)
res_mv_dep_in_odd
#in pounds
mv_dep_odds_pounds <- res_mv_dep_in_odd[2,]

mv_dep_odds_pounds <-mutate(mv_dep_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_dep_odds_pounds

#MVMR PACKAGE DEP
#mapea para quitar los doubles
dep_in_list <- map(mvdat_dep_in, as.data.table)
# Convert nested list to data frame by column
dep_in_mvmr <- as.data.frame(do.call(cbind,dep_in_list))
class(dep_in_mvmr)
head(dep_in_mvmr)
dat_dep_in <-dep_in_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_dep_in) <- c("EA_beta", "dep_beta", "P_ea", "P_dep", "EA_se", "dep_se", "income_beta", "p_income", "income_se")
head(dat_dep_in)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_dep_in[,c("EA_beta", "dep_beta")])
bxse <- as.matrix(dat_dep_in[,c("EA_se", "dep_beta")])
mrmv_dep_in <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_dep_in$income_beta,
                        byse = dat_dep_in$income_se)

mrmv_dep_in <- mrmvinput_to_mvmr_format(mrmv_dep_in)
class(mrmv_dep_in)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_dep_in <- strength_mvmr(r_input = mrmv_dep_in, gencov = 0)
sres_dep_in
#het_mvmr
pres_dep_in <- pleiotropy_mvmr(r_input = mrmv_dep_in, gencov = 0)
pres_dep_in
#res
res_dep_in <- ivw_mvmr(r_input = mrmv_dep_in)
res_dep_in
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_dep_in <- qhet_mvmr(mrmv_dep_in, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_dep_in 


#asd income
expau <- TwoSampleMR::extract_instruments("ieu-a-1185", p1 = 5e-07)
#calculo F statistic
F_au <- qf(expau$pval.exposure, df1=1, df2= expau$samplesize.exposure-1, lower.tail=FALSE)
F_au
mean(F_au)
range(F_au)

out_au_in<- TwoSampleMR::extract_outcome_data(expau$SNP, "ukb-b-7408", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_au_in <- TwoSampleMR::harmonise_data(expau, out_au_in)
# Perform MR
resau_in <- TwoSampleMR::mr(dat_au_in)
resau_in

het_au = mr_heterogeneity(dat_au_in)
het_au

pleo_au<- mr_pleiotropy_test(dat_au_in)
pleo_au

#odds
res_au_odds <- generate_odds_ratios(resau_in)
res_au_odds
#pounds
res_au_odds_pounds <-mutate(res_au_odds,
                             pounds = b*0.693*33181,
                             lo_ci_p= lo_ci*0.693*33181,
                             up_ci_p = up_ci*0.693*33181
)
res_au_odds_pounds

#MRPRESSO
res_presso_au_inc <-  TwoSampleMR::run_mr_presso(dat_au_in, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_au_inc

#put all data frames into list
auin_list <- list(res_au_odds_pounds, het_au, pleo_au)   
auin_list
#merge all data frames together
au_in_df<- auin_list %>% reduce(full_join, by='id.exposure')

au_in_df <- filter(au_in_df, method.y == "Inverse variance weighted")
au_in_df

#ASD controlada por EA en income

id_exposure_asd <- c("ieu-a-1239", "ieu-a-1185" )
id_outcome_in <- "ukb-b-7408"

#primero conseguimos los exposures combinados
exposure_dat_asd <- mv_extract_exposures(id_exposure_asd)
outcome_dat_asd_in <- extract_outcome_data(exposure_dat_asd$SNP, id_outcome_in)

mvdat_asd_in <- mv_harmonise_data(exposure_dat_asd, outcome_dat_asd_in)

res_asd_in <- mv_multiple(mvdat_asd_in)
res_asd_in

input <- mrmvinput_to_mvmr_format("ieu-a-1239")

#ODDS
res_mv_asd_in <- ldply (res_asd_in, data.frame)
res_mv_asd_in_odd <- generate_odds_ratios(res_mv_asd_in)
res_mv_asd_in_odd
#in pounds
mv_asd_odds_pounds <- res_mv_asd_in_odd[1,]

mv_asd_odds_pounds <-mutate(mv_asd_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_asd_odds_pounds


#MVMR PACKAGE ASD
#mapea para quitar los doubles
asd_in_list <- map(mvdat_asd_in, as.data.table)
# Convert nested list to data frame by column
asd_in_mvmr <- as.data.frame(do.call(cbind,asd_in_list))
class(asd_in_mvmr)
head(asd_in_mvmr)
dat_asd_in <-asd_in_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_asd_in) <- c("EA_beta", "asd_beta", "P_ea", "P_asd", "EA_se", "asd_se", "income_beta", "p_income", "income_se")
head(dat_asd_in)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_asd_in[,c("EA_beta", "asd_beta")])
bxse <- as.matrix(dat_asd_in[,c("EA_se", "asd_beta")])
mrmv_asd_in <-mr_mvinput(bx = bx,
                         bxse = bxse,
                         by = dat_asd_in$income_beta,
                         byse = dat_asd_in$income_se)

mrmv_asd_in <- mrmvinput_to_mvmr_format(mrmv_asd_in)
class(mrmv_asd_in)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_asd_in <- strength_mvmr(r_input = mrmv_asd_in, gencov = 0)

#het_mvmr
pres_asd_in <- pleiotropy_mvmr(r_input = mrmv_asd_in, gencov = 0)
pres_asd_in
#res
res_asd_in <- ivw_mvmr(r_input = mrmv_asd_in)
res_asd_in
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_asd_in <- qhet_mvmr(mrmv_asd_in, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_asd_in 

#inversa adhd
#adhd in financial satisfaction
expadhd <- TwoSampleMR::extract_instruments("ieu-a-1183")

#calculo F statistic
F_ADHD <- qf(expadhd$pval.exposure, df1=1, df2=55374-1, lower.tail=FALSE)

out_adhd_inc <- TwoSampleMR::extract_outcome_data(expadhd$SNP, "ukb-b-7408", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_adhd_in <- TwoSampleMR::harmonise_data(expadhd, out_adhd_inc)
# Perform MR
resadhd_in <- TwoSampleMR::mr(dat_adhd_in)
resadhd_in


het_adhd = mr_heterogeneity(dat_adhd_in)
het_adhd

pleo_adhd<- mr_pleiotropy_test(dat_adhd_in)
pleo_adhd

#odds
res_adhd_odds <- generate_odds_ratios(resadhd_in)
res_adhd_odds
#pounds
res_adhd_odds_pounds <-mutate(res_adhd_odds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
res_adhd_odds_pounds

#MRPRESSO
res_presso_adhd_inc <-  TwoSampleMR::run_mr_presso(dat_adhd_in, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_adhd_inc

#put all data frames into list
adhdin_list <- list(res_adhd_odds_pounds, het_adhd, pleo_adhd)   
adhdin_list
#merge all data frames together
adhd_in_df<- adhdin_list %>% reduce(full_join, by='id.exposure')

adhd_in_df <- filter(adhd_in_df, method.y == "Inverse variance weighted")
adhd_in_df



#genero df de todos los UNIVARIANT MR MENTAL HEALTH TO INCOME
#combino todas los dataframes de todos los resultados  Mental disorders to income
mentaldisorders_income <- rbind(scz_in_df,bip_in_df, dep_in_df, au_in_df, adhd_in_df)
write.xlsx(mentaldisorders_income, "UnivariantMR_MentalD_income.xlsx")

#ADHD controlada por EA en income

id_exposure_adhd <- c("ieu-a-1239", "ieu-a-1183")
id_outcome_in <- "ukb-b-7408"

#primero conseguimos los exposures combinados
exposure_dat_adhd <- mv_extract_exposures(id_exposure_adhd)
outcome_dat_adhd_in <- extract_outcome_data(exposure_dat_adhd$SNP, id_outcome_in)

mvdat_adhd_in <- mv_harmonise_data(exposure_dat_adhd, outcome_dat_adhd_in)

res_adhd_in <- mv_multiple(mvdat_adhd_in)
res_adhd_in

#ODDS
res_mv_adhd_in <- ldply (res_adhd_in, data.frame)
res_mv_adhd_in_odd <- generate_odds_ratios(res_mv_adhd_in)
res_mv_adhd_in_odd

#in pounds
res_adhd_in_odds_pounds <- res_mv_adhd_in_odd[1,]

res_adhd_in_odds_pounds <-mutate(res_adhd_in_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
res_adhd_in_odds_pounds


#MVMR PACKAGE ADHD
#mapea para quitar los doubles
adhd_in_list <- map(mvdat_adhd_in, as.data.table)
# Convert nested list to data frame by column
adhd_in_mvmr <- as.data.frame(do.call(cbind,adhd_in_list))
class(adhd_in_mvmr)
head(adhd_in_mvmr)
dat_adhd_in <-adhd_in_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_adhd_in) <- c("EA_beta", "adhd_beta", "P_ea", "P_adhd", "EA_se", "adhd_se", "income_beta", "p_income", "income_se")
head(dat_adhd_in)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_adhd_in[,c("EA_beta", "adhd_beta")])
bxse <- as.matrix(dat_adhd_in[,c("EA_se", "adhd_beta")])
mrmv_adhd_in <-mr_mvinput(bx = bx,
                         bxse = bxse,
                         by = dat_adhd_in$income_beta,
                         byse = dat_adhd_in$income_se)

mrmv_adhd_in <- mrmvinput_to_mvmr_format(mrmv_adhd_in)
class(mrmv_adhd_in)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_adhd_in <- strength_mvmr(r_input = mrmv_adhd_in, gencov = 0)
sres_adhd_in 
#het_mvmr
pres_adhd_in <- pleiotropy_mvmr(r_input = mrmv_adhd_in, gencov = 0)
pres_adhd_in 
#res
res_adhd_in <- ivw_mvmr(r_input = mrmv_adhd_in)
res_adhd_in
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_adhd_in <- qhet_mvmr(mrmv_adhd_in, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_adhd_in 






#INVERSA EDUCATIONAL ATTAINMENT


#inversa scz
#scz vs income
expscz <- TwoSampleMR::extract_instruments("ieu-a-22")
out_ea<- TwoSampleMR::extract_outcome_data(expscz$SNP, "ieu-a-1239", proxies=FALSE)


# Harmonise the exposure and outcome data
dat_scz_ea <- TwoSampleMR::harmonise_data(expscz, out_ea)
# Perform MR
res_scz_ea <- TwoSampleMR::mr(dat_scz_ea)
res_scz_ea


#odds
res_scz_ea_odds <- generate_odds_ratios(res_scz_ea)
res_scz_ea_odds

het_scz_ea = mr_heterogeneity(dat_scz_ea)
het_scz_ea

pleo_scz_ea<- mr_pleiotropy_test(dat_scz_ea)
pleo_scz_ea

#how to represent burgess say for binary exposure:causal estimate represents the average change in the outcome per 2.72-fold increase 
# It may be more interpretable to think instead about the average change in the outcome per doubling (2-fold increase)
# This can be obtained by multiplying the causal estimate by 0.693 (= loge 2) in de logs_odds
#EA SD  4.2 YEARS= 50 MONTHS

#por tanto hacemos la multiplicacion para representarlo
res_scz_ea_odds_months <-mutate(res_scz_ea_odds,
                                 months_ea = b*0.693*50,
                                 lo_ci_m= lo_ci*0.693*50,
                                 up_ci_m = up_ci*0.693*50
)
res_scz_ea_odds_months

#put all data frames into list
scz_ea_list <- list(res_scz_ea_odds_months, het_scz_ea, pleo_scz_ea)   
scz_ea_list
#merge all data frames together
scz_ea_df<- scz_ea_list %>% reduce(full_join, by='id.exposure')

scz_ea_df <- filter(scz_ea_df, method.y == "Inverse variance weighted")
scz_ea_df

#presso
res_presso_scz_ea <-  TwoSampleMR::run_mr_presso(dat_scz_ea, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_scz_ea

#MULTIVARIANT
#SCZ controlada por income ukb-b-7408 en EA

id_exposure_scz_ea <- c("ukb-b-7408", "ieu-a-22")
id_outcome_ea <- "ieu-a-1239"

#primero conseguimos los exposures combinados
exposure_dat_scz_ea <- mv_extract_exposures(id_exposure_scz_ea)
outcome_dat_scz_ea <- extract_outcome_data(exposure_dat_scz_ea$SNP, id_outcome_ea)

mvdat_scz_ea <- mv_harmonise_data(exposure_dat_scz_ea, outcome_dat_scz_ea)

res_scz_ea <- mv_multiple(mvdat_scz_ea)
res_scz_ea

res_mv_scz_ea <- ldply (res_scz_ea, data.frame)
res_mv_scz_ea_odd <- generate_odds_ratios(res_mv_scz_ea)
res_mv_scz_ea_odd
#in pounds
mv_scz_odds_pounds <- res_mv_scz_ea_odd[2,]

mv_scz_odds_pounds <-mutate(mv_scz_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_scz_odds_pounds


#vamos a controlar tambien por intellgence ebi-a-GCST006250

id_exposure_scz_ea_noi <- c("ukb-b-7408", "ebi-a-GCST006250", "ieu-a-22")
id_outcome_ea <- "ieu-a-1239"

#primero conseguimos los exposures combinados
exposure_dat_scz_ea_noi <- mv_extract_exposures(id_exposure_scz_ea_noi)
outcome_dat_scz_ea <- extract_outcome_data(exposure_dat_scz_ea_noi$SNP, id_outcome_ea)

mvdat_scz_ea_noi <- mv_harmonise_data(exposure_dat_scz_ea_noi, outcome_dat_scz_ea)

res_scz_ea_noi <- mv_multiple(mvdat_scz_ea_noi)
res_scz_ea_noi


#lo hacemos ahora con MVMR PACKAGE
#MVMR SCZ INDEPENDIENT OF income TO ea
#MVMR SCZ

id_exposure_scz_ea <- c("ukb-b-7408", "ieu-a-22")
id_outcome_ea <- "ieu-a-1239"
#primero conseguimos los exposures combinados
exposure_dat_scz <- mv_extract_exposures(id_exposure_scz_ea)
outcome_dat_scz_ea <- extract_outcome_data(exposure_dat_scz$SNP, id_outcome_ea)

mvdat_scz_ea <- mv_harmonise_data(exposure_dat_scz, outcome_dat_scz_ea)

#mapea para quitar los doubles
sczea_list <- map(mvdat_scz_ea, as.data.table)
# Convert nested list to data frame by column
sczea_mvmr <- as.data.frame(do.call(cbind,sczea_list))
class(sczea_mvmr)
head(sczea_mvmr)
dat_sczea <-sczea_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_sczea) <- c("income_beta", "scz_beta", "P_income", "P_scz", "income_se", "scz_se", "ea_beta", "p_ea", "ea_se")
head(dat_sczea)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_sczea[,c("income_beta", "scz_beta")])
bxse <- as.matrix(dat_sczea[,c("income_se", "scz_beta")])
mrmv_sczea <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_sczea$ea_beta,
                        byse = dat_sczea$ea_se)

mrmv_sczea <- mrmvinput_to_mvmr_format(mrmv_sczea)
class(mrmv_sczea)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_sczea <- strength_mvmr(r_input = mrmv_sczea, gencov = 0)
sres_sczea
#het_mvmr
pres_sczea <- pleiotropy_mvmr(r_input = mrmv_sczea, gencov = 0)
pres_sczea
#res
res_sczea <- ivw_mvmr(r_input = mrmv_sczea)
res_sczea 
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_sczea <- qhet_mvmr(mrmv_sczea, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_sczea 

#inversa bip
#bip  vs ea
expb <- TwoSampleMR::extract_instruments("ieu-b-41")
out_ea<- TwoSampleMR::extract_outcome_data(expb$SNP, "ieu-a-1239", proxies=FALSE)

# Harmonise the exposure and outcome data
dat_bip_ea <- TwoSampleMR::harmonise_data(expb, out_ea)
# Perform MR
res_bip_ea <- TwoSampleMR::mr(dat_bip_ea)
res_bip_ea


#odds
res_bip_ea_odds <- generate_odds_ratios(res_bip_ea)
res_bip_ea_odds

het_bip_ea = mr_heterogeneity(dat_bip_ea)
het_bip_ea

pleo_bip_ea<- mr_pleiotropy_test(dat_bip_ea)
pleo_bip_ea

#how to represent burgess say for binary exposure:causal estimate represents the average change in the outcome per 2.72-fold increase 
# It may be more interpretable to think instead about the average change in the outcome per doubling (2-fold increase)
# This can be obtained by multiplying the causal estimate by 0.693 (= loge 2) in de logs_odds
#EA SD  4.2 YEARS= 50 MONTHS

#por tanto hacemos la multiplicacion para representarlo
res_bip_ea_odds_months <-mutate(res_bip_ea_odds,
                                months_ea = b*0.693*50,
                                lo_ci_m= lo_ci*0.693*50,
                                up_ci_m = up_ci*0.693*50
)
res_bip_ea_odds_months

#put all data frames into list
bip_ea_list <- list(res_bip_ea_odds_months, het_bip_ea, pleo_bip_ea)   
bip_ea_list
#merge all data frames together
bip_ea_df<- bip_ea_list %>% reduce(full_join, by='id.exposure')

bip_ea_df <- filter(bip_ea_df, method.y == "Inverse variance weighted")
bip_ea_df

#mrpresso
res_presso_bip_ea <-  TwoSampleMR::run_mr_presso(dat_bip_ea, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_bip_ea

#MULTIVARIANT
#bipolar controlada por income ukb-b-7408 en EA

id_exposure_bip_ea <- c("ukb-b-7408", "ieu-b-41")
id_outcome_ea <- "ieu-a-1239"

#primero conseguimos los exposures combinados
exposure_dat_bip_ea <- mv_extract_exposures(id_exposure_bip_ea)
outcome_dat_bip_ea <- extract_outcome_data(exposure_dat_bip_ea$SNP, id_outcome_ea)

mvdat_bip_ea <- mv_harmonise_data(exposure_dat_bip_ea, outcome_dat_bip_ea)

res_bip_ea <- mv_multiple(mvdat_bip_ea)
res_bip_ea

res_mv_bip_ea <- ldply (res_bip_ea, data.frame)
res_mv_bip_ea_odd <- generate_odds_ratios(res_mv_bip_ea)
res_mv_bip_ea_odd
#in pounds
mv_bip_odds_pounds <- res_mv_bip_ea_odd[2,]

mv_bip_odds_pounds <-mutate(mv_bip_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_bip_odds_pounds

#lo hacemos ahora con MVMR PACKAGE
#MVMR bip INDEPENDIENT OF income TO ea
#MVMR bip

id_exposure_bip_ea <- c("ukb-b-7408", "ieu-b-41")
id_outcome_ea <- "ieu-a-1239"
#primero conseguimos los exposures combinados
exposure_dat_bip <- mv_extract_exposures(id_exposure_bip_ea)
outcome_dat_bip_ea <- extract_outcome_data(exposure_dat_bip$SNP, id_outcome_ea)

mvdat_bip_ea <- mv_harmonise_data(exposure_dat_bip, outcome_dat_bip_ea)

#mapea para quitar los doubles
bipea_list <- map(mvdat_bip_ea, as.data.table)
# Convert nested list to data frame by column
bipea_mvmr <- as.data.frame(do.call(cbind,bipea_list))
class(bipea_mvmr)
head(bipea_mvmr)
dat_bipea <-bipea_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_bipea) <- c("income_beta", "bip_beta", "P_income", "P_bip", "income_se", "bip_se", "ea_beta", "p_ea", "ea_se")
head(dat_bipea)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_bipea[,c("income_beta", "bip_beta")])
bxse <- as.matrix(dat_bipea[,c("income_se", "bip_beta")])
mrmv_bipea <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_bipea$ea_beta,
                        byse = dat_bipea$ea_se)

mrmv_bipea <- mrmvinput_to_mvmr_format(mrmv_bipea)
class(mrmv_bipea)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_bipea <- strength_mvmr(r_input = mrmv_bipea, gencov = 0)
sres_bipea
#het_mvmr
pres_bipea <- pleiotropy_mvmr(r_input = mrmv_bipea, gencov = 0)
pres_bipea
#res
res_bipea <- ivw_mvmr(r_input = mrmv_bipea)
res_bipea 
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_bipea <- qhet_mvmr(mrmv_bipea, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_bipea 






#dep vs ea
expd <- TwoSampleMR::extract_instruments("ieu-b-102")
out_ea<- TwoSampleMR::extract_outcome_data(expd$SNP, "ieu-a-1239", proxies=FALSE)


# Harmonise the exposure and outcome data
dat_dep_ea <- TwoSampleMR::harmonise_data(expd, out_ea)
# Perform MR
res_dep_ea <- TwoSampleMR::mr(dat_dep_ea)
res_dep_ea


#odds
res_dep_ea_odds <- generate_odds_ratios(res_dep_ea)
res_dep_ea_odds

het_dep_ea = mr_heterogeneity(dat_dep_ea)
het_dep_ea

pleo_dep_ea<- mr_pleiotropy_test(dat_dep_ea)
pleo_dep_ea

#how to represent burgess say for binary exposure:causal estimate represents the average change in the outcome per 2.72-fold increase 
# It may be more interpretable to think instead about the average change in the outcome per doubling (2-fold increase)
# This can be obtained by multiplying the causal estimate by 0.693 (= loge 2) in de logs_odds
#EA SD  4.2 YEARS= 50 MONTHS

#por tanto hacemos la multiplicacion para representarlo
res_dep_ea_odds_months <-mutate(res_dep_ea_odds,
                                months_ea = b*0.693*50,
                                lo_ci_m= lo_ci*0.693*50,
                                up_ci_m = up_ci*0.693*50
)
res_dep_ea_odds_months

#put all data frames into list
dep_ea_list <- list(res_dep_ea_odds_months, het_dep_ea, pleo_dep_ea)   
dep_ea_list
#merge all data frames together
dep_ea_df<- dep_ea_list %>% reduce(full_join, by='id.exposure')

dep_ea_df <- filter(dep_ea_df, method.y == "Inverse variance weighted")
dep_ea_df

#presso
res_presso_dep_ea <-  TwoSampleMR::run_mr_presso(dat_dep_ea, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_dep_ea

#MULTIVARIANT
#depression controlada por income ukb-b-7408 en EA

id_exposure_dep_ea <- c("ukb-b-7408", "ieu-b-102")
id_outcome_ea <- "ieu-a-1239"

#primero conseguimos los exposures combinados
exposure_dat_dep_ea <- mv_extract_exposures(id_exposure_dep_ea)
outcome_dat_dep_ea <- extract_outcome_data(exposure_dat_dep_ea$SNP, id_outcome_ea)

mvdat_dep_ea <- mv_harmonise_data(exposure_dat_dep_ea, outcome_dat_dep_ea)

res_dep_ea <- mv_multiple(mvdat_dep_ea)
res_dep_ea

res_mv_dep_ea <- ldply (res_dep_ea, data.frame)
res_mv_dep_ea_odd <- generate_odds_ratios(res_mv_dep_ea)
res_mv_dep_ea_odd
#in pounds
mv_dep_odds_pounds <- res_mv_dep_ea_odd[2,]

mv_dep_odds_pounds <-mutate(mv_dep_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_dep_odds_pounds

#lo hacemos ahora con MVMR PACKAGE
#MVMR dep INDEPENDIENT OF income TO ea
#MVMR dep

id_exposure_dep_ea <- c("ukb-b-7408", "ieu-b-102")
id_outcome_ea <- "ieu-a-1239"
#primero conseguimos los exposures combinados
exposure_dat_dep <- mv_extract_exposures(id_exposure_dep_ea)
outcome_dat_dep_ea <- extract_outcome_data(exposure_dat_dep$SNP, id_outcome_ea)

mvdat_dep_ea <- mv_harmonise_data(exposure_dat_dep, outcome_dat_dep_ea)

#mapea para quitar los doubles
depea_list <- map(mvdat_dep_ea, as.data.table)
# Convert nested list to data frame by column
depea_mvmr <- as.data.frame(do.call(cbind,depea_list))
class(depea_mvmr)
head(depea_mvmr)
dat_depea <-depea_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_depea) <- c("income_beta", "dep_beta", "P_income", "P_dep", "income_se", "dep_se", "ea_beta", "p_ea", "ea_se")
head(dat_depea)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_depea[,c("income_beta", "dep_beta")])
bxse <- as.matrix(dat_depea[,c("income_se", "dep_beta")])
mrmv_depea <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_depea$ea_beta,
                        byse = dat_depea$ea_se)

mrmv_depea <- mrmvinput_to_mvmr_format(mrmv_depea)
class(mrmv_depea)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_depea <- strength_mvmr(r_input = mrmv_depea, gencov = 0)
sres_depea
#het_mvmr
pres_depea <- pleiotropy_mvmr(r_input = mrmv_depea, gencov = 0)
pres_depea
#res
res_depea <- ivw_mvmr(r_input = mrmv_depea)
res_depea 
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_depea <- qhet_mvmr(mrmv_depea, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_depea 



#ASD
expau <- TwoSampleMR::extract_instruments("ieu-a-1185", p1 = 5e-07)
out_ea<- TwoSampleMR::extract_outcome_data(expau$SNP, "ieu-a-1239", proxies=FALSE)


# Harmonise the exposure and outcome data
dat_au_ea <- TwoSampleMR::harmonise_data(expau, out_ea)
# Perform MR
resau_ea <- TwoSampleMR::mr(dat_au_ea)
resau_ea

het_au_ea = mr_heterogeneity(dat_au_ea)
het_au_ea

pleo_au_ea<- mr_pleiotropy_test(dat_au_ea)
pleo_au_ea

#odds
res_au_ea_odds <- generate_odds_ratios(resau_ea)
res_au_ea_odds

#presso
res_presso_au_ea <-  TwoSampleMR::run_mr_presso(dat_au_ea, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_au_ea

#how to represent burgess say for binary exposure:causal estimate represents the average change in the outcome per 2.72-fold increase 
# It may be more interpretable to think instead about the average change in the outcome per doubling (2-fold increase)
# This can be obtained by multiplying the causal estimate by 0.693 (= loge 2) in de logs_odds
#EA SD  4.2 YEARS= 50 MONTHS

#por tanto hacemos la multiplicacion para representarlo
res_au_ea_odds_months <-mutate(res_au_ea_odds,
                                 months_ea = b*0.693*50,
                                 lo_ci_m= lo_ci*0.693*50,
                                 up_ci_m = up_ci*0.693*50
)
res_au_ea_odds_months

#put all data frames into list
au_ea_list <- list(res_au_ea_odds_months, het_au_ea, pleo_au_ea)   
au_ea_list
#merge all data frames together
au_ea_df<- au_ea_list %>% reduce(full_join, by='id.exposure')

au_ea_df <- filter(au_ea_df, method.y == "Inverse variance weighted")
au_ea_df


#MULTIVARIANT
#ASD  controlada por income ukb-b-7408 en EA

id_exposure_asd_ea <- c("ukb-b-7408", "ieu-a-1185")
id_outcome_ea <- "ieu-a-1239"

#primero conseguimos los exposures combinados
exposure_dat_asd_ea <- mv_extract_exposures(id_exposure_asd_ea)
outcome_dat_asd_ea <- extract_outcome_data(exposure_dat_asd_ea$SNP, id_outcome_ea)

mvdat_asd_ea <- mv_harmonise_data(exposure_dat_asd_ea, outcome_dat_asd_ea)

res_asd_ea <- mv_multiple(mvdat_asd_ea)
res_asd_ea

res_mv_asd_ea <- ldply (res_asd_ea, data.frame)
res_mv_asd_ea_odd <- generate_odds_ratios(res_mv_asd_ea)
res_mv_asd_ea_odd
#in pounds
mv_asd_odds_pounds <- res_mv_asd_ea_odd[2,]

mv_asd_odds_pounds <-mutate(mv_asd_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_asd_odds_pounds

#lo hacemos ahora con MVMR PACKAGE
#MVMR asd INDEPENDIENT OF income TO ea
#MVMR asd

id_exposure_asd_ea <- c("ukb-b-7408", "ieu-a-1185")
id_outcome_ea <- "ieu-a-1239"
#primero conseguimos los exposures combinados
exposure_dat_asd <- mv_extract_exposures(id_exposure_asd_ea)
outcome_dat_asd_ea <- extract_outcome_data(exposure_dat_asd$SNP, id_outcome_ea)

mvdat_asd_ea <- mv_harmonise_data(exposure_dat_asd, outcome_dat_asd_ea)

#mapea para quitar los doubles
asdea_list <- map(mvdat_asd_ea, as.data.table)
# Convert nested list to data frame by column
asdea_mvmr <- as.data.frame(do.call(cbind,asdea_list))
class(asdea_mvmr)
head(asdea_mvmr)
dat_asdea <-asdea_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_asdea) <- c("income_beta", "asd_beta", "P_income", "P_asd", "income_se", "asd_se", "ea_beta", "p_ea", "ea_se")
head(dat_asdea)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_asdea[,c("income_beta", "asd_beta")])
bxse <- as.matrix(dat_asdea[,c("income_se", "asd_beta")])
mrmv_asdea <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_asdea$ea_beta,
                        byse = dat_asdea$ea_se)

mrmv_asdea <- mrmvinput_to_mvmr_format(mrmv_asdea)
class(mrmv_asdea)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_asdea <- strength_mvmr(r_input = mrmv_asdea, gencov = 0)
sres_asdea
#het_mvmr
pres_asdea <- pleiotropy_mvmr(r_input = mrmv_asdea, gencov = 0)
pres_asdea
#res
res_asdea <- ivw_mvmr(r_input = mrmv_asdea)
res_asdea 
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_asdea <- qhet_mvmr(mrmv_asdea, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_asdea 


#ADHD
expadhd <- TwoSampleMR::extract_instruments("ieu-a-1183")

out_adhd<- TwoSampleMR::extract_outcome_data(expadhd$SNP, "ieu-a-1239", proxies=FALSE)

# Harmonise the exposure and outcome data
dat_adhd_ea <- TwoSampleMR::harmonise_data(expadhd, out_adhd)
# Perform MR
res_adhd_ea <- TwoSampleMR::mr(dat_adhd_ea)
res_adhd_ea

#mrpresso
res_presso_adhd_ea <-  TwoSampleMR::run_mr_presso(dat_adhd_ea, NbDistribution = 2000, SignifThreshold = 0.05)
res_presso_adhd_ea

#odds
res_adhd_ea_odds <- generate_odds_ratios(res_adhd_ea)
res_adhd_ea_odds

het_adhd_ea = mr_heterogeneity(dat_adhd_ea)
het_adhd_ea

pleo_adhd_ea<- mr_pleiotropy_test(dat_adhd_ea)
pleo_adhd_ea

#how to represent burgess say for binary exposure:causal estimate represents the average change in the outcome per 2.72-fold increase 
# It may be more interpretable to think instead about the average change in the outcome per doubling (2-fold increase)
# This can be obtained by multiplying the causal estimate by 0.693 (= loge 2) in de logs_odds
#EA SD  4.2 YEARS= 50 MONTHS

#por tanto hacemos la multiplicacion para representarlo
res_adhd_ea_odds_months <-mutate(res_adhd_ea_odds,
                             months_ea = b*0.693*50,
                             lo_ci_m= lo_ci*0.693*50,
                             up_ci_m = up_ci*0.693*50
)
res_adhd_ea_odds_months

#put all data frames into list
adhd_ea_list <- list(res_adhd_ea_odds_months, het_adhd_ea, pleo_adhd_ea)   
adhd_ea_list
#merge all data frames together
adhd_ea_df<- adhd_ea_list %>% reduce(full_join, by='id.exposure')

adhd_ea_df <- filter(adhd_ea_df, method.y == "Inverse variance weighted")
adhd_ea_df



#MULTIVARIANT
#ADHD controlada por income ukb-b-7408 en EA

id_exposure_adhd_ea <- c("ukb-b-7408", "ieu-a-1183")
id_outcome_ea <- "ieu-a-1239"

#primero conseguimos los exposures combinados
exposure_dat_adhd_ea <- mv_extract_exposures(id_exposure_adhd_ea)
outcome_dat_adhd_ea <- extract_outcome_data(exposure_dat_adhd_ea$SNP, id_outcome_ea)

mvdat_adhd_ea <- mv_harmonise_data(exposure_dat_adhd_ea, outcome_dat_adhd_ea)

res_adhd_ea <- mv_multiple(mvdat_adhd_ea)
res_adhd_ea

res_mv_adhd_ea <- ldply (res_adhd_ea, data.frame)
res_mv_adhd_ea_odd <- generate_odds_ratios(res_mv_adhd_ea)
res_mv_adhd_ea_odd
#in pounds
mv_adhd_odds_pounds <- res_mv_adhd_ea_odd[2,]

mv_adhd_odds_pounds <-mutate(mv_adhd_odds_pounds,
                            pounds = b*0.693*33181,
                            lo_ci_p= lo_ci*0.693*33181,
                            up_ci_p = up_ci*0.693*33181
)
mv_adhd_odds_pounds

#lo hacemos ahora con MVMR PACKAGE
#MVMR adhd INDEPENDIENT OF income TO ea
#MVMR adhd

id_exposure_adhd_ea <- c("ukb-b-7408", "ieu-a-1183")
id_outcome_ea <- "ieu-a-1239"
#primero conseguimos los exposures combinados
exposure_dat_adhd <- mv_extract_exposures(id_exposure_adhd_ea)
outcome_dat_adhd_ea <- extract_outcome_data(exposure_dat_adhd$SNP, id_outcome_ea)

mvdat_adhd_ea <- mv_harmonise_data(exposure_dat_adhd, outcome_dat_adhd_ea)

#mapea para quitar los doubles
adhdea_list <- map(mvdat_adhd_ea, as.data.table)
# Convert nested list to data frame by column
adhdea_mvmr <- as.data.frame(do.call(cbind,adhdea_list))
class(adhdea_mvmr)
head(adhdea_mvmr)
dat_adhdea <-adhdea_mvmr[,1:9]
#hago un df con mejores nombres
colnames(dat_adhdea) <- c("income_beta", "adhd_beta", "P_income", "P_adhd", "income_se", "adhd_se", "ea_beta", "p_ea", "ea_se")
head(dat_adhdea)


#perdemos SNPs, buscar la forma en las listas

bx <- as.matrix(dat_adhdea[,c("income_beta", "adhd_beta")])
bxse <- as.matrix(dat_adhdea[,c("income_se", "adhd_beta")])
mrmv_adhdea <-mr_mvinput(bx = bx,
                        bxse = bxse,
                        by = dat_adhdea$ea_beta,
                        byse = dat_adhdea$ea_se)

mrmv_adhdea <- mrmvinput_to_mvmr_format(mrmv_adhdea)
class(mrmv_adhdea)

#si distitnas muestras no hace falta generar matrix
#sres <- strength_mvmr(r_input = mrvm, gencov = 0)

#genero matrix de correlaciones fenotipicas de los exposures  si fuesesen samples distintas
#mvmrcovmatrix<-matrix(c(1,0.87,0.87,1), nrow = 2, ncol = 2)
#Xcovmat<-phenocov_mvmr(mvmrcovmatrix, mrvm[,6:7])


#strenght calculation
sres_adhdea <- strength_mvmr(r_input = mrmv_adhdea, gencov = 0)
sres_adhdea
#het_mvmr
pres_adhdea <- pleiotropy_mvmr(r_input = mrmv_adhdea, gencov = 0)
pres_adhdea
#res
res_adhdea <- ivw_mvmr(r_input = mrmv_adhdea)
res_adhdea 
#resultado controlando heterogeneicidad 
#comprobar porque mvmrcovmatrix es distinto aqui
mvmrcovmatrix2<-matrix(c(1,0,0,1), nrow = 2, ncol = 2)
res_het_adhdea <- qhet_mvmr(mrmv_adhdea, mvmrcovmatrix2, CI = T, iterations = 1000)
res_het_adhdea 


##############################################################


#genero df de todos los UNIVARIANT MR MENTAL HEALTH TO EA
#combino todas los dataframes de todos los resultados  Mental disorders to income
mentaldisorders_income <- rbind(scz_ea_df,bip_ea_df, dep_ea_df, au_ea_df, adhd_ea_df)
write.xlsx(mentaldisorders_income, "UnivariantMR_MentalD_EA.xlsx")


#UNIVARIANT MR FINANCIAL SATISFACTION #################################################################################################

#ukb-b-2830 financial satisfaction vs disorders
finan <- TwoSampleMR::extract_instruments("ukb-b-2830", p1 = 1e-06)

#calculo F statistic
F_finan <- qf(finan$pval.exposure, df1=1, df2= finan$samplesize.exposure-1, lower.tail=FALSE)
F_finan
mean(F_finan)
#scz
out_fin <- TwoSampleMR::extract_outcome_data(finan$SNP, "ieu-a-22", proxies=FALSE)
# Harmonise the exposure and outcome data
datfin <- TwoSampleMR::harmonise_data(finan, out_fin)

# Perform MR
resfin <- TwoSampleMR::mr(datfin)
resfin

#using MRRAPS
res_RAPS <- mr(datfin, method_list = c("mr_raps"))
res_RAPS

res_RAPS <- mr(datfin, method_list = c("mr_raps"), parameters = list(over.dispersion = T))

#bipolar
finan <- TwoSampleMR::extract_instruments("ukb-b-2830", p1 = 1e-06)

out_fin_bip <- TwoSampleMR::extract_outcome_data(finan$SNP, "ieu-b-41", proxies=FALSE)
# Harmonise the exposure and outcome data
bip_fin <- TwoSampleMR::harmonise_data(finan, out_fin_bip )

# Perform MR
resfin_bip <- TwoSampleMR::mr(bip_fin)
resfin_bip

bip_presso <- run_mr_presso(bip_fin, NbDistribution = 1000, SignifThreshold = 0.05)
bip_presso


#dep
finan <- TwoSampleMR::extract_instruments("ukb-b-2830", p1 = 1e-06)

out_fin_dep <- TwoSampleMR::extract_outcome_data(finan$SNP, "ieu-b-102", proxies=FALSE)
# Harmonise the exposure and outcome data
dep_fin <- TwoSampleMR::harmonise_data(finan, out_fin_dep )
# Perform MR
resfin_dep <- TwoSampleMR::mr(dep_fin)
resfin_dep

dep_presso <- run_mr_presso(dep_fin, NbDistribution = 1000, SignifThreshold = 0.05)
dep_presso

#autism
out_fin_au <- TwoSampleMR::extract_outcome_data(finan$SNP, "ieu-a-1185", proxies=FALSE)
# Harmonise the exposure and outcome data
datfin_au <- TwoSampleMR::harmonise_data(finan, out_fin_au)

# Perform MR
resfin_au <- TwoSampleMR::mr(datfin_au)
resfin_au

au_presso <- run_mr_presso(datfin_au, NbDistribution = 1000, SignifThreshold = 0.05)
au_presso
#adhd
out_fin_adhd <- TwoSampleMR::extract_outcome_data(finan$SNP, "ieu-a-1183", proxies=FALSE)
# Harmonise the exposure and outcome data
datfin_adhd <- TwoSampleMR::harmonise_data(finan, out_fin_adhd)

# Perform MR
resfin_adhd <- TwoSampleMR::mr(datfin_adhd)
resfin_adhd

adhd_presso <- run_mr_presso(datfin_adhd, NbDistribution = 1000, SignifThreshold = 0.05)
adhd_presso


#UNIVARIANT MR disorders vs financial satisfaction

#inversa dep
#dep in financial satisfaction
expd <- TwoSampleMR::extract_instruments("ieu-b-102")
outfin <- TwoSampleMR::extract_outcome_data(expd$SNP, "ukb-b-2830", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_dep_fin <- TwoSampleMR::harmonise_data(expd, outfin)
# Perform MR
resd_f <- TwoSampleMR::mr(dat_dep_fin)
resd_f

#inversa bip
#bip in financial satisfaction
expb <- TwoSampleMR::extract_instruments("ieu-b-41")
outf_b <- TwoSampleMR::extract_outcome_data(expb$SNP, "ukb-b-2830", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_bip_fin <- TwoSampleMR::harmonise_data(expb, outf_b)
# Perform MR
resb_f <- TwoSampleMR::mr(dat_bip_fin)
resb_f 


#inversa scz
#scz in financial satisfaction
expscz <- TwoSampleMR::extract_instruments("ieu-a-22")
outf_s <- TwoSampleMR::extract_outcome_data(expscz$SNP, "ukb-b-2830", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_scz_fin <- TwoSampleMR::harmonise_data(expscz, outf_s)
# Perform MR
ress_f <- TwoSampleMR::mr(dat_scz_fin)
ress_f 

#inversa adhd
#adhd in financial satisfaction
expadhd <- TwoSampleMR::extract_instruments("ieu-a-1183")
outf_adhd<- TwoSampleMR::extract_outcome_data(expadhd$SNP, "ukb-b-2830", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_adhd_fin <- TwoSampleMR::harmonise_data(expadhd, outf_adhd)
# Perform MR
resadhd_f <- TwoSampleMR::mr(dat_adhd_fin)
resadhd_f

#inversa autism 
#adhd in financial satisfaction
expau <- TwoSampleMR::extract_instruments("ieu-a-1185")
outf_au<- TwoSampleMR::extract_outcome_data(expau$SNP, "ukb-b-2830", proxies=FALSE)
# Harmonise the exposure and outcome data
dat_au_fin <- TwoSampleMR::harmonise_data(expau, outf_au)
# Perform MR
resau_f <- TwoSampleMR::mr(dat_au_fin)
resau_f

#COMO ANALISIS AD HOC HAGO MULTIVARIANT CON INTELLIGENCE
#Mental disorders: Schizophrenia(ieu-a-22), Bipolar disorder(ieu-b-41), Major depression (ieu-b-102), autism (ieu-a-1185), ADHD (ieu-a-1183)

#hago un multivariant metiendo tambien intelligence ebi-a-GCST006250

id_exposure <- c("ukb-b-7408", "ieu-a-1239", "ebi-a-GCST006250")
id_outcome_scz <- "ieu-a-22"

exposure_dat <- mv_extract_exposures(id_exposure)
outcome_dat <- extract_outcome_data(exposure_dat$SNP, id_outcome_scz)
head(exposure_dat)
mvdat_i <- mv_harmonise_data(exposure_dat, outcome_dat)
head(mvdat_i)
library (plyr)

res_MV_i <- mv_multiple(mvdat_i)
res_MV_i

library (plyr)
res_df_i <- ldply (res_MV_i, data.frame)
res_inc_scz_mvmr_i <- generate_odds_ratios(res_df_i)
res_inc_scz_mvmr_i


#la mismma prueba pero con bipolar 
id_outcome_bip <- "ieu-b-41"
outcome_bip <- extract_outcome_data(exposure_dat$SNP, id_outcome_bip)
mvdat_bip <- mv_harmonise_data(exposure_dat, outcome_bip)

res_mv_bip_i <- mv_multiple(mvdat_bip)
res_mv_bip_i

#genero odds
res_mv_bip_i <- ldply (res_mv_bip_i, data.frame)
res_inc_bip_mvmr <- generate_odds_ratios(res_mv_bip_i)
res_inc_bip_mvmr



#con depresion
id_outcome_dep<- "ieu-b-102"
outcome_dep <- extract_outcome_data(exposure_dat$SNP, id_outcome_dep)
mvdat_dep <- mv_harmonise_data(exposure_dat, outcome_dep)

res_mv_dep_i <- mv_multiple(mvdat_dep)
res_mv_dep_i

#genero odds
res_mv_dep_i <- ldply (res_mv_dep_i, data.frame)
res_inc_dep_mvmr <- generate_odds_ratios(res_mv_dep_i)
res_inc_dep_mvmr


#con ASD
id_outcome_asd <- "ieu-a-1185"
outcome_asd <- extract_outcome_data(exposure_dat$SNP, id_outcome_asd)
mvdat_asd <- mv_harmonise_data(exposure_dat, outcome_asd)

res_mv_asd_i <- mv_multiple(mvdat_asd)
res_mv_asd_i

#genero odds
res_mv_asd_i <- ldply (res_mv_asd_i, data.frame)
res_inc_asd_mvmr <- generate_odds_ratios(res_mv_asd_i)
res_inc_asd_mvmr


#con ADHD
id_outcome_adhd<- "ieu-a-1183"
outcome_adhd <- extract_outcome_data(exposure_dat$SNP, id_outcome_adhd)
mvdat_adhd <- mv_harmonise_data(exposure_dat, outcome_adhd)

res_mv_adhd_i <- mv_multiple(mvdat_adhd)
res_mv_adhd_i

#genero odds
res_mv_adhd_i <- ldply (res_mv_adhd_i, data.frame)
res_inc_adhd_mvmr <- generate_odds_ratios(res_mv_adhd_i)
res_inc_adhd_mvmr



mvmr_md_three <- rbind(res_inc_scz_mvmr_i, res_inc_bip_mvmr, res_inc_dep_mvmr,res_inc_asd_mvmr, res_inc_adhd_mvmr)
mvmr_md_three
write.xlsx(mvmr_md_three, "multivariant_with_intelligence_MD.xlsx")



#lo hago ahora mvmr md to income and ea

#COMO ANALISIS AD HOC HAGO MULTIVARIANT CON INTELLIGENCE
#Mental disorders: Schizophrenia(ieu-a-22), Bipolar disorder(ieu-b-41), Major depression (ieu-b-102), autism (ieu-a-1185), ADHD (ieu-a-1183)

#hago un multivariant metiendo tambien intelligence ebi-a-GCST006250

#income with scz

id_exposure_scz <- c("ieu-a-22", "ieu-a-1239", "ebi-a-GCST006250")
id_outcome_inc <- "ukb-b-7408"

exposure_dat_scz_int <- mv_extract_exposures(id_exposure_scz)

outcome_scz_inc <- extract_outcome_data(exposure_dat_scz_int$SNP, id_outcome_inc)
mvdat_scz_int_inc<- mv_harmonise_data(exposure_dat_scz_int, outcome_scz_inc)

res_mv_scz_inc <- mv_multiple(mvdat_scz_int_inc)
res_mv_scz_inc

library (plyr)
res_mv_scz_inc <- ldply (res_mv_scz_inc, data.frame)
res_mv_scz_inc_odds <- generate_odds_ratios(res_mv_scz_inc)
res_mv_scz_inc_odds

#income by bip

id_exposure_bip <- c("ieu-b-41", "ieu-a-1239", "ebi-a-GCST006250")
id_outcome_inc <- "ukb-b-7408"

exposure_dat_bip_int <- mv_extract_exposures(id_exposure_bip)

outcome_bip_inc <- extract_outcome_data(exposure_dat_bip_int$SNP, id_outcome_inc)
mvdat_bip_int_inc<- mv_harmonise_data(exposure_dat_bip_int, outcome_bip_inc)

res_mv_bip_inc <- mv_multiple(mvdat_bip_int_inc)
res_mv_bip_inc

library (plyr)
res_mv_bip_inc <- ldply (res_mv_bip_inc, data.frame)
res_mv_bip_inc_odds <- generate_odds_ratios(res_mv_bip_inc)
res_mv_bip_inc_odds

#income by dep

id_exposure_dep <- c("ieu-b-102", "ieu-a-1239", "ebi-a-GCST006250")
id_outcome_inc <- "ukb-b-7408"

exposure_dat_dep_int <- mv_extract_exposures(id_exposure_dep)

outcome_dep_inc <- extract_outcome_data(exposure_dat_dep_int$SNP, id_outcome_inc)
mvdat_dep_int_inc<- mv_harmonise_data(exposure_dat_dep_int, outcome_dep_inc)

res_mv_dep_inc <- mv_multiple(mvdat_dep_int_inc)
res_mv_dep_inc

library (plyr)
res_mv_dep_inc <- ldply (res_mv_dep_inc, data.frame)
res_mv_dep_inc_odds <- generate_odds_ratios(res_mv_dep_inc)
res_mv_dep_inc_odds




#ea with scz

id_exposure_scz <- c("ieu-a-22", "ukb-b-7408", "ebi-a-GCST006250")
id_outcome_ea <- "ieu-a-1239"

exposure_dat_scz_ea <- mv_extract_exposures(id_exposure_scz)

outcome_scz_ea <- extract_outcome_data(exposure_dat_scz_ea$SNP, id_outcome_ea)
mvdat_scz_int_ea<- mv_harmonise_data(exposure_dat_scz_ea, outcome_scz_ea)

res_mv_scz_ea <- mv_multiple(mvdat_scz_int_ea)
res_mv_scz_ea

library (plyr)
res_mv_scz_ea <- ldply (res_mv_scz_ea, data.frame)
res_mv_scz_ea_odds <- generate_odds_ratios(res_mv_scz_ea)
res_mv_scz_ea_odds

#income by bip

id_exposure_bip <- c("ieu-b-41", "ukb-b-7408", "ebi-a-GCST006250")
id_outcome_ea <- "ieu-a-1239"

exposure_dat_bip_ea <- mv_extract_exposures(id_exposure_bip)

outcome_bip_ea <- extract_outcome_data(exposure_dat_bip_ea$SNP, id_outcome_ea)
mvdat_bip_int_ea<- mv_harmonise_data(exposure_dat_bip_ea, outcome_bip_ea)

res_mv_bip_ea <- mv_multiple(mvdat_bip_int_ea)
res_mv_bip_ea

library (plyr)
res_mv_bip_ea <- ldply (res_mv_bip_ea, data.frame)
res_mv_bip_ea_odds <- generate_odds_ratios(res_mv_bip_ea)
res_mv_bip_ea_odds

#income by dep

id_exposure_dep <- c("ieu-b-102", "ukb-b-7408", "ebi-a-GCST006250")
id_outcome_ea <- "ieu-a-1239"

exposure_dat_dep_int <- mv_extract_exposures(id_exposure_dep)

outcome_dep_ea <- extract_outcome_data(exposure_dat_dep_int$SNP, id_outcome_ea)
mvdat_dep_int_ea<- mv_harmonise_data(exposure_dat_dep_int, outcome_dep_ea)

res_mv_dep_ea <- mv_multiple(mvdat_dep_int_ea)
res_mv_dep_ea

library (plyr)
res_mv_dep_ea <- ldply (res_mv_dep_ea, data.frame)
res_mv_dep_ea_odds <- generate_odds_ratios(res_mv_dep_ea)
res_mv_dep_ea_odds

mvmr_ses_three <- rbind(res_mv_scz_inc_odds, res_mv_bip_inc_odds, res_mv_dep_inc_odds,res_mv_scz_ea_odds, res_mv_bip_ea_odds,res_mv_dep_ea_odds )
mvmr_ses_three
write.xlsx(mvmr_ses_three, "multivariant_with_intelligence_ses.xlsx")
