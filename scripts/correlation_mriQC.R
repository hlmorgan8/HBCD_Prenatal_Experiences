#################################################################################
##                                                                            ##
##                      Protective Factors Project                            ##
##                           Quality Control MRI                              ##
##                                                                            ##
#################################################################################

################Hannah Morgan 08APRAN2026#########################################
##                                                                            ##
################################################################################

#install.packages("dplyr")
#install.packages("psych")
#nstall.packages("knitr")


library(dplyr)
library(psych)
library(knitr)
library(ggplot2)


df_cov <- readRDS("##/##/Protective_Factors_Data_MRI_QC_Cleaned_2_08APR2026.Rds")

#####################################################################
summary(df_cov$img_mriqc_T2w_snrd_total)
summary(df_cov$img_mriqc_T2w_cjv)



####################################################################

vars <- df_cov %>%
  select(pex_bm_apa_apa2_depr_promisrawscore, img_mriqc_T2w_snrd_total)

cor(vars, use = "pairwise.complete.obs")


####################################################################
##Pre and postnatal
vars <- df_cov %>%
  select(pex_bm_apa_apa2_depr_promisrawscore, img_mriqc_T2w_snrd_total)

##img_mriqc_T2w_cjv or img_mriqc_T2w_snrd_total

cor(vars, use = "pairwise.complete.obs")

cor.test(
  df_cov$pex_bm_apa_apa2_depr_promisrawscore,
  df_cov$img_mriqc_T2w_snrd_total,
  use = "pairwise.complete.obs"
)





##Infant Age
df_cov <- df_cov %>%
  filter(!is.na(pex_bm_apa_apa2_depr_promisrawscore))

vars <- df_cov %>%
  select(V2_T2_vol_adjusted_age, img_mriqc_T2w_cjv)



cor(vars, use = "pairwise.complete.obs")

cor.test(
  df_cov$V2_T2_vol_adjusted_age,
  df_cov$img_mriqc_T2w_cjv,
  use = "pairwise.complete.obs"
)

##img_mriqc_T2w_cjv or img_mriqc_T2w_snrd_total






######Scatterplot
ggplot(df_cov, aes(
  x = pex_bm_apa_apa2_depr_promisrawscore,    #pex_bm_apa_apa2_depr_promisrawscore; V2_T2_adjusted_age
  y = img_mriqc_T2w_snrd_total
)) +
  geom_point(alpha = .6) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    x = "Depression (v01)",
    y = "MRI QC (SNR Total)",
    title = "SNR Total vs. Depression"
  ) +
  theme_minimal()


ggsave(
  filename = "####/####.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)

