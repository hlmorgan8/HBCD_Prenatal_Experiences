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


df_cov <- readRDS("data/processed/Protective_Factors_Data_MRI_QC_Cleaned_2_08APR2026.Rds")

#####################################################################
summary(df_cov$img_mriqc_T2w_snr_total)


####################################################################

vars <- df_cov %>%
  select(pex_bm_apa_apa2_depr_promisrawscore, img_mriqc_T2w_snrd_total)

cor(vars, use = "pairwise.complete.obs")


####################################################################
##Pre and postnatal
vars <- df_cov %>%
  select(pex_bm_apa_apa2_depr_promisrawscore, img_mriqc_T2w_snrd_total)

cor(vars, use = "pairwise.complete.obs")

cor.test(
  df_cov$pex_bm_apa_apa2_depr_promisrawscore,
  df_cov$img_mriqc_T2w_snrd_total,
  use = "pairwise.complete.obs"
)



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
  filename = "output/Depression/Depression_MRIQC_08APR2026.png",   # file name (can be .png, .pdf, .jpeg, etc.)
  width = 8,                      # width in inches
  height = 6,                     # height in inches
  dpi = 300                        # resolution (good for publications)
)

