# early steps 

rm(list = ls(all.names = TRUE)) # clear everything from the global environment 


# load packages 
# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("tidyverse")    # alternative installation of the %>%
#install.packages("readr")
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
library(readr)
# install.packages("tidyverse", repo = 'https://mac.R-project.org')


scn1 <- read.csv(file = '~/Desktop/tatone_j/3_coded-questionnaires/1_ffq/nguyen1.scn1.csv', strip.white = TRUE, sep = ",")
scn2 <- read.csv(file = '~/Desktop/tatone_j/3_coded-questionnaires/1_ffq/nguyen1.scn2.csv', strip.white = TRUE, sep = ",")
data.df <- left_join(scn1, scn2, by = 'id')

# make a brand-new data set so you can convert to spf
data.spd <- data.df 

# append _spd to the variable names
colnames(data.spd) <- paste(colnames(data.spd),'_spd', sep='')

# take id and make it the row name so you can perform the same operation on the entire dataframe 
rownames(data.spd) <- data.spd$id_spd

# now remove the id column
data.spd <- data.spd %>%
  select(-id_spd)
data.spd <- data.spd %>%
  select(-oil_spd, -mulbrnd_spd, -cer_spd, -mb_spd, -other1_spd, -other2_spd)


food_items <- 
  c(
    "oil_spd",
    "o_spd",
    "rapeseed_spd",
    "c_spd",
    "soy.90_spd",
    "veg.oil_spd",
    "multvit_spd",
    "mulfrq_spd",
    "q1bpt_spd",
    "centrum.silver_spd",
    "theragran.m_spd",
    "centrum_spd",
    "oneaday.essent_spd",
    "other.mv_spd",
    "brndpt_spd",
    "mulbrnd_spd",
    "vipt_spd",
    "vitamin.a_spd",
    "ad_spd",
    "k.supp_spd",
    "kd_spd",
    "vitamin.c_spd",
    "cd_spd",
    "b6_spd",
    "b6d_spd",
    "vitamin.e_spd",
    "ed_spd",
    "etype_spd",
    "calcium_spd",
    "cad_spd",
    "sel_spd",
    "seld_spd",
    "vitamin.d_spd",
    "vitdd_spd",
    "zinc_spd",
    "znd_spd",
    "otherptb_spd",
    "metamucil_spd",
    "cod.liv.oil_spd",
    "b12_spd",
    "linseed_spd",
    "flax_spd",
    "bcar_spd",
    "magnesium_spd",
    "omega.3.epa_spd",
    "niacin2_spd",
    "cr_spd",
    "lecithin_spd",
    "coq10_spd",
    "chol_spd",
    "folic.acid_spd",
    "bcompl_spd",
    "lyco_spd",
    "dhea_spd",
    "iron_spd",
    "others_spd",
    "sugpt_spd",
    "sug_spd",
    "cerpt_spd",
    "cer_spd",
    "margptb_spd",
    "mn_spd",
    "ms_spd",
    "mt_spd",
    "msp_spd",
    "msq_spd",
    "mfpt_spd",
    "mreg_spd",
    "mlt_spd",
    "mnf_spd",
    "mtpt_spd",
    "mbpt_spd",
    "mb_spd",
    "dpt_spd",
    "skim.kids_spd",
    "milk2_spd",
    "milk_spd",
    "soymilk.fort_spd",
    "cream_spd",
    "cof.wht_spd",
    "yogurt.froz_spd",
    "ice.cr_spd",
    "yog.plain_spd",
    "yog_spd",
    "margarine_spd",
    "bu_spd",
    "cot.ch_spd",
    "cr.ch_spd",
    "oth.ch_spd",
    "ch.soy_spd",
    "ch.reg_spd",
    "ch.lofat_spd",
    "ch.nofat_spd",
    "ch.none_spd",
    "ch.pt_spd",
    "fpt_spd",
    "raisgrp_spd",
    "prun_spd",
    "prun.j_spd",
    "ban_spd",
    "cant_spd",
    "avocado_spd",
    "apple_spd",
    "a.j_spd",
    "orang_spd",
    "o.j.calc_spd",
    "o.j_spd",
    "grfrt_spd",
    "oth.f.j_spd",
    "straw_spd",
    "blue_spd",
    "peaches_spd",
    "apricot_spd",
    "vpt_spd",
    "tom_spd",
    "tom.j_spd",
    "tom.s_spd",
    "salsa_spd",
    "st.beans_spd",
    "beans_spd",
    "tofu_spd",
    "peas_spd",
    "broc_spd",
    "caul_spd",
    "cabb_spd",
    "brusl_spd",
    "carrot.r_spd",
    "carrot.c_spd",
    "corn_spd",
    "mix.veg_spd",
    "swt.pot_spd",
    "yel.sqs_spd",
    "zuke_spd",
    "kale_spd",
    -
    "spin.ckd_spd",
    "spin.raw_spd",
    -
    "ice.let_spd",
    "rom.let_spd",
    "celery_spd",
    "peppers_spd",
    "onions_spd",
    "onions1_spd",
    "meatpt_spd",
    "eggs.omega_spd",
    "eggs_spd",
    "hotdog_spd",
    "chix.dog_spd",
    "chix.sand_spd",
    "chix.sk_spd",
    "chix.no_spd",
    "bacon_spd",
    "p2exbub_spd",
    "mpt_spd",
    "bologna_spd",
    "proc.mts_spd",
    "xtrlean.hamburg_spd",
    "hamb_spd",
    "sand.bf.ham_spd",
    "pork_spd",
    "beef02_spd",
    "tuna_spd",
    "fr.fish.kids_spd",
    "shrimp.ckd_spd",
    "dk.fish_spd",
    "oth.fish_spd",
    "bpt_spd",
    "cold.cereal_spd",
    "cheerio_spd",
    "rz.b.k_spd",
    "c.flk.K_spd",
    "sh.wht_spd",
    "grpnut_spd",
    "total_spd",
    "spec.k_spd",
    "whties_spd",
    "oatmeal.bran_spd",
    "ckd.cer_spd",
    "wh.br_spd",
    "rye.br_spd",
    "dk.br_spd",
    "crax_spd",
    "eng.muff_spd",
    "muff_spd",
    "pancak_spd",
    "br.rice_spd",
    "wh.rice_spd",
    "pasta_spd",
    "tortillas_spd",
    "ff.pot_spd",
    "pot_spd",
    "pot.chip_spd",
    "pizza_spd",
    "bvpt_spd",
    "localcaf_spd",
    "localno_spd",
    "coke_spd",
    "oth.carb_spd",
    "punch_spd",
    "beer_spd",
    "beer.lite_spd",
    "r.wine_spd",
    "w.wine_spd",
    "liq_spd",
    "h2o_spd",
    "herb.tea_spd",
    "tea_spd",
    "decaf_spd",
    "coff_spd",
    "coffdrink_spd",
    "p3exbub_spd",
    "spt_spd",
    "choc_spd",
    "choc.dark_spd",
    "candy_and_nuts_spd",
    "candy_spd",
    "coox.nofat_spd",
    "coox.other_spd",
    "brownie_spd",
    "donut_spd",
    "cake.lofat_spd",
    "cake.other_spd",
    "pie.comm_spd",
    "jam_spd",
    "p.bu_spd",
    "popc.ff.lt_spd",
    "popc_spd",
    "s.roll.lf_spd",
    "s.roll.c_spd",
    "brkfast.bars_spd",
    "power.bar_spd",
    "zone.bar_spd",
    "pretzel_spd",
    "nuts_spd",
    "walnuts_spd",
    "oth.nuts_spd",
    "oat.bran_spd",
    "bran_spd",
    "chow_spd",
    "catsup_spd",
    "splenda_spd",
    "nutrasweet_spd",
    "olive.oil_spd",
    "mayo.d_spd",
    "mayo_spd",
    "o_and_v_spd",
    "dress.nofat_spd",
    "dress.lofat_spd",
    "dress.olive_spd",
    "dress.other_spd",
    "dress.pt_spd",
    "liver_spd",
    "chix.liver_spd",
    "liver.pt_spd",
    "ffh_spd",
    "fb_spd")


#replace NAs with 0's to avoid the logical error... change this?
data.spd[is.na(data.spd)] <- 0

not.liver <- colnames(data.spd) 
liver <- c('liver_spd', 'chix.liver_spd', 'liver.pt_spd')

  
not.liver <- not.liver[!not.liver %in% liver]



#convert to spd
data.spd %>% 
  mutate_at(vars(not.liver), recode, `0` = 0, `1` = 0.07, `2` = 0.14, `3` = 0.43, 
            `4` = 0.79, `5` = 1, `6` = 2.5, `7` = 4.5, `8` = 6.0, `9` = 0)

#create a new df for just liver variables
liver.df <- data.spd
liver.df <- liver.df %>% 
  select(-not.liver)

#take liver variables out of old df
data.spd <- data.spd %>%
  select(-liver)

#recode liver 
liver.df%>%
  mutate_at(vars(liver), recode, `1` = 0,  `2` = 0.01, `3` = 0.03, `4` = 0.08, 
            `5` = 0.14)

 #merge data frames
#this df gives 2916 observations instead of 54 like the rest
ffq.df <- merge(data.spd, liver.df)


#/*distinguish between whole grain and refined grain breakfast cereals*/... from example code
#if cerbrffq1 in
#(29,43,84,96,111,112,114,115,119,127,128,129,131,132,133,134,146,147,
# 148,151,152,154,155,157,164,165,166,167,176,184,185,186,188,0,4,11,12,
# 13,15,16,17,19,20,21,22,23,25,30,31,33,35,42,45,46,59,60,61,62,63,64,
# 65,66,68,74,75,76,79,80,81,82,83,85,86,88,95,97,98,101,109,113,123,
# 124,149,150,153,158,170,173,177,37,38,47,73,130,182)
#then rcerffq1 = cerffq1;
#else wcerffq1 = cerffq1;

#if rcerffq1=. then rcerffq1 = 0;
#if wcerffq1=. then wcerffq1 = 0;
