getwd()
source('~/Dropbox/tatone_j/1_yaq-single-food-process.R')



food_items <- c("ffh_spd", 
                 "ffa_spd",   "localcaf_spd",   "coke_spd",  "soda_size_spd"    ,    "punch_spd"        ,   
                 "gatorade_spd"     ,   "red.bull.sf_spd"   ,   "red.bull_spd"       ,  "smoothie_spd"     ,    "milkshake_spd"    ,   
                 "tea_spd"          ,   "decaf_spd"         ,   "coff_spd"           ,  "coff.ff_spd"      ,    "coff.lf_spd"      ,   
                 "icoff.ff_spd"     ,   "icoff.lf_spd"      ,   "h2o_spd"            ,  "beer_spd"         ,    "w.wine_spd"       ,   
                 "liquor_spd"       ,   "milk_spd"          ,   "milk.choc_spd"      ,  "c.inst.brkft_spd" ,    "whey.bey_spd"     ,   
                 "yog.lt.asp_spd"   ,   "yog_spd"           ,   "cot.ch_spd"         ,  "am.ch_spd"        ,    "cr.ch_spd"        ,   
                 "ch.nofat_spd"     ,   "ch.lofat_spd"      ,   "ch.reg_spd"         ,  "ch.dk_spd"        ,    "ch.pt_spd"        ,   
                 "bu_spd"           ,   "margarine_spd"     ,   "wh.top_spd"         ,  "mn_spd"           ,    "mst_spd"          ,   
                 "mt_spd"           ,   "msq_spd"           ,   "msp_spd"            ,  "mdn_spd"          ,    "formpt_spd"       ,   
                 "burger.bun_spd"   ,   "tofu_spd"          ,   "veg.burg_spd"       ,  "pizza.f.r_spd"    ,    "taco_spd"         ,   
                 "burrito_spd"      ,   "tacofl.bb_spd"     ,   "tacofl.bf_spd"      ,  "tacofl.ch_spd"    ,    "tacofl.bn_spd"    ,   
                 "tacofl.pt_spd"    ,   "chix.nug_spd"      ,   "hotdog_spd"         ,  "chix.dog_spd"     ,    "chix.mix_spd"     ,   
                 "chix.no_spd"      ,   "fr.fish.kids_spd"  ,   "dk.fish_spd"        ,  "oth.fish_spd"     ,    "shrimp.ckd_spd"   ,   
                 "beef.mix_spd"     ,   "beef_spd"          ,   "pork_spd"           ,  "meatballs_spd"    ,    "pasta_spd"        ,   
                 "lasagna_spd"      ,   "mac.cheese_spd"    ,   "spagh.sce2_spd"     ,  "eggs_spd"         ,    "proc.mts_spd"     ,   
                 "bacon_spd"        ,   "liver_spd"         ,   "pbj.sand_spd"       ,  "chix.turk.sand_spd",   "beef.sand_spd"    ,   
                 "bologna.sand_spd" ,   "tuna.sand_spd"     ,   "gr.cheese.sand_spd" ,  "catsup_spd"       ,    "chow_spd"         ,   
                 "chix.nood_spd"    ,   "mayo_spd"          ,   "local.sal.dr_spd"   ,  "salsa_spd"        ,    "sug_spd"          ,   
                 "cold.cereal_spd"  ,   "ckd.oats_spd"      ,   "ckd.cer_spd"        ,  "wh.br_spd"        ,    "dk.br_spd"        ,   
                 "eng.muff_spd"     ,   "muff_spd"          ,   "croissant_spd"      ,  "bisc_spd"         ,    "wh.rice_spd"      ,   
                 "br.rice_spd"      ,   "tortillas_spd"     ,   "pancak_spd"         ,  "french.toast_spd" ,    "ff.pot_spd"       ,   
                 "pot_spd"          ,   "rais_spd"          ,   "grapes_spd"         ,  "ban_spd"          ,    "french_fries_spd" ,   
                 "a.sce_spd"        ,   "pear_spd"          ,   "cant_spd"           ,  "h2omelon_spd"     ,    "orang_spd"        ,   
                 "grfrt_spd"        ,   "straw_spd"         ,   "blue_spd"           ,  "peach.cn_spd"     ,    "p.apple_spd"      ,   
                 "o.j_spd"          ,   "a.j_spd"           ,   "tom_spd"            ,  "tom.j_spd"        ,    "v8.fusion_spd"    ,   
                 "st.beans_spd"     ,   "beans_spd"         ,   "broc_spd"           ,  "caul_spd"         ,    "corn_spd"         ,   
                 "peas_spd"         ,   "mix.veg_spd"       ,   "spin.raw_spd"       ,  "kale_spd"         ,    "peppers_spd"      ,   
                 "swt.pot_spd"      ,   "zuke_spd"          ,   "carrot.c_spd"       ,  "carrot.r_spd"     ,    "celery_spd"       ,   
                 "ice.let_spd"      ,   "coleslaw_spd"      ,   "cabb_spd"           ,  "pot.salad_spd"    ,    "pasta.salad_spd"  ,   
                 "okra_spd"         ,   "onions_spd"        ,   "pot.chip_spd"       ,  "corn.chips_spd"   ,    "popc_spd"         ,   
                 "pretzel_spd"      ,   "mix.dr.frt_spd"    ,   "nuts_spd"           ,  "oth.nuts_spd"     ,    "frt.rollups_spd"  ,   
                 "graham.crax_spd"  ,   "crax_spd"          ,   "poptart_spd"        ,  "cake.comm_spd"    ,    "cake.snack_spd"   ,   
                 "s.roll.c_spd"     ,   "donut_spd"         ,   "choc.chip.cookie_spd", "brownie_spd"      ,    "pie.comm_spd"     ,   
                 "choc_spd"         , "candy...nuts_spd",  "candy_spd"         ,   "jello_spd"          ,  "pudd_spd"         ,    "yogurt.frozen_spd",   
                 "ice.cr_spd"       ,   "popsicle_spd"      ,   "seeds_spd"          ,  "snack.bar_spd"    ,    "energy.bar_spd"   ,   
                 "prot.bar_spd"     ,   "jam_spd")


select.data <- data.df %>%
  select (food_items)

food_groups <- data.frame(select.data)



food_groups$fried_foods <- food_groups$ffh_spd + food_groups$ffa_spd

food_groups$processed_meat <- food_groups$hotdog_spd + food_groups$chix.dog_spd 
+ food_groups$bacon_spd + food_groups$beef.mix_spd + food_groups$bologna.sand_spd
+ food_groups$beef.sand_spd + food_groups$tuna.sand_spd

food_groups$red_meat <-  food_groups$burger.ch.bun_spd + food_groups$burger.bun_spd + food_groups$tacofl.bf_spd
+ food_groups$tacofl.bb_spd + food_groups$beef_main_spd + food_groups$meatballs_meatloaf_spd + food_groups$taco_pt_spd
+ food_groups$beef.mix_spd + food_groups$beef_spd

food_groups$organ_meats <- food_groups$liver_spd

food_groups$fish_seafood <- food_groups$fr.fish.kids_spd + food_groups$dk.fish_spd
+ food_groups$oth.fish_spd + food_groups$shrimp.ckd_spd

food_groups$poultry <-  food_groups$chix.no_spd + food_groups$eatskin_spd + food_groups$chix.nug_spd
+ food_groups$chix.mix_spd + food_groups$tacofl.ch_spd + food_groups$chix.turk.sand_spd 

food_groups$eggs <-  food_groups$eggs_spd

food_groups$butter <-  food_groups$bu_spd

food_groups$margarine <-  food_groups$margarine_spd + food_groups$mst_spd + food_groups$mt_spd 
+ food_groups$msq_spd + food_groups$msp_s + food_groups$mdn + food_groups$mn_spd

food_groups$dairy_lowfat <-  food_groups$yog.lt.asp_spd + food_groups$coff.lf_spd + food_groups$icoff.lf_spd + 
  food_groups$icoff.ff_spd + food_groups$coff.ff_spd

#high protein drink  data.df$instant_brkfst_drink go in here
food_groups$dairy_highfat <-  food_groups$yog_spd + food_groups$cot.ch_spd
+ food_groups$cr.ch_spd + food_groups$milk_spd + food_groups$milk.choc_spd
+ food_groups$whey.bev_sp + food_groups$c.inst.brkft_spd

food_groups$dairy_type <-  food_groups$ch.pt_spd + food_groups$ch.reg_spd + food_groups$ch.lofat_spd + food_groups$ch.nofat_spd
+ food_groups$ch.dk_spd

food_groups$liquor <- food_groups$liquor_spd

food_groups$beer <-  food_groups$beer_spd

food_groups$wine <-  food_groups$wine_spd

food_groups$tea <-  food_groups$tea_spd

food_groups$coffee <-  food_groups$coff_spd + food_groups$decaf_spd + food_groups$coff.ff_spd 
+ food_groups$icoff.ff_spd + food_groups$coff.lf_spd + food_groups$icoff.lf_spd

food_groups$fruit <-  food_groups$rais_spd + food_groups$grapes_spd
+ food_groups$ban_spd + food_groups$apple_spd + food_groups$a.sce_spd
+ food_groups$pear_spd + food_groups$cant_spd
+ food_groups$h2omelon_spd + food_groups$orang_spd + food_groups$grfrt_spd
+ food_groups$straw_spd + food_groups$blue_spd
+ food_groups$peach.cn_spd
+ food_groups$p.apple_spd + food_groups$tom_spd

food_groups$fruit_juices <-  food_groups$o.j_spd + food_groups$a.j_spd
+ food_groups$tom.j_spd + food_groups$v8.fusion_spd

food_groups$cruciferous_veg <- + food_groups$broc_spd + food_groups$caul_spd 
+ food_groups$cabb_spd + food_groups$mix.veg_spd

food_groups$dark_yellow_veg <-  food_groups$corn_spd + food_groups$swt.pot_spd
+ food_groups$carrot.c_spd + food_groups$carrot.r_spd

food_groups$tomatoes <-  food_groups$tom_spd + food_groups$tom.j_spd + food_groups$v8.fusion_spd 
+ food_groups$salsa_spd + food_groups$spagh.sce2_spd

food_groups$leafy_greens <-  food_groups$ice.let_spd + food_groups$spin.raw_spd + food_groups$mix.veg_spd
+ food_groups$kale_spd

food_groups$legumes <- food_groups$peas_spd + food_groups$beans_spd + food_groups$tacofl.bn_spd
+ food_groups$tacofl.bb_spd + food_groups$st.beans_spd

food_groups$other_veg <-  food_groups$okra_spd + food_groups$zuke_spd + food_groups$peppers_spd
+ food_groups$coleslaw_spd + food_groups$pasta.salad_spd


food_groups$potatoes <- + food_groups$pot.salad_spd + food_groups$pot_spd

food_groups$french_fries <-  food_groups$ff.pot_spd + food_groups$onions_spd + food_groups$ffh_spd + food_groups$ffa_spd

food_groups$whole_grains <- sum(data.df$ckd.oats_spd, data.df$dk.br_spd, data.df$br_rice_spd)
+ food_groups
food_groups$refined_grains <-  food_groups$eng_muff_spd + food_groups$bisc_spd 
                              + food_groups$muff_spd + food_groups$croissant_spd
                              + food_groups$pancak_spd + food_groups$french.toast_spd
                              + food_groups$wh.rice_spd + food_groups$tortillas_spd
                              + food_groups$wh.br_spd + food_groups$ckd.cer_spd + food_groups$pasta_spd
                              + food_groups$spagh.sce2_spd
                              + food_groups$mac.cheese_spd + food_groups$lasagna_spd

food_groups$cold_cereal <-  food_groups$cold.cereal_spd

#dont know what to do with burrito and taco
food_groups$pizza <-  food_groups$pizza + food_groups$taco_spd + food_groups$burrito_spd

food_groups$snacks <-  food_groups$pot.chip_spd + food_groups$popc_spd + food_groups$pretzel_spd 
                      + food_groups$corn.chips_spd + food_groups$frt.rollups_spd + food_groups$crax_spd
                      + food_groups$graham.crax_spd + food_groups$poptart_spd + food_groups$seeds_spd 
                      + food_groups$snack.bar_spd + food_groups$energy.bar_spd + food_groups$prot.bar_spd
+ food_groups$pbj.sand_spd 


food_groups$nuts <-  food_groups$mix.dr.frt_spd + food_groups$nuts_spd + food_groups$oth.nuts_spd

food_groups$sweets_desserts <-  food_groups$cake.snack_spd + food_groups$cake.comm_spd + food_groups$s.roll.c_spd
                        + food_groups$donut_spd + food_groups$choc.chip.cookie_spd + food_groups$brownie_spd
                        + food_groups$pie.comm_spd + food_groups$choc_spd
                        + food_groups$candy...nuts_spd + food_groups$jello_spd + food_groups$pudd_spd
                        + food_groups$candy_spd + food_groups$yogurt.frozen_spd + food_groups$ice.cr_spd
+ food_groups$popsicle_spd + food_groups$milkshake_spd + food_groups$sug_spd

food_groups$high_energy_drinks <-  food_groups$red.bull_spd + food_groups$red.bull.sf_spd

food_groups$low_energy_drinks <-  food_groups$coke_spd + food_groups$localcaf_spd + food_groups$punch_spd
+ food_groups$smoothie_spd + food_groups$gatorade_spd + food_groups$sodasize_spd

food_groups$oil_vinegar_salad_dressing <-  food_groups$oolive_spd + food_groups$ocan_spd + food_groups$o...v_spd 
+ food_groups$osaf_spd + food_groups$oveg_spd 
+ food_groups$ocorn_spd + food_groups$omega.3.epa_spd

food_groups$creamy_salad_dressing <-  food_groups$lowcal.sal.dr_spd + food_groups$mayo_spd

food_groups$chowder_cream_soup <-  food_groups$chow_spd

food_groups$other_soup <-  food_groups$chix.nood_spd

food_groups$condiments <-  food_groups$catsup_spd + food_groups$mayo_spd
+ food_groups$jam_spd

food_groups <- food_groups %>%
  select(-food_items)

#VISUALIZATION

#source('2_yaq-food-group-construction.R')
#
#install.packages("ggplot2")
#library(ggplot2)
#install.packages("reshape")
#library(reshape)
#
#melt.data.df <- melt(data.df)
#melt.data.df
#
#colnames(data.df)
#ggplot(data = data.df, aes(x = condiments, y =  SPD, group = ""))



