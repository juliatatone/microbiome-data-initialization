# early steps 

rm(list = ls(all.names = TRUE)) # clear everything from the global environment 

# load packages 
# install.packages("magrittr") # package installations are only needed the first time you use it
# install.packages("tidyverse")    # alternative installation of the %>%
library(magrittr) # needs to be run every time you start R and want to use %>%
library(dplyr)
# install.packages("tidyverse", repo = 'https://mac.R-project.org')

# path to home directory 
data <- read.csv(file = '~/Dropbox/tatone_j/3_coded-questionnaires/2_yaq/nguyen1.yaq.scn.csv', strip.white = TRUE, sep = ",")

# read in and then make a new df 
data.df <- data.frame(data)

#Fried food at home

data.df <- data.df %>%
  mutate(ffh_spd = case_when(
    ffh == 1 ~ 0,
    ffh == 2 ~ 0.29,
    ffh == 3 ~ 0.71,
    ffh == 4 ~ 1.00,
    TRUE ~ 0))
    
#Fried food away from home

data.df <- data.df %>%
  mutate(ffa_spd = case_when(
    ffa == 1 ~ 0,
    ffa == 2 ~ 0.29,
    ffa == 3 ~ 0.71,
    ffa == 4 ~ 1.0,
    TRUE ~ 0))

#Q1: Diet Soda 

data.df <- data.df %>%
  mutate(localcaf_spd = case_when(
    localcaf== 1 ~ 0,
    localcaf == 2 ~ 0.067,
    localcaf == 3 ~ 0.14,
    localcaf == 4 ~ 0.43,
    localcaf == 5 ~ 0.79,
    localcaf == 6 ~ 1.0,
    localcaf == 7 ~ 2.0,
    localcaf == 8 ~ 3.0,
    TRUE ~ 0))

#Q2: Soda - Not Diet

data.df <- data.df %>%
  mutate(coke_spd = case_when(
    coke == 1 ~ 0,
    coke == 2 ~ 0.067,
    coke == 3 ~ 0.14,
    coke == 4 ~ 0.43,
    coke == 5 ~ 0.79,
    coke == 6 ~ 1.0,
    coke == 7 ~ 2.0,
    coke == 8 ~ 3.0,
    TRUE ~ 0))

#Q3: Usual Soda serv size... 12 oz is 1 serving

data.df <- data.df %>%
  mutate(sodasize_spd = case_when(
    sodasize == 1 ~ 0.5,
    sodasize == 2 ~ 1.0,
    sodasize == 3 ~ 1.5,
    sodasize == 4 ~ 1.75,
    sodasize == 5 ~ 0,
    TRUE ~ 0))

#Q4: Other Sweetended drinks

data.df <- data.df %>%
  mutate(punch_spd = case_when(
    punch== 1 ~ 0,
    punch== 2 ~ 0.067,
    punch== 3 ~ 0.14,
    punch== 4 ~ 0.43,
    punch== 5 ~ 0.79,
    punch== 6 ~ 1.0,
    punch== 7 ~ 2.0,
    punch== 8 ~ 3.0,
    TRUE ~ 0))

#Booklet Lithocode Page 1


#Q5: Sports drinks

data.df <- data.df %>%
  mutate(gatorade_spd = case_when(
    gatorade == 1 ~ 0,
    gatorade == 2 ~ 0.067,
    gatorade == 3 ~ 0.14,
    gatorade == 4 ~ 0.43,
    gatorade == 5 ~ 0.79,
    gatorade == 6 ~ 1.0,
    gatorade == 7 ~ 2.0,
    gatorade == 8 ~ 3.0,
    TRUE ~ 0))

#Q6: Sf or low cal energy drink

data.df <- data.df %>%
  mutate(red.bull.sf_spd = case_when(
    red.bull.sf == 1 ~ 0,
    red.bull.sf == 2 ~ 0.067,
    red.bull.sf == 3 ~ 0.14,
    red.bull.sf == 4 ~ 0.43,
    red.bull.sf == 5 ~ 0.79,
    red.bull.sf == 6 ~ 1.0,
    red.bull.sf == 7 ~ 2.0,
    red.bull.sf == 8 ~ 3.0,
    TRUE ~ 0))

#Q7: Reg. energy drink

data.df <- data.df %>%
  mutate(red.bull_spd = case_when(
    red.bull == 1 ~ 0,
    red.bull == 2 ~ 0.067,
    red.bull == 3 ~ 0.14,
    red.bull == 4 ~ 0.43,
    red.bull == 5 ~ 0.79,
    red.bull == 6 ~ 1.0,
    red.bull == 7 ~ 2.0,
    red.bull == 8 ~ 3.0,
    TRUE ~ 0))

#Q8: Smoothie

data.df <- data.df %>%
  mutate(smoothie_spd = case_when(
    smoothie == 1 ~ 0,
    smoothie == 2 ~ 0.067,
    smoothie == 3 ~ 0.14,
    smoothie == 4 ~ 0.43,
    smoothie == 5 ~ 0.79,
    smoothie == 6 ~ 1.0,
    smoothie == 7 ~ 2.0,
    smoothie == 8 ~ 3.0,
    TRUE ~ 0))

#Q9: Milkshake

data.df <- data.df %>%
  mutate(milkshake_spd = case_when(
    milkshake == 1 ~ 0,
    milkshake == 2 ~ 0.067,
    milkshake == 3 ~ 0.14,
    #more than one per week = 2 per week = 2/7
    milkshake == 4 ~ 0.29,
    TRUE ~ 0 ))

#Q10: Hot Tea w/caff

data.df <- data.df %>%
  mutate(tea_spd = case_when(
    tea == 1 ~ 0,
    tea == 2 ~ 0.067,
    tea == 3 ~ 0.21,
    tea == 4 ~ 0.64,
    tea == 5 ~ 1.0,
    tea == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q11: Decaf Coffee

data.df <- data.df %>%
  mutate(decaf_spd = case_when(
    decaf == 1 ~ 0,
    decaf == 2 ~ 0.067,
    decaf == 3 ~ 0.21,
    decaf == 4 ~ 0.64,
    decaf == 5 ~ 1.0,
    decaf == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q12: Coffee -not decaf

data.df <- data.df %>%
  mutate(coff_spd = case_when(
    coff == 1 ~ 0,
    coff == 2 ~ 0.067,
    coff == 3 ~ 0.21,
    coff == 4 ~ 0.64,
    coff == 5 ~ 1.0,
    coff == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q13: Coffee drink w/nf milk 

data.df <- data.df %>%
  mutate(coff.ff_spd = case_when(
    coff.ff == 1 ~ 0,
    coff.ff == 2 ~ 0.067,
    coff.ff == 3 ~ 0.21,
    coff.ff == 4 ~ 0.64,
    coff.ff == 5 ~ 1.0,
    coff.ff == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q14: Coffee drink w/lf/reg milk

data.df <- data.df %>%
  mutate(coff.lf_spd = case_when(
    coff.lf == 1 ~ 0,
    coff.lf == 2 ~ 0.067,
    coff.lf == 3 ~ 0.21,
    coff.lf == 4 ~ 0.64,
    coff.lf == 5 ~ 1.0,
    coff.lf == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q15: Iced Coffee w/nf milk

data.df <- data.df %>%
  mutate(icoff.ff_spd = case_when(
    icoff.ff == 1 ~ 0,
    icoff.ff == 2 ~ 0.067,
    icoff.ff == 3 ~ 0.21,
    icoff.ff == 4 ~ 0.64,
    icoff.ff == 5 ~ 1.0,
    icoff.ff == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q16: Iced Coffeew/lf/reg milk

data.df <- data.df %>%
  mutate(icoff.lf_spd = case_when(
    icoff.lf == 1 ~ 0,
    icoff.lf == 2 ~ 0.067,
    icoff.lf == 3 ~ 0.21,
    icoff.lf == 4 ~ 0.64,
    icoff.lf == 5 ~ 1.0,
    icoff.lf == 6 ~ 2.0,
    TRUE ~ 0 ))

#Q17: Water

data.df <- data.df %>%
  mutate(h2o_spd = case_when(
    h2o == 1 ~ 0,
    h2o == 2 ~ 0.067,
    h2o == 3 ~ 0.14,
    h2o == 4 ~ 0.43,
    h2o == 5 ~ 0.79,
    h2o == 6 ~ 1.0,
    h2o == 7 ~ 2.0,
    h2o == 8 ~ 3.0,
    TRUE ~ 0))

#Q18: Beer

data.df <- data.df %>%
  mutate(beer_spd = case_when(
    beer == 1 ~ 0,
    beer == 2 ~ 0.067,
    beer == 3 ~ 0.14,
    beer == 4 ~ 0.43,
    beer == 5 ~ 0.79,
    beer == 6 ~ 1.0,
    beer == 7 ~ 2.0,
    beer == 8 ~ 3.0,
    TRUE ~ 0))

#Q19: Wine

data.df <- data.df %>%
  mutate(w.wine_spd = case_when(
    w.wine == 1 ~ 0,
    w.wine == 2 ~ 0.067,
    w.wine == 3 ~ 0.14,
    w.wine == 4 ~ 0.43,
    w.wine == 5 ~ 0.79,
    w.wine == 6 ~ 1.0,
    w.wine == 7 ~ 2.0,
    w.wine == 8 ~ 3.0,
    TRUE ~ 0))

#Q20: Liquor

data.df <- data.df %>%
  mutate(liquor_spd = case_when(
    liq == 1 ~ 0,
    liq == 2 ~ 0.067,
    liq == 3 ~ 0.14,
    liq == 4 ~ 0.43,
    liq == 5 ~ 0.79,
    liq == 6 ~ 1.0,
    liq == 7 ~ 2.0,
    liq == 8 ~ 3.0,
    TRUE ~ 0))

#D1: Milk type

data.df <- data.df %>%
  mutate(milk.form_spd = case_when(
    milk.form == 1 ~ 'Skim/Nonfatmilk',
    milk.form == 2 ~ '1% Milk',
    milk.form == 3 ~ '2% Milk',
    milk.form == 4 ~ 'Whole Milk',
    milk.form == 5 ~ 'Soy Milk',
    milk.form == 6 ~ "Don't Know",
    milk.form == 7 ~ "Don't Drink Milk",
    TRUE ~ 0))

#D2: White Milk

data.df <- data.df %>%
  mutate(milk_spd = case_when(
    milk == 1 ~ 0,
    milk == 2 ~ 0.14,
    milk == 3 ~ 0.57,
    milk == 4 ~ 1.0,
    milk == 5 ~ 2.5,
    milk == 6 ~ 4.0,
    TRUE ~ 0))

#D3: Choc/flavored milk

data.df <- data.df %>%
  mutate(milk.choc_spd = case_when(
    milk.choc == 1 ~ 0,
    milk.choc == 2 ~ 0.07,
    milk.choc == 3 ~ 0.14,
    milk.choc == 4 ~ 0.57,
    milk.choc == 5 ~ 1.5,
    milk.choc == 6 ~ 3.0,
    TRUE ~ 0))

#D4: Inst Brkfst Drink

data.df <- data.df %>%
  mutate(c.inst.brkft_spd = case_when(
    c.inst.brkft == 1 ~ 0,
    c.inst.brkft == 2 ~ 0.07,
    c.inst.brkft == 3 ~ 0.14,
    c.inst.brkft == 4 ~ 0.43,
    c.inst.brkft == 5 ~ 0.71,
    TRUE ~ 0))

#D5: High Prot. Drink

data.df <- data.df %>%
  mutate(whey.bey_spd = case_when(
    whey.bev == 1 ~ 0,
    whey.bev == 2 ~ 0.07,
    whey.bev == 3 ~ 0.14,
    whey.bev == 4 ~ 0.43,
    whey.bev == 5 ~ 0.71,
    TRUE ~ 0))

#D6: Light/low cal/plain yog

data.df <- data.df %>%
  mutate(yog.lt.asp_spd = case_when(
    yog.lt.asp == 1 ~ 0,
    yog.lt.asp == 2 ~ 0.07,
    yog.lt.asp == 3 ~ 0.14,
    yog.lt.asp == 4 ~ 0.57,
    yog.lt.asp == 5 ~ 1.0,
    yog.lt.asp == 6 ~ 2.0,
    TRUE ~ 0))

#D7: Reg yog w/fruit/sweetener

data.df <- data.df %>%
  mutate(yog_spd = case_when(
    yog == 1 ~ 0,
    yog == 2 ~ 0.07,
    yog == 3 ~ 0.14,
    yog == 4 ~ 0.57,
    yog == 5 ~ 1.0,
    yog == 6 ~ 2.0,
    TRUE ~ 0))

#DAIRY- Cottage or ricotta cheese

data.df <- data.df %>%
  mutate(cot.ch_spd = case_when(
    cot.ch == 1 ~ 0,
    cot.ch == 2 ~ 0.07,
    cot.ch == 3 ~ 0.14,
    cot.ch == 4 ~ 0.29,
    TRUE ~ 0))

#DAIRY- Other cheese

data.df <- data.df %>%
  mutate(am.ch_spd = case_when(
    am.ch == 1 ~ 0,
    am.ch == 2 ~ 0.07,
    am.ch == 3 ~ 0.14,
    am.ch == 4 ~ 0.57,
    am.ch == 5 ~ 1.0,
    am.ch == 6 ~ 2.5,
    am.ch == 7 ~ 4.0,
    TRUE ~ 0))

#DAIRY- Cream cheese

data.df <- data.df %>%
  mutate(cr.ch_spd = case_when(
    cr.ch == 1 ~ 0,
    cr.ch == 2 ~ 0.07,
    cr.ch == 3 ~ 0.14,
    cr.ch == 4 ~ 0.57,
    cr.ch == 5 ~ 1.0,
    cr.ch == 6 ~ 2.0,
    TRUE ~ 0))

#DAIRY- Cheese Type: Nonfat

data.df <- data.df %>%
  mutate(ch.nofat_spd = case_when(
    ch.nofat == 1 ~ 1,
    ch.nofat == 2 ~ 0,
    ch.nofat == 3 ~ 0,
    ch.nofat == 4 ~ 0,
    TRUE ~ 0))

#DAIRY- Cheese Type: LF

data.df <- data.df %>%
  mutate(ch.lofat_spd = case_when(
    ch.lofat == 1 ~ 0,
    ch.lofat == 2 ~ 1,
    ch.lofat == 3 ~ 0,
    ch.lofat == 4 ~ 0,
    TRUE ~ 0))

#DAIRY- Cheese Type: Regular

data.df <- data.df %>%
  mutate(ch.reg_spd = case_when(
    ch.reg == 1 ~ 0,
    ch.reg == 2 ~ 0,
    ch.reg == 3 ~ 1,
    ch.reg == 4 ~ 0,
    TRUE ~ 0))

#DAIRY- Cheese Type: Don't Know

data.df <- data.df %>%
  mutate(ch.dk_spd = case_when(
    ch.dk == 1 ~ 0,
    ch.dk == 2 ~ 0,
    ch.dk == 3 ~ 0,
    ch.dk == 4 ~ 1,
    TRUE ~ 0))

#DAIRY- Cheese Type: PT

data.df <- data.df %>%
  mutate(ch.pt_spd = case_when(
    ch.pt == 1 ~ 0,
    ch.pt == 2 ~ 0,
    ch.pt == 3 ~ 0,
    ch.pt == 4 ~ 0,
    TRUE ~ 0))

#Butter

data.df <- data.df %>%
  mutate(bu_spd = case_when(
    bu == 1 ~ 0,
    bu == 2 ~ 0.07,
    bu == 3 ~ 0.14,
    bu == 4 ~ 0.57,
    bu == 5 ~ 1.0,
    bu == 6 ~ 3.0,
    bu == 7 ~ 5.0,
    TRUE ~ 0))

#Margarine

data.df <- data.df %>%
  mutate(margarine_spd = case_when(
    margarine == 1 ~ 0,
    margarine == 2 ~ 0.07,
    margarine == 3 ~ 0.14,
    margarine == 4 ~ 0.57,
    margarine == 5 ~ 1.0,
    margarine == 6 ~ 3.0,
    margarine == 7 ~ 5.0,
    TRUE ~ 0))

#Whipped Cream

data.df <- data.df %>%
  mutate(wh.top_spd = case_when(
    wh.top == 1 ~ 0,
    wh.top == 2 ~ 0.07,
    wh.top == 3 ~ 0.14,
    wh.top == 4 ~ 0.57,
    wh.top == 5 ~ 1.0,
    wh.top == 6 ~ 1.5,
    wh.top == 7 ~ 3.0,
    TRUE ~ 0))

#Booklet Lithocode Page 3

litho3 

#Margarine Form/brand question PT

formbrndpt

#Margarine Form Used - None

data.df <- data.df %>%
  mutate(mn_spd = case_when(
    mn == 1 ~ 1,
    mn == 2 ~ 0,
    mn == 3 ~ 0,
    mn == 4 ~ 0,
    mn == 5 ~ 0,
    mn == 6 ~ 0,
    TRUE ~ 0))

#Margarine Form Used - Stick

data.df <- data.df %>%
  mutate(mst_spd = case_when(
    mst == 1 ~ 0,
    mst == 2 ~ 1,
    mst == 3 ~ 0,
    mst == 4 ~ 0,
    mst == 5 ~ 0,
    mst == 6 ~ 0,
    TRUE ~ 0))

#Margarine Form Used - Tub

data.df <- data.df %>%
  mutate(mt_spd = case_when(
    mt == 1 ~ 0,
    mt == 2 ~ 0,
    mt == 3 ~ 1,
    mt == 4 ~ 0,
    mt == 5 ~ 0,
    mt == 6 ~ 0,
    TRUE ~ 0))

#Margarine Form Used - Squeeze

data.df <- data.df %>%
  mutate(msq_spd = case_when(
    msq == 1 ~ 0,
    msq == 2 ~ 0,
    msq == 3 ~ 0,
    msq == 4 ~ 1,
    msq == 5 ~ 0,
    msq == 6 ~ 0,
    TRUE ~ 0))

#Margarine Form Used - Spray

data.df <- data.df %>%
  mutate(msp_spd = case_when(
    msp == 1 ~ 0,
    msp == 2 ~ 0,
    msp == 3 ~ 0,
    msp == 4 ~ 0,
    msp == 5 ~ 1,
    msp == 6 ~ 0,
    TRUE ~ 0))

#Margarine Form - Don't Know

data.df <- data.df %>%
  mutate(mdn_spd = case_when(
    mdn == 1 ~ 0,
    mdn == 2 ~ 0,
    mdn == 3 ~ 0,
    mdn == 4 ~ 0,
    mdn == 5 ~ 0,
    mdn == 6 ~ 1,
    TRUE ~ 0))

#Margarine Form PT

data.df <- data.df %>%
  mutate(formpt_spd = case_when(
    formpt == 1 ~ 0,
    formpt == 2 ~ 0,
    formpt == 3 ~ 0,
    formpt == 4 ~ 0,
    formpt == 5 ~ 0,
    formpt == 6 ~ 0,
    TRUE ~ 0))

#Q15a: Margarine Brand

margb

#Q15a: Margarine Brand PT

mrgbrnpt

#Type of Oil used



#Cheeseburger

data.df <- data.df %>%
  mutate(burger.ch.bun_spd = case_when(
    burger.ch.bun == 1 ~ 0,
    burger.ch.bun == 2 ~ 0.07,
    burger.ch.bun == 3 ~ 0.14,
    burger.ch.bun == 4 ~ 0.43,
    burger.ch.bun == 5 ~ 0.71,
    TRUE ~ 0))

#Hamburger

data.df <- data.df %>%
  mutate(burger.bun_spd = case_when(
    burger.bun == 1 ~ 0,
    burger.bun == 2 ~ 0.07,
    burger.bun == 3 ~ 0.14,
    burger.bun == 4 ~ 0.43,
    burger.bun == 5 ~ 0.71,
    TRUE ~ 0))

#Tofu

data.df <- data.df %>%
  mutate(tofu_spd = case_when(
    tofu == 1 ~ 0,
    tofu == 2 ~ 0.07,
    tofu == 3 ~ 0.14,
    tofu == 4 ~ 0.43,
    tofu == 5 ~ 0.71,
    TRUE ~ 0))

#Veggieburger

data.df <- data.df %>%
  mutate(veg.burg_spd = case_when(
    veg.burg == 1 ~ 0,
    veg.burg == 2 ~ 0.07,
    veg.burg == 3 ~ 0.14,
    veg.burg == 4 ~ 0.43,
    veg.burg == 5 ~ 0.71,
    TRUE ~ 0))

#Pizza (2 slices)

data.df <- data.df %>%
  mutate(pizza.f.r_spd = case_when(
    pizza.f.r == 1 ~ 0,
    pizza.f.r == 2 ~ 0.07,
    pizza.f.r == 3 ~ 0.14,
    pizza.f.r == 4 ~ 0.43,
    pizza.f.r == 5 ~ 0.71,
    TRUE ~ 0))

#Tacos

data.df <- data.df %>%
  mutate(taco_spd = case_when(
    taco == 1 ~ 0,
    taco == 2 ~ 0.07,
    taco == 3 ~ 0.14,
    taco == 4 ~ 0.43,
    taco == 5 ~ 0.71,
    TRUE ~ 0))

#Burrito

data.df <- data.df %>%
  mutate(burrito_spd = case_when(
    burrito == 1 ~ 0,
    burrito == 2 ~ 0.07,
    burrito == 3 ~ 0.14,
    burrito == 4 ~ 0.43,
    burrito == 5 ~ 0.71,
    TRUE ~ 0))

#Taco filling used: Beef and Bean

data.df <- data.df %>%
  mutate(tacofl.bb_spd = case_when(
    tacofl.bb == 1 ~ 1,
    tacofl.bb == 2 ~ 0,
    tacofl.bb == 3 ~ 0,
    tacofl.bb == 4 ~ 0,
    TRUE ~ 0))

#Taco filling used: Beef

data.df <- data.df %>%
  mutate(tacofl.bf_spd = case_when(
    tacofl.bf == 1 ~ 0,
    tacofl.bf == 2 ~ 1,
    tacofl.bf == 3 ~ 0,
    tacofl.bf == 4 ~ 0,
    TRUE ~ 0))

#Taco filling used: Chicken

data.df <- data.df %>%
  mutate(tacofl.ch_spd = case_when(
    tacofl.ch == 1 ~ 0,
    tacofl.ch == 2 ~ 0,
    tacofl.ch == 3 ~ 1,
    tacofl.ch == 4 ~ 0,
    TRUE ~ 0))

#Taco filling used: Bean

data.df <- data.df %>%
  mutate(tacofl.bn_spd = case_when(
    tacofl.bn == 1 ~ 0,
    tacofl.bn == 2 ~ 0,
    tacofl.bn == 3 ~ 0,
    tacofl.bn == 4 ~ 1,
    TRUE ~ 0))

#Taco filling used: Pass thru

data.df <- data.df %>%
  mutate(tacofl.pt_spd = case_when(
    tacofl.pt == 1 ~ 0,
    tacofl.pt == 2 ~ 0,
    tacofl.pt == 3 ~ 0,
    tacofl.pt == 4 ~ 0,
    TRUE ~ 0))

#Chicken Nuggets (6)

data.df <- data.df %>%
  mutate(chix.nug_spd = case_when(
    chix.nug == 1 ~ 0,
    chix.nug == 2 ~ 0.07,
    chix.nug == 3 ~ 0.14,
    chix.nug == 4 ~ 0.43,
    chix.nug == 5 ~ 0.71,
    TRUE ~ 0))

#Beef/pork hot dogs

data.df <- data.df %>%
  mutate(hotdog_spd = case_when(
    hotdog == 1 ~ 0,
    hotdog == 2 ~ 0.07,
    hotdog == 3 ~ 0.14,
    hotdog == 4 ~ 0.43,
    hotdog == 5 ~ 0.71,
    TRUE ~ 0))

#Chicken or turkey hot dogs

data.df <- data.df %>%
  mutate(chix.dog_spd = case_when(
    chix.dog == 1 ~ 0,
    chix.dog == 2 ~ 0.07,
    chix.dog == 3 ~ 0.14,
    chix.dog == 4 ~ 0.43,
    chix.dog == 5 ~ 0.71,
    TRUE ~ 0))

#Chix/turk as a mixed dish

data.df <- data.df %>%
  mutate(chix.mix_spd = case_when(
    chix.mix == 1 ~ 0,
    chix.mix == 2 ~ 0.07,
    chix.mix == 3 ~ 0.14,
    chix.mix == 4 ~ 0.43,
    chix.mix == 5 ~ 0.71,
    TRUE ~ 0))

#Chix/Turk as a main dish

data.df <- data.df %>%
  mutate(chix.no_spd = case_when(
    chix.no == 1 ~ 0,
    chix.no == 2 ~ 0.07,
    chix.no == 3 ~ 0.14,
    chix.no == 4 ~ 0.43,
    chix.no == 5 ~ 0.71,
    TRUE ~ 0))

#Eat skin of chix/turk

data.df <- data.df %>%
  mutate(eatskin_spd = case_when(
    eatskin == 1 ~ 'Yes',
    eatskin == 2 ~ 'No',
    eatskin == 3 ~ 'Sometimes',
    eatskin == 4 ~ "Don't Eat chicken or turkey",
    TRUE ~ 0))

#Fish sticks/cakes/sandwich

data.df <- data.df %>%
  mutate(fr.fish.kids_spd = case_when(
    fr.fish.kids == 1 ~ 0,
    fr.fish.kids == 2 ~ 0.07,
    fr.fish.kids == 3 ~ 0.14,
    fr.fish.kids == 4 ~ 0.29,
    TRUE ~ 0))

#Dark meat fish

data.df <- data.df %>%
  mutate(dk.fish_spd = case_when(
    dk.fish == 1 ~ 0,
    dk.fish == 2 ~ 0.07,
    dk.fish == 3 ~ 0.14,
    dk.fish == 4 ~ 0.43,
    dk.fish == 5 ~ 0.71,
    TRUE ~ 0))

#Other Fish

data.df <- data.df %>%
mutate(oth.fish_spd = case_when(
  oth.fish == 1 ~ 0,
  oth.fish == 2 ~ 0.07,
  oth.fish == 3 ~ 0.14,
  oth.fish == 4 ~ 0.43,
  oth.fish == 5 ~ 0.71,
  TRUE ~ 0))

#Shrimp, Lobster, Scallops

data.df <- data.df %>%
  mutate(shrimp.ckd_spd = case_when(
    shrimp.ckd == 1 ~ 0,
    shrimp.ckd == 2 ~ 0.07,
    shrimp.ckd == 3 ~ 0.14,
    shrimp.ckd == 4 ~ 0.29,
    TRUE ~ 0))

#Beef/Pork/Lamb/ as a mixed dish

data.df <- data.df %>%
  mutate(beef.mix_spd = case_when(
    beef.mix == 1 ~ 0,
    beef.mix == 2 ~ 0.07,
    beef.mix == 3 ~ 0.14,
    beef.mix == 4 ~ 0.43,
    beef.mix == 5 ~ 0.71,
    TRUE ~ 0))

#Beef or Lamb as a main dish

data.df <- data.df %>%
  mutate(beef_spd = case_when(
    beef == 1 ~ 0,
    beef == 2 ~ 0.07,
    beef == 3 ~ 0.14,
    beef == 4 ~ 0.43,
    beef == 5 ~ 0.71,
    TRUE ~ 0))

#Pork as a main dish

data.df <- data.df %>%
  mutate(pork_spd = case_when(
    pork == 1 ~ 0,
    pork == 2 ~ 0.07,
    pork == 3 ~ 0.14,
    pork == 4 ~ 0.43,
    pork == 5 ~ 0.71,
    TRUE ~ 0))

#Meatballs or Meatloaf

data.df <- data.df %>%
  mutate(meatballs_spd = case_when(
    meatballs == 1 ~ 0,
    meatballs == 2 ~ 0.07,
    meatballs == 3 ~ 0.14,
    meatballs == 4 ~ 0.43,
    meatballs == 5 ~ 0.71,
    TRUE ~ 0))

#Noodles/Pasta

data.df <- data.df %>%
  mutate(pasta_spd = case_when(
    pasta == 1 ~ 0,
    pasta == 2 ~ 0.07,
    pasta == 3 ~ 0.14,
    pasta == 4 ~ 0.43,
    pasta == 5 ~ 0.71,
    TRUE ~ 0))

#Lasagna/Baked Ziti/ Ravioli

data.df <- data.df %>%
  mutate(lasagna_spd = case_when(
    lasagna == 1 ~ 0,
    lasagna == 2 ~ 0.07,
    lasagna == 3 ~ 0.14,
    lasagna == 4 ~ 0.29,
    TRUE ~ 0))

#Mac and Cheese

data.df <- data.df %>%
  mutate(mac.cheese_spd = case_when(
    mac.cheese == 1 ~ 0,
    mac.cheese == 2 ~ 0.07,
    mac.cheese == 3 ~ 0.14,
    mac.cheese == 4 ~ 0.29,
    TRUE ~ 0))

#Spaghetti/Pasta w/ tomato sauce

data.df <- data.df %>%
  mutate(spagh.sce2_spd = case_when(
    spagh.sce2 == 1 ~ 0,
    spagh.sce2 == 2 ~ 0.07,
    spagh.sce2 == 3 ~ 0.14,
    spagh.sce2 == 4 ~ 0.43,
    spagh.sce2 == 5 ~ 0.71,
    TRUE ~ 0))

#Eggs(1)

data.df <- data.df %>%
  mutate(eggs_spd = case_when(
    eggs == 1 ~ 0,
    eggs == 2 ~ 0.07,
    eggs == 3 ~ 0.14,
    eggs == 4 ~ 0.43,
    eggs == 5 ~ 0.71,
    TRUE ~ 0))

#Sausage (beef/pork)

data.df <- data.df %>%
  mutate(proc.mts_spd = case_when(
    proc.mts == 1 ~ 0,
    proc.mts == 2 ~ 0.07,
    proc.mts == 3 ~ 0.14,
    proc.mts == 4 ~ 0.43,
    proc.mts == 5 ~ 0.71,
    TRUE ~ 0))

#Bacon

data.df <- data.df %>%
  mutate(bacon_spd = case_when(
    bacon == 1 ~ 0,
    bacon == 2 ~ 0.07,
    bacon == 3 ~ 0.14,
    bacon == 4 ~ 0.43,
    bacon == 5 ~ 0.71,
    TRUE ~ 0))

#Liver

data.df <- data.df %>%
  mutate(liver_spd = case_when(
    liver == 1 ~ 0,
    liver == 2 ~ 0.03,
    liver == 3 ~ 0.08,
    liver == 4 ~ 0.29,
    TRUE ~ 0))

#Peanut butter sandwich/pb crax

data.df <- data.df %>%
  mutate(pbj.sand_spd = case_when(
    pbj.sand == 1 ~ 0,
    pbj.sand == 2 ~ 0.07,
    pbj.sand == 3 ~ 0.14,
    pbj.sand == 4 ~ 0.43,
    pbj.sand == 5 ~ 0.71,
    TRUE ~ 0))

#Chicken or turkey sandwich

data.df <- data.df %>%
  mutate(chix.turk.sand_spd = case_when(
    chix.turk.sand == 1 ~ 0,
    chix.turk.sand == 2 ~ 0.07,
    chix.turk.sand == 3 ~ 0.14,
    chix.turk.sand == 4 ~ 0.43,
    chix.turk.sand == 5 ~ 0.71,
    TRUE ~ 0))

#Roast beef sandwich

data.df <- data.df %>%
  mutate(beef.sand_spd = case_when(
    beef.sand == 1 ~ 0,
    beef.sand == 2 ~ 0.07,
    beef.sand == 3 ~ 0.14,
    beef.sand == 4 ~ 0.43,
    beef.sand == 5 ~ 0.71,
    TRUE ~ 0))

#Salami/bologna/ham-deli meat sand
data.df <- data.df %>%
  mutate(bologna.sand_spd = case_when(
    bologna.sand == 1 ~ 0,
    bologna.sand == 2 ~ 0.07,
    bologna.sand == 3 ~ 0.14,
    bologna.sand == 4 ~ 0.43,
    bologna.sand == 5 ~ 0.71,
    TRUE ~ 0))
#Tuna sandwich

data.df <- data.df %>%
  mutate(tuna.sand_spd = case_when(
    tuna.sand == 1 ~ 0,
    tuna.sand == 2 ~ 0.07,
    tuna.sand == 3 ~ 0.14,
    tuna.sand == 4 ~ 0.43,
    tuna.sand == 5 ~ 0.71,
    TRUE ~ 0))

#Grilled cheese sandwich

data.df <- data.df %>%
  mutate(gr.cheese.sand_spd = case_when(
    gr.cheese.sand == 1 ~ 0,
    gr.cheese.sand == 2 ~ 0.07,
    gr.cheese.sand == 3 ~ 0.14,
    gr.cheese.sand == 4 ~ 0.43,
    gr.cheese.sand == 5 ~ 0.71,
    TRUE ~ 0))

#Booklet Lithocode Page 5

litho5

#Ketchup

data.df <- data.df %>%
  mutate(catsup_spd = case_when(
    catsup == 1 ~ 0,
    catsup == 2 ~ 0.07,
    catsup == 3 ~ 0.14,
    catsup == 4 ~ 0.43,
    catsup == 5 ~ 0.71,
    TRUE ~ 0))

#Chowder or cream soup

data.df <- data.df %>%
  mutate(chow_spd = case_when(
    chow == 1 ~ 0,
    chow == 2 ~ 0.07,
    chow == 3 ~ 0.14,
    chow == 4 ~ 0.57,
    chow == 5 ~ 1.00,
    TRUE ~ 0))

#Clear soup(w/Rice,noodles,veg)

data.df <- data.df %>%
  mutate(chix.nood_spd = case_when(
    chix.nood == 1 ~ 0,
    chix.nood == 2 ~ 0.07,
    chix.nood == 3 ~ 0.14,
    chix.nood == 4 ~ 0.57,
    chix.nood == 5 ~ 1.00,
    TRUE ~ 0))

#Mayonnaise

data.df <- data.df %>%
  mutate(mayo_spd = case_when(
    mayo == 1 ~ 0,
    mayo == 2 ~ 0.07,
    mayo == 3 ~ 0.14,
    mayo == 4 ~ 0.57,
    mayo == 5 ~ 1.00,
    TRUE ~ 0))

#Low Cal Salad Dressing

data.df <- data.df %>%
  mutate(local.sal.dr_spd = case_when(
    local.sal.dr == 1 ~ 0,
    local.sal.dr == 2 ~ 0.07,
    local.sal.dr == 3 ~ 0.14,
    local.sal.dr == 4 ~ 0.57,
    local.sal.dr == 5 ~ 1.00,
    TRUE ~ 0))

#Salad dressing- not low cal

data.df <- data.df %>%
  mutate(o...v_spd = case_when(
    o...v == 1 ~ 0,
    o...v == 2 ~ 0.07,
    o...v == 3 ~ 0.14,
    o...v == 4 ~ 0.57,
    o...v == 5 ~ 1.00,
    TRUE ~ 0))

#Salsa

data.df <- data.df %>%
  mutate(salsa_spd = case_when(
    salsa == 1 ~ 0,
    salsa == 2 ~ 0.07,
    salsa == 3 ~ 0.14,
    salsa == 4 ~ 0.57,
    salsa == 5 ~ 1.00,
    TRUE ~ 0))

#Sugar (in teaspoons)

data.df <- data.df %>%
  mutate(sug_spd = case_when(
    sug == 1 ~ 0,
    sug == 2 ~ 1.5,
    sug == 3 ~ 3.5,
    sug == 4 ~ 5.0,
    TRUE ~ 0))

#Cereal Brand: Passthru

cerpt

#Usual brand of cold cereal

cer

#Cold breakfast cereal

data.df <- data.df %>%
  mutate(cold.cereal_spd = case_when(
    cold.cereal == 1 ~ 0,
    cold.cereal == 2 ~ 0.07,
    cold.cereal == 3 ~ 0.14,
    cold.cereal == 4 ~ 0.43,
    cold.cereal == 5 ~ 0.86,
    cold.cereal == 6 ~ 2.00,
    TRUE ~ 0))

#Cheerios- Cheat Bubble

cheerio

#Honey Nut Cheerios- Cheat

cheerio.hn

#Frosted mini wheat- Cheat Bubble

fr.miniwht

#Frosted Flakes- Cheat Bubble

fr.flk

#Cinnamon Toast Crunch- Cheat

cintstcr

#Rick Krispies - Cheat Bubble

r.krisp

#Honey Bunches of Oats- Cheat

hon.bun.oats

#Lucky Charms - Cheat Bubble

lucky.ch

#Froot Loops - Cheat Bubble

frt.loop

#Life - Cheat Bubble

life

#Reese's Puffs - Cheat Bubble

reese.pb

#Special K - Cheat Bubble

spec.k

#Cap'n Crunch - Cheat Bubble

capn

#Oatmeal

data.df <- data.df %>%
  mutate(ckd.oats_spd = case_when(
    ckd.oats == 1 ~ 0,
    ckd.oats == 2 ~ 0.07,
    ckd.oats == 3 ~ 0.14,
    ckd.oats == 4 ~ 0.43,
    ckd.oats == 5 ~ 0.86,
    ckd.oats == 6 ~ 2.00,
    TRUE ~ 0))

#Other Hot Breakfast Cereal

data.df <- data.df %>%
  mutate(ckd.cer_spd = case_when(
    ckd.cer == 1 ~ 0,
    ckd.cer == 2 ~ 0.07,
    ckd.cer == 3 ~ 0.14,
    ckd.cer == 4 ~ 0.43,
    ckd.cer == 5 ~ 0.86,
    ckd.cer == 6 ~ 2.00,
    TRUE ~ 0))

#White Bread/Pita/Toast

data.df <- data.df %>%
  mutate(wh.br_spd = case_when(
    wh.br == 1 ~ 0,
    wh.br == 2 ~ 0.14,
    wh.br == 3 ~ 0.43,
    wh.br == 4 ~ 0.86,
    wh.br == 5 ~ 2.50,
    wh.br == 6 ~ 4.00,
    TRUE ~ 0))

#Dark Bread

data.df <- data.df %>%
  mutate(dk.br_spd = case_when(
    dk.br == 1 ~ 0,
    dk.br == 2 ~ 0.14,
    dk.br == 3 ~ 0.43,
    dk.br == 4 ~ 0.86,
    dk.br == 5 ~ 2.50,
    dk.br == 6 ~ 4.00,
    TRUE ~ 0))

#English Muffins or Bagels

data.df <- data.df %>%
  mutate(eng.muff_spd = case_when(
    eng.muff == 1 ~ 0,
    eng.muff == 2 ~ 0.07,
    eng.muff == 3 ~ 0.14,
    eng.muff == 4 ~ 0.43,
    eng.muff == 5 ~ 0.71,
    TRUE ~ 0))

#Muffin or cornbread

data.df <- data.df %>%
  mutate(muff_spd = case_when(
    muff == 1 ~ 0,
    muff == 2 ~ 0.07,
    muff == 3 ~ 0.14,
    muff == 4 ~ 0.43,
    muff == 5 ~ 0.71,
    TRUE ~ 0))

#Croissant

data.df <- data.df %>%
  mutate(croissant_spd = case_when(
    croissant == 1 ~ 0,
    croissant == 2 ~ 0.07,
    croissant == 3 ~ 0.14,
    croissant == 4 ~ 0.43,
    croissant == 5 ~ 0.71,
    TRUE ~ 0))

#Biscuit

data.df <- data.df %>%
  mutate(bisc_spd = case_when(
    bisc == 1 ~ 0,
    bisc == 2 ~ 0.07,
    bisc == 3 ~ 0.14,
    bisc == 4 ~ 0.43,
    bisc == 5 ~ 0.71,
    TRUE ~ 0))

#Rice

data.df <- data.df %>%
  mutate(wh.rice_spd = case_when(
    wh.rice == 1 ~ 0,
    wh.rice == 2 ~ 0.07,
    wh.rice == 3 ~ 0.14,
    wh.rice == 4 ~ 0.43,
    wh.rice == 5 ~ 0.71,
    TRUE ~ 0))

#Brown Rice

data.df <- data.df %>%
  mutate(br.rice_spd = case_when(
    br.rice == 1 ~ 0,
    br.rice == 2 ~ 0.07,
    br.rice == 3 ~ 0.14,
    br.rice == 4 ~ 0.43,
    br.rice == 5 ~ 0.71,
    TRUE ~ 0))

#Tortilla - No Filling

data.df <- data.df %>%
  mutate(tortillas_spd = case_when(
    tortillas == 1 ~ 0,
    tortillas == 2 ~ 0.07,
    tortillas == 3 ~ 0.14,
    tortillas == 4 ~ 0.43,
    tortillas == 5 ~ 0.71,
    TRUE ~ 0))

#Pancakes (2) or Waffles (1)

data.df <- data.df %>%
  mutate(pancak_spd = case_when(
    pancak == 1 ~ 0,
    pancak == 2 ~ 0.07,
    pancak == 3 ~ 0.14,
    pancak == 4 ~ 0.43,
    pancak == 5 ~ 0.71,
    TRUE ~ 0))

#French Toast (2 slices)

data.df <- data.df %>%
  mutate(french.toast_spd = case_when(
    french.toast == 1 ~ 0,
    french.toast == 2 ~ 0.07,
    french.toast == 3 ~ 0.14,
    french.toast == 4 ~ 0.43,
    french.toast == 5 ~ 0.71,
    TRUE ~ 0))

#French Fries (Large Order)

data.df <- data.df %>%
  mutate(ff.pot_spd = case_when(
    ff.pot == 1 ~ 0,
    ff.pot == 2 ~ 0.07,
    ff.pot == 3 ~ 0.14,
    ff.pot == 4 ~ 0.43,
    ff.pot == 5 ~ 0.71,
    TRUE ~ 0))

#Potato:Baked/Boiled/Mashed

data.df <- data.df %>%
  mutate(pot_spd = case_when(
    pot == 1 ~ 0,
    pot == 2 ~ 0.07,
    pot == 3 ~ 0.14,
    pot == 4 ~ 0.43,
    pot == 5 ~ 0.71,
    TRUE ~ 0))

#Raisins (Small Pack)

data.df <- data.df %>%
  mutate(rais_spd = case_when(
    rais == 1 ~ 0,
    rais == 2 ~ 0.07,
    rais == 3 ~ 0.14,
    rais == 4 ~ 0.43,
    rais == 5 ~ 0.71,
    TRUE ~ 0))

#Grapes (Bunch)

data.df <- data.df %>%
  mutate(grapes_spd = case_when(
    grapes == 1 ~ 0,
    grapes == 2 ~ 0.07,
    grapes == 3 ~ 0.14,
    grapes == 4 ~ 0.43,
    grapes == 5 ~ 0.71,
    TRUE ~ 0))

#Bananas

data.df <- data.df %>%
  mutate(ban_spd = case_when(
    ban == 1 ~ 0,
    ban == 2 ~ 0.07,
    ban == 3 ~ 0.14,
    ban == 4 ~ 0.43,
    ban == 5 ~ 0.71,
    TRUE ~ 0))

#Apples

data.df <- data.df %>%
  mutate(french_fries_spd = case_when(
    apple == 1 ~ 0,
    apple == 2 ~ 0.07,
    apple == 3 ~ 0.14,
    apple == 4 ~ 0.57,
    apple == 5 ~ 1.00,
    TRUE ~ 0))

#Applesauce

data.df <- data.df %>%
  mutate(a.sce_spd = case_when(
    a.sce == 1 ~ 0,
    a.sce == 2 ~ 0.07,
    a.sce == 3 ~ 0.14,
    a.sce == 4 ~ 0.57,
    a.sce == 5 ~ 1.00,
    TRUE ~ 0))

#Pears

data.df <- data.df %>%
  mutate(pear_spd = case_when(
    pear == 1 ~ 0,
    pear == 2 ~ 0.07,
    pear == 3 ~ 0.14,
    pear == 4 ~ 0.57,
    pear == 5 ~ 1.00,
    TRUE ~ 0))

#Cantaloupe, Melons

data.df <- data.df %>%
  mutate(cant_spd = case_when(
    cant == 1 ~ 0,
    cant == 2 ~ 0.07,
    cant == 3 ~ 0.14,
    cant == 4 ~ 0.29,
    TRUE ~ 0))

#Watermelon

data.df <- data.df %>%
  mutate(h2omelon_spd = case_when(
    h2omelon == 1 ~ 0,
    h2omelon == 2 ~ 0.07,
    h2omelon == 3 ~ 0.14,
    h2omelon == 4 ~ 0.29,
    TRUE ~ 0))

#Oranges

data.df <- data.df %>%
  mutate(orang_spd = case_when(
    orang == 1 ~ 0,
    orang == 2 ~ 0.07,
    orang == 3 ~ 0.14,
    orang == 4 ~ 0.57,
    orang == 5 ~ 1.00,
    TRUE ~ 0))

#Grapefruit

data.df <- data.df %>%
  mutate(grfrt_spd = case_when(
    grfrt == 1 ~ 0,
    grfrt == 2 ~ 0.07,
    grfrt == 3 ~ 0.14,
    grfrt == 4 ~ 0.57,
    grfrt == 5 ~ 1.00,
    TRUE ~ 0))

#Strawberries

data.df <- data.df %>%
  mutate(straw_spd = case_when(
    straw == 1 ~ 0,
    straw == 2 ~ 0.07,
    straw == 3 ~ 0.14,
    straw == 4 ~ 0.43,
    straw == 5 ~ 0.71,
    TRUE ~ 0))

#Blueberries

data.df <- data.df %>%
  mutate(blue_spd = case_when(
    blue == 1 ~ 0,
    blue == 2 ~ 0.07,
    blue == 3 ~ 0.14,
    blue == 4 ~ 0.43,
    blue == 5 ~ 0.71,
    TRUE ~ 0))

#Peaches, Plums, Apricots

data.df <- data.df %>%
  mutate(peach.cn_spd = case_when(
    peach.cn == 1 ~ 0,
    peach.cn == 2 ~ 0.07,
    peach.cn == 3 ~ 0.14,
    peach.cn == 4 ~ 0.43,
    peach.cn == 5 ~ 0.71,
    TRUE ~ 0))

#Pineapple

data.df <- data.df %>%
  mutate(p.apple_spd = case_when(
    p.apple == 1 ~ 0,
    p.apple == 2 ~ 0.07,
    p.apple == 3 ~ 0.14,
    p.apple == 4 ~ 0.29,
    TRUE ~ 0))

#Orange Juice

data.df <- data.df %>%
  mutate(o.j_spd = case_when(
    o.j == 1 ~ 0,
    o.j == 2 ~ 0.07,
    o.j == 3 ~ 0.14,
    o.j == 4 ~ 0.57,
    o.j == 5 ~ 1.00,
    o.j == 6 ~ 2.00,
    TRUE ~ 0))

#Apple Juice/Other Fruit Juice

data.df <- data.df %>%
  mutate(a.j_spd = case_when(
    a.j == 1 ~ 0,
    a.j == 2 ~ 0.07,
    a.j == 3 ~ 0.14,
    a.j == 4 ~ 0.57,
    a.j == 5 ~ 1.00,
    a.j == 6 ~ 2.00,
    TRUE ~ 0))

#Tomatoes

data.df <- data.df %>%
  mutate(tom_spd = case_when(
    tom == 1 ~ 0,
    tom == 2 ~ 0.07,
    tom == 3 ~ 0.14,
    tom == 4 ~ 0.57,
    tom == 5 ~ 1.00,
    TRUE ~ 0))

#Tomato juice

data.df <- data.df %>%
  mutate(tom.j_spd = case_when(
    tom.j == 1 ~ 0,
    tom.j == 2 ~ 0.07,
    tom.j == 3 ~ 0.14,
    tom.j == 4 ~ 0.57,
    tom.j == 5 ~ 1.00,
    TRUE ~ 0))

#V8 Fusion

data.df <- data.df %>%
  mutate(v8.fusion_spd = case_when(
    v8.fusion == 1 ~ 0,
    v8.fusion == 2 ~ 0.07,
    v8.fusion == 3 ~ 0.14,
    v8.fusion == 4 ~ 0.57,
    v8.fusion == 5 ~ 1.00,
    TRUE ~ 0))

#green Beans

data.df <- data.df %>%
  mutate(st.beans_spd = case_when(
    st.beans == 1 ~ 0,
    st.beans == 2 ~ 0.07,
    st.beans == 3 ~ 0.14,
    st.beans == 4 ~ 0.43,
    st.beans == 5 ~ 0.71,
    TRUE ~ 0))

#Beans/Lentils/Soybeans

data.df <- data.df %>%
  mutate(beans_spd = case_when(
    beans == 1 ~ 0,
    beans == 2 ~ 0.07,
    beans == 3 ~ 0.14,
    beans == 4 ~ 0.57,
    beans == 5 ~ 1.00,
    TRUE ~ 0))

#Broccoli

data.df <- data.df %>%
  mutate(broc_spd = case_when(
    broc == 1 ~ 0,
    broc == 2 ~ 0.07,
    broc == 3 ~ 0.14,
    broc == 4 ~ 0.43,
    broc == 5 ~ 0.71,
    TRUE ~ 0))

#Cauliflower

data.df <- data.df %>%
  mutate(caul_spd = case_when(
    caul == 1 ~ 0,
    caul == 2 ~ 0.07,
    caul == 3 ~ 0.14,
    caul == 4 ~ 0.43,
    caul == 5 ~ 0.71,
    TRUE ~ 0))

#Corn

data.df <- data.df %>%
  mutate(corn_spd = case_when(
    corn == 1 ~ 0,
    corn == 2 ~ 0.07,
    corn == 3 ~ 0.14,
    corn == 4 ~ 0.43,
    corn == 5 ~ 0.71,
    TRUE ~ 0))

#Peas or Lima Beans

data.df <- data.df %>%
  mutate(peas_spd = case_when(
    peas == 1 ~ 0,
    peas == 2 ~ 0.07,
    peas == 3 ~ 0.14,
    peas == 4 ~ 0.43,
    peas == 5 ~ 0.71,
    TRUE ~ 0))

#Mixed Vegetables

data.df <- data.df %>%
  mutate(mix.veg_spd = case_when(
    mix.veg == 1 ~ 0,
    mix.veg == 2 ~ 0.07,
    mix.veg == 3 ~ 0.14,
    mix.veg == 4 ~ 0.43,
    mix.veg == 5 ~ 0.71,
    TRUE ~ 0))

#Spinach

data.df <- data.df %>%
  mutate(spin.raw_spd = case_when(
    spin.raw == 1 ~ 0,
    spin.raw == 2 ~ 0.07,
    spin.raw == 3 ~ 0.14,
    spin.raw == 4 ~ 0.43,
    spin.raw == 5 ~ 0.71,
    TRUE ~ 0))

#Collard Greens/Kale/Ckd spinach

data.df <- data.df %>%
  mutate(kale_spd = case_when(
    kale == 1 ~ 0,
    kale == 2 ~ 0.07,
    kale == 3 ~ 0.14,
    kale == 4 ~ 0.43,
    kale == 5 ~ 0.71,
    TRUE ~ 0))

#Green/Red Peppers

data.df <- data.df %>%
  mutate(peppers_spd = case_when(
    peppers == 1 ~ 0,
    peppers == 2 ~ 0.07,
    peppers == 3 ~ 0.14,
    peppers == 4 ~ 0.43,
    peppers == 5 ~ 0.71,
    TRUE ~ 0))

#Yams/Sweet Potatoes

data.df <- data.df %>%
  mutate(swt.pot_spd = case_when(
    swt.pot == 1 ~ 0,
    swt.pot == 2 ~ 0.07,
    swt.pot == 3 ~ 0.14,
    swt.pot == 4 ~ 0.43,
    swt.pot == 5 ~ 0.71,
    TRUE ~ 0))

#Zucchini/Summ Squash/Eggplant

data.df <- data.df %>%
  mutate(zuke_spd = case_when(
    zuke == 1 ~ 0,
    zuke == 2 ~ 0.07,
    zuke == 3 ~ 0.14,
    zuke == 4 ~ 0.43,
    zuke == 5 ~ 0.71,
    TRUE ~ 0))

#Carrots, Cooked

data.df <- data.df %>%
  mutate(carrot.c_spd = case_when(
    carrot.c == 1 ~ 0,
    carrot.c == 2 ~ 0.07,
    carrot.c == 3 ~ 0.14,
    carrot.c == 4 ~ 0.43,
    carrot.c == 5 ~ 0.71,
    TRUE ~ 0))

#Carrots, Raw

data.df <- data.df %>%
  mutate(carrot.r_spd = case_when(
    carrot.r == 1 ~ 0,
    carrot.r == 2 ~ 0.07,
    carrot.r == 3 ~ 0.14,
    carrot.r == 4 ~ 0.43,
    carrot.r == 5 ~ 0.71,
    TRUE ~ 0))

#Celery

data.df <- data.df %>%
  mutate(celery_spd = case_when(
    celery == 1 ~ 0,
    celery == 2 ~ 0.07,
    celery == 3 ~ 0.14,
    celery == 4 ~ 0.43,
    celery == 5 ~ 0.71,
    TRUE ~ 0))

#Lettuce/Tossed Salad

data.df <- data.df %>%
  mutate(ice.let_spd = case_when(
    ice.let == 1 ~ 0,
    ice.let == 2 ~ 0.07,
    ice.let == 3 ~ 0.14,
    ice.let == 4 ~ 0.57,
    ice.let == 5 ~ 1.00,
    TRUE ~ 0))

#Coleslaw

data.df <- data.df %>%
  mutate(coleslaw_spd = case_when(
    coleslaw == 1 ~ 0,
    coleslaw == 2 ~ 0.07,
    coleslaw == 3 ~ 0.14,
    coleslaw == 4 ~ 0.29,
    TRUE ~ 0))

#Cabbage

data.df <- data.df %>%
  mutate(cabb_spd = case_when(
    cabb == 1 ~ 0,
    cabb == 2 ~ 0.07,
    cabb == 3 ~ 0.14,
    cabb == 4 ~ 0.29,
    TRUE ~ 0))

#Potato salad

data.df <- data.df %>%
  mutate(pot.salad_spd = case_when(
    pot.salad == 1 ~ 0,
    pot.salad == 2 ~ 0.07,
    pot.salad == 3 ~ 0.14,
    pot.salad == 4 ~ 0.29,
    TRUE ~ 0))

#Pasta Salad

data.df <- data.df %>%
  mutate(pasta.salad_spd = case_when(
    pasta.salad == 1 ~ 0,
    pasta.salad == 2 ~ 0.07,
    pasta.salad == 3 ~ 0.14,
    pasta.salad == 4 ~ 0.29,
    TRUE ~ 0))

#Okra

data.df <- data.df %>%
  mutate(okra_spd = case_when(
    okra == 1 ~ 0,
    okra == 2 ~ 0.07,
    okra == 3 ~ 0.14,
    okra == 4 ~ 0.29,
    TRUE ~ 0))

#Booklet Lithocode Page 8

litho8

#Onion rings, ckd onions, or soup

data.df <- data.df %>%
  mutate(onions_spd = case_when(
    onions == 1 ~ 0,
    onions == 2 ~ 0.07,
    onions == 3 ~ 0.14,
    onions == 4 ~ 0.29,
    TRUE ~ 0))

#Potato chips

data.df <- data.df %>%
  mutate(pot.chip_spd = case_when(
    pot.chip == 1 ~ 0,
    pot.chip == 2 ~ 0.07,
    pot.chip == 3 ~ 0.14,
    pot.chip == 4 ~ 0.57,
    pot.chip == 5 ~ 1.00,
    TRUE ~ 0))

#Corn chips

data.df <- data.df %>%
  mutate(corn.chips_spd = case_when(
    corn.chips == 1 ~ 0,
    corn.chips == 2 ~ 0.07,
    corn.chips == 3 ~ 0.14,
    corn.chips == 4 ~ 0.57,
    corn.chips == 5 ~ 1.00,
    TRUE ~ 0))

#Popcorn

data.df <- data.df %>%
  mutate(popc_spd = case_when(
    popc == 1 ~ 0,
    popc == 2 ~ 0.07,
    popc == 3 ~ 0.36,
    popc == 4 ~ 0.71,
    TRUE ~ 0))

#Pretzels

data.df <- data.df %>%
  mutate(pretzel_spd = case_when(
    pretzel == 1 ~ 0,
    pretzel == 2 ~ 0.07,
    pretzel == 3 ~ 0.14,
    pretzel == 4 ~ 0.29,
    TRUE ~ 0))

#Mixed dried fruit/trail mix

data.df <- data.df %>%
  mutate(mix.dr.frt_spd = case_when(
    mix.dr.frt == 1 ~ 0,
    mix.dr.frt == 2 ~ 0.07,
    mix.dr.frt == 3 ~ 0.14,
    mix.dr.frt == 4 ~ 0.57,
    mix.dr.frt == 5 ~ 1.00,
    TRUE ~ 0))

#Peanuts

data.df <- data.df %>%
  mutate(nuts_spd = case_when(
    nuts == 1 ~ 0,
    nuts == 2 ~ 0.07,
    nuts == 3 ~ 0.36,
    nuts == 4 ~ 0.71,
    TRUE ~ 0))

#Other nuts

data.df <- data.df %>%
  mutate(oth.nuts_spd = case_when(
    oth.nuts == 1 ~ 0,
    oth.nuts == 2 ~ 0.07,
    oth.nuts == 3 ~ 0.36,
    oth.nuts == 4 ~ 0.71,
    TRUE ~ 0))

#Fruit snacks or fruit rollups

data.df <- data.df %>%
  mutate(frt.rollups_spd = case_when(
    frt.rollups == 1 ~ 0,
    frt.rollups == 2 ~ 0.07,
    frt.rollups == 3 ~ 0.36,
    frt.rollups == 4 ~ 0.71,
    TRUE ~ 0))

#Graham crackers

data.df <- data.df %>%
  mutate(graham.crax_spd = case_when(
    graham.crax == 1 ~ 0,
    graham.crax == 2 ~ 0.07,
    graham.crax == 3 ~ 0.36,
    graham.crax == 4 ~ 0.71,
    TRUE ~ 0))

#Crackers

data.df <- data.df %>%
  mutate(crax_spd = case_when(
    crax == 1 ~ 0,
    crax == 2 ~ 0.07,
    crax == 3 ~ 0.36,
    crax == 4 ~ 0.71,
    TRUE ~ 0))

#Poptarts (1)

data.df <- data.df %>%
  mutate(poptart_spd = case_when(
    poptart == 1 ~ 0,
    poptart == 2 ~ 0.07,
    poptart == 3 ~ 0.50,
    poptart == 4 ~ 1.00,
    TRUE ~ 0))

#Cake or cupcake with frosting

data.df <- data.df %>%
  mutate(cake.comm_spd = case_when(
    cake.comm == 1 ~ 0,
    cake.comm == 2 ~ 0.07,
    cake.comm == 3 ~ 0.14,
    cake.comm == 4 ~ 0.29,
    TRUE ~ 0))

#Snack cakes-Ring Dings/Twinkies

data.df <- data.df %>%
  mutate(cake.snack_spd = case_when(
    cake.snack == 1 ~ 0,
    cake.snack == 2 ~ 0.07,
    cake.snack == 3 ~ 0.14,
    cake.snack == 4 ~ 0.57,
    cake.snack == 5 ~ 1.00,
    TRUE ~ 0))

#Danish, Sweetroll, Pastry

data.df <- data.df %>%
  mutate(s.roll.c_spd = case_when(
    s.roll.c == 1 ~ 0,
    s.roll.c == 2 ~ 0.07,
    s.roll.c == 3 ~ 0.14,
    s.roll.c == 4 ~ 0.43,
    s.roll.c == 5 ~ 0.71,
    TRUE ~ 0))

#Donuts or churros

data.df <- data.df %>%
  mutate(donut_spd = case_when(
    donut == 1 ~ 0,
    donut == 2 ~ 0.07,
    donut == 3 ~ 0.14,
    donut == 4 ~ 0.57,
    donut == 5 ~ 2.00,
    TRUE ~ 0))

#Cookies, ready made

data.df <- data.df %>%
  mutate(choc.chip.cookie_spd = case_when(
    choc.chip.cookie == 1 ~ 0,
    choc.chip.cookie == 2 ~ 0.07,
    choc.chip.cookie == 3 ~ 0.14,
    choc.chip.cookie == 4 ~ 0.57,
    choc.chip.cookie == 5 ~ 2.00,
    choc.chip.cookie == 6 ~ 4.00,
    TRUE ~ 0))

#Brownies

data.df <- data.df %>%
  mutate(brownie_spd = case_when(
    brownie == 1 ~ 0,
    brownie == 2 ~ 0.07,
    brownie == 3 ~ 0.14,
    brownie == 4 ~ 0.43,
    brownie == 5 ~ 0.71,
    TRUE ~ 0))

#Pie or fruit crisp

data.df <- data.df %>%
  mutate(pie.comm_spd = case_when(
    pie.comm == 1 ~ 0,
    pie.comm == 2 ~ 0.07,
    pie.comm == 3 ~ 0.14,
    pie.comm == 4 ~ 0.29,
    TRUE ~ 0))

#Chocolate (Bar or Packet)

data.df <- data.df %>%
  mutate(choc_spd = case_when(
    choc == 1 ~ 0,
    choc == 2 ~ 0.07,
    choc == 3 ~ 0.14,
    choc == 4 ~ 0.57,
    choc == 5 ~ 1.00,
    TRUE ~ 0))

#Other Candy Bars

data.df <- data.df %>%
  mutate(candy...nuts_spd = case_when(
    candy...nuts == 1 ~ 0,
    candy...nuts == 2 ~ 0.07,
    candy...nuts == 3 ~ 0.14,
    candy...nuts == 4 ~ 0.57,
    candy...nuts == 5 ~ 1.00,
    TRUE ~ 0))

#Other Candy Without Chocolate

data.df <- data.df %>%
  mutate(candy_spd = case_when(
    candy == 1 ~ 0,
    candy == 2 ~ 0.07,
    candy == 3 ~ 0.14,
    candy == 4 ~ 0.57,
    candy == 5 ~ 1.00,
    TRUE ~ 0))

#Jello - not sugar free

data.df <- data.df %>%
  mutate(jello_spd = case_when(
    jello == 1 ~ 0,
    jello == 2 ~ 0.07,
    jello == 3 ~ 0.14,
    jello == 4 ~ 0.43,
    jello == 5 ~ 0.71,
    TRUE ~ 0))

#Pudd/pudd pops, not sugar free

data.df <- data.df %>%
  mutate(pudd_spd = case_when(
    pudd == 1 ~ 0,
    pudd == 2 ~ 0.07,
    pudd == 3 ~ 0.14,
    pudd == 4 ~ 0.43,
    pudd == 5 ~ 0.71,
    TRUE ~ 0))

#Frozen Yogurt

data.df <- data.df %>%
  mutate(yogurt.frozen_spd = case_when(
    yogurt.frozen == 1 ~ 0,
    yogurt.frozen == 2 ~ 0.07,
    yogurt.frozen == 3 ~ 0.14,
    yogurt.frozen == 4 ~ 0.43,
    yogurt.frozen == 5 ~ 0.71,
    TRUE ~ 0))

#Ice Cream

data.df <- data.df %>%
  mutate(ice.cr_spd = case_when(
    ice.cr == 1 ~ 0,
    ice.cr == 2 ~ 0.07,
    ice.cr == 3 ~ 0.14,
    ice.cr == 4 ~ 0.43,
    ice.cr == 5 ~ 0.71,
    TRUE ~ 0))

#Popsicles

data.df <- data.df %>%
  mutate(popsicle_spd = case_when(
    popsicle == 1 ~ 0,
    popsicle == 2 ~ 0.07,
    popsicle == 3 ~ 0.14,
    popsicle == 4 ~ 0.43,
    popsicle == 5 ~ 0.71,
    TRUE ~ 0))

#Seeds - Sunflower or Pumpkin

data.df <- data.df %>%
  mutate(seeds_spd = case_when(
    seeds == 1 ~ 0,
    seeds == 2 ~ 0.07,
    seeds == 3 ~ 0.14,
    seeds == 4 ~ 0.43,
    seeds == 5 ~ 0.71,
    TRUE ~ 0))

#Snack bars-granola/Nutrigrain

data.df <- data.df %>%
  mutate(snack.bar_spd = case_when(
    snack.bar == 1 ~ 0,
    snack.bar == 2 ~ 0.07,
    snack.bar == 3 ~ 0.14,
    snack.bar == 4 ~ 0.43,
    snack.bar == 5 ~ 0.71,
    TRUE ~ 0))

#Energy bars-Clif/Luna/Powerbar

data.df <- data.df %>%
  mutate(energy.bar_spd = case_when(
    energy.bar == 1 ~ 0,
    energy.bar == 2 ~ 0.07,
    energy.bar == 3 ~ 0.14,
    energy.bar == 4 ~ 0.43,
    energy.bar == 5 ~ 0.71,
    TRUE ~ 0))

#High-protein bars-Atkins/Zone

data.df <- data.df %>%
  mutate(prot.bar_spd = case_when(
    prot.bar == 1 ~ 0,
    prot.bar == 2 ~ 0.07,
    prot.bar == 3 ~ 0.14,
    prot.bar == 4 ~ 0.43,
    prot.bar == 5 ~ 0.71,
    TRUE ~ 0))

#Jams/jellies/fluff/syrup/honey
data.df <- data.df %>%
  mutate(jam_spd = case_when(
    jam == 1 ~ 0,
    jam == 2 ~ 0.07,
    jam == 3 ~ 0.14,
    jam == 4 ~ 0.43,
    jam == 5 ~ 2.00,
    TRUE ~ 0))


data.df
