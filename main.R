pacman::p_load(
  tidyverse,
  psych,
  gtExtras # svglite dependency
)

rm(list=ls())

# 1.0 DATA LOAD AND PREP -------------------------------------------------------

#DentMed  HERO
#DontoRay VILLAIN (Key competitor)
#OxyMed   VILLAIN (Key competitor)

data <- read.csv("data_clustering_dmed.csv")
data <- as_tibble(data)

#rename variables
names(data) <-
  c(
    #time
    "t_hero",
    "t_villains",
    "t_total",
    
    #past purchase beh
    "p_percent",
    "p_historic",
    "p_sales",
    "p_length",
    "p_referrals",
    
    #needs
    "n_image",
    "n_flexibility",
    "n_integration",
    "n_assistance",
    "n_price",
    
    #trust
    "tr_tv",
    "tr_radio",
    "tr_internet",
    "tr_magazine",
    "tr_peers"
  )

#add id
data <- 
  data %>%
    mutate(id = 1:nrow(data), .before = t_hero)

summary(data)

#TODO: outliers
#TODO: make proportion of times spent


# 2.0 CLEAN DATA ---------------------------------------------------------------


## 2.1 NA CHECK ----------------------------------------------------------------
any(is.na(data))

data %>%
  filter(if_any(everything(), is.na)) # 10 full NA rows

data <- na.omit(data)
# removed rows containing at least 1 NA (n= 2330 > 2320)
# all happened to be full rows of NA

## 2.2 TIME CHECK --------------------------------------------------------------
any(data$t_hero > data$t_total | data$t_villains > data$t_total) # none faulty
any((data$t_hero + data$t_villains) > data$t_total) #none faulty
#checked if either (or together) are more than total

range(data$t_hero)
range(data$t_villains)
range(data$t_total)


## 2.3 LIKERTS RANGE CHECK -----------------------------------------------------
data %>%
  select(c(id, n_image:tr_peers)) %>%
  filter(
    if_any(
      -one_of("id"), 
      ~!between(., 1, 7)
      )
    )
# 20 rows with at least 1 value outside of likert range 1-7
  
data <- 
  data %>%
    filter(
      !if_any(
        (starts_with("n_") | starts_with("tr_")),
        ~!between(., 1, 7)
        )
      )
# removed rows containing values outside of likert range (n= 2320 > 2300)


## 2.4 PERCENT AND INDEX CHECK -------------------------------------------------
range(data$p_percent) # 5-101 some must be faulty
range(data$p_historic) # 8-65 should be ok

data %>%
  select(c(id, p_percent)) %>%
  filter(
    if_any(
      -one_of("id"), 
      ~!between(., 1, 100)
    )
  ) %>%
  print(n=30)
# 28 rows with 101 percentage

data <- 
  data %>%
  filter(
    !if_any(
      "p_percent",
      ~!between(., 1, 100)
    )
  )
# removed rows with values outside of percentage range (n= 2300 > 2272)


## 2.5 CHECK LAST REMAINING VARIABLES (p_sales, p_length, p_referrals) ---------
describe(data$p_sales) #sales freq (yearly number of orders from HERO)
describe(data$p_length) #length of relationship with HERO

# there are observations with yearly number of order of ZERO
# but all have relationship for at least 4 years
# assumption: p_sales tells us the orders for the past year
# THUS
# those with zero could be classified as LOST CUSTOMERS (or ONE TIMERS)
# since they have established relationship with us

describe(data$p_referrals)
# seems ok


# 3.0 EDA ----------------------------------------------------------------------

## 3.1 TRADE SHOW --------------------------------------------------------------
data %>%
  select(starts_with("t_")) %>%
  gt_plt_summary()
# worrying numbers, most spend more at competition

describe(data$t_hero)
describe(data$t_villains)

data %>%
  select(t_villains) %>%
  ggplot() +
  geom_histogram(aes(t_villains), bins = 70)


## 3.2 PAST PURCHASE BEH -------------------------------------------------------
data %>%
  select(starts_with("p_")) %>%
  gt_plt_summary()

data %>%
  select(starts_with("p_")) %>%
  describe()

# TODO: needs detailed look
# p_percent (3 groups = loyal, competing, one timers)
# p_historic (2-3 groups = heavy buyers, low-mid, low)
# p_sales (possibly used to ignore low buyers, maybe 2 groups high and med)
# p_length (very useful, combine with p_percent maybe, 2 groups longtime & new)
# p_referrals (useful to see which segments have advocates)


## 3.3 NEEDS -------------------------------------------------------------------
data %>%
  select(starts_with("n_")) %>%
  gt_plt_summary()

data %>%
  select(n_flexibility) %>%
  ggplot() +
  geom_histogram(aes(n_flexibility))

data %>%
  select(n_assistance) %>%
  ggplot() +
  geom_histogram(aes(n_assistance))

data %>%
  select(starts_with("n_")) %>%
  describe()
# most care a lot about the price
# our advantage in image and flexibility might not be that important
# we might need to focus on tech assistance


## 3.4 TRUST -------------------------------------------------------------------
data %>%
  select(starts_with("tr_")) %>%
  gt_plt_summary()

data %>%
  select(tr_internet) %>%
  ggplot() +
  geom_histogram(aes(tr_internet))

data %>%
  select(starts_with("tr_")) %>%
  describe()
# TV & RADIO 2 groups mediocre trust
# Internet normally dist lowest/still mediocre

# Magazine better
# Peers best


# 4.0 ADD METRICS --------------------------------------------------------------


## 4.1 TIME PROPORTIONS --------------------------------------------------------

# proportions of total time at stands in Trade show
# tp_ time proportions
data <- 
  data %>%
  mutate(tp_hero = t_hero/t_total, .before = p_percent) %>%
  round(2)

data <- 
  data %>%
  mutate(tp_villains = t_villains/t_total, .before = p_percent) %>%
  round(2)


## 4.2 EXPLORE -----------------------------------------------------------------
data %>%
  select(starts_with("tp_")) %>%
  gt_plt_summary()

data %>%
  select(starts_with("tp_")) %>%
  describe()
# roughly the same but hero proportions are overall lower
# more useful when looking at segments


# 5.0 EDA CONTINUED ------------------------------------------------------------

corrs?




















