pacman::p_load(
  tidyverse,
  psych,
  svglite, # gtExtras dependency
  gtExtras,
  cluster,
  factoextra
)

rm(list=ls())

# 1.0 Data load and prep -------------------------------------------------------

# DentMed  HERO
# DontoRay VILLAIN (Key competitor)
# OxyMed   VILLAIN (Key competitor)
 
# TSB - TRADE SHOW BEHAVIOR     t_
# PPB - PAST PURCAHSE BEHAVIOR  p_
# NEEDS/PREFERENCES             n_
# TRUST IN MEDIA                tr_

data <- read.csv("data_clustering_dmed.csv")
data <- as_tibble(data)

#rename variables
names(data) <-
  c(
    #TSB 
    "t_hero",
    "t_villains",
    "t_total",
    
    #PPB
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

#TODO: check outliers, practice data set doubt it has them


# 2.0 Clean data ---------------------------------------------------------------


## 2.1 NA check ----------------------------------------------------------------
any(is.na(data))

data %>%
  filter(if_any(everything(), is.na)) # 10 full NA rows

data <- na.omit(data)
# removed rows containing at least 1 NA (n= 2330 > 2320)
# all happened to be full rows of NA

## 2.2 Time check --------------------------------------------------------------
any(data$t_hero > data$t_total | data$t_villains > data$t_total) # none faulty
any((data$t_hero + data$t_villains) > data$t_total) #none faulty
#checked if either (or together) are more than total

range(data$t_hero)
range(data$t_villains)
range(data$t_total)


## 2.3 Likerts check -----------------------------------------------------------
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


## 2.4 Percent and Index check -------------------------------------------------
range(data$p_percent) # 5-101 some must be faulty
range(data$p_historic) # 8-65 should be ok, weird but ok

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


## 2.5 Check remaining (p_sales, p_length, p_referrals) ------------------------
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


# 3.0 Checks -------------------------------------------------------------------

## 3.1 TSB ---------------------------------------------------------------------
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


## 3.2 PPB ---------------------------------------------------------------------
data %>%
  select(starts_with("p_")) %>%
  gt_plt_summary()

data %>%
  select(starts_with("p_")) %>%
  describe()

## 3.3 Needs -------------------------------------------------------------------
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


## 3.4 Trust -------------------------------------------------------------------
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


# 4.0 Add columns --------------------------------------------------------------


## 4.1 Time proportions --------------------------------------------------------

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

# TODO: add agreggate overall media trust? maybe not necessary


## 4.2 Check -------------------------------------------------------------------
data %>%
  select(starts_with("tp_")) %>%
  gt_plt_summary()

data %>%
  select(starts_with("tp_")) %>%
  describe()
# roughly the same but hero proportions are overall lower
# more useful when looking at segments


# 5.0 Clustering (Needs) -------------------------------------------------------
set.seed(155)

fviz_nbclust(
  data,
  kmeans,
  method = "wss",
  k.max = 20
  ) +
  theme_minimal()
# after 4 k the slope diminishes
# from 7 to 8 is completely horizontal
# give a chance for up to 7
# 4 or 5 probably best


## 5.1 Hierarchical ------------------------------------------------------------

dmat_needs <- 
  data %>%
  select(starts_with("n_")) %>% 
  scale() %>%
  daisy()

# complete
m1_hc <- 
  dmat_needs %>%
  hclust(
    .,
    method = "complete"
  )

# ward.D
m2_hc <- 
  dmat_needs %>%
  hclust(
    .,
    method = "ward.D"
  )

# ward.D2
m3_hc <- 
  dmat_needs %>%
  hclust(
    .,
    method = "ward.D2"
  )

### 5.1.1 Comparisons of three dendrograms -------------------------------------

# complete ---
plot(m1_hc)
rect.hclust(m1_hc, k=2, border="orange")
rect.hclust(m1_hc, k=4, border="red")
# since height represents dissimilarity among clusters
# we see significant vertical jumps on 2 and 4 k
# same sized jumps, also both split
# makes sense to go with 4 for more insights

# ward.D1  ---
plot(m2_hc)
rect.hclust(m2_hc, k=3, border="orange")
rect.hclust(m2_hc, k=4, border="red")
# vertical jumps on 3 and 4 k
# 4 makes more sense
# going with 3 will ignore obvious heterogeneity in one group

# ward.D2  ---
plot(m3_hc)
rect.hclust(m3_hc, k=4, border="red")
# obvious 4 k

# quick check for 5
plot(m3_hc)
rect.hclust(m3_hc, k=5, border="red")
# fifth is too small

# quick check for 6
plot(m3_hc)
rect.hclust(m3_hc, k=6, border="red")
# doesnt achieve goal of segmenting the largest one further

### 5.1.2 Observe differences --------------------------------------------------

data_tester <-
  data %>%
  select(id, starts_with("n_"))

data_tester$complete <- cutree(m1_hc, k=4) # complete
data_tester$ward1 <- cutree(m2_hc, k=4) # ward.D1
data_tester$ward2 <- cutree(m3_hc, k=4) # ward.D2


# Check sizes ---
table(data_tester$complete)
table(data_tester$ward1)
table(data_tester$ward2)
# basically same sized clusters probably same obs grouped together


# Check if completely equal ---
data_tester %>%
  filter(
    data_tester$complete != data_tester$ward1 |
    data_tester$complete != data_tester$ward2 |
    data_tester$ward1 != data_tester$ward2
  ) %>%
  nrow(.)/nrow(data_tester)
# only 81 obs differ
# less than 4%
# Basically doesn't matter much which linkage we use, but check cpcc


# Check CPCC ---
cor(cophenetic(m1_hc), dmat_needs)
cor(cophenetic(m2_hc), dmat_needs)
cor(cophenetic(m3_hc), dmat_needs)
# but since the third one has the best CPCC with 0.88
# we will go with ward.D2


### 5.1.3 Explore main candidate -----------------------------------------------
data_tester %>%
  group_by(ward2) %>%
  summarise(across(starts_with("n_"), \(x) mean(x, na.rm=TRUE)))
# Check K means clustering too, before settling with 'ward.d2 4 clusters'
# will probably go with this one



## 5.2 K Means -----------------------------------------------------------------


# 2 or 3 clusters miss out on splitting obviously heterogeneous groups
# so lets see if 5 or 6 gives more info
# and we compare 'kmeans k4' to 'hierarchical k4'
m1_k <- kmeans(dmat_needs, centers=4, nstart=25)
m2_k <- kmeans(dmat_needs, centers=5, nstart=25)
m3_k <- kmeans(dmat_needs, centers=6, nstart=25)

data_tester$k_4 <- m1_k$cluster
data_tester$k_5 <- m2_k$cluster
data_tester$k_6 <- m3_k$cluster

### 5.2.1 Compare k=4 with candidate from hierarchical -------------------------

m1_k$size
table(data_tester$ward2)
# hierarchical and kmeans with 4 clusters very similar cluster sizes

# Check how many differ
# need to remap kmeans
data_tester$k_4a <- 0
data_tester$k_4a <- ifelse(data_tester$k_4 == 1, 10, data_tester$k_4a) #uninit k_4a
data_tester$k_4a <- ifelse(data_tester$k_4 == 2, 20, data_tester$k_4a)
data_tester$k_4a <- ifelse(data_tester$k_4 == 3, 30, data_tester$k_4a)
data_tester$k_4a <- ifelse(data_tester$k_4 == 4, 40, data_tester$k_4a)

data_tester$k_4a <- ifelse(data_tester$k_4a == 10, 4, data_tester$k_4a)
data_tester$k_4a <- ifelse(data_tester$k_4a == 20, 1, data_tester$k_4a)
data_tester$k_4a <- ifelse(data_tester$k_4a == 30, 2, data_tester$k_4a)
data_tester$k_4a <- ifelse(data_tester$k_4a == 40, 3, data_tester$k_4a)
#TODO: make a single and dynamic statement for this, if i have time
# if it doesnt work on other pcs skip
# basically negligible difference between
# hierarchical k=4 and kmeans k=4

data_tester %>%
  filter(
    data_tester$k_4a != data_tester$ward2 #k_4a remapped values
  )
# negligible difference
# no need to examine it separately

### 5.2.1 Check k=5 and k=6 ----------------------------------------------------

m1_k$size #k=4
m2_k$size #k=5
m3_k$size #k=6

data_tester %>%
  group_by(k_5) %>%
  summarise(across(starts_with("n_"), \(x) mean(x, na.rm=TRUE)))

data_tester %>%
  group_by(k_6) %>%
  summarise(across(starts_with("n_"), \(x) mean(x, na.rm=TRUE)))
# segments 'pulled out' by k5 and k6
# are mid two clusters
# and they are relatively small cca 100 observations
# i believe nothing is gained by it
# check needs table above just in case



# 6.0 EDA across segments ------------------------------------------------------

data$segment <- as.factor(cutree(m3_hc, k=4))

## 6.1 Differences across Needs ------------------------------------------------

data %>%
  group_by(segment) %>%
  mutate(size = n()) %>%
  mutate(size_prop = size/nrow(data)) %>%
  summarise(
    across(starts_with("n_"), \(x) mean(x, na.rm=TRUE)),
    size = unique(size),
    size_prop = unique(size_prop)
    )

#TODO: sizing couldnt use group size figure out after project
# data %>%
#   group_by(segment) %>%
#   group_size()
# 
# nrow(data[data$segment == "3",])


# Visual inspection ---

#exploration
data %>%
  ggplot() +
  geom_bar(aes(n_image)) +
  facet_grid(.~segment)

data %>%
  ggplot() +
  geom_bar(aes(n_flexibility)) +
  facet_grid(.~segment)

data %>%
  ggplot() +
  geom_bar(aes(n_integration)) +
  facet_grid(.~segment)

data %>%
  ggplot() +
  geom_bar(aes(n_assistance)) +
  facet_grid(.~segment)

data %>%
  ggplot() +
  geom_bar(aes(n_price)) +
  facet_grid(.~segment)
#TODO: need to melt the data for better plots, or 2d facets


# check segment 2 ---
data %>%
  filter(segment == 2) %>%
  select(starts_with("t_")) %>%
  gt_plt_summary()
#TODO: cant bother with this right now
# use below

data %>%
  select(starts_with("n_"), "segment") %>%
  filter(segment == 2) %>%
  pivot_longer(
    starts_with("n_"),
    names_to = "key", 
    values_to = "value"
  ) %>%
  ggplot(aes(value)) +
  geom_bar() +
  facet_grid(.~key) +
  labs(x = "Needs; Likert 1-7", y = "Counts")
# some low outliers from price maybe should have been clustered with Seg3



## 6.2 Differences across PPB --------------------------------------------------
data %>%
  group_by(segment) %>%
  mutate(size = n()) %>%
  mutate(size_prop = size/nrow(data)) %>%
  summarise(
    across(starts_with("p_"), \(x) mean(x, na.rm=TRUE)),
    size = unique(size),
    size_prop = unique(size_prop)
  )
# key descriptors for segments

range(data$p_historic)
range(data$p_referrals)
range(data$p_length)

data %>%
  ggplot() +
  geom_histogram(aes(p_percent)) +
  facet_grid(.~segment)

data %>%
  ggplot() +
  geom_histogram(aes(p_historic)) +
  facet_grid(.~segment)
# above two look similar

# p_percent by p_historic
data %>%
  ggplot() +
  geom_point(aes(p_percent, p_historic, color = segment))
  

data %>%
  ggplot() +
  geom_histogram(aes(p_sales)) +
  facet_grid(.~segment)
  
data %>%
  ggplot() +
  geom_histogram(aes(p_length)) +
  facet_grid(.~segment)


## 6.3 Differences across TSB --------------------------------------------------

# absolute minutes
data %>%
  group_by(segment) %>%
  mutate(size = n()) %>%
  mutate(size_prop = size/nrow(data)) %>%
  summarise(
    across(starts_with("t_"), \(x) mean(x, na.rm=TRUE)),
    size = unique(size),
    size_prop = unique(size_prop)
  )

# proportion of total
data %>%
  group_by(segment) %>%
  mutate(size = n()) %>%
  mutate(size_prop = size/nrow(data)) %>%
  summarise(
    across(starts_with("t_"), \(x) mean(x, na.rm=TRUE)),
    size = unique(size),
    size_prop = unique(size_prop)
  )


## 6.4 Differences across Trust ------------------------------------------------

data %>%
  group_by(segment) %>%
  mutate(size = n()) %>%
  mutate(size_prop = size/nrow(data)) %>%
  summarise(
    across(starts_with("tr_"), \(x) mean(x, na.rm=TRUE)),
    size = unique(size),
    size_prop = unique(size_prop)
  )

# 7.0 Elements for final report ------------------------------------------------

data <- 
  data %>%
  mutate(
    segment = fct_recode(
      segment,
      "Small Fish" = "1",
      "Big Fish" = "2",
      "Whale" = "3",
      "Pebble" = "4",
    )
  )

# continued in rmd file




  
#stop























