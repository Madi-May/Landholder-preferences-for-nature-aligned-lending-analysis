# Claude attempt at converting stata BWS set-up to R code
################################################################################
# Anchored BWS
# 10 items | 8 tasks | 4 alternatives per task
################################################################################

library(haven)        # read_sav(), read_dta()
library(dplyr)        # data wrangling
library(tidyr)        # pivot_longer(), uncount()
library(purrr)        # map_dfr()
library(psych)        # fa() for factor scores
library(survival)     # clogit()
library(modelsummary) # results table


################################################################################
# IMPORT & CLEAN
################################################################################

# read in your data downloaded from qualtrics, remove any non-completes and consider dropping super speeders
raw <- read_sav("...") |>
  filter(Finished != 0,
         Duration__in_seconds_ >= 300) |>
  mutate(id = row_number())


################################################################################
# DERIVED VARIABLES
################################################################################

base <- raw |>
  select(-matches("^b\\d+$"),     # Qualtrics MaxDiff best indicators
         -matches("^v\\d+_1$"),   # display-order tracking
         -matches("^t\\d+$"),     # task timing
         -matches("^_v\\d+$"),    # internal version variables
         -any_of(c(
           "StartDate","EndDate","Status","IPAddress","Progress","Finished",
           "RecordedDate","ResponseId","RecipientLastName","RecipientFirstName",
           "RecipientEmail","ExternalReference","LocationLatitude","LocationLongitude",
           "DistributionChannel","UserLanguage","comment","psid","entrySigCheck",
           "verification","expectedSignature","errorMessage","completeLink",
           "screenOutLink","overQuotaLink","invalidSignatureLink",
           "customLink1","customLink2","customLink3"
         )))

saveRDS(base, "name.rds")


################################################################################
# ANCHOR DATASET
# One row per respondent x item x 2 alternatives (unacceptable vs acceptable OR in madi's case important vs. unimportant)
# alt 1 = unacceptable | alt 2 = acceptable (the BWS item)
# bwsanc_x == 1 means respondent rated item x as UNacceptable
#
# If Qualtrics presented anchor items in a different order to the BWS items,
# update anc_map so that anc_map[i] points to the right bwsanc_ column.
################################################################################

anc_map <- 1:10   # anc_map[i] = which bwsanc_ column belongs to BWS item i

anchor_base <- base |>
  mutate(across(paste0("bwsanc_", 1:10), ~ replace_na(.x, 0)))

df_anchor <- map_dfr(1:10, function(i) {
  
  anc_col <- paste0("bwsanc_", anc_map[i])
  
  anchor_base |>
    select(id, all_of(anc_col)) |>
    mutate(qu   = 100L + i,
           idqu = id * 1000L + qu) |>
    uncount(2, .id = "alt") |>         # row 1 = unacceptable, row 2 = acceptable
    mutate(
      across(paste0("d", 1:10), ~ 0L),
      !!paste0("d", i) := as.integer(alt == 2),   # d{i} = 1 on the acceptable row
      choi = case_when(
        alt == 1 ~ as.integer(.data[[anc_col]] == 1),   # chose unacceptable
        alt == 2 ~ as.integer(.data[[anc_col]] != 1)    # chose acceptable
      ),
      bw     = "an",
      idqubw = idqu * 10L + 3L
    )
})


################################################################################
# BEST–WORST DATASET
# One row per respondent x task x alternative
################################################################################

# Reshape C columns from wide to long
bw_long <- base |>
  select(id, vers_MAXDIFF, paste0("C", rep(1:8, each = 4), "_", rep(1:4, times = 8))) |>
  pivot_longer(
    cols          = starts_with("C"),
    names_to      = c("qu", "alt"),
    names_pattern = "C(\\d+)_(\\d+)",
    values_to     = "bwans"
  ) |>
  mutate(
    qu    = as.integer(qu),
    alt   = as.integer(alt),
    best  = as.integer(bwans == 1),
    worst = as.integer(bwans == 2),
    set   = as.integer(vers_MAXDIFF),
    idqu  = id * 100L + qu
  )

# Merge design file to get item identities (must contain: set, qu, item1-item4)
bwsdesign <- read_dta("NAME...bwsdesign.dta")

bw_long <- bw_long |>
  left_join(bwsdesign, by = c("set", "qu")) |>
  arrange(idqu, alt) |>
  mutate(
    item = case_when(
      alt == 1 ~ item1,
      alt == 2 ~ item2,
      alt == 3 ~ item3,
      alt == 4 ~ item4
    )
  )

# Item dummies d1-d10
for (i in 1:10) {
  bw_long[[paste0("d", i)]] <- as.integer(bw_long$item == i)
}

# --- Best component ----------------------------------------------------------
df_best <- bw_long |>
  mutate(choi   = best,
         bw     = "best",
         idqubw = idqu * 100L + 1L)

# --- Worst component (sign-flip dummies: worst = negative best) --------------
df_worst <- bw_long |>
  mutate(choi   = worst,
         bw     = "worst",
         idqubw = idqu * 100L + 2L,
         across(paste0("d", 1:10), ~ -.x))


################################################################################
#STACK ALL THREE COMPONENTS & FINALISE
################################################################################

biobws <- bind_rows(df_best, df_worst, df_anchor) |>
  filter(!is.na(id)) |>
  arrange(id, idqubw) |>
  mutate(
    verworst = as.integer(bw == "worst"),
    across(paste0("d", 1:10),
           list(w = ~ verworst * .x),
           .names = "wd{col}")   # wd1-wd10: worst x item interactions
  )

saveRDS(biobws, "biobws.rds")

##
