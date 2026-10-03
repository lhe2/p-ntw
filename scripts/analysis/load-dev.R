# load ntw dev data
# 2026-06-12

# filter out ntw-only dev data and load into analysis scripts
# (bc i did a lot of stuff in 2023)

source(here::here("scripts/tidy/R/dev.R"))

# util fn (useful here but also elsewhere for on-the-fly manipulation of wide data)
# TODO could drop jdate column after pivot maybe?

DevToLong <- function(wide_df){
  wide_df %>%
    pivot_longer(cols = starts_with(c("jdate", "mass", "tt")),
                 names_to = c(".value", "instar"),
                 values_drop_na = TRUE,
                 names_pattern = ("([a-z]*\\d*)\\.(\\d*[a-z]*)")) %>%
    
    # # drop empty dev times but keep masses
    # mutate(tt = case_when(is.na(tt) & !is.na(mass) ~ 999,
    #                       TRUE ~ as.numeric(tt))) %>%
    # drop_na(tt) %>%
    # mutate(tt = na_if(tt, 999)) %>%
    
    # drop longevity durations for short-lived eclosed bugs/culled on eclosion
    filter(!(instar == "long" & tt == 0))
}

# data prep

dfs_tidy <- list(
  wide = df_wide %>%
    # NOTE: not sure where else this used aside from dev EDAs?
    # mutate(mass.change = (mass.pupa - mass.eclose)/mass.pupa,
    #        mass.prop = mass.pupa/mass.eclose) %>%
    filter((year == 2023 & (instar.enter == "hatch" |
                             pop == "col" |
                             (pop == "field" & instar.enter %in% c("1st", "2nd")))) |
             year %in% c(2024, 2025),
           trt %in% c(260, 419, 426, 433),
           !(year == 2023 & src == "F1")
    ) %>%
    select(-c(instar.enter, parent.tent))
)

dfs_tidy <- list_modify(
  dfs_tidy,
  long = dfs_tidy$wide %>% DevToLong()
)

rm(df_wide)