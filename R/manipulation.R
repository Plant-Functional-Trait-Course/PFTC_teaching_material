raw_traits <- raw_traits |>
  # simplify
  select(-Project, -Functional_group, -Year) |>
  #introduce errors
  # wrong date
  mutate(Date = as.character(Date),
         Date = if_else(ID == "AMO3822", "18", Date),
         # giant leaf
         Value = if_else(ID == "ANH3472" & Trait == "Leaf_Area_cm2", 17965, Value),
         # diverse species names
         Taxon = case_when(ID == "BON2388" ~ "oxiria digyna",
                           ID == "ATX7216" ~ "oxyria digina",
                           ID == "BSV7581" ~ "oxyra digyna",
                           TRUE ~ Taxon),
         # introduce NAs
         Value = if_else(ID %in% c("BLQ1061", "AZO1656", "AMV4451") & Trait == "LDMC", NA_real_, Value))

# add duplicate row
duplicate <- raw_traits |>
  slice(378)

# introduce dry weight measurements with wrong unit (mg instead of g)
wrong_decimal <- raw_traits |>
  filter(Trait == "Dry_Mass_g") |>
  sample_n(size = 50, replace = FALSE) |>
  mutate(Value2 = Value / 1000)

raw_traits <- raw_traits |>
  left_join(wrong_decimal, by = c("Date", "Gradient", "Site", "PlotID", "Individual_nr", "ID", "Taxon", "Trait", "Value", "Elevation_m", "Latitude_N", "Longitude_E")) |>
  mutate(Value = if_else(!is.na(Value2), Value2, Value)) |>
  select(-Value2) |>
  bind_rows(duplicate)
