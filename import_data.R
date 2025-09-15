library(tidyverse)
library(readxl)
library(tsibble)

# Relative prices until 1994

sector_bg_str <- c(
  "Общо",
  "Производство на електроенергия и топлоенергия",
  "Каменовъглена промишленост",
  "Нефтодобивна и газодобивна промишленост",
  "Черна металургия (вкл. добив на руди)",
  "Цветна металургия (вкл. добив на руди)",
  "Машиностроителна и металообработваща промишленост",
  "Електротехническа и електронна промишленост",
  "Химическа и нефтопреработваща промишленост",
  # Extra
  "Химическа и каучукова промишленост",
  "Промишленост за строителни материали",
  "Дърводобивна и дървообработваща промишленост",
  "Целулозно-хартиена промишленост",
  "Стъкларска и порцеланово-фаянсова промишленост",
  "Текстилна и трикотажна промишленост",
  "Шивашка промишленост",
  "Кожарска кожухарска и обувна промишленост",
  "Полиграфическа промишленост",
  "Хранително-вкусова промишленост",
  "Други отрасли на промишлеността"
)

sector_bg_str_short <- c(
  "Общо",
  "Eлектроенергия и ",
  "Каменовъглена пр.",
  "Нефтодоб/газодобив",
  "Черна м-я",
  "Цветна м-я",
  "Машиностр.",
  "Електротехническа промишленост",
  "Хим. и нефтопреработка",
  # Extra
  "Хим. и каучукова пр.",
  "Стр. материали",
  "Дърводобив/обработка",
  "Целулозна/хартия",
  "Стъкларска пр.",
  "Текстилна пр.",
  "Шивашка пр.",
  "Кожарска и обувна пр.",
  "Полиграфическа пр.",
  "Хранително-вк. пр.",
  "Други"
)

sector_en_str <- c(
  "Total",
  "Production of electricity and heat",
  "Coal industry",
  "Oil and gas extraction industry",
  "Ferrous metallurgy (incl. ore extraction)",
  "Non-ferrous metallurgy (incl. ore extraction)",
  "Machinery and metalworking industry",
  "Electrical and electronic industry",
  "Chemical and oil refining industry",
  # Extra
  "Chemical and oil refining industry",
  "Construction materials industry",
  "Logging and woodworking industry",
  "Pulp and paper industry",
  "Glass and porcelain-faience industry",
  "Textile and knitwear industry",
  "Garment industry",
  "Leather, fur, and footwear industry",
  "Printing industry",
  "Food and beverage industry",
  "Other industrial sectors"
)

sector_en_str_short <- c(
  "Total",
  "Electricity",
  "Coal",
  "Oil",
  "Steel",
  "Non-ferrous metallurgy",
  "Machinery",
  "Electrical",
  "Chemical",
  # Extra
  "Chemical",
  "Construction",
  "Wood",
  "Paper",
  "Glass",
  "Textile",
  "Garment",
  "Leather",
  "Printing",
  "Food",
  "Other"
)

names(sector_en_str_short) <- sector_bg_str
names(sector_bg_str_short) <- sector_en_str_short
data_path <- "data"

dt <- read_excel(file.path(data_path, "Industry all.xlsx"), sheet="Цени на производител", range = "A8:L27", col_types = "text") %>%
  dplyr::select(-starts_with("drop")) %>%
  filter(!is.na(sector)) %>%
  mutate(
    sector = str_replace_all(sector, "\\.+$", ""),
    sector = recode(sector, !!!sector_en_str_short),
  ) %>%
  dplyr::select(sector, everything())

dtl <- dt %>%
  pivot_longer(
    cols = -sector,
    names_to = "year",
    values_to = "ind_idx"
  ) %>%
  mutate(
    year = as.numeric(year),
    ind_idx = as.numeric(ind_idx),
    before90 = ifelse(year < 1990, "Преди 1990", "След 1990"),
    is_before90 = ifelse(year < 1990, 1, 0),
  ) %>%
  filter(
    !is.na(ind_idx)
  )

avg_ind <- dtl %>%
  filter(
    sector == "Total"
  ) %>%
  rename(
    avg_ind = ind_idx
  ) %>%
  ungroup() %>%
  dplyr::select(year, avg_ind)

dtl <- dtl %>%
  left_join(
    avg_ind,
    by = c("year")
  )

dtl <- dtl %>%
  mutate(
    rel_price = ind_idx / avg_ind
  )
dtl


# Producer price indices 1996-2024

excel_file <- file.path("data/PPI Annual Graphs for the paper.xlsx")

ms_map_bg_to_en_long <- c(
  "ШИРОКИ ПАРИ М3" = "M3BM",
  "ПАРИ М1" = "M1",
  "Пари извън ПФИ" = "M1ExPFI",
  "Oвърнайт-депозити" = "OvernightDeposits",
  "Oвърнайт-депозити в левове" = "OvernightDepositsBG",
  "Oвърнайт-депозити в чуждестранна валута" = "OvernightDepositsFC",
  "ПАРИ М2 (М1 + КВАЗИПАРИ)" = "M2",
  "КВАЗИПАРИ" = "QuasiMoney",
  "Депозити с договорен матуритет до 2 години" = "Deposits2Y",
  "Депозити с договорен матуритет до 2 години в левове" = "Deposits2YBGN",
  "Депозити с договорен матуритет до 2 години в чуждестранна валута" = "Deposits2YFC",
  "Депозити, договорени за ползване след предизвестие до 3 месеца" = "Deposits3M",
  "Депозити, договорени за ползване след предизвестие до 3 месеца в левове" = "Deposits3MBGN",
  "Депозити, договорени за ползване след предизвестие до 3 месеца в чуждестранна валута" = "Deposits3MFC",
  "ПАРИ М3 (М2 + ТЪРГУЕМИ ИНСТРУМЕНТИ)" = "M3"
)

# names_map_en_bg_long <-c(
#   "Total" = "Промишленост - общо",
#   "MiningCoal" = "Добив на въглища", 
#   "MiningMeta" = "Добив на метални руди",
#   "MiningNonMetal" = "Добив на неметални материали и суровини",
#   "FoodBeverage" = "Производство на хранителни продукти и напитки",
#   "Tobacco" = "Производство на тютюневи изделия",
#   "Textile" = "Производство на текстил и изделия от текстил, без облекло",
#   "Clothing" = "Производство на облекло", 
#   "LeatherFootwear" = "Обработка на кожи; производство на обувки и други изделия от обработени кожи без косъм",
#   "Woods" = "Производство на дървен материал и изделия от него, без мебели",
#   "Paper" = "Производство на хартия, картон и изделия от хартия и картон",
#   "Printing" = "Печатна дейност и възпроизвеждане на записани носители",
#   "Chemical" = "Manufacture of chemicals and chemical products; basic pharmaceutical products and pharmaceutical preparations",
#   "Rubber" = "Производство на изделия от каучук и пластмаси",
#   "Mineral" = "Производство на изделия от други неметални минерални суровини",
#   "MetalsBasic" = "Производство на основни метали",
#   "MetalProducs" = "Производство на метални изделия, без машини и оборудване", 
#   "Machinery" = "Производство на машини и оборудване, с общо и специално предназначение",
#   "Computers" = "Производство на компютърна и комуникационна техника, електронни и оптични продукти",
#   "Electrical" = "Производство на електрически съоръжения",
#   "Auto" = "Производство на автомобили, ремаркета и полуремаркета",
#   "Transport" = "Производство на превозни средства, без автомобили",
#   "Furniture" = "Производство на мебели",
#   "Energy" = "Производство и разпределение на електрическа и топлоенергия и газ"
# )

# map_bg_to_en <- names(names_map_en_bg_long)
# names(map_bg_to_en) <- names_map_en_bg_long

nok_to_en <-c(
  "B-E36"= "Total",
  "B05" = "MiningCoal", 
  "B07" = "MiningMetal",
  "B08" = "MiningNonMetal",
  "X0" = "Food",
  "C12" = "Tobacco",
  "C13" = "Textile",
  "C14" = "Clothing",
  "C15" = "LeatherFootwear",
  "C16" = "Woods",
  "C17" = "Paper",
  "C18" = "Printing",
  "C20+C21" = "Chemical",  
  "C22" = "Rubber",
  "C23" = "Mineral",
  "C24" = "MetalsBasic",
  "C25" = "MetalProducs",
  "C28" = "Machinery",
  "C26" = "Computers",
  "C27" = "Electrical",
  "C29" = "Auto",
  "C30" = "Transport",
  "C31" = "Furniture",
  "D" = "Energy"
)

nok_to_group <-c(
  "B-E36"= "Total",
  "B05" = "HI", 
  "B07" = "HI",
  "B08" = "HI",
  "X0" = "FBCL",
  "C12" = "FBCL",
  "C13" = "FBCL",
  "C14" = "FBCL",
  "C15" = "FBCL",
  "C16" = "OTHER",
  "C17" = "OTHER",
  "C18" = "OTHER",
  "C20+C21" = "CHEM",  
  "C22" = "CHEM",
  "C23" = "HI",
  "C24" = "HI",
  "C25" = "HI",
  "C28" = "CHEM",
  "C26" = "CHEM",
  "C27" = "CHEM",
  "C29" = "CHEM",
  "C30" = "OTHER",
  "C31" = "OTHER",
  "D" = "HI"
)

rel_annual <- read_excel(excel_file, sheet=1, range="B32:AT37")
names(rel_annual) <- c("Name", 1981:2024)

rel_annual <- rel_annual %>%
  filter(Name %in% c("StdDevAll", "StdDevNoElectricity")) %>%
  pivot_longer(
    cols = -Name,
    names_to = "Year",
    values_to = "Value"
  ) %>%
  mutate(
    Year = as.integer(Year),
  )


transform_monthly <- function(d, start_date, id_col) {
  id_col <- enquo(id_col)
  
  d %>% pivot_longer(
    cols = -c(!!id_col),
    names_to = "year",
    values_to = "value"
  ) %>%
  dplyr::select(-year) %>%
  group_by(!!id_col) %>%
  mutate(
    date = seq.Date(
      from = as.Date(start_date),
      by = "month",
      length.out = n()
    ),
    value = as.numeric(value)
  )
}

dt_money <- read_excel(excel_file, sheet = "money supply monthly", range="A5:MM20") %>%
  transform_monthly("1995-12-01", id_col = Desc) %>%
  mutate(
    Id = ms_map_bg_to_en_long[Desc]
  )

dt_money$Id %>% is.na() %>% sum()

dt_money_w <- dt_money %>%
  pivot_wider(
    id_cols = c(date),
    names_from = Id,
    values_from = value,
  ) %>%
  arrange(date) %>%
  ungroup() %>%
  mutate(
    across(
      c(M1, M2, M3, Deposits2Y, Deposits3M),
      ~ log(.x) - dplyr::lag(log(.x)),
      .names = "{col}d"
    ),
  )

dt_abs_init <- read_excel(excel_file, sheet = "monthly", range="A4:MP28") %>%
  dplyr::select(-Desc) %>%
  transform_monthly("1996-01-01", id_col = NOK) %>%
  mutate(
    Id = nok_to_en[NOK],
    IdGr = nok_to_group[NOK]
  )

dt_abs_total <- dt_abs_init %>%
  dplyr::select(date, value, Id) %>%
  ungroup() %>%
  filter(Id == "Total") %>%
  dplyr::select(Total = value, date)

dt_abs <- dt_abs_init %>%
  filter(Id != "Total") %>%
  left_join(
    dt_abs_total,
    by = "date"
  ) %>%
  group_by(Id) %>%
  mutate(
    Rel = value / Total,
    ValueM = value / dplyr::lag(value),
    TotalM = Total / dplyr::lag(Total),
    RelM = ValueM / TotalM,
  ) %>%
  ungroup()

# dt_rel <- read_excel(excel_file, sheet = "monthly", range="A33:MP57") %>%
#   dplyr::select(-Desc) %>%
#   transform_monthly("1996-01-01", id_col = NOK) %>%
#   mutate(
#     Id = nok_to_en[NOK],
#     IdGr = nok_to_group[NOK]
#   ) %>%
#   filter(Id != "Total")

# dt_abs$Id %>% is.na() %>% sum()


# A function to extract the impulse-response data from irf

extract_irf_data <- function(irf_obj, impulse) {
  irf_data <- irf_obj$irf[[impulse]] %>%
    as_tibble() %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "irf"
    )
  lb <- irf_obj$Lower[[impulse]] %>%
    as_tibble() %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "lower"
    )
  ub <- irf_obj$Upper[[impulse]] %>%
    as_tibble() %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable",
      values_to = "upper"
    )
  irf_data <- irf_data %>%
    bind_cols(lb$lower, ub$upper)
  
  names(irf_data) <- c("variable", "irf", "lower", "upper")
  
  irf_data <- irf_data %>%
    mutate(
      lg = rep(seq(1, nrow(irf_data) / length(unique(irf_data$variable))), each = length(unique(irf_data$variable))),
    )
  
  return(irf_data)
}
