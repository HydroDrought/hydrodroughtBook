library(tidyverse)

# Regional dataset -----
dict <- tibble(
  site = c(
    1151L, 208L, 3473L, 135L, 148L, 1430L,
    1431L, 460L, 3330L, 382L, 401L, 1348L, 1476L, 1438L, 2325L, 4410L,
    1343L, 1346L, 370L, 1446L, 3326L, 2463L, 1448L, 4414L, 107L,
    353L, 480L, 2457L, 1450L, 463L, 1456L, 3311L, 2459L, 1465L, 1169L,
    2356L, 455L, 141L, 1487L, 363L, 153L, 2365L, 1304L, 407L, 1120L,
    3303L, 1325L, 478L, 1489L, 1421L, 1491L, 207L, 139L, 2359L, 2476L,
    3304L, 1424L, 396L, 3301L, 205L, 1432L, 192L, 226L, 412L, 2472L,
    1349L, 3318L, 2357L, 1131L, 1452L, 367L, 1464L, 1480L, 1467L,
    1344L, 1162L, 2474L, 2455L, 479L, 213L, 384L, 1495L, 433L
  ),
  number = c(
    2L,
    23L, 27L, 30L, 47L, 49L, 59L, 92L, 96L, 99L, 127L, 130L, 141L,
    147L, 156L, 162L, 180L, 207L, 213L, 235L, 239L, 247L, 251L, 266L,
    267L, 271L, 272L, 273L, 276L, 288L, 293L, 317L, 324L, 355L, 359L,
    378L, 379L, 398L, 399L, 401L, 407L, 412L, 417L, 430L, 438L, 443L,
    454L, 463L, 470L, 487L, 499L, 501L, 506L, 519L, 531L, 532L, 1L,
    9L, 15L, 43L, 60L, 93L, 113L, 135L, 155L, 233L, 250L, 270L, 278L,
    284L, 307L, 351L, 375L, 377L, 406L, 414L, 422L, 478L, 491L, 508L,
    510L, 535L, 557L
  )
)

regional_old <- read_rds("../../data/firstEdition/regional.rds") %>%
  left_join(dict, by = "number") %>%
  rename(river = River, data = discharge)




# Global (international dataset) ----

ord <- c("Lindenborg", "Dawib", "Bagamati", "Rhine", "Hurunui", "Ngaruroro", "Lågen",
         "Ostri",
         "Inva", "Elands",
         "Sabar",  "Lambourn",  "Ray", "Arroyo Seco", "Honokohau", "Pecos",
         "Trent", "Upper Guadiana")

global_old <- read_rds("../../data/firstEdition/global.rds") %>%
  mutate(river = factor(River, levels = ord)) %>%
  arrange(river)



# shortcut for river Ngaruroro ----
ngaruroro_old <- global_old %>%
  filter(river == "Ngaruroro") %>%
  select(discharge) %>%
  unnest(discharge)


# Export ----
dataFirstEdition <- list(
  global = global_old,
  regional = regional_old,
  ng = ngaruroro_old
)

usethis::use_data(dataFirstEdition, internal = TRUE, overwrite = TRUE)
