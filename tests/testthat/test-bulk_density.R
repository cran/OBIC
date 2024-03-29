test_that("calc_bulk_density works", {
  expect_equal(
    calc_bulk_density(
      A_SOM_LOI = 2,
      B_SOILTYPE_AGR = "zeeklei"
    ),
    expected = 1230,
    tolerance = 1
  )
  expect_equal(
    calc_bulk_density(
      A_SOM_LOI = c(0.5, 3, 6, 8, 7, 5, 15, 25, 6),
      B_SOILTYPE_AGR = c("duinzand", "dekzand", "zeeklei", "rivierklei", "maasklei", "dalgrond", "moerige_klei", "veen", "loess")
    ),
    expected = c(1500, 1370, 1074, 1020, 1045, 1281, 912, 840, 124),
    tolerance = 1
  )
})