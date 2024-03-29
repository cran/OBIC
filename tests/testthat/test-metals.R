test_that("calc_cu works", {
  expect_equal(
    calc_copper_availability(
      A_CU_CC = seq(1,200,length.out = 7),
      A_SOM_LOI = rep(4,7),
      A_MN_CC = rep(1300,7),
      A_CLAY_MI = rep(15,7),
      A_K_CC = rep(85, 7),
      B_LU_BRP = rep(3732,7)
      ),
    expected = c(1.97,6.34,7.93,9.06,9.96,10.71,11.38),
    tolerance = 0.01
  )
  expect_equal(
    calc_copper_availability(
      A_CU_CC = seq(1,50,length.out = 7),
      A_SOM_LOI = rep(4,7),
      A_MN_CC = rep(1300,7),
      A_CLAY_MI = rep(15,7),
      A_K_CC = rep(85, 7),
      B_LU_BRP = rep(265,7)
    ),
    expected = c(1.97,4.1,5.06,5.75,6.31,6.78,7.19),
    tolerance = 0.01
  )
  
})

test_that("ind_copper works", {
  expect_equal(
    ind_copper(
      D_CU = c(1.57,3.05,3.47,3.74,3.94,4.11,4.25),
      B_LU_BRP = rep(3732,7)
    ),
    expected = c(0,0.34,0.54,0.65,0.72,0.77,0.81),
    tolerance = 0.99
  )
  expect_equal(
    ind_copper(
      D_CU = c(1.97,4.10,5.06,5.75,6.31,6.78,7.2),
      B_LU_BRP = rep(265,7)
    ),
    expected = c(0.17,0.79,0.92,0.96,0.98,0.99,0.99),
    tolerance = 0.99
  )
})

test_that("calc_zn works", {
  expect_equal(
    calc_zinc_availability(
      A_ZN_CC = seq(100,5000,length.out = 7),
      A_PH_CC = rep(4.5,7),
      B_LU_BRP = rep(3732,7),
      B_SOILTYPE_AGR = rep('zeeklei',7)
    ),
    expected = c(8.04,27.79,39.70,49.28,57.57,62.02,71.85),
    tolerance = 0.01
  )
  expect_equal(
    calc_zinc_availability(
      A_ZN_CC = c(seq(100,1000,length.out = 6),2500),
      A_PH_CC = rep(4.5,7),
      B_LU_BRP = rep(265,7),
      B_SOILTYPE_AGR = rep('dekzand',7)
    ),
    expected = c(3.47,6.91,9.64,12.03,14.199,16.22,29.97),
    tolerance = 1
  )
})

test_that("ind_zinc works", {
  expect_equal(
    ind_zinc(
      D_ZN = c(8.04,27.79,39.70,49.28,57.57,62.02,71.85)
    ),
    expected = c(0.4,0.479,0.636,0.743,0.820,0.856,0.921),
    tolerance = 0.01
  )
  expect_equal(
    ind_zinc(
      D_ZN = c(3.47,6.91,9.64,12.03,14.199,16.22,29.97)
    ),
    expected = c(0.4,0.4,0.4,0.4,0.4,0.4,0.5096),
    tolerance = 0.01
  )
})

