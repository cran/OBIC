test_that("calc_winderodibility works", {
  expect_equal(
    calc_winderodibility(
      A_CLAY_MI=25,
      A_SILT_MI=15,
      B_LU_BRP = 3732
      ),
    expected = c(0.27),
    tolerance = 0.01
  )
  a = c(14, 22, 25, 26, 5, 19, 9, 3, 8, 7)
    expect_equal(
      calc_winderodibility(
        A_CLAY_MI=a,
        A_SILT_MI=rev(a),
        B_LU_BRP = rep(3732,10)
      ),
    expected = c(0.4556666, 0.3536575, 0.3733895, 0.3095705, 0.4174766, 0.4174766, 0.3095705, 0.3733895, 0.3536575, 0.4556666),
    tolerance = 0.01
  )
})

test_that("ind_winderodibility works", {
  expect_equal(
    ind_winderodibility(
      D_P_DU = 0.01
    ),
    expected = c(0.27),
    tolerance = 0.99
  )
  expect_equal(
    ind_winderodibility(
      D_P_DU = c(.48,0.21,.33,.29,.38,.38,.29,.33,.21,.48)
    ),
    expected = 1-c(.48,0.21,.33,.29,.38,.38,.29,.33,.21,.48),
    tolerance = 0.01
  )
})


