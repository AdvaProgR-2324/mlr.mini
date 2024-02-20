test_that("Test Hyperparameter", {
  hpx <- hp(x = p_num(0, 1), y = p_int(1, Inf), z = p_fct(letters))
  
  expect_true(hpCheck(list(x = 1, y = 1, z = "a"), hpx))
  expect_true(hpCheck(list(z = "a"), hpx))
  expect_equal(hpCheck(list(z = "A"), hpx), "Must be element of set {'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z'}, but is 'A'")
  expect_error(hpCheck(c(x = 1, y = 1), hpx))
  
  
  expect_error(p_num("a", 1))
  expect_error(p_num(1, "a"))
  expect_error(p_int("a", 1))
  expect_error(p_int(1, "a"))
  expect_error(p_num(1))
  expect_error(p_int(1))
  expect_error(p_num(1, 0, 2))
  expect_error(p_int(1, 2, 0))
})