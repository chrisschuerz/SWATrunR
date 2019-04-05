context("Loading demo datasets")

library(sf)
library(here)

os <- SWATplusR:::get_os()

test_that("Test wrong or missing inputs:", {
  expect_error(load_demo(dataset = "abc"))
  expect_error(load_demo(dataset = "pro",
                         path = here("tests")))
  expect_error(load_demo(dataset = "pro",
                         swat_version = 2012))
  expect_error(load_demo(dataset = "sub"))
  expect_error(load_demo(dataset = "riv"))
  expect_error(load_demo(dataset = "hru"))
})


test_that("Loading demo Projects:", {
  demo2012 <- load_demo(dataset = "pro",
                        path = here("tests"),
                        swat_version = 2012)
  expect_true(is.character(demo2012))
  expect_true(dir.exists(here("tests/swat2012_demo")))
  expect_warning(load_demo(dataset = "pro",
                           path = here("tests"),
                           swat_version = "2012"))
  unlink(demo2012, recursive = TRUE, force = TRUE)

  if(os == "win") {
    demoplus <- load_demo(dataset = "pro",
                          path = here("tests"),
                          swat_version = "+")
    expect_true(is.character(demoplus))
    expect_true(dir.exists(here("tests/swatplus_demo")))
    expect_warning(load_demo(dataset = "pro",
                             path = here("tests"),
                             swat_version = "Plus"))
    unlink(demoplus, recursive = TRUE, force = TRUE)
  } else {
    expect_error(load_demo(dataset = "pro",
                           path = here("tests"),
                           swat_version = "+"))
  }
})

test_that("Loading observation data:", {
  obs <- load_demo("obs")
  expect_is(obs, "data.frame")
})

test_that("Loading spatial data:", {
  sub2012 <- load_demo("sub", swat_version = 2012)
  subplus <- load_demo("sub", swat_version = "plus")
  riv2012 <- load_demo("riv", swat_version = 2012)
  rivplus <- load_demo("riv", swat_version = "plus")
  hru2012 <- load_demo("hru", swat_version = 2012)
  hruplus <- load_demo("hru", swat_version = "plus")

  expect_is(read_sf(sub2012), "sf")
  expect_is(read_sf(subplus), "sf")
  expect_is(read_sf(riv2012), "sf")
  expect_is(read_sf(rivplus), "sf")
  expect_is(read_sf(hru2012), "sf")
  expect_is(read_sf(hruplus), "sf")
})
