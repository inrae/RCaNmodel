test_that("writeTimeLine works", {
  expect_equal(writeTimeLine("aliases", 
                             data.frame(Alias = "toto", Formula = 33),
                             NULL),
               tibble(Task = "added alias toto"))
  
  expect_equal(writeTimeLine("observations", 
                             data.frame(Year = "Year", Serie = 33),
                             data.frame(Year = "Year", Serie = 33.3)),
               tibble(Task = "val of series Serie changed 33.3 -> 33"))
  

  expect_equal(writeTimeLine("metaobs", 
                             data.frame(id = "toto", 
                                        Observation = "Serienew",
                                        Comment = "blabla"),
                             data.frame(id = "toto", 
                                        Observation = "Serieold",
                                        Comment = "blabla2")),
               tibble(Task = "observation Serieold renamed to Serienewseries Serienew Comment changed blabla2 -> blabla"))
})
