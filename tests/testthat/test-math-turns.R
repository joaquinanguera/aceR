context("test turns")

test_that("accuracy turn-counting helper functions work", {
  sample_flanker = paste(aceR_sample_data_path("explorer"), "sample-ace-flanker-2.csv", sep = "/")
  flanker_proc = load_ace_file(sample_flanker, app_type = "explorer")
  
  rws = aceR:::to_numeric(flanker_proc$rw)
  # Just for this test, patch the "no_response" values with "incorrect" responses
  accs = aceR:::to_numeric(flanker_proc$correct_button)
  accs = dplyr::coalesce(accs, 1)
  turns = aceR:::identify_turns(accs)
  avg_turns = aceR:::ace_turns(rws, accs, n = 3)
  
  for (i in turns) {
    m = accs[i]
    n = accs[i - 1]
    expect_equal(m, 1)
    expect_equal(n, 0)
  }
})
