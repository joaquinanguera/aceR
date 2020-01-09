context("test turns")

test_that("accuracy turn-counting helper functions work", {
  sample_flanker = paste(aceR_sample_data_path(), "sample-ace-flanker-2.csv", sep = "/")
  flanker_proc = load_ace_file(sample_flanker)
  
  rws = aceR:::to_numeric(flanker_proc$rw)
  accs = aceR:::to_numeric(flanker_proc$correct_button)
  turns = aceR:::identify_turns(accs)
  avg_turns = aceR:::ace_turns(rws, accs, n = 3)
  
  for (i in turns) {
    m = accs[i]
    n = accs[i - 1]
    expect_equal(m, 1)
    expect_equal(n, 0)
  }
})
