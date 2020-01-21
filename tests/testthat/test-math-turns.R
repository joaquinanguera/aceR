context("test turns")

sample_boxed = paste(aceR_sample_data_path(), "sample-boxed-email.csv", sep = "/")
boxed_proc = load_ace_file(sample_boxed)

# note: for simplicity's sake, we're collapsing data into single block.

rws = aceR:::to_numeric(boxed_proc$rw)
accs = aceR:::to_numeric(boxed_proc$correct_button)
turns = aceR:::identify_turns(accs)
avg_turns = aceR:::ace_turns(rws, accs, n = 3)

for (i in turns) {
  m = accs[i]
  n = accs[i - 1]
  expect_equal(m, 1)
  expect_equal(n, 0)
}
