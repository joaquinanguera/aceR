# aceR

[![Build Status](https://travis-ci.org/josegallegos07/aceR.svg?branch=master)](https://travis-ci.org/josegallegos07/aceR)

`aceR` is an R package around ACE data. 

## Getting Started

See [here](inst/docs/getting-started.md) for a detailed install guide and annotated example.

## Installation

`aceR` is currently only available on github.

Install the latest release from github using [devtools](https://github.com/hadley/devtools):

```
# install.packages("devtools")
devtools::install_github("joaquinanguera/aceR")
```

## Updates

Reinstall the package to update to the latest **stable** version:

```
devtools::install_github("joaquinanguera/aceR")
```

To install the beta version, use `"joaquinanguera/aceR@development"` in the above command. Please be aware that the beta version may have breaking changes relative to your current setup, or just bugs!

The package, as of March 2020, has transitioned to the [CalVer](https://calver.org/) versioning system, with the `0Y.MINOR.MICRO` setup.

- `0Y`: Zero-padded short year. It is assumed that if the year version rolls over, there are breaking changes between this and the last version dated to the previous year.
- `MINOR`: Minor version. The minor version will be rolled over for any of the following. These changes are likely to be breaking to your current pipeline.
    - new functions are added
    - existing functions take a different argument structure
    - existing functions output different data structure
- `MICRO`: Bug fixes. The micro version will be rolled over when internal cleanup or other changes are made that _should_ have no obvious impact to the user.

### Brief release notes

#### 21.1.1  (Current):

Minor implementation changes:

- `load_ace_bulk(app_type = "email")`/`load_ace_bulk(app_type = "pulvinar")` now recodes all valid (>0) BRT RTs as "correct" before adjusting for other app behavior. This should adjust for older versions of the app marking late BRT RTs as "incorrect", which is technically invalid because it's impossible to give an incorrect BRT response.
- Both `post_clean_*()` functions now take a new argument, `extra_demos`, to allow users to pass in a character vector of custom demographic columns to pass through to the cleaned processed data. In the event that users hand-code processed data from `proc_by_module()` with new demographic identifiers, they can now manually specify those columns to be handled with app-default demographics.

#### 21.1.0:

New outputs:

- `load_ace_bulk()` now creates the `trial_number` column if it's not found in the raw data, counting from 0 for each participant/module/condition (so, like in the Boxed task, counting will start over from 0 for the next condition by the same participant). Should only activate for data from very old versions of ACE, where trial number was not automatically written out by the app.

Major implementation changes:
- `load_ace_bulk()` now NAs out all RTs less than 150 ms as "no response".
- `trim_rt_trials()` previously did both range-based and SD-based scrubbing, always doing any SD scrubbing _after_ range scrubbing. These features have now been split into two functions, `trim_rt_trials_range()` and `trim_rt_trials_sd()`, to allow for greater customization of the behavior of either function. Both functions now allow certain modules to be un-scrubbed with the `exclude` argument. `trim_rt_trials_range()` now takes two length-1 arguments for the minimum and maximum range cutoffs, as opposed to the old functionality where both cutoffs were specified in a length-2 vector.

Minor implementation changes:

- `post_clean_chance()` now takes an `app_type` argument with either `"classroom"` or `"explorer"` as values, to account for differing numbers of choices (and differing chance-level accuracy values) in the Task Switch task between ACE Classroom and Explorer.
- Slowly, app output messages are being rendered in color with the [`crayon`](https://github.com/r-lib/crayon) package. Successful processing messages should be rendered in green, non-fatal warnings rendered in yellow, and errors rendered in red.

#### 21.0.1:

Bug fixes:

- `ace_dprime()` (called under the hood to process summaries for SAAT and TNT, among others) now explicitly calculates hit and FA counts with `na.rm = TRUE`, so should stop returning NA for participants with trials NA'd out by cleaning functions like `trim_rt_trials()`.

#### 21.0.0:

New outputs:

- `proc_by_module()` now outputs additional accuracy columns from a recoded version of accuracy, where all late trials are labeled as incorrect irrespective of their original response. (Only appears in data with valid RT/response-window mappings. May not appear for very old ACE Classroom data.)

Minor implementation changes:

- Removing instances of deprecated `dplyr` functions like `funs()` (totally removed?) and the `*_at()`/`*_if()` functions (in progress) to keep up with best practice and silence deprecation errors from `dplyr`
- Subtle changes under the hood to silence cosmetic warnings that don't signify unexpected failure to process

#### 20.5.0:

Minor implementation changes:

- `load_ace_bulk()`: Internal changes to `replace_nas()` to accommodate changes in how tibble data types behave with base R bracket indexing. **Fixes a breaking error with aceR and newer versions of tidyverse packages. Package now requires dplyr >= 1.0.0.**
- ACE Explorer Boxed: Now forcibly recodes accuracy based on `button_pressed` and `position_is_top`
- ACE Explorer Filter: Now forcibly recodes accuracy based on `button_pressed` and `cue_rotated`. Retains previous recoding from earlier versions of the task when `button_pressed` was not included.
- ACE BRT: Now more strict about checking for handedness in demographics. If handedness does not match `"right"` or `"left"`, `module_brt()` throws warning through `proc_by_module()`.

#### 20.4.0:

Minor implementation changes:

- ACE BRT: Only Classroom BRT is checked for `rt != inter_time_interval`. Explorer BRT is now unmodified in this respect.

Bug fixes:

- In June (ish?) 2020, ACE Explorer began labeling Flanker cues as A vs. B, instead of A/B vs. C/D. Previous versions of `load_ace_bulk()` would thus accidentally mis-code all responses when `displayed_cue == "B"`. The code now splits Flanker cues into A vs. B or A/B vs. C/D depending on whether C/D are present in the data. **This is a necessary update for anyone looking to process ACE Explorer Flanker data in the new format.**

#### 20.3.0:

Minor implementation changes:

- ACE Explorer task switch: accuracy is now manually recoded to check that the color or shape of the pressed button is equal to either the color or shape displayed, depending on the cue

Bug fixes:

- In June (ish?) 2020, ACE Explorer began collecting and outputting SAAT data as two separate tasks for the impulsive and sustained conditions. Previous versions of `load_ace_bulk()` would accidentally remove data from each player's second SAAT module as "duplicated". The code now considers task condition when de-duplicating raw data. **This is a necessary update for anyone looking to process ACE Explorer SAAT data in the new format.**

#### 20.2.0:

New outputs of `proc_by_module()` added:

- ACE spatial cueing: For newer Explorer data that has a "neutral" condition, outputs costs for neutral - incongruent and neutral - incongruent
- ACE filter: Outputs Snodgrass-corrected d'. Use with caution when trial counts are low!
- ACE modules in general:
    - where response window summaries are output, now outputs minimum response window as well
    - For ACE Explorer data, now outputs `practice_count`, or the number of practice rounds that subject did for that module. Note that at the max value, 5 rounds, it does _not_ currently differentiate between whether the subject maxed out practice and failed the last round, or passed the last round.

Bug fixes:

- `proc_by_module()` failed when `app_type = "explorer"` and `output = "wide"` because it attempted to drop a column that didn't exist in demographics
- `load_ace_bulk()` now returns all `tap_.*_rt` columns in forward/backward spatial span data as double instead of character
- `post_clean_low_trials()` checks for the minimum trial count only in at-all-responded trials within each condition. Previously, was accidentally checking within early-responded trials, which is likely too strict of a response requirement.

Now requires `dplyr >= 0.8.0` as well. Tidyverse updates are generally solid about backwards compatibility, so we boldly go for more sensible features!

#### 20.1.1:

Previously, no recoding was done on no-go "RTs" in go/no-go ACE tasks (SAAT and TNT). Now, these no-go RTs are coded as **-99**. This is not included in RT summary statistics, but these trials are counted in `ace_count()`.

#### 20.1.0:

New functions `nest_ace_raw()` and `unnest_ace_raw()`, to pull loaded ACE data into traditional unnested dataframe form for custom analysis, and to re-package ACE data back into nested form for analysis with `proc_by_module()`.

## Example Scripts

See [here](scripts/) for example scripts.

## Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for development notes and guidelines.

### Branches

#### master

Stable release branch. Supports the **newest** version of the ACE Explorer app. **Mostly backwards compatible** with data from older ACE apps (e.g. ACE Classroom). However, early builds of ACE Classroom were often unstable, and there may be idiosyncratic problems in legacy ACE Classroom data that cannot be repaired by `aceR`.

#### development 

Development of new features, i.e. non-trivial changes. All development is for features intended for the `master` branch.

#### classroom

Backwards-compatible stable release branch for data from older ACE apps. Use only when processing older ACE data, for example from ACE Classroom. **No longer actively maintained.**
