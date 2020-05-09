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

20.1.1: Previously, no recoding was done on no-go "RTs" in go/no-go ACE tasks (SAAT and TNT). Now, these no-go RTs are coded as **-99**. This is not included in RT summary statistics, but these trials are counted in `ace_count()`.

20.1.0: New functions `nest_ace_raw()` and `unnest_ace_raw()`, to pull loaded ACE data into traditional unnested dataframe form for custom analysis, and to re-package ACE data back into nested form for analysis with `proc_by_module()`.

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
