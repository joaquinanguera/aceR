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

Reinstall the package to update to the latest version:

```
devtools::install_github("joaquinanguera/aceR")
```

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
