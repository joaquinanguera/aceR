# aceR

`aceR` is an R package around ACE data. 

## Getting Started

See [here](inst/docs/getting-started.md) for a detailed install guide and annotated example.

## Installation

`aceR` is currently only available on github.

Install the latest release from github using [devtools](https://github.com/hadley/devtools):

```
# install.packages("devtools")
GITHUB_PAT = "64235145b808f152d84467ea1362dc0b7a0def6c"
devtools::install_github("josegallegos07/aceR", auth_token = GITHUB_PAT)
```

## Updates

Reinstall the package to update to the latest version:

```
GITHUB_PAT = "64235145b808f152d84467ea1362dc0b7a0def6c"
devtools::install_github("josegallegos07/aceR", auth_token = GITHUB_PAT)
```

## Example Scripts

See [here](scripts/) for example scripts.

## Contributing

See [CONTRIBUTING](CONTRIBUTING.md) for development notes and guidelines.

### Branches

#### development 

Development of new features - i.e. non-trivial changes

#### master

Stable release branch.