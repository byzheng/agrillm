
[![R-CMD-check.yaml](https://github.com/byzheng/agrillm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/byzheng/agrillm/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/agrillm)](https://cran.r-project.org/package=agrillm)

[![](http://cranlogs.r-pkg.org/badges/grand-total/agrillm?color=green)](https://cran.r-project.org/package=agrillm)
[![](http://cranlogs.r-pkg.org/badges/last-month/agrillm?color=green)](https://cran.r-project.org/package=agrillm)
[![](http://cranlogs.r-pkg.org/badges/last-week/agrillm?color=green)](https://cran.r-project.org/package=agrillm)

# agrillm

**agrillm** helps turn farm decision questions into structured, reproducible workflows using large language models and APSIM-based analysis.

## What this package does

The package currently supports a few simple tasks:

- classify a free-text decision such as "sowing time choice" with `resolve_decision()`
- create a Quarto report from APSIM results with `generate_decision_qmd()`
- send prompts to a configured LLM backend with `llm_chat()`

## Installation

Install the development version from GitHub:

```r
# install.packages("remotes")
remotes::install_github("byzheng/agrillm")
```

## Quick start

```r
library(agrillm)

# See the default crop names
agrillm_options$get("crops")

# Classify a decision question
resolve_decision("What about sowing time choice?")
```

To use the AI features, make sure your Azure LLM settings are available. The package expects the Azure environment variables `AZURE_OPENAI_API_KEY` and `AZURE_ENDPOINT_URL` to be set.

## Getting help

If you find a bug or want to suggest a feature, please open an issue at:
<https://github.com/byzheng/agrillm/issues>

## License

MIT License. See the LICENSE file for details.
