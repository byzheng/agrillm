[![](https://www.r-pkg.org/badges/version/agrillm?color=green)](https://cran.r-project.org/package=agrillm)
[![R-CMD-check.yaml](https://github.com/byzheng/agrillm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/byzheng/agrillm/actions/workflows/R-CMD-check.yaml)

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

To use the AI features, make sure your Azure LLM settings are available. The package expects the Azure environment variables `AZURE_OPENAI_API_KEY`, `AZURE_ENDPOINT_URL` and `AZURE_RESOURCE_NAME` to be set.

## Citation

If you use **agrillm** in your research, please cite:

> **Zheng B**, **Kikuchi Y**, **Wang E**, **Verburg K** (2026). A transparent crop model-driven and AI-assisted framework for agricultural decision support. *Climate Smart Agriculture* **3**: 100143. https://doi.org/10.1016/j.csag.2026.100143

**BibTeX:**

```bibtex
@article{Zheng2026,
  author = {Zheng, Bangyou and Kikuchi, Yuki and Wang, Enli and Verburg, Kees},
  title = {A transparent crop model-driven and AI-assisted framework for agricultural decision support},
  journal = {Climate Smart Agriculture},
  year = {2026},
  volume = {3},
  pages = {100143},
  doi = {10.1016/j.csag.2026.100143}
}
```

## Getting help

If you find a bug or want to suggest a feature, please open an issue at:
<https://github.com/byzheng/agrillm/issues>

## License

MIT License. See the LICENSE file for details.
