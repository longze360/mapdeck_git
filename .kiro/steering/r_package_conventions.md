---
title: R Package Conventions
inclusion: always
---

# R Package Conventions

## Package Structure
- This is an R package following standard R package structure
- Source code is in the `R/` directory
- C++ code is in the `src/` directory
- Documentation is generated using roxygen2
- Tests are in the `tests/` directory using testthat framework
- Data is stored in the `data/` directory
- Raw data processing scripts are in `data-raw/`

## Coding Style
- Use roxygen2 for documentation
- Function names use snake_case
- R6 class methods use camelCase
- Document all exported functions with roxygen2 comments
- Include examples in function documentation
- Use `@export` tag for functions that should be exported

## C++ Integration
- C++ code uses Rcpp
- C++ standard is C++14
- Header files are in `inst/include/`
- Use LinkingTo in DESCRIPTION for C++ dependencies

## Dependencies
- List all R package dependencies in the DESCRIPTION file
- Use `Imports:` for packages that are required
- Use `Suggests:` for packages that are optional
- Use `LinkingTo:` for C++ header dependencies

## Testing
- Write tests for all exported functions
- Use testthat framework
- Tests should be organized by functionality

## Documentation
- Keep README.md and README.Rmd in sync
- Use pkgdown for website generation
- Include vignettes for complex functionality
- Document all parameters and return values

## Data Handling
- All spatial data is expected to be in EPSG:4326 (WGS 84) coordinate system
- Use sf objects for spatial data when possible
- Support for encoded geometries via sfencoded objects