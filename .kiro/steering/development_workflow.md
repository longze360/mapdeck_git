---
title: Development Workflow
inclusion: always
---

# Development Workflow

## Getting Started
- Clone the repository
- Install dependencies: `remotes::install_deps(dependencies = TRUE)`
- Build and install the package: `devtools::install()`
- Run tests: `devtools::test()`

## Adding New Features
1. **Plan the feature**:
   - Determine which Deck.gl layer to implement
   - Review the Deck.gl documentation for required parameters
   - Consider how it fits with existing R API patterns

2. **Implementation steps**:
   - Add R function in `R/` directory
   - Add C++ code in `src/` if needed
   - Document with roxygen2 comments
   - Add examples
   - Add tests in `tests/testthat/`

3. **Testing**:
   - Write unit tests for all new functions
   - Test with various data types (data.frame, sf, sfencoded)
   - Test edge cases and error handling

## Code Organization
- Keep layer implementations in separate files
- Use consistent parameter naming across similar functions
- Follow existing patterns for handling different data types

## Documentation
- Update roxygen2 documentation for all new functions
- Add examples showing typical usage
- Consider adding a vignette for complex features
- Update README.Rmd if the feature is significant

## JavaScript Integration
- JavaScript dependencies are in `inst/htmlwidgets/`
- When updating Deck.gl or Mapbox GL versions:
  1. Update the JavaScript files
  2. Test thoroughly across different browsers
  3. Update version numbers in DESCRIPTION

## Release Process
1. Update version number in DESCRIPTION
2. Update NEWS.md with changes
3. Run R CMD check: `devtools::check()`
4. Build documentation: `devtools::document()`
5. Build site: `pkgdown::build_site()`
6. Submit to CRAN if applicable

## Debugging Tips
- Use `browser()` in R code for interactive debugging
- For JavaScript issues, use browser developer tools
- For C++ issues, compile with debug flags
- Test with minimal reproducible examples