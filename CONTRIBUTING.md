# Contributing to RScriptsandStuff

Thank you for your interest in contributing!

## Getting Started

1. Fork the repository.
2. Clone your fork locally.
3. Install development dependencies:
   ```r
   install.packages(c("devtools", "testthat", "roxygen2", "covr", "lintr"))
   devtools::install_deps(dependencies = TRUE)
   ```
4. Create a feature branch:
   ```bash
   git checkout -b feature/my-feature
   ```

## Development Workflow

### Code Style

* Follow [tidyverse style](https://style.tidyverse.org/).
* All exported functions must have roxygen2 documentation with `@param`,
  `@return`, `@export`, and at least one `@examples` block.
* Add `stopifnot()` input validation at the top of every exported function.
* Wrap external API calls in `tryCatch()`.

### Testing

* Every new function needs corresponding tests in `tests/testthat/`.
* Test both happy-path and error-path behaviour.
* Run the full suite before committing:
  ```r
  devtools::test()
  ```

### Documentation

* Rebuild docs after changes:
  ```r
  devtools::document()
  ```
* Update `NEWS.md` with a summary of your changes.

## Pull Request Process

1. Ensure `R CMD check` passes with no errors, warnings, or notes.
2. Update documentation and tests.
3. Open a PR against `main` with a clear description.
4. One approval required before merge.

## Reporting Issues

Use [GitHub Issues](https://github.com/Scientialibera/RScriptsandStuff/issues)
with a minimal reproducible example when possible.
