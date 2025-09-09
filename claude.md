# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Build Commands
- Package documentation: `devtools::document()`
- Build package: `devtools::build()`
- Install package locally: `devtools::install()`
- Run API: `library(plumber); pr("api/plumber.R") %>% pr_run(port=8000)`
- Render Quarto: `quarto::quarto_render("index.qmd")`

## Code Style Guidelines
- **Formatting**: 2-space indentation; snake_case for names; 
  pipe operators (`|>` preferred over `%>%`) for data transformations;
  try to vertically align `=` arguments within the same function;
  use end parentheses on the same line as the function call, but add a space if it completes outside the line
- **Functions**: Inside R packages, document with roxygen2 style; 
  API endpoints with plumber annotations (`#*`); use default parameter values
- **Imports**: Inside R packages, use `@importFrom pkg fun1 fun2` for all used functions; Outside R packages, use `librarian::shelf(..., quiet = T)` at top of file to load (and install if needed) R libraries 
- **Error handling**: Use `stopifnot()` for validation; informative error messages
- **Database**: Use `dbplyr` for queries; `glue()` for SQL interpolation; parameterized queries for safety
- **Files**: Organize code in R packages by function type (read.R, analyze.R, viz.R, etc.) and apply roxygen2 `@concept` for use with `pkgdown::build_reference_index()`
- **Comments**: use lowercase (not capital); explanatory comments for complex operations; extra section headings (suffixed by ----), especially for Shiny ui.R and server.R files
- **Naming**: snake_case for variables and functions; UI_CAPS for constants
