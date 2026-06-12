# Copilot / Agent Instructions — hero-wars-guild-data

This repository contains data and utilities specific to Hero Wars guild management and analytics.

## Code Style
- **Elisp/Lisp**: Use `lexical-binding: t` and standard `;;;` headers. Indent with two spaces.
- **JSON/YAML**: Use consistent indentation and descriptive key names.
- Document data structures and processing logic clearly.

## Architecture
- Guild member data, stats, and related information.
- Data processing and aggregation utilities.
- Utilities for querying and reporting on guild metrics.
- Integration with external game APIs as applicable.

## Build and Test
- Data files may simply be read and processed; no traditional build required.
- Test scripts verify data integrity and processing logic.
- Use `emacs --batch -Q` for running Elisp data processors.

## Integration Points
- Hero Wars API or game data sources (credentials via environment if needed).
- Possible integration with the `hero-wars/` and `guild/` repositories.
- Data export or reporting to external formats.

## Quick Checklist
1. Review README.md for data structure documentation.
2. Check for data validation and processing scripts.
3. Ensure no sensitive guild member data is committed (if applicable).
4. Verify processing scripts work on test data.

---

For details, see the README and data structure documentation in this repository.
