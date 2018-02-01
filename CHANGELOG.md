# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [Unreleased]
### Added
- "AverageMaps" plot category
    - Average First Guess Departure Map
    - Average Analysis Departure Map
    - Average Analysis Increment Map
- Possibility to start obsmon from any directory (obsmon must be in the PATH)

### Changed
- Auto-discovery cache is now sqlite-based (Fixes: #148)

### Fixed
- Bias correction and First Guess Departure+Bias Correction maps are back

## [2.1.0] - 2017-10-20
### Added
- Date selection for single times
- Cycle selection for date ranges
- Plottype filtering offers only plots valid for selected criteria
- Auto-discovery of obtypes and stations (cached)
- Progress bar for long operations
- Plot registry facilitates adding new plots/plot maintenance

### Changed
- UI retains user choices
- Streamlined UI
- Window resizing supported
- Surface diagnostics moved to more general station diagnostics plottype
- Plottypes split in categories
- Data table interface improved
- Color handling improved
- Cairo based plotting improves plot quality
- TOML based config.toml replaces hardcoded and environment configuration
- Only install missing packages in install.R
- Logging via futile.logger replaces print statements
- Database handling now via object oriented interface
- Plotting via object oriented interface

### Removed
- Pre-defined plots
- Dump database
- Settings tab
- Environment variables for configuration

[Unreleased]: https://git.smhi.se/paulo/obsmon/compare/obsmon-2.1.0...obsmon-2.2.x
[2.1.0]: https://git.smhi.se/a002160/obsmon/compare/obsmon-2.0.0...obsmon-2.1.0
