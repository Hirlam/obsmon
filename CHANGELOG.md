# Change Log
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](http://keepachangelog.com/)
and this project adheres to [Semantic Versioning](http://semver.org/).

## [2.2.0] 2018-09-17
### Added
- "AverageMaps" plot category
    - Average First Guess Departure Map
    - Average Analysis Departure Map
    - Average Analysis Increment Map
- Possibility to start obsmon from any directory (obsmon must be in the PATH)
- Support to command line options in (both in the install script as well as in obsmon itself)
- Possibility to install R-packages used by obsmon locally (as part of obsmon
  itself), making it independent of R libraries installed in the system. This
  is useful for packaging.
- Support to offline installation
- Support to using pre-compiled R libraries
- Progress of caching process shown in the GUI

### Changed
- Auto-discovery cache is now sqlite-based (Fixes: #148)
- Auto-discovery cache is also done asynchronously now. This means that, if
  multiple experiments are being dealt with, the auto-discovery caching for
  all of them can be performed simultaneously. Any experiment can now be
  selected as soon as it is ready, without the need to wait for the others
  to finish caching.
- Info about installtion moved to INSTALL.md file
- Automatic identification of imported R libraries during install
    - R dependencies recursively determined prior to installation
- obsmon and install executables are now R scripts (used to be bash)

### Fixed
- Bias correction and First Guess Departure+Bias Correction maps are back
- Look for an alternative TCP port if cannot use the one initially chosen
- Some issues dealing with unavailable data

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

[2.2.0]: https://git.smhi.se/foum/obsmon/compare/obsmon-2.1.0...obsmon-2.2.0
[2.1.0]: https://git.smhi.se/a002160/obsmon/compare/obsmon-2.0.0...obsmon-2.1.0
