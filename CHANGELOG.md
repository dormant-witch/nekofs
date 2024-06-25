# Changelog for `nekofs`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [Unreleased]

### Added

- Support for extraction of type-0 (store) nekofile entries.

## [0.2.2] - 2024-02-22

### Fixed

- Normalize path separator to Unix format (issue #2)

## [0.2.1] - 2023-06-12

### Fixed

- Unicode encoding of filename on `createNeko`
- Better error message on too large input

## [0.2.0] - 2023-05-08

### Added

- Generate `files.meta` for directory or file (using `-m` option)

## [0.1.0] - 2023-05-03

### Added

- Extract (assuming compressed)
- Create (always compress)
- Optional: verify file integrity after extraction (TODO: could also add in-place verification)

