# Changelog

## (to be released)
* Changed `schemaMigrationExecutedAt` from LocalTime to UTCTime by storing a
  `timestamp with time zone` (instead of `timestamp without time zone`) in the
  schema_migrations table.  Old versions of the schema_migrations table are
  migrated automatically before running migrations or validations.

  If you don't want postgresql to convert the timestamps using its time zone,
  change the type of the `schema_migrations.executed_at` column to `timestamp
  with time zone` in a migration (which allows you to control how the timestamps
  are converted) you run prior to updating postgresql-simple-migration.

  If you use `getMigrations`, which returns a `SchemaMigration`, which has the
  `schemaMigrationExecutedAt` attribute, be aware that the type changed.

## 0.1.14.0
* Bumped dependencies

## 0.1.13.1
* Bumped dependencies

## 0.1.13.0
* Bumped dependencies

## 0.1.12.0
* Support for GHC 8.4

## 0.1.11.0
* Improved documentation
* Fixed exists_table

## 0.1.10.1
* Fixed hackage warnings

## 0.1.10.0
* Relaxed time bounds

## 0.1.9.0
* Bumped dependencies

## 0.1.8.0
* Added MigrationCommands allowing sequencing of migrations in the Haskell API
* Derived more datatypes for MigrationResult
* Bumped dependencies

## 0.1.7.0
* Propagate migration and validation result to application exit code

## 0.1.6.0
* Support for GHC 8

## 0.1.5.0
* Bumped dependencies

## 0.1.4.0
* Improved error logging in standalone binary

## 0.1.3.0
* Better transaction handling
* Improved documentation

## 0.1.2.0
* Moved Util module
* Improved documentation

## 0.1.1.0
* Support for schema validations.
* Improved Haskell API

## 0.1.0.0
* Support for file-based and Haskell migrations.
