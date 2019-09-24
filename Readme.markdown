# PostgreSQL Migrations for Haskell

[![Build Status](https://api.travis-ci.org/ameingast/postgresql-simple-migration.png)](https://travis-ci.org/ameingast/postgresql-simple-migration)

Welcome to postgresql-simple-migrations, a tool for helping you with
PostgreSQL schema migrations.

This project is an open-source database migration tool. It favors simplicity
over configuration.

It is implemented in Haskell and uses the (excellent) postgresql-simple
library to communicate with PostgreSQL.

It comes in two flavors: a library that features an easy to use Haskell
API and as a standalone application.

Database migrations can be written in SQL (in this case PostgreSQL-sql)
or in Haskell.

## Why?
Database migrations should not be hard. They should be under version control
and documented both in your production systems and in your project files.

## What?
This library executes SQL/Haskell migration scripts and keeps track of their
meta information.

Scripts are be executed exactly once and any changes to scripts will cause
a run-time error notifying you of a corrupted database.

The meta information consists of:
* an MD5 checksum of the executed script to make sure already existing
  scripts cannot be modified in your production system.
* a time-stamp of the date of execution so you can easily track when a change
  happened.

This library also supports migration validation so you can ensure (some)
correctness before your application logic kicks in.

## How?
This utility can be used in two ways: embedded in your Haskell program or as
a standalone binary.

### Standalone
The standalone program supports file-based migrations. To execute all SQL-files
in a directory $BASE\_DIR, execute the following command to initialize the database
in a first step.

```bash
CON="host=$host dbname=$db user=$user password=$pw"
./dist/build/migrate/migrate init $CON
./dist/build/migrate/migrate migrate $CON $BASE_DIR
```

To validate already executed scripts, execute the following:
```bash
CON="host=$host dbname=$db user=$user password=$pw"
./dist/build/migrate/migrate init $CON
./dist/build/migrate/migrate validate $CON $BASE_DIR
```

For more information about the PostgreSQL connection string, see:
[libpq-connect](http://www.postgresql.org/docs/9.3/static/libpq-connect.html).

### Library
The library supports more actions than the standalone program.

Initializing the database:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    con <- connectPostgreSQL (BS8.pack url)
    withTransaction con $ runMigration $
        MigrationContext MigrationInitialization True con
```

For file-based migrations, the following snippet can be used:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    let dir = "."
    con <- connectPostgreSQL (BS8.pack url)
    withTransaction con $ runMigration $
        MigrationContext (MigrationDirectory dir) True con
```

To run Haskell-based migrations, use this:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    let name = "my script"
    let script = "create table users (email varchar not null)";
    con <- connectPostgreSQL (BS8.pack url)
    withTransaction con $ runMigration $
        MigrationContext (MigrationScript name script) True con
```

Validations wrap _MigrationCommands_. This means that you can re-use all
MigrationCommands to perform a read-only validation of your migrations.

To perform a validation on a directory-based migration, you can use the
following code:

```haskell
main :: IO ()
main = do
    let url = "host=$host dbname=$db user=$user password=$pw"
    con <- connectPostgreSQL (BS8.pack url)
    withTransaction con $ runMigration $ MigrationContext
        (MigrationValidation (MigrationDirectory dir)) True con
```

Database migrations should always be performed in a transactional context.

The standalone binary takes care of proper transaction handling automatically.

The library does not make any assumptions about the current transactional state
of the system. This means that the caller of the library has to take care of
opening/closing/rolling-back transactions. This way you can execute multiple
migration-commands or validations in sequence while still staying in the
transaction you opened.

The tests make use of this. After executing all migration-tests, the
transaction is rolled back.

## Compilation and Tests
The program is built with the _cabal_ build system. The following command
builds the library, the standalone binary and the test package.

```bash
cabal configure --enable-tests && cabal build -j
```

To execute the tests, you need a running PostgreSQL server with an empty
database called _test_. Tests are executed through cabal as follows:

```bash
cabal configure --enable-tests && cabal test
```

To build the project in a cabal sandbox, use the following code:

```bash
cabal sandbox init
cabal install -j --only-dependencies --enable-tests --disable-documentation
cabal configure --enable-tests
cabal test
```

To remove the generated cabal sandbox, use:
```bash
cabal sandbox delete
```

## To Do
* Collect executed scripts and check if already executed scripts have been
  deleted.
