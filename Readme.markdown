# PostgreSQL Migrations for Haskell
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
and documented in both your production systems and in your project files.

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

## Usage
This utility can be used in two ways: embedded in your Haskell program or as
a standalone binary.

### Standalone Program

### Haskell Library

## To Do
* Version syntax for SQL text-files
* Validate command for the standalone program
