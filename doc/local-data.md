# Local Data

***
__If you encounter any difficulties or problems with govuk-guix, please [open an issue][open-an-issue].__
***
[open-an-issue]: https://github.com/alphagov/govuk-guix/issues

## Contents

 - [Quickstart](#quickstart)
 - [Implementation](#implementation)

## Quickstart

To check what data is available, run `govuk data list`. The results
can be filtered by service name, as well as the database and date. For
example, `govuk data list publishing-api --before=01/01/2017` will
list data for the Publishing API that is dated before 01/01/2017.

To load data, use the `load` command. It takes the same arguments as
the `list` command, e.g. `govuk data load publishing-api
--before=01/01/2017` will load the latest available data for the
Publishing API, that is dated before 01/01/2017.

### Filtering

Both the `load` and `list` commands supports `--database`, `--before`
and `--after` filtering options.

The `--database` option is used to filter by database, it can be
specified multiple times.

Older dumps can be restored (if available) by using the `--after` or
`--before` options, which will select the first dataset either after
or before the specified date.

## Implementation

There are two parallel implementations of the `govuk data` script. The first,
corresponding to the [`bin/govuk-data`][govuk-data] file is written in Guile,
and the second, corresponding to the
[`/bin/govuk-data-standalone`][govuk-data-standalone] file is written in
Ruby. The `govuk` wrapper script will automatically run
`govuk-data-standalone` if you run `govuk data` on a system without Guile, so
this distinction shouldn't matter during normal use.

The standalone version of `govuk data` just supports the data directory with
index data source, and by default will use a AWS S3 bucket. This provides the
fastest way to get the data. The non-standalone version of `govuk data` is
useful for testing data extracts without having to generate a data directory
to use with the standalone version of `govuk data`, however to do this, it
requires Guix is available on the local machine.

[govuk-data]: ../bin/govuk-data
[govuk-data-standalone]: ../bin/govuk-data-standalone

### Data sources

A data source can be queried as to what data it has available. Each
returned item must have:

 - date
   - This should approximate to when the data was extracted.
 - database (e.g. PostgreSQL, MySQL, ...)
   - The database the data was extracted from.
 - services (e.g. publishing-api-service-type)
   - The services that can use this data.

#### data-directory-with-index data source

This data source works from a generated set of data, with an index file. By
default, it uses a data directory fetched from an AWS S3 bucket.

The data available to download from the AWS S3 bucket is populated with the
[`govuk update-development-data`][govuk-update-development-data]
script. This script:

 - Runs [`govuk download-backups`][govuk-download-backups] to download the
   source data from the `govuk-integration-database-backups` S3 bucket.
    - To save some space in storing the source data, provide smaller extracts
      and avoid any memory issues when passing the source data to the Guix
      daemon, the `download-backups` script re-compresses the data where
      possible to reduce the size. This can take some time.
 - Runs `govuk data build-data-directory-with-index` to create a directory
   containing all the data extracts, along with a index describing the
   available extracts (represented as a JSON file). To prevent the size of the
   bucket from continually growing, only data from the last two weeks is used.
    - The `build-data-directory-with-index` command involves generating every
      data extract, and as some of the variants require loading the data in to
      the respective database, and then dumping it back out, this can take
      some time.
 - The AWS S3 bucket used by `govuk data` is then updated to match the local
   directory, deleting any old files, and uploading any new ones.

[govuk-update-development-data]: ../bin/govuk-update-development-data

#### govuk-puppet-aws data source

This data source is designed to be used with the govuk-puppet
repository. govuk-puppet contains scripts to download data from the
GOV.UK integration environment. After these scripts are run, the
downloaded files are stored in the
`development-vm/replication/backups` directory.

These files, specifically only the downloaded archives, not any of the
extracted files can be used by the local data source.

To simplify the process of downloading data, the
[`govuk download-backups`][govuk-download-backups] script exists, which will
download data in a way that works with this data source.

[govuk-download-backups]: ../bin/govuk-download-backups

#### govuk-puppet data source

This data source is similar to the govuk-puppet-aws data source. The migration
to AWS changed the way data was managed in govuk-puppet, and this data source
handles the way data was managed prior to these changes.

[govuk-guix]: https://github.com/alphagov/govuk-guix
