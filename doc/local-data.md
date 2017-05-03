# Local Data

***
__If you encounter any difficulties or problems with govuk-guix, please [open an issue][open-an-issue].__
***
[open-an-issue]: https://github.com/alphagov/govuk-guix/issues

## Contents

 - [Quickstart](#quickstart)
 - [Implementation](#implementation)

## Quickstart

Currently, the only available source of data is the
["govuk-puppet" data source](#govuk-puppet-data-source), so its
necessary to have some files downloaded through the
`replicate-data-local.sh` script for the `govuk data` command to be
useful.

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
speficied multiple times.

Older dumps can be restored (if available) by using the `--after` or
`--before` options, which will select the first dataset either after
or before the specified date.

## Implementation

### Data sources

A data source can be queried as to what data it has available. Each
returned item must have:

 - date
   - This should approxomate to when the data was extracted.
 - database (e.g. PostgreSQL, MySQL, ...)
   - The database the data was extracted from.
 - services (e.g. publishing-api-service-type)
   - The services that can use this data.

#### govuk-puppet data source

This data source is designed to be used with the govuk-puppet
repository. govuk-puppet contains scripts to download data from the
GOV.UK integration environment. After these scripts are run, the
downloaded files are stored in the
`development-vm/replication/backups` directory.

These files, specifically only the downloaded archives, not any of the
extracted files can be used by the local data source.


[govuk-guix]: https://github.com/alphagov/govuk-guix
