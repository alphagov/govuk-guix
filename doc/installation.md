# Installation

***
__If you encounter any difficulties or problems with govuk-guix, please [open an issue][open-an-issue].__
***

[open-an-issue]: https://github.com/alphagov/govuk-guix/issues

## Contents

 - [Prerequisites](#prerequisites)
 - [Steps](#steps)

## Prerequisites

Using govuk-guix requires a Guix build daemon. Guix can be installed
either [alongside an existing operating system][guix-installation], or
as as an operating system itself (called [GuixSD][guixsd]).

Additionally, if you are using govuk-puppet, there is a
[guix branch of govuk-puppet][govuk-puppet-guix] which contains a
Puppet module to automate the installation of the binary release of
Guix.

[guix-installation]: https://www.gnu.org/software/guix/manual/html_node/Installation.html#Installation
[guixsd]: https://www.gnu.org/software/guix/manual/html_node/GNU-Distribution.html
[govuk-puppet-guix]: https://github.com/alphagov/govuk-puppet/tree/guix

## Steps

Once the [prerequisites](#prerequisites) are met, the easiest way to
use govuk-guix is to add the `bin/` directory to the `PATH`.

Once the `govuk` script can be run, it can be used to run the commands
related to [data][local-data] and [systems][local-development].

[local-data]: local-data.md
[local-development]: local-development.md

### `govuk` script

The `govuk` script provides a consistent way to run other programs
related to GOV.UK, while helping to handle differences in environment.

The 2nd argument to `govuk` is the program to run. For example, `govuk
data` will run the `govuk-data` program. Programs are found through
searching the `PATH`, combined with the `GOVUK_EXEC_PATH`, which will
default to include the directory in which the `govuk` script itself
resides.

Other programs can be run through the `govuk` script, they must be on
the `PATH` or `GOVUK_EXEC_PATH`, and the file must start with
`govuk-`.
