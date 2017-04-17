# Local Development of GOV.UK related software

***
__If you encounter any difficulties or problems with govuk-guix, please [open an issue][open-an-issue].__
***
[open-an-issue]: https://github.com/alphagov/govuk-guix/issues

## Contents

 - [Quickstart](#quickstart)
 - [Sharing files for local development](#sharing-files-for-local-development)
 - [DNS Setup](#dns-setup)

## Quickstart

To access locally running services, the local DNS configuration must
resolve domains like `*.dev.gov.uk` to your local machine, if this is
not setup, see the [DNS Setup](#dns-setup) section for guidence on how
to do this.

When using govuk-guix, you can either start all services, or a subset
of all services and their dependencies. To start all services, run:

```
govuk system start
```

To start only certain services, and their dependencies, list those
services after the start command, for example:

```
govuk system start short-url-manager publisher
```

The [Signon][signon] application is used for authentication by other
applications, and also provides links to those applications to which
you have access. A user is automaticaly created, with access to all
applications with the email address `dev@dev.gov.uk`. The passphrase
randomly generated and saved so that it is the same when `govuk
system` is run again. You can find out what the passphrase is by
running the `govuk system passphrase` command.

[signon]: https://docs.publishing.service.gov.uk/apps/signon.html

If local DNS is configigured, and the system started succesfully,
Signon should now be reachable at:

```
http://signon.dev.gov.uk:50080/
```

The port is intentionally non-standard (50080 rather than 80) to avoid
issues around binding to low ports, and conflicts with other services
using ports on the default network interface. This is a temporary
workaround until more complex networking support is available.

At this point, you have a GOV.UK system running locally with many of
the services that you might expect. The software behind these services
sits within the `/gnu/store` and cannot be modified. For guidence in
starting systems for developing services, see the section on
[sharing files for local development](#sharing-files-for-local-development).

## Sharing files for local development

The `govuk system start` command supports the `--share` option. This
uses the same syntax that can be used with the `guix system container`
command.

Modifying the code for a service within a running system depends on
what service it is, and at the moment this may be infeasible for some
services.

### Services using Rails

For services using rails, it should be possible to share the source
directory in to a running system, modify the files outside of the
system, and for those changes to affect the running service.

For example, for the short-url-manager service if you share the source
directory from the host system (assumed to be
`/home/dev/govuk/short-url-manager`), in to the isolated system
started by `govuk system start` at the `/var/apps/short-url-manager`
location, the short-url-manager service will run using the code from
the host system, rather than that contained within the /gnu/store.

To do this, the share option would be:

```
--share=/home/dev/govuk/short-url-manager=/var/apps/short-url-manager
```

## DNS Setup

GOV.UK services are usually deployed to separate domains, e.g. the
Signon service is accessible at
`signon.publishing.service.gov.uk`. Doing similarly for local
devleopment can be convinient, but does require setting up the DNS
configuration for your machine.

### govuk-puppet Development VM

The Development VM created by using govuk-puppet has DNS setup through
govuk-puppet, and this should work for using `govuk-guix`.

The DNS is configured through the `/etc/hosts` file.

### NetworkManager/dnsmasq

NetworkManager is commonly used to manage networking on free software
operating systems, e.g. Ubuntu.

NetworkManager can use dnsmasq for DNS. dnsmasq provides a easy way of
setting up DNS for any `dev.gov.uk` subdomain.

Setting this up can differ between systems depending on how the
NetworkManager configuration is managed. Assuming that the
NetworkManager configuration is in `/etc/NetworkManager`, add or
change the value for `dns` to `dnsmasq` in the `main` section of
`/etc/NetworkManager/NetworkManager.conf`. For example, if the
configuration file looks like:

```
[main]
plugins=ifupdown,keyfile

[ifupdown]
managed=true
```

To have NetworkManager use dnsmasq for DNS, add the line `dns=dnsmasq`.


```
[main]
plugins=ifupdown,keyfile
dns=dnsmasq

[ifupdown]
managed=true
```

To configure the `dev.gov.uk` domain, create a file called
`dev.gov.uk.conf` in `/etc/NetworkManager/dnsmasq.d`. In that file,
put the following.

```
address=/dev.gov.uk/127.0.0.1
```


[govuk-guix]: https://github.com/alphagov/govuk-guix
[guix]: https://gnu.org/software/guix
[shepherd]: https://www.gnu.org/software/shepherd/
