***
# __This documentation and the respective tooling is still being written, it does not currently represent reality__
***
<br>

# Local Development of GOV.UK

## Contents

 - [Quickstart](#quickstart)
 - [Introduction](#introduction)
 - [DNS Setup](#dns-setup)

## Quickstart

Start the system by running:

```
govuk system start
```

Now, open a web browser your machine, and enter the following url:

```
http://signon.dev.gov.uk (TODO: Check dns works in the development VM)
```

If this does not work, ... (TODO: talk about DNS)

The system start script will pick a random password for the
devlelopment user, and this will be printed to the screen.

Login to signon using the email address `dev@dev.gov.uk` and the
password shown by the start script.

At this point, you have a GOV.UK system running locally with many of
the services that you might expect. The software behind these services
sits within the `/gnu/store` and cannot be modified. To modify
services running within this system, you can switch the service to run
from software stored outside of the store.

By default, the development system will mount the directory above the
govuk-guix repository in to the development system, with the
assumption that you have the software for the services that you want
to develop in this directory. (TODO: Why mention this?)

Login to the devleopment system using the `login` command. Change
(`cd`) to the `/home/dev/govuk/` directory. Check that the software
for the services that you wish to modify are present in this
directory, if not (TODO: do something).

To switch a service to running from code in the `/home/dev/govuk/`
directory, run the `TODO` command. To switch back, run the `TODO`
command.

## Introduction

govuk-guix provides packages, services and systems for working with
software and data related to GOV.UK.

System definitions written using [Guix][guix] can be used to create
containers (process or set of processes running with some isolation),
virtual machines, or configure physical systems.

For development, you may find it convinient to run the GOV.UK
development system provided by `govuk-guix` as a container on your
local machine.

This container contains a service manager called [shepherd][shepherd],
configured to run services within the container, both 

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
