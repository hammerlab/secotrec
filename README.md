Secotrec: Deploy Coclobas/Ketrew with class.
============================================


`secotrec` is a library, it provides a bunch of hacks to create more or less
generic deployments.

It comes with 2 preconfigured “examples:”

1. `secotrec-gke`: 
    - sets up a new GCloud host with a bunch of services:
      Postgresql, Ketrew, Coclobas (in GKE mode), Tlstunnel (with valid
      domain-name and certificate), and optionally an `nginx` basic-auth HTTP
      proxy.
    - creates an new NFS server.
    - it also deals with the GKE cluster, firewall rules, etc.
2. `secotrec-local`: sets up Postgresql, Ketrew, and Coclobas (in local docker
   mode) running locally or on a fresh GCloud box.
   
Both examples have the option of preparing the default biokepi-work directory
with
[`b37decoy_20160927.tgz`](https://storage.googleapis.com/hammerlab-biokepi-data/precomputed/b37decoy_20160927.tgz).

For more “administrator” roles, we also provide `secotrec-make-dockerfiles`, as
`Dockerfile` generation tool.

Install
-------

If you have an `opam` environment just install secotrec:

### With Opam

For now we need a few packages pinned:

    opam pin -n add coclobas https://github.com/hammerlab/coclobas.git
    opam pin -n add ketrew https://github.com/hammerlab/ketrew.git
    opam pin -n add biokepi https://github.com/hammerlab/biokepi.git
    opam pin -n add genspio https://github.com/hammerlab/genspio.git
    opam pin -n add secotrec https://github.com/hammerlab/secotrec.git
    opam upgrade
    opam install tls secotrec biokepi

Notes:

- We need `tls` to submit jobs to the deployed Ketrew server (`prepare` and
  `test-biokepi-machine` sub-commands).
- `biokepi` is only used by generated code (biokepi machine and its test).


### Dockerized

Get the image and enter the container:

    docker pull hammerlab/secotrec
    docker run -v /var/run/docker.sock:/var/run/docker.sock -it hammerlab/secotrec bash

If you do not plan to use `secotrec-local` you don't need the
Docker socket (`-v /var/run/docker.sock:/var/run/docker.sock`),
see `docker run --help` for more options.

Usage
-----

### Secotrec-gke

#### Configure

Generate a template configuration file:

```
secotrec-gke generate-configuration my-config.env
```

Edit the nicely documented `my-config.env` file
until `. my-config.env ; secotrec-gke print-configuration` is happy and you are
too.

**Note:** if you decide to use HTTP-Basic-Auth (`htpasswd` option), you will
need to append a user-name and password to some of the commands below
(see the optional `--username myuser --password mypassword` arguments).

#### Deploy

Then just bring everything up (can take about 15 minutes):

```
. my-config.env
secotrec-gke up
```

Check:

```
secotrec-gke status
```

Output should say that all services are up:

```
...
       Name                      Command               State           Ports          
-------------------------------------------------------------------------------------
coclotest_coclo_1     opam config exec -- /bin/b ...   Up      0.0.0.0:8082->8082/tcp 
coclotest_kserver_1   opam config exec -- dash - ...   Up      0.0.0.0:8080->8080/tcp 
coclotest_pg_1        /docker-entrypoint.sh postgres   Up      0.0.0.0:5432->5432/tcp 
coclotest_tlstun_1    opam config exec -- sh -c  ...   Up      0.0.0.0:443->8443/tcp  
...
```

It should show some more interesting information (incl. Ketrew WebUI URL(s)), if
there is no display like:

```
SECOTREC: Getting Ketrew Hello:
Curl ketrew/hello says: ''
[
  "V0",
  [
    "Server_status",
    {
    ...
    /* A few lines of JSON */
...
```

it means that Ketrew is not (yet) ready to answer (depending on Opam-pins and
configuration; the server may not be up right away).

#### Prepare/Test

The deployment is usable as is, but to use it with Biokepi more efficiently
one can start the “preparation” Ketrew workflow:

```
secotrec-gke prepare [--username myuser --password mypassword]
```

and go baby-sit the Ketrew workflow on the WebUI.

There is also a test of the Biokepi-Machine (for now, it can be run concurrently
to the preparation workflow):

```
secotrec-gke test-biokepi-machine [--username myuser --password mypassword]
```

the workflow uses Coclobas/Kubernetes.

#### Generate Ketrew/Biokepi Stuff

Generate a Ketrew client configuration:

```
secotrec-gke ketrew-configuration /tmp/kc.d [--username myuser --password mypassword]
```

(then you can use `export KETREW_CONFIG=/tmp/kc.d/configuration.ml`).

Generate a Biokepi `Machine.t`:

```
secotrec-gke biokepi-machine /tmp/bm.ml [--username myuser --password mypassword]
```

#### More Inspection / Tools

We can have a `top`-like display:

```
secotrec-gke top
```

We can talk directly to the database used by Ketrew and Coclobas:

```
secotrec-gke psql
```

The subcommand `docker-compose` (alias `dc`) forwards its arguments to
`docker-compose` on the node, with the configuration, e.g.:

```
secotrec-gke dc ps
secotrec-gke dc logs coclo
secotrec-gke dc exec kserver ps aux
secotrec-gke get-coclobas-logs somewhere.tar.gz
...
# See:
secotrec-gke --help
```

#### Destroy

Take down everything with:

```
secotrec-gke down
```

**Warning:** this includes the Extra-NFS server and its storage!


### Secotrec-local

Configuration is all optional (the `gcloud` version adds some constrains;
cf. the generated config-file), but it works the same way as Secotrec-GKE:

```
secotrec-local generate-configuration my-config.env
# Edit my-config.env
source my-config.env
secotrec-local print-configuration
secotrec-local up
secotrec-local status
```

Other commands work as well:

```
secotrec-local top       # top-like display of the containers
...
secotrec-local prepare    # submits the preparation workflow to Ketrew (get `b37decoy`)
...
secotrec-local ketrew-configuration /path/to/config/dir/     # generate a Ketrew config
secotrec-local biokepi-machine /path/to/biokepi-machine.ml   # generate a Biokepi config/machine
...
secotrec-local test biokepi-machine
...
secotrec-local down
```

### Secotrec-Make-Dockerfiles

`secotrec-make-dockerfiles` is designed to update
the repository
[`hammerlab/keredofi`](https://github.com/hammerlab/keredofi)
(but can start from a mostly empty repo),
and hence the
[`hammerlab/keredofi`](https://hub.docker.com/r/hammerlab/keredofi/builds/)
Docker-hub images.

Display all the Dockerfiles on stdout:

    do=view secotrec-make-dockerfiles

Write the `Dockerfile`s in their respective branches and commit if something
changed:

    dir=/path/to/keredofi do=write secotrec-make-dockerfiles
    
when done, the tool displays the Git graph of the Keredofi repo; if you're
happy, just go there and `git push --all`.


Submit a Ketrew workflow that test-builds all the `Dockerfiles` (for now this
expects a `secotrec-local`-like setup):

    do=test secotrec-make-dockerfiles
