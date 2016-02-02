# Various sensors implementation in Ocaml

This is a ocaml implementation to read various sensors value.

Targeted list of sensors:
- HTS221
- VL6180X

## pre-build

Install ocaml, wiringpi, m4 and opam (packet manager):

  sudo apt-get install ocaml opam m4 wiringpi

### Setting up opam for your user

run:

  opam init

This will take less than 2 minutes (on Raspberry 2 B).

Then it asks you if you want that it configures itself the bashrc to make your
shell aware of opam programs. Say yes if you don't know what to do.

Then run this command (not needed again after, unless you'd said no in the
previous step):

  eval `opam config env`

### Install necessary opam packages

Install:
- ocamlfind to ease compilation
- wiringpi to get access to GPIO of the raspberry pi with the WiringPi library

### Ready

You are ready to build

## build

  make all

## HTS221 readings

Temperature (expressed in °C) :

  ./htc221.native -t

Humidity (expressed in %) :

  ./htc221.native -h

Once it has been built, run it like this :

  ./emb.laborne.taxi.native --id 1234

The --id will be the id sent to the laborne.taxi API.


## Install

  make install

## Uninstall

  make uninstall
