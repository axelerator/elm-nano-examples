#!/bin/sh
printf '\33c\e[3J'
stack build && stack exec oncard-server-exe
stack build

