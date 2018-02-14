#! /usr/bin/env bash

brew install $1 || (brew upgrade $1 && brew link --overwrite $1)
