#!/bin/bash

cat <(echo ${@} ) | ncat localhost 2333
