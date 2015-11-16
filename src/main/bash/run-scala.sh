#!/bin/bash

readonly TASKS_VERSION=${TASKS_VERSION:-'0.1'}
readonly TASKS=${TASKS:-"${HOME}/.m2/repository/org/belaran/tasks/${TASKS_VERSION}/tasks-${TASKS_VERSION}.jar"}
readonly SCRIPT=${1}
shift

if [ -z ${SCRIPT} ]; then
  scala  -classpath ".:${TASKS}"
  exit 0
fi

if [ ! -e "${SCRIPT}" ]; then
  echo "No such file:${SCRIPT}"
  exit 2
fi

scala  -classpath ".:${TASKS}" "${SCRIPT}" ${@}
