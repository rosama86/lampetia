#!/bin/bash


echo Creating directory /var/run/${APP_NAME}
mkdir -p /var/run/${APP_NAME}
echo Unpacking ${APP_PKG}
tar xf ${APP_PKG} -C /var/run/${APP_NAME}
echo Starting Service
/var/run/${APP_NAME}/${APP_NAME}-${APP_VERSION}/bin/${APP_NAME} &

PID=$!

echo Service started with PID=${PID}

trap "echo Stopping Service ${PID}; kill -s SIGINT ${PID}" SIGINT SIGTERM EXIT

wait ${PID}


