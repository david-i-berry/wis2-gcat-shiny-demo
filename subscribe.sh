#!/bin/bash
# terminate current subscription (if it exists)
if kill -0 $(cat sub.pid) > /dev/null 2>&1; then
  kill $(cat sub.pid)
else
  echo "Process with PID $(cat sub.pid) does not exist or is not valid."
fi
# start new subscriber
nohup python3 subscribe.py >& `date --iso-8601=minutes`.log &
# get pid so we can kill if we need to restart
echo $! > sub.pid