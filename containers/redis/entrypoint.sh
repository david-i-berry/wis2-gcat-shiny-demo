#!/bin/bash
#start redis
redis-server /usr/local/etc/redis/redis.conf >& /local/redis.log &
echo "redis started"
# start wis2 subscriber
./subscribe.sh
echo "wis2 subscriber started"
bash