#!/usr/bin/env sh

while
  PORT=$(shuf -n 1 -i 1000-65535)
  netstat -atun | grep -q "$PORT"
do
  continue
done

echo "$PORT"