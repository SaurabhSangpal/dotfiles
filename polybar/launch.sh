#!/usr/bin/env bash

killall -q polybar

echo "---" | tee -a /tmp/polybarleft.log /tmp/polybarright.log
polybar --config=~/.config/polybar/config.ini right 2>&1 | tee -a /tmp/polybarright.log & disown
polybar --config=~/.config/polybar/config.ini left 2>&1 | tee -a /tmp/polybarleft.log & disown
