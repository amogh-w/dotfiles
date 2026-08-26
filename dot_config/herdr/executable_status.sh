#!/bin/sh
# battery percentage + charging state, for herdr's tab_bar_right command entry
batt=$(pmset -g batt)
pct=$(echo "$batt" | grep -oE '[0-9]+%' | head -1)
if echo "$batt" | grep -q "AC Power"; then
    if echo "$batt" | grep -q "charging;"; then
        echo "$pct ⚡"
    else
        echo "$pct 🔌"
    fi
else
    echo "$pct 🔋"
fi
