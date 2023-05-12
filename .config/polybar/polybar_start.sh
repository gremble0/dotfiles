# kill old instances
killall polybar

# If there are multiple displays connected load 2 polybar instances, if not one
if [ $(xrandr | grep -c ' connected') -gt 1 ]; then
    polybar --reload main &
    polybar --reload second &
else 
    polybar --reload laptop &
fi
