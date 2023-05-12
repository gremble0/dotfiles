# kill old instances
killall polybar

# If there are multiple displays connected load 2 polybar instances, else one
if $(xrandr | grep ' connected' | wc -l) > 1; then
    polybar --reload main &
    polybar --reload second &
else 
    polybar --reload laptop &
fi
