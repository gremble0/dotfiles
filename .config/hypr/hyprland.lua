--- Monitors

hl.monitor({ output = "DP-6", mode = "3840x2160@60", position = "0x0", scale = 1 })
hl.monitor({ output = "eDP-1", mode = "1920x1200@60", position = "3840x0", scale = 1 })

-- Bind workspaces 1-10 to DP-6
for i = 1, 10 do
  hl.workspace_rule({ workspace = tostring(i), monitor = "DP-6" })
end

-- Autostart
hl.on("hyprland.start", function()
  hl.exec_cmd("uwsm finalize")
  hl.exec_cmd("waybar")
  hl.exec_cmd("hyprpaper")
end)

--- Env vars
hl.env("XCURSOR_SIZE", "24")
hl.env("XDG_CURRENT_DESKTOP", "Hyprland")

--- Cursor
hl.config({
  cursor = {
    no_hardware_cursors = true,
  },
})

--- Input
hl.config({
  input = {
    kb_layout = "us, no",
    kb_variant = "",
    kb_model = "",
    kb_options = "caps:escape, grp:win_space_toggle",
    kb_rules = "",

    repeat_delay = 300,
    repeat_rate = 35,

    follow_mouse = 1,

    sensitivity = 0.1,

    touchpad = {
      natural_scroll = true,
      scroll_factor = 0.3,
    },
  },
})

--- Look and feel
hl.config({
  general = {
    gaps_in = 3,
    gaps_out = 6,

    border_size = 1,

    col = {
      active_border = "rgb(e1b655)",
      inactive_border = "rgba(595959aa)",
    },

    layout = "dwindle",
  },

  decoration = {
    rounding = 8,

    blur = {
      enabled = false,
    },
  },

  animations = {
    enabled = true,
  },
})

hl.curve("myBezier", { type = "bezier", points = { { 0.05, 0.4 }, { 0.05, 1.05 } } })

hl.animation({ leaf = "windows", enabled = true, speed = 7, bezier = "myBezier" })
hl.animation({ leaf = "windowsOut", enabled = true, speed = 7, bezier = "default", style = "popin 80%" })
hl.animation({ leaf = "border", enabled = true, speed = 10, bezier = "default" })
hl.animation({ leaf = "fade", enabled = true, speed = 7, bezier = "default" })
hl.animation({ leaf = "workspaces", enabled = true, speed = 6, bezier = "myBezier" })

--- Keybinds
local mainMod = "SUPER"

-- Start programs
hl.bind(mainMod .. " + RETURN", hl.dsp.exec_cmd("alacritty -e tmux"))
hl.bind(mainMod .. " + SHIFT + B", hl.dsp.exec_cmd("chromium-browser"))
hl.bind(mainMod .. " + D", hl.dsp.exec_cmd("wofi --allow-images --show drun"))
hl.bind(mainMod .. " + SHIFT + BACKSPACE", hl.dsp.exec_cmd("boot-menu"))
hl.bind(mainMod .. " + SHIFT + D", hl.dsp.exec_cmd("discord"))

hl.bind("XF86MonBrightnessDown", hl.dsp.exec_cmd("brightnessctl set 5%-"))
hl.bind("XF86MonBrightnessUp", hl.dsp.exec_cmd("brightnessctl set +5%"))

-- Screenshots
hl.bind("Print", hl.dsp.exec_cmd('grim -g "$(slurp)" - | wl-copy'))
hl.bind(
  mainMod .. " + Print",
  hl.dsp.exec_cmd(
    'grim -g "$(slurp)" - | tee ~/Pictures/Screenshots/screenshot_$(date +%Y-%m-%d_%H-%M-%S).png | wl-copy'
  )
)

-- Manage active window
hl.bind(mainMod .. " + W", hl.dsp.window.close())
hl.bind(mainMod .. " + S", hl.dsp.window.float())
hl.bind(mainMod .. " + F", hl.dsp.window.fullscreen())

-- Move focus with mainMod + arrow keys
hl.bind(mainMod .. " + H", hl.dsp.focus({ direction = "l" }))
hl.bind(mainMod .. " + J", hl.dsp.focus({ direction = "d" }))
hl.bind(mainMod .. " + K", hl.dsp.focus({ direction = "u" }))
hl.bind(mainMod .. " + L", hl.dsp.focus({ direction = "r" }))

hl.bind(mainMod .. " + SHIFT + H", hl.dsp.window.move({ direction = "l" }))
hl.bind(mainMod .. " + SHIFT + J", hl.dsp.window.move({ direction = "d" }))
hl.bind(mainMod .. " + SHIFT + K", hl.dsp.window.move({ direction = "u" }))
hl.bind(mainMod .. " + SHIFT + L", hl.dsp.window.move({ direction = "r" }))

-- Switch workspaces with mainMod + [0-9]
-- Move active window to a workspace with mainMod + SHIFT + [0-9]
for i = 1, 10 do
  local key = i % 10 -- 10 maps to key 0
  hl.bind(mainMod .. " + " .. key, hl.dsp.focus({ workspace = i }))
  hl.bind(mainMod .. " + SHIFT + " .. key, hl.dsp.window.move({ workspace = i }))
end

-- Resize windows with mainMod + CTRL + [hjkl]
hl.bind(mainMod .. " + CTRL + H", hl.dsp.window.resize({ x = -20, y = 0, relative = true }))
hl.bind(mainMod .. " + CTRL + J", hl.dsp.window.resize({ x = 0, y = 20, relative = true }))
hl.bind(mainMod .. " + CTRL + K", hl.dsp.window.resize({ x = 0, y = -20, relative = true }))
hl.bind(mainMod .. " + CTRL + L", hl.dsp.window.resize({ x = 20, y = 0, relative = true }))

-- Scroll through existing workspaces with mainMod + scroll
hl.bind(mainMod .. " + mouse_down", hl.dsp.focus({ workspace = "e+1" }))
hl.bind(mainMod .. " + mouse_up", hl.dsp.focus({ workspace = "e-1" }))

-- Move/resize windows with mainMod + LMB/RMB and dragging
hl.bind(mainMod .. " + mouse:272", hl.dsp.window.drag(), { mouse = true })
hl.bind(mainMod .. " + mouse:273", hl.dsp.window.resize(), { mouse = true })
