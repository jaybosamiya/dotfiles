local wezterm = require 'wezterm'
local config = wezterm.config_builder()

config.default_domain = 'WSL:Ubuntu'
config.front_end = "OpenGL"

-- Since I am only using this for WSL, the default of CRLF on Windows is somewhat annoying since it leads to double-newline. Changing it to just CR does the right thing.
config.canonicalize_pasted_newlines = "CarriageReturn"

config.window_decorations = "RESIZE"

config.initial_cols = 120
config.initial_rows = 28

config.font_size = 12
config.font = wezterm.font('Berkeley Mono', {})

config.color_scheme = 'DoomOne'
config.color_scheme = 'Dracula'
config.color_scheme = 'Nord (Gogh)'

config.use_fancy_tab_bar = true
config.hide_tab_bar_if_only_one_tab = true

config.audible_bell = "Disabled"
config.visual_bell = {
  fade_out_duration_ms = 75,
}
config.colors = {
  visual_bell = '#202020',
}

config.scrollback_lines = 10000000

-- Add split-pane and navigation keybindings
local directions = {
  Up = 'UpArrow',
  Down = 'DownArrow',
  Left = 'LeftArrow',
  Right = 'RightArrow',
}
config.keys = {}
for dir, key in pairs(directions) do
  table.insert(config.keys, {
    key = key,
    mods = 'ALT|SHIFT',
    action = wezterm.action.SplitPane {
      direction = dir,
      size = { Percent = 50 },
    },
  })
  table.insert(config.keys, {
    key = key,
    mods = 'ALT',
    action = wezterm.action.ActivatePaneDirection(dir),
  })
end

-- Add mouse bindings to disable plain-click link opening and enable Ctrl-click
-- instead
config.mouse_bindings = {
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'NONE',
    action = wezterm.action.Nop,
  },
  {
    event = { Up = { streak = 1, button = 'Left' } },
    mods = 'CTRL',
    action = wezterm.action.OpenLinkAtMouseCursor,
  },
}

return config
