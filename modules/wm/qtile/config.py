from libqtile.config import EzKey as Key, KeyChord, Group, Screen
from libqtile.lazy import lazy
from libqtile import layout, bar, widget

mod = "mod4"

groups = [
    Group("freelance"),
    Group("web"),
    Group("edit"),
    Group("tools"),
    Group("scan"),
    Group("ent"),
    Group("shell"),
    Group("read"),
    Group("im"),
    Group("scratch")
]

keys = [
    Key("M-<Tab>", lazy.next_layout()),
    # Change focus
    Key("M-<Down>", lazy.layout.down()),
    Key("M-<Up>", lazy.layout.up()),
    Key("M-<Left>", lazy.layout.left()),
    Key("M-<Right>", lazy.layout.right()),
    # Move window
    Key("M-S-<Down>", lazy.layout.shuffle_down()),
    Key("M-S-<Up>", lazy.layout.shuffle_up()),
    Key("M-S-<Left>", lazy.layout.shuffle_left()),
    Key("M-S-<Right>", lazy.layout.shuffle_right()),
    # Reset
    Key("M-S-n", lazy.layout.normalize()),
    # Toggle split
    Key("M-<space>", lazy.layout.toggle_split()),

    Key("M-l", lazy.group["freelance"].toscreen(toggle=True)),
    Key("M-<F1>", lazy.group["web"].toscreen(toggle=True)),
    Key("M-<F2>", lazy.group["edit"].toscreen(toggle=True)),
    Key("M-<F3>", lazy.group["shell"].toscreen(toggle=True)),
    Key("M-<F4>", lazy.group["tools"].toscreen(toggle=True)),
    Key("M-<F5>", lazy.group["scan"].toscreen(toggle=True)),
    Key("M-<F6>", lazy.group["ent"].toscreen(toggle=True)),
    Key("M-4", lazy.group["read"].toscreen(toggle=True)),
    Key("M-c", lazy.group["im"].toscreen(toggle=True)),
    Key("M-<Escape>", lazy.group["scratch"].toscreen(toggle=True)),

    Key("M-S-l", lazy.window.togroup("freelance")),
    Key("M-S-<F1>", lazy.window.togroup("web")),
    Key("M-S-<F2>", lazy.window.togroup("edit")),
    Key("M-S-<F3>", lazy.window.togroup("shell")),
    Key("M-S-<F4>", lazy.window.togroup("tools")),
    Key("M-S-<F5>", lazy.window.togroup("scan")),
    Key("M-S-<F6>", lazy.window.togroup("ent")),
    Key("M-S-4", lazy.window.togroup("read")),
    Key("M-S-c", lazy.window.togroup("im")),
    Key("M-S-<Escape>", lazy.window.togroup("scratch")),

    Key("M-f", lazy.window.toggle_fullscreen()),
    Key("M-<F12>", lazy.window.kill()),

    # Programs shortcuts
    Key("M-S-<Return>", lazy.spawn("alacritty")),
    Key("M-S-p", lazy.spawn("rofi -modi combi -show combi -combi-modi run,drun")),
    KeyChord([mod], "d", [
        Key("p", lazy.spawn("@projectsBinary@ open"))
    ]),

    Key("M-S-q", lazy.restart()),
]

screens = [
    Screen(
        bottom=bar.Bar(
            [
                widget.CurrentLayoutIcon(scale=0.65),
                widget.GroupBox(
                    font="Iosevka Bold",
                    fontsize=12,
                    border_width=2,
                    disable_drag=True,
                    highlight_method="line",
                    highlight_color=['#000000', '#000000'],
                    hide_unused= True,
                    spacing=0,
                ),
                widget.Prompt(
                    font="Iosevka Bold",
                    fontsize=14,
                    prompt="run: ",
                    ignore_dups_history=True,
                ),
                widget.WindowName(
                    font="Iosevka Bold",
                    fontsize=14,
                ),
                widget.CPUGraph(
                    font="Iosevka Bold",
                    fontsize=14,
                    width=30,
                    border_width=1,
                    border_color="#000000",
                    frequency=5,
                    line_width=1,
                    samples=50,
                ),
                widget.MemoryGraph(
                    font="Iosevka Bold",
                    fontsize=14,
                    width=30,
                    border_width=1,
                    border_color="#000000",
                    line_width=1,
                    frequency=5,
                    fill_color="EEE8AA"
                ),
                widget.Battery(
                    font="Iosevka Bold",
                    fontsize=14,
                    discharge_char='↓',
                    charge_char='↑',
                    format='{char} {hour:d}:{min:02d}',
                    # foreground=YELLOW,
                    # low_foreground=RED
                ),
                widget.Volume(
                    font="Iosevka Bold",
                    fontsize=14,
                    update_interval=2
                ),
                widget.Clock(
                    font="Iosevka Bold",
                    fontsize=14,
                    format='%a %d-%m-%y %H:%M',
                ),
                widget.Systray(
                    font="Iosevka Bold",
                    fontsize=14,
                ),

                widget.KeyboardLayout(
                    configured_keyboards=['us', 'ru'],
                    display_map={
                        'us': 'us ',
                        'ru': 'ru ',
                    },
                    # options='compose:rctrl',
                    # foreground=GREEN
                ),
            ],
            30,
        ),
    )
]
