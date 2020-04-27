# config.source('theme/theme.py') # example of config modularization for later use

config.aliases = {}

c.auto_save.session = True
c.editor.command = ['@emacsClient@', '-c', '{file}']
c.hints.chars = "9876543210"
c.url.default_page = 'about:blank'
c.zoom.levels = ['50%', '75%', '100%', '125%', '150%', '200%']

c.colors.statusbar.url.success.https.fg = "white"
c.colors.tabs.even.bg = "silver"
c.colors.tabs.even.fg = "#666666"
c.colors.tabs.odd.bg = "gainsboro"
c.colors.tabs.odd.fg = c.colors.tabs.even.fg

c.completion.height = "20%"
c.completion.quick = False
c.completion.show = "auto"

c.confirm_quit = ["always"]

# c.content.user_stylesheets = "user.css"
c.content.media_capture = 'ask'
c.content.notifications = 'ask'
c.content.pdfjs = True
c.content.proxy = "none"
c.content.ssl_strict = True
c.content.webgl = True

c.downloads.location.directory = "@downloadPath@"
c.downloads.location.prompt = False

c.input.insert_mode.auto_leave = True
c.input.insert_mode.auto_load = True

c.scrolling.bar = "always"
c.scrolling.smooth = True

c.statusbar.hide = False
c.statusbar.padding = {'top': 4, 'bottom': 4, 'left': 4, 'right': 4}
c.statusbar.widgets = ['keypress', 'url', 'history', 'tabs', 'progress']

c.tabs.background = True
c.tabs.last_close = "close"
c.tabs.new_position.related = 'next'
c.tabs.padding = { "left": 5, "right": 5, "top": 0, "bottom": 1}
c.tabs.select_on_remove = "prev"
c.tabs.show = "multiple"
c.tabs.tabs_are_windows = False

c.url.default_page = "about:blank"

c.window.title_format = "{private}{perc}{current_title}{title_sep}qutebrowser"

config.unbind("<ctrl+tab>")
config.unbind("O")
config.unbind("T")
config.unbind("b")
config.unbind("th")
config.unbind("tl")
config.unbind('<ctrl+x>')

# consider moving this on xkeysnail level later
config.bind("<Alt-,>", "back")
config.bind("<Alt-.>", "forward")
config.bind("<Ctrl-t>", "set-cmd-text -s :open -t ")
config.bind("<ctrl+shift+tab>", "tab-prev")
config.bind("<ctrl+tab>", "tab-next")
config.bind("<f12>", "inspector")
config.bind("O", "set-cmd-text :open {url:pretty}")
config.bind("T", "set-cmd-text :open -t {url:pretty}")
config.bind("b", "set-cmd-text -s :buffer")
config.bind("d", "tab-close")
config.bind("gi", "hint inputs")
config.bind("t", "set-cmd-text -s :open -t")
config.bind("yy", "yank")
