config.aliases = {
    'jsd': 'set content.javascript.enabled false',
    'jse': 'set content.javascript.enabled true',
}

c.auto_save.interval = 15000
c.auto_save.session = True

c.editor.command = ['@emacsClient@', '-c', '-s @emacsServerSocketPath@', '+{line}:{column}', '{}']
c.new_instance_open_target = "window"
c.url.default_page = 'about:blank'
c.zoom.levels = ['25%', '33%', '50%', '67%', '75%', '90%', '100%', '110%', '125%', '150%', '175%', '200%', '250%', '300%', '400%', '500%']

c.colors.statusbar.url.success.https.fg = "white"
c.colors.tabs.even.bg = "silver"
c.colors.tabs.even.fg = "#666666"
c.colors.tabs.odd.bg = "gainsboro"
c.colors.tabs.odd.fg = c.colors.tabs.even.fg
c.colors.webpage.prefers_color_scheme_dark = True

c.completion.height = "20%"
c.completion.quick = False
c.completion.show = "auto"
c.completion.shrink = True
c.completion.timestamp_format = '%d-%m-%Y'
c.completion.use_best_match = False

c.confirm_quit = ["downloads"]

c.content.autoplay = False
c.content.cache.appcache = True
c.content.cache.size = 5242880
c.content.canvas_reading = True
c.content.cookies.store = True
c.content.geolocation = 'ask'
c.content.javascript.enabled = True
c.content.media_capture = 'ask'
c.content.mute = True
c.content.notifications = True
c.content.pdfjs = True
c.content.plugins = True
c.content.plugins = True
c.content.proxy = "none"
c.content.register_protocol_handler = True
c.content.ssl_strict = True
c.content.webgl = True
config.set('content.javascript.enabled', True, 'chrome://*/*')
config.set('content.javascript.enabled', True, 'file://*')
config.set('content.javascript.enabled', True, 'qute://*/*')

c.downloads.location.directory = "@qbDownloadPath@"
c.downloads.location.prompt = False
c.downloads.location.remember = True
c.downloads.location.suggestion = 'both'

c.hints.border = '1px solid #E3BE23'
c.hints.hide_unmatched_rapid_hints = True
c.hints.leave_on_load = True
c.hints.min_chars = 1
c.hints.mode = 'number'
c.hints.next_regexes = ['\\bnext\\b', '\\bmore\\b', '\\bnewer\\b', '\\b[>→≫]\\b', '\\b(>>|»)\\b', '\\bcontinue\\b']
c.hints.prev_regexes = ['\\bprev(ious)?\\b', '\\bback\\b', '\\bolder\\b', '\\b[<←≪]\\b', '\\b(<<|«)\\b']
c.hints.scatter = False
c.hints.uppercase = False

c.history_gap_interval = 30

c.input.insert_mode.auto_leave = True
c.input.insert_mode.auto_load = True
c.input.insert_mode.plugins = False
c.input.links_included_in_focus_chain = True
c.input.partial_timeout = 2000
c.input.spatial_navigation = True

c.keyhint.delay = 20

c.new_instance_open_target = 'tab'
c.new_instance_open_target_window = 'last-focused'

c.prompt.filebrowser = True

c.scrolling.bar = "always"
c.scrolling.smooth = True

c.search.ignore_case = 'smart'

c.statusbar.padding = {'top': 4, 'bottom': 4, 'left': 4, 'right': 4}
c.statusbar.widgets = ['keypress', 'url', 'history', 'tabs', 'progress']

c.tabs.background = True
c.tabs.last_close = "close"
c.tabs.new_position.related = 'next'
c.tabs.new_position.unrelated = 'last'
c.tabs.padding = { "left": 5, "right": 5, "top": 0, "bottom": 1}
c.tabs.position = 'top'
c.tabs.select_on_remove = "next"
c.tabs.show = "multiple"
c.tabs.tabs_are_windows = False
c.tabs.title.format = '{audio}{current_title}'
c.tabs.title.format_pinned = '{audio}'

c.url.auto_search = 'never'
c.url.default_page = "about:blank"
c.url.incdec_segments = ['path', 'query']
c.url.yank_ignored_parameters = ['ref', 'utm_source', 'utm_medium', 'utm_campaign', 'utm_term', 'utm_content']

c.window.title_format = "{private}{perc}{current_title}{title_sep}qutebrowser | {current_url}"

config.source('keybindings.py')
