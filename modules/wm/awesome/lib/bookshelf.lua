local bookshelf = {}

local os = require("os")
local awful = require("awful")
local naughty = require("naughty")

local utils = require("utils")

-- TODO: abstract options below
bookshelf.books_path = os.getenv("HOME") .. "/bookshelf/"
bookshelf.reader_app = "zathura"

bookshelf.tag_to_switch = "⧉"
bookshelf.tag_activate_function = function (tag)
   awful.tag.viewtoggle(tag)
end

function bookshelf:show_books_menu()
   books_filelist = utils:get_filelist(bookshelf.books_path)
   awful.spawn.easy_async({"bash", "-c", "echo -e \"" .. table.concat(books_filelist, "\n") .. "\" | rofi -dmenu -i"},
                          function (stdout, stderr, reason, exit_code)
                             local screen = awful.screen.focused()
                             local _tag
                             for i, tag in ipairs(screen.tags) do
                                if tag.name == bookshelf.tag_to_switch then
                                   _tag = tag
                                   break
                                end
                             end
                             cmd = string.gsub(bookshelf.reader_app .. " \"" .. stdout .. "\"", "\n", "")
                             awful.spawn(cmd)
                             bookshelf.tag_activate_function(_tag)
   end)
end

return bookshelf
