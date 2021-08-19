
--[[
                                                  
     Licensed under GNU General Public License v2 
      * (c) 2013,      Luke Bonham                
      * (c) 2010-2012, Peter Hofmann              
                                                  
--]]

local newtimer        = require("lain.helpers").newtimer
local read_pipe       = require("lain.helpers").read_pipe
local spairs          = require("lain.helpers").spairs

local wibox           = require("wibox")

local awful           = require("awful")
local util            = require("lain.util")

local io              = { popen  = io.popen }
local os              = { getenv = os.getenv }
local pairs           = pairs
local string          = { len    = string.len,
                          match  = string.match }

local setmetatable    = setmetatable

-- Maildir check
-- lain.widgets.maildir
local maildir = {}

local function worker(args)
    local args         = args or {}
    local timeout      = args.timeout or 60
    local mailpath     = args.mailpath or os.getenv("HOME") .. "/Mail"
    local ignore_boxes = args.ignore_boxes or {}
    local settings     = args.settings or function() end
    local ext_mail_cmd = args.external_mail_cmd

    maildir.widget = wibox.widget.textbox('')

    function update()
        if ext_mail_cmd ~= nil
        then
            awful.util.spawn(ext_mail_cmd)
        end

        -- Find pathes to mailboxes.
        local p = io.popen("find " .. mailpath ..
                           " -mindepth 1 -maxdepth 2 -type d" ..
                           " -not -name .git")
        local boxes = {}
        repeat
            line = p:read("*l")
            if line ~= nil
            then
                -- Find all files in the "new" subdirectory. For each
                -- file, print a single character (no newline). Don't
                -- match files that begin with a dot.
                -- Afterwards the length of this string is the number of
                -- new mails in that box.
                local mailstring = read_pipe("find " .. line ..
                                    "/new -mindepth 1 -type f " ..
                                    "-not -name '.*' -printf a")

                -- Strip off leading mailpath.
                local box = string.match(line, mailpath .. "/(.*)")
                local nummails = string.len(mailstring)
                if nummails > 0
                then
                    boxes[box] = nummails
                end
            end
        until line == nil

        p:close()

        newmail = "no mail"
        -- Count the total number of mails irrespective of where it was found
        total = 0

        for box, number in spairs(boxes)
        do
            -- Add this box only if it's not to be ignored.
            if not util.element_in_table(box, ignore_boxes)
            then
                total = total + number
                if newmail == "no mail"
                then
                    newmail = box .. "(" .. number .. ")"
                else
                    newmail = newmail .. ", " ..
                              box .. "(" .. number .. ")"
                end
            end
        end

        widget = maildir.widget
        settings()
    end

    newtimer(mailpath, timeout, update, true)
    return maildir.widget
end

return setmetatable(maildir, { __call = function(_, ...) return worker(...) end })
