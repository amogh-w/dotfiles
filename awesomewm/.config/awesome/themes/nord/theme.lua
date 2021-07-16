--[[

     Powerarrow Awesome WM theme
     github.com/lcpz

--]] local gears = require("gears")
local lain = require("lain")
local awful = require("awful")
local wibox = require("wibox")
local dpi = require("beautiful.xresources").apply_dpi

local math, string, os = math, string, os
local my_table = awful.util.table or gears.table -- 4.{0,1} compatibility

local theme = {}
theme.dir = os.getenv("HOME") .. "/.config/awesome/themes/nord"
theme.wallpaper = theme.dir .. "/wall.png"
theme.font = "Sauce Code Pro 10"
theme.fg_normal = "#ECEFF4"
theme.fg_focus = "#81A1C1"
theme.fg_urgent = "#BF616A"
theme.bg_normal = "#2E3440"
theme.bg_focus = "#2E3440"
theme.bg_urgent = "#2E3440"
theme.taglist_fg_focus = "#ECEFF4"
theme.taglist_bg_focus = "#5E81AC"
theme.taglist_fg_occupied = "#ECEFF4"
theme.taglist_bg_occupied = "#81A1C1"
theme.tasklist_bg_focus = "#2E3440"
theme.tasklist_fg_focus = "#81A1C1"
theme.border_width = dpi(2)
theme.border_normal = "#ECEFF4"
theme.border_focus = "#81A1C1"
theme.border_marked = "#D08770"
theme.titlebar_bg_focus = "#2E3440"
theme.titlebar_bg_normal = "#2E3440"
theme.titlebar_bg_focus = theme.bg_focus
theme.titlebar_bg_normal = theme.bg_normal
theme.titlebar_fg_focus = theme.fg_focus
theme.menu_height = dpi(16)
theme.menu_width = dpi(140)
theme.tasklist_plain_task_name = true
theme.tasklist_disable_icon = true
theme.useless_gap = 3
theme.gap_single_client = true

local markup = lain.util.markup
local separators = lain.util.separators

-- Logout
local logout_menu_widget = require("themes.nord.logout-menu-widget.logout-menu")

-- Spotify
local spotify = require("themes.nord.spotify-widget.spotify")

-- Battery Widget
local battery_widget = require("themes.nord.battery-widget")

local BAT0 = battery_widget {
    ac = "AC",
    adapter = "BAT1",
    ac_prefix = "AC: ",
    battery_prefix = "",
    percent_colors = {{25, "white"}, {50, "white"}, {999, "white"}},
    listen = true,
    timeout = 10,
    widget_text = "${AC_BAT}${color_on}${percent}%${color_off}",
    widget_font = "Sauce Code Pro Nerd Font 10",
    tooltip_text = "Battery ${state}${time_est}\nCapacity: ${capacity_percent}%",
    alert_threshold = 5,
    alert_timeout = 0,
    alert_title = "Low battery !",
    alert_text = "${AC_BAT}${time_est}"
}

-- Binary clock
local binclock = require("themes.powerarrow.binclock") {
    height = dpi(32),
    show_seconds = true,
    color_active = theme.fg_normal,
    color_inactive = theme.bg_focus
}

-- Calendar
theme.cal = lain.widget.cal({
    -- cal = "cal --color=always",
    attach_to = {binclock.widget},
    notification_preset = {
        font = "Sauce Code Pro 10",
        fg = theme.fg_normal,
        bg = theme.bg_normal
    }
})

-- Taskwarrior
local task = wibox.widget.imagebox(theme.widget_task)
lain.widget.contrib.task.attach(task, {
    -- do not colorize output
    show_cmd = "task | sed -r 's/\\x1B\\[([0-9]{1,2}(;[0-9]{1,2})?)?[mGK]//g'"
})
task:buttons(my_table.join(awful.button({}, 1, lain.widget.contrib.task.prompt)))

-- Scissors (xsel copy and paste)
local scissors = wibox.widget.imagebox(theme.widget_scissors)
scissors:buttons(my_table.join(awful.button({}, 1, function()
    awful.spawn.with_shell("xsel | xsel -i -b")
end)))

-- Mail IMAP check
--[[ commented because it needs to be set before use
local mailicon = wibox.widget.imagebox(theme.widget_mail)
mailicon:buttons(my_table.join(awful.button({ }, 1, function () awful.spawn(mail) end)))
theme.mail = lain.widget.imap({
    timeout  = 180,
    server   = "server",
    mail     = "mail",
    password = "keyring get mail",
    settings = function()
        if mailcount > 0 then
            widget:set_text(" " .. mailcount .. " ")
            mailicon:set_image(theme.widget_mail_on)
        else
            widget:set_text("")
            mailicon:set_image(theme.widget_mail)
        end
    end
})
--]]

-- ALSA volume
theme.volume = lain.widget.alsabar({
    -- togglechannel = "IEC958,3",
    notification_preset = {font = "Terminus 10", fg = theme.fg_normal}
})

-- MPD
local musicplr = awful.util.terminal ..
                     " -title Music -g 130x34-320+16 -e ncmpcpp"
local mpdicon = wibox.widget.imagebox(theme.widget_music)
mpdicon:buttons(my_table.join(awful.button({modkey}, 1, function()
    awful.spawn.with_shell(musicplr)
end), awful.button({}, 1, function()
    os.execute("mpc prev")
    theme.mpd.update()
end), awful.button({}, 2, function()
    os.execute("mpc toggle")
    theme.mpd.update()
end), awful.button({}, 3, function()
    os.execute("mpc next")
    theme.mpd.update()
end)))
theme.mpd = lain.widget.mpd({
    settings = function()
        if mpd_now.state == "play" then
            artist = " " .. mpd_now.artist .. " "
            title = mpd_now.title .. " "
            mpdicon:set_image(theme.widget_music_on)
            widget:set_markup(markup.font(theme.font,
                                          markup("#FF8466", artist) .. " " ..
                                              title))
        elseif mpd_now.state == "pause" then
            widget:set_markup(markup.font(theme.font, " mpd paused "))
            mpdicon:set_image(theme.widget_music_pause)
        else
            widget:set_text("")
            mpdicon:set_image(theme.widget_music)
        end
    end
})

theme.icon_size = 12
theme.icon_font = "Font Awesome 5 Free Solid " -- attention to space at the end!
theme.icon_color = "#FFFFFF"

-- FontAwesome
local function make_fa_icon(code)
    return wibox.widget {
        font = theme.icon_font .. theme.icon_size,
        markup = ' <span color="' .. theme.icon_color .. '">' .. code ..
            '</span> ',
        align = 'center',
        valign = 'center',
        widget = wibox.widget.textbox
    }
end

-- MEM
local memicon = wibox.widget.imagebox(theme.widget_mem)
local mem = lain.widget.mem({
    settings = function()
        widget:set_markup(markup.font(theme.font, " " .. mem_now.used .. "MB "))
    end
})

-- CPU
local cpuicon = wibox.widget.imagebox(theme.widget_cpu)
local cpu = lain.widget.cpu({
    settings = function()
        widget:set_markup(markup.font(theme.font, " " .. cpu_now.usage .. "% "))
    end
})

--[[ Coretemp (lm_sensors, per core)
local tempwidget = awful.widget.watch({awful.util.shell, '-c', 'sensors | grep Core'}, 30,
function(widget, stdout)
    local temps = ""
    for line in stdout:gmatch("[^\r\n]+") do
        temps = temps .. line:match("+(%d+).*°C")  .. "° " -- in Celsius
    end
    widget:set_markup(markup.font(theme.font, " " .. temps))
end)
--]]
-- Coretemp (lain, average)
local temp = lain.widget.temp({
    settings = function()
        widget:set_markup(markup.font(theme.font, " " .. coretemp_now .. "°C "))
    end
})
-- ]]
local tempicon = wibox.widget.imagebox(theme.widget_temp)

-- / fs
local fsicon = wibox.widget.imagebox(theme.widget_hdd)
--[[ commented because it needs Gio/Glib >= 2.54
theme.fs = lain.widget.fs({
    notification_preset = { fg = theme.fg_normal, bg = theme.bg_normal, font = "Terminus 10" },
    settings = function()
        local fsp = string.format(" %3.2f %s ", fs_now["/"].free, fs_now["/"].units)
        widget:set_markup(markup.font(theme.font, fsp))
    end
})
--]]

-- Battery
local baticon = wibox.widget.imagebox(theme.widget_battery)
local bat = lain.widget.bat({
    settings = function()
        if bat_now.status and bat_now.status ~= "N/A" then
            if bat_now.ac_status == 1 then
                widget:set_markup(markup.font(theme.font, " AC "))
                baticon:set_image(theme.widget_ac)
                return
            elseif not bat_now.perc and tonumber(bat_now.perc) <= 5 then
                baticon:set_image(theme.widget_battery_empty)
            elseif not bat_now.perc and tonumber(bat_now.perc) <= 15 then
                baticon:set_image(theme.widget_battery_low)
            else
                baticon:set_image(theme.widget_battery)
            end
            widget:set_markup(markup.font(theme.font,
                                          " " .. bat_now.perc .. "% "))
        else
            widget:set_markup()
            baticon:set_image(theme.widget_ac)
        end
    end
})

-- Net
local neticon = wibox.widget.imagebox(theme.widget_net)
local net = lain.widget.net({
    settings = function()
        widget:set_markup(markup.fontfg(theme.font, "#FEFEFE", " " ..
                                            net_now.received .. " ↓↑ " ..
                                            net_now.sent .. " "))
    end
})

-- Brigtness
local brighticon = wibox.widget.imagebox(theme.widget_brightness)
-- If you use xbacklight, comment the line with "light -G" and uncomment the line bellow
-- local brightwidget = awful.widget.watch('xbacklight -get', 0.1,
local brightwidget = awful.widget.watch('light -G', 0.1,
                                        function(widget, stdout, stderr,
                                                 exitreason, exitcode)
    local brightness_level = tonumber(string.format("%.0f", stdout))
    widget:set_markup(markup.font(theme.font, " " .. brightness_level .. "%"))
end)

-- Separators
local arrow = separators.arrow_left

function theme.powerline_rl(cr, width, height)
    local arrow_depth, offset = height / 2, 0

    -- Avoid going out of the (potential) clip area
    if arrow_depth < 0 then
        width = width + 2 * arrow_depth
        offset = -arrow_depth
    end

    cr:move_to(offset + arrow_depth, 0)
    cr:line_to(offset + width, 0)
    cr:line_to(offset + width - arrow_depth, height / 2)
    cr:line_to(offset + width, height)
    cr:line_to(offset + arrow_depth, height)
    cr:line_to(offset, height / 2)

    cr:close_path()
end

function theme.at_screen_connect(s)
    -- Quake application
    s.quake = lain.util.quake({app = awful.util.terminal})

    -- If wallpaper is a function, call it with the screen
    local wallpaper = theme.wallpaper
    if type(wallpaper) == "function" then wallpaper = wallpaper(s) end
    gears.wallpaper.maximized(wallpaper, s, true)

    -- Tags
    awful.tag(awful.util.tagnames, s, awful.layout.layouts[1])

    -- Create a promptbox for each screen
    s.mypromptbox = awful.widget.prompt()
    -- Create an imagebox widget which will contains an icon indicating which layout we're using.
    -- We need one layoutbox per screen.
    s.mylayoutbox = awful.widget.layoutbox(s)
    s.mylayoutbox:buttons(my_table.join(awful.button({}, 1, function()
        awful.layout.inc(1)
    end), awful.button({}, 2,
                       function() awful.layout.set(awful.layout.layouts[1]) end),
                                        awful.button({}, 3, function()
        awful.layout.inc(-1)
    end), awful.button({}, 4, function() awful.layout.inc(1) end), awful.button(
                                            {}, 5,
                                            function()
            awful.layout.inc(-1)
        end)))
    -- Create a taglist widget
    -- s.mytaglist = awful.widget.taglist(s, awful.widget.taglist.filter.all,
    --                                    awful.util.taglist_buttons)
    s.mytaglist = awful.widget.taglist {
        screen = s,
        filter = awful.widget.taglist.filter.all,
        style = {shape = gears.shape.powerline},
        layout = {
            -- spacing = 12,
            -- spacing_widget = {
            --     color = '#81A1C1',
            --     shape = gears.shape.powerline,
            --     widget = wibox.widget.separator
            -- },
            layout = wibox.layout.fixed.horizontal
        },
        widget_template = {
            {
                -- {
                --     {
                --         {id = 'index_role', widget = wibox.widget.textbox},
                --         margins = 4,
                --         widget = wibox.container.margin
                --     },
                --     bg = '#81A1C1',
                --     shape = gears.shape.circle,
                --     widget = wibox.container.background
                -- },
                -- {
                --     {id = 'icon_role', widget = wibox.widget.imagebox},
                --     margins = 2,
                --     widget = wibox.container.margin
                -- },
                {
                    {id = 'text_role', widget = wibox.widget.textbox},
                    widget = wibox.container.margin,
                    left = 16,
                    right = 16
                },
                layout = wibox.layout.fixed.horizontal
            },
            id = 'background_role',
            widget = wibox.container.background,
            -- Add support for hover colors and an index label
            create_callback = function(self, c3, index, objects) -- luacheck: no unused args
                self:get_children_by_id('text_role')[1].markup =
                    '<b> ' .. index .. ' </b>'
                self:connect_signal('mouse::enter', function()
                    if self.bg ~= '#4C566A' then
                        self.backup = self.bg
                        self.has_backup = true
                    end
                    self.bg = '#4C566A'
                end)
                self:connect_signal('mouse::leave', function()
                    if self.has_backup then
                        self.bg = self.backup
                    end
                end)
            end,
            update_callback = function(self, c3, index, objects) -- luacheck: no unused args
                self:get_children_by_id('text_role')[1].markup =
                    '<b> ' .. index .. ' </b>'
            end
        },
        buttons = taglist_buttons
    }

    -- Create a tasklist widget
    s.mytasklist = awful.widget.tasklist(s, awful.widget.tasklist.filter
                                             .currenttags,
                                         awful.util.tasklist_buttons)

    -- Create the wibox
    s.mywibox = awful.wibar({
        position = "top",
        screen = s,
        height = dpi(24),
        bg = theme.bg_normal,
        fg = theme.fg_normal
    })

    -- Add widgets to the wibox
    s.mywibox:setup{
        layout = wibox.layout.align.horizontal,
        { -- Left widgets
            layout = wibox.layout.fixed.horizontal,
            -- spr,
            s.mytaglist,
            s.mypromptbox,
            spr
        },
        s.mytasklist, -- Middle widget
        { -- Right widgets
            layout = wibox.layout.fixed.horizontal,
            -- wibox.widget.systray(),
            arrow(theme.bg_normal, "#81A1C1"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                -- memicon,
                make_fa_icon('\u{f0c9} '),
                wibox.widget.systray(),
                layout = wibox.layout.align.horizontal
            }, dpi(2), dpi(3)), "#81A1C1"),
            arrow("#81A1C1", "#5E81AC"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                -- memicon,
                make_fa_icon('\u{f001} '),
                spotify({
                    font = 'Sauce Code Pro 10',
                    play_icon = '/usr/share/icons/Papirus-Light/24x24/categories/spotify.svg',
                    pause_icon = '/usr/share/icons/Papirus-Dark/24x24/panel/spotify-indicator.svg'
                }),
                layout = wibox.layout.align.horizontal
            }, dpi(2), dpi(3)), "#5E81AC"),
            arrow("#5E81AC", "#81A1C1"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                -- memicon,
                make_fa_icon('\u{f538}'),
                mem.widget,
                layout = wibox.layout.align.horizontal
            }, dpi(2), dpi(3)), "#81A1C1"),
            arrow("#81A1C1", "#5E81AC"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                -- cpuicon,
                make_fa_icon('\u{f004}'),
                cpu.widget,
                layout = wibox.layout.align.horizontal
            }, dpi(3), dpi(4)), "#5E81AC"),
            arrow("#5E81AC", "#81A1C1"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                -- tempicon,
                make_fa_icon('\u{f2c9}'),
                temp.widget,
                layout = wibox.layout.align.horizontal
            }, dpi(4), dpi(4)), "#81A1C1"),
            arrow("#81A1C1", "#5E81AC"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                nil,
                -- neticon,
                make_fa_icon('\u{f1eb}'),
                net.widget,
                layout = wibox.layout.align.horizontal
            }, dpi(3), dpi(3)), "#5E81AC"),
            arrow("#5E81AC", "#81A1C1"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                make_fa_icon('\u{f242}'),
                BAT0,
                layout = wibox.layout.align.horizontal
            }, dpi(4), dpi(8)), "#81A1C1"),
            arrow("#81A1C1", "#5E81AC"),
            wibox.container.background(wibox.container.margin(wibox.widget {
                make_fa_icon('\u{f017}'),
                wibox.widget.textclock(),
                layout = wibox.layout.align.horizontal
            }, dpi(4), dpi(8)), "#5E81AC"),
            arrow("#5E81AC", "#81A1C1"),
            -- ]]
            wibox.container.background(wibox.container.margin(s.mylayoutbox,
                                                              dpi(4), dpi(1)),
                                       "#81A1C1"),
            wibox.container.background(wibox.container.margin(
                                           logout_menu_widget(), dpi(4), dpi(1)),
                                       "#81A1C1"),
            arrow("#81A1C1", "#81A1C1")

        }
    }
end

return theme
