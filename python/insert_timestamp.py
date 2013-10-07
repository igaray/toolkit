# This is a code snippet for sublime text
# Copy it into ~/.config/sublime-text-2/Packages/User and compile it.
# Then add a keybinding in ~/.config/sublime-text-2/Packages/User/Default (Linux).sublime-keymap
# { "keys": ["alt+d"], "command": "insert_timestamp" },

import datetime
import sublime_plugin

class InsertTimestampCommand(sublime_plugin.TextCommand):
  def run(self, edit):

    dayofweek = [' Monday', ' Tuesday', ' Wednesday', ' Thursday', ' Friday', ' Saturday', ' Sunday']

    #generate the timestamp
    now           = datetime.datetime.now()
    weekday       = dayofweek[datetime.date.weekday(now)]
    timestamp_str = now.strftime("%d.%m.%Y %H:%M") + weekday

    #for region in the selection
    #(i.e. if you have multiple regions selected,
    # insert the timestamp in all of them)
    for r in self.view.sel():
      #put in the timestamp
      #(if text is selected, it'll be
      # replaced in an intuitive fashion)
      if r.size() > 0:
        self.view.replace(edit, r, timestamp_str)
      else:
        self.view.insert(edit, r.begin(), timestamp_str)
