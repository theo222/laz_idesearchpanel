# Lazarus IDESearchPanel
Search panel plugin for the Lazarus IDE

<img src="https://www.theo.ch/upload/Win_Searchpanel1.png" alt="Searchpanel Windows 10" width="600"/>

<img src="https://www.theo.ch/upload/GTK_Searchpanel1.png" alt="Searchpanel Linux GTK2 with Options" width="600"/>


## Installation:

Like any other Package.
Rebuild the IDE.
https://wiki.lazarus.freepascal.org/Install_Packages


## How to use:
To toggle the Search Panel on and off, there is a menu item called "Show Search Panel" under "Search".<br>
You can also use CTRL+P.

"Find Next" can be performed by the button or by simply pressing "Enter" again, while the focus is on the search edit.

Ctrl+F, F3 etc. are reserved for the standard search window of the IDE and are not used by the Search Panel.

If you encounter conflicts with CTRL+P, for example if "printers4lazide" is installed, you can:
<ul>
<li>Uninstall printers4lazide OR
<li>Change the Key Mappings in "Tools -> Options -> Editor -> Key Mappings" OR
<li>Add a line to the "idesearchpanelconfig.xml" to define a different shortcut:
</ul>
<pre>
  &lt;SPConfig ShortCut=&quot;Ctrl+P&quot;/&gt;
</pre>

Use non-localized names for shiftstates, like Ctrl, Shift, Alt.., or "Unknown" for no shortcut.

idesearchpanelconfig.xml can be usually found in places like:
<ul>
<li>Fpcupdeluxe: &lt;fpcupdeluxe&gt;/config_lazarus/
<li>Windows: C:\users\&lt;your_name&gt;\appdata\lazarus
<li>Linux: ~/.lazarus
</ul>
If you are not sure, see: View -> IDE Internals -> About IDE -> Global IDE options -> "Primary config directory"

If you prefer blue instead of black icons, add to idesearchpanelconfig.xml:
<pre>
   &lt;SPConfig ShortCut=&quot;Ctrl+P&quot; BlackIcons=&quot;False&quot;/&gt;
</pre>

### Options:

Please Note: "Regular Expressions" and "Search-as-you-type" are mutually exclusive.

Search in selected text only works if "Search-as-you-type" is deactivated.

The rest should be self-explanatory.

HAVE FUN!
