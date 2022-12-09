# Lazarus IDESearchPanel
Search panel plugin for the Lazarus IDE


<img src="https://www.theo.ch/upload/Win_Searchpanel1.png" alt="Searchpanel Windows 10" width="800"/>


### Advantages over the standard "Find" window of the IDE (Ctrl+F)
<ul>
<li>Modern, convenient, accessible and intuitive interface.
<li>No forms and message boxes popping up.
<li>Function keys such as F3, which are often difficult to reach on notebook-computers, are not required. For the next occurrence of the search term, simply press "Enter" again.
<li>Search as you type: See directly whether and how often the search term was found.
<li>Correct your search term easily with the backspace key without having to open a search form again.
<li>Incremental search: Marks all occurrences of the search term in the visible area.
</ul>

### Advantages over the "Incremental Find" function of the IDE (Ctrl+E)
<ul>
<li>User-friendly, intuitive interface.
<li>Search options.
</ul>


<img src="https://www.theo.ch/upload/GTK_Searchpanel1.png" alt="Searchpanel Linux GTK2 with Options" width="800"/>


<img src="https://www.theo.ch/upload/Searchpanel_dark.png" alt="Searchpanel Dark with Options" width="800"/>


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
