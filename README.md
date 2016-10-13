svlad2
======

[![forthebadge](http://forthebadge.com/images/badges/designed-in-ms-paint.svg)](http://forthebadge.com)

Simple table editor scriptable with Lua.

author: Severák

license: MIT

## API reference

 - `svlad.alert(message)` - display mesage box
 - `confirmed = svlad.confirm(question)` -  display confirmation window, return true if user choosed `OK`
 - `text = svlad.get(row, col)` - get contents of cell
 - `svlad.set(row, col, text)` - set contents of cell
 - `svlad.focus(row, col)` - set focus on cell
 - `svlad.autosize()` - autosize grid
 - `row, col = svlad.get_max()` - get coordinate of bottom-right corner of filled part of grid
 - `rows, cols = svlad.get_bounds()` - get bounds of table
 - `svlad.clean()` - clean grid contents
 - `svlad.add_menu(label, function_name)` - adds item to `Actions` menu
 - `svlad.clean_menu()` - clean `Actions` menu


---

this project was abandonned, use [CSVPad](http://www.trustfm.net/GeneralTools/CSVpad.php)
