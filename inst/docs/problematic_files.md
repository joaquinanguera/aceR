# Problematic files

Keeping track of any manual changes we've had to make to files so that they can be read and processed by `aceR` without creating a mess (artifacts).

Taking this approach instead of convoluting the codebase with un-generalizable conditionals. 

```
EVO/Control Boys/1014/1014_SAAT.xlsx
```
 - Removed random chunk of rows in `Post Raw` sheet.
 
 
 ```
 Brighten/newsaat_filtered.xls
 ```
 
 - saved as .xlsx
 - removed first "id" column
 - renamed "brightenid" to "pid"
 - manually changed all column names to snake_case
 
 
 ```
 Brighten/newflanker_BRIGHTEN_filtered.xls
 ```
 
 - saved as .xlsx
 - removed first "id" column
 - manually changed all column names to snake_case
 - make pid column programmatically:
 ```
file = "~/Desktop/ACE Studies_Raw Data/Brighten/newflanker_BRIGHTEN_filtered.xlsx"
raw = openxlsx::read.xlsx(file, sheet = 1)
raw$pid = ifelse(nchar(raw$id) == 2, paste0(raw$group, "-000", raw$id), paste0(raw$group, "-00", raw$id))
openxlsx::write.xlsx(raw, file)
 ```
 
