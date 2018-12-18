app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

app$uploadFile(inDATA = "../../../../data/data.RData") # <-- This should be the path to the file, relative to the app's tests/ directory
app$snapshot()
