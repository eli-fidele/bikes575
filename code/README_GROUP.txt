
Hi folks, Ali here

I've created this "dev-template.Rmd" file for each one of us to duplicate and then work on (for "scratchwork") in parallel.

This way, merge conflicts don't occur since you would be working on your own copy of the template file. 

Additionally, the first (setup) chunk contains a "module" of code, so if you run it, you should get a standardized environment with 
specific datasets and functions that is standardized for everyone (helps with fixing bugs in future). In practice, this is probably going to be just the wrangled dataset and a few model functions.


=========================================
           Duplicating the File
=========================================

You could duplicate the template file in one of two ways:

==========================
   In RStudio (EASY WAY)
==========================

1. In the "Files" pane, check the box next to the "dev-template.Rmd" file to select it.
2. Click on "More" > "Copy..." and name your personal copy of the file.


==========================
          Manually
==========================

1. Create a new .Rmd file 
2. Copy and paste (at least) the first chunk in the "dev-template.Rmd" file