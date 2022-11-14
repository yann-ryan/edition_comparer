# Edition Comparer 

This Shiny application needs to run locally as it uses local environment variables for authentification. 

You need to have a working MariaDB setup on your local machine already. 

- Clone (or download) this repository. 
- Set the correct username and password for the Maria DB server with the following:

```r
Sys.setenv("DB_USER" = "<USER HERE>")
Sys.setenv("DB_PASS" = "<PASSWORD HERE>")

```

- Open the project in R, and open the App. 
- Install any missing packages as prompted

## Using the application

The application is quite simple. Copy and paste a work ID into the text input box. Wait a couple of seconds, and each edition of that work ID will be displayed in the drop-down below. Leave as-is or change to another seed edition. 

Click the 'generate' button and wait (up to a couple of minutes, so be patient). 

An interactive display will appear below the input controls. 

Each entry on the y-axis is one edition, and each red area represents a 'gap': a portion of the second text which is not a reused section of the seed text. We can infer that these are likely to be new pieces of text inserted and not present in the seed text. 

Clicking on one of these horizontal sections will load the octavo reader with that reuse chunk highlighted, plus the equivalent offsets for the seed text. 
