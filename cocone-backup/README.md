OSX Backup to Google Drive
==========================

Scripts and config to back up my laptop data to my Google storage.
[Rclone][rclone] is the backup tool I'm using; the various scripts conveniently
wrap calls to `rclone` to transfer data to my two Google drives:

1. *andrea.falconi* (referring to this as `gd1`)
2. *andrea.falconi.cell*: (referring to this as `gd2`)

The idea is to back up `kb` to `gd2` and everything else to `gd1`. The `*gd1.sh`
scripts transfer data to `gd1`, whereas the `*gd2.sh` ones transfer to `gd2`.


Deployment
----------
Run the deployment script from this directory to install the latest version of
the scripts in `/Volumes/data`; the script will create a `_sync_` directory
with `bin` and `config` sub-directories where the scripts and configuration
will go. (Directories will be created only if needed.)
Additionally, if installing from scratch, you also have to install Rclone and
generate the configuration files, see below.

### Installing Rclone
[Download][rclone-download] the latest Rclone for OSX (AMD64), then unzip, `cd`
into the unzipped directory, and do a manual install:

    $ sudo cp rclone /usr/bin/
    $ sudo xattr -d com.apple.quarantine /usr/bin/rclone
    $ chmod +x /usr/bin/rclone
    $ sudo cp rclone.1 /usr/share/man/man1/ 
    $ sudo xattr -d com.apple.quarantine /usr/share/man/man1/rclone.1 

### Generating Rclone config
You need to generate a config file for each Drive account. Run:

    $ rclone config

and follow the instructions as explained [here][rclone-gd-config]. Call the
remote `gd` and log in as *andrea.falconi* when prompted. Now you should have
a brand new Rclone config file in your home that you need to move to `_sync_`:

    $ mv ~/.rclone.conf /Volumes/data/_sync_/config/gd1.conf

Do the same for the other account *andrea.falconi.cell* but call the file
`gd2.conf`.

**WARNING**. Never ever put these files in GitHub as they contain the API keys
to access your Drive accounts!


Usage
-----
The scripts either *copy* or *sync* data to my Google drives:

1. *copy* my local data to Google Drive. 
   Doesn’t transfer unchanged files. Doesn’t delete files from Drive.
   Use the `rcopy-to` script to do a *copy*; the corresponding `show-rcopy-to`
   script will do a *copy* dry run.
2. *sync* local to Drive, changing Drive data only. 
   Doesn’t transfer unchanged files. Drive data is updated to match local, 
   including deleting files if necessary. Use the `rsync-to` script to do a
   *sync*; the corresponding `show-rsync-to` script will do a *sync* dry run.
   **Warning**. Since a *sync* can cause data loss, test first with the
   `show-rsync-to` script.

All the above scripts take a mandatory configuration name argument (either `gd1`
or `gd2`) followed by optional Rclone additional options (e.g. `--checksum` as
explained below).

Normally Rclone will look at modification time and size of files to  see if they
are equal. If you pass the `--checksum` option to the scripts, then Rclone checks 
MD5SUM and size to determine if files are equal. (But this is slower and does not
update `mtimes` of remote files if they are incorrect as it would normally.)

Rclone output goes into the `_sync_/log` directory (log files are named after
script names), except for the progress update messages which are printed to
`stdout`.



[rclone]: http://rclone.org/
    "Rclone Home Page"

[rclone-download]: http://downloads.rclone.org/rclone-current-osx-amd64.zip
    "Latest Rclone for OSX (AMD64)"

[rclone-gd-config]: http://rclone.org/drive/
    "Rclone Google Drive Configuration"
