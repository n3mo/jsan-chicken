# jsan
The **J**SON **S**wiss **A**rmy k**N**ife. Convert, subset, repurpose, and otherwise extract information in JSON streams/files from the command line

`jsan` provides extraction of data from JSON streams (from file or standard input) and returns (to file or standard output) the result in user-customizable format. A common application is to convert JSON-encoded data into comma separated values (CSV) format.  Conversion is fast and does not consume memory, allowing for conversion of arbitrarily large (or endlessly streaming) JSON files. Data fields can optionally be filtered during conversion so that only desired fields are retained in the resulting output. 

Because `jsan` can also write to standard output, it can also serve as a useful exploration tool for quickly exploring JSON data (e.g., pulling user names across all entries). Output can be piped to other common command line tools (e.g., awk) as in traditional unix-like workflows.

## Usage Examples

**For usage help:**

    jsan --help

**To convert the file mydata.json to results.csv:**

    jsan --input=mydata.json --output=results.csv

**To list available data fields in mydata.json:**

    jsan --input=mydata.json --list

**To convert AND only keep the data fields "text" and "created_on" while writing to standard output:**

    jsan --input=mydata.json --keep text created_on

**You can also remove named data fields. The following will keep all fields EXCEPT "email" and "phone":**

    jsan --input=mydata.json --remove email phone

**Nested JSON data fields can be accessed by using a colon. To keep the fields user->screen_name and user->bio->address**

    jsan --input=mydata.json --keep user:screen_name user:bio:address

**Use pipes to use the output of other commands as input:**

    cat mydata.json | jsan

**Combine and get creative. Let's list all users whose screen names start with "M":**

    cat mydata.json | jsan -k user:screen_name | grep '^"m'

**Same as above, but dump the results into a file called names.txt**

    cat mydata.json | jsan -k user:screen_name | grep '^"m' > names.txt

**The delimeter can optionally be set (default = ","):**

    jsan --delimeter=','

**A header with "column" names is output by default. Suppress with:**

    jsan --noheader

## Installation

[Self-contained binary packages](http://www.nicholasvanhorn.com/software/) have been pre-built for Mac OS X and x86_64 Linux systems. Linux binaries were built on Arch Linux and should work on similar systems (it has also been tested to work on Ubuntu 12.04). Mac binaries were built on OS X 10.10.5, but should work for other recent version (your mileage may vary). For other systems you will need to build from source (see **Manual Build**).

- Download the [latest version x.x.x](http://www.nicholasvanhorn.com/software/) in zip or tar.gz format.
- Unpack it in a destination folder of your choice, replacing x.x.x with an appropriate version number

```sh
unzip jsan-x.x.x-linux-x86_64.zip
cd jsan
```
or

```sh
tar -xzf jsan-x.x.x-linux-x86_64.tar.gz
cd jsan
```

- An executable named jsan is located in the newly created folder "jsan". You can run this file directly with `./jsan`
- Better yet, make a symbolic link to the executable somewhere on your path. Working from within the `jsan` directory created by the unpacking command, run the following:

```sh
sudo ln -s `pwd`/jsan /usr/local/bin
```

Now you can call `jsan` from within any directory.

### Manual Build

#### Prerequisites
To build jsan from source, you will need [Chicken Scheme](http://www.call-cc.org/) installed on your system. This software was developed and tested on Chicken 4.9.0. Your mileage may vary on other versions. 

Additionally, you will need the following [Eggs](http://wiki.call-cc.org/eggs) installed:
* medea
* args

jsan also uses the following Units (which are included by default):
* file
* srfi-1
* srfi-13

#### Interpreted
Installation is simple once Chicken is properly installed:

1. Clone the git repository into a directory of your choice
2. Ensure that the file jsan.scm is executable (`chmod +x jsan.scm`)

#### Compiled
For better performance, you should consider compiling the script to a binary executable. To do so, first follow the installation steps above, then compile the script with

```sh
csc jsan.scm -o jsan
```

The resulting executable "jsan" should be placed somewhere on your PATH.

# Bugs & Improvements

Please report any problems that you find, along with any suggestions or contributions to the theme. 

You can support this project, or my other projects via [ChangeTip](http://n3mo.tip.me)

[![Support via ChangeTip](http://www.nicholasvanhorn.com/images/changetip.png)](http://n3mo.tip.me)


## License

Copyright (C) 2015-2018 Nicholas M. Van Horn

Author: Nicholas M. Van Horn <nvanhorn@nicholasvanhorn.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.
