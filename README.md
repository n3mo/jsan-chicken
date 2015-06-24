# json2csv
Convert (and subset) json files to csv from the command line

`json2csv` converts a json stream (from file or standard input) to a comma separated values (csv) file. Conversion is fast and does not consume memory, allowing for conversion of arbitrarily large json files. Data fields can optionally be filtered during conversion so that only desired fields are retained in the resulting csv file. 

Because `json2csv` can also write to standard output (in addition to writing to file), it can also serve as a useful exploration tool for quickly exploring json data (e.g., pulling user names across all entries). Output can be piped to other common command line tools (e.g., awk) as in traditional unix-like workflows.

## Usage

**For usage help:**

    json2csv --help

**To convert the file mydata.json to results.csv:**

    json2csv --input=mydata.json --output=results.csv

**To list available data fields in mydata.json:**

    json2csv --input=mydata.json --list

**To convert AND only keep the data fields "text" and "created_on" while writing to standard output:**

    json2csv --input=mydata.json --keep text created_on

**You can also remove named data fields. The following will keep all fields EXCEPT "email" and "phone":**

    json2csv --input=mydata.json --remove email phone

**Nested JSON data fields can be accessed by using a colon. To keep the fields user->screen_name and user->bio->address**

    json2csv --input=mydata.json --keep user:screen_name user:bio:address

**Use pipes to use the output of other commands as input:**

    cat mydata.json | json2csv

**Combine and get creative. Let's list all users whose screen names start with "M":**

    cat mydata.json | json2csv -k user:screen_name | grep '^"m'

**Same as above, but dump the results into a file called names.txt**

    cat mydata.json | json2csv -k user:screen_name | grep '^"m' > names.txt

**The delimeter can optionally be set (default = ","):**

    json2csv --delimeter=','

## Installation

[Self-contained binary packages](http://www.nicholasvanhorn.com/software/) have been pre-built for x86_64 Linux systems. These binaries were built on Arch Linux and should work on similar systems (it has also been tested to work on Ubuntu 12.04). For other systems you will need to build from source (see **Manual Build**).

- Download the [latest version x.x.x](http://www.nicholasvanhorn.com/software/) in zip or tar.gz format.
- Unpack it in a destination folder of your choice, replacing x.x.x with an appropriate version number

```sh
unzip json2csv-x.x.x-linux-x86_64.zip
cd json2csv
```
or

```sh
tar -xzf json2csv-x.x.x-linux-x86_64.tar.gz
cd json2csv
```

- An executable named json2csv is located in the newly created folder "json2csv". You can run this file directly with `./json2csv`
- Better yet, make a symbolic link to the executable somewhere on your path. Assuming ~/bin/ is on your path:

```sh
ln -s ~/path/to/json2csv ~/bin/json2csv
```

Now you can call `json2csv` from within any directory.

### Manual Build

#### Prerequisites
To build json2csv from source, you will need [Chicken Scheme](http://www.call-cc.org/) installed on your system. This software was developed and tested on Chicken 4.9.0. Your mileage may vary on other versions. 

Additionally, you will need the following [Eggs](http://wiki.call-cc.org/eggs) installed:
* medea
* args

json2csv also uses the following Units (which are included by default):
* file
* srfi-1
* srfi-13

#### Interpreted
Installation is simple once Chicken is properly installed:

1. Clone the git repository into a directory of your choice
2. Ensure that the file json2csv.scm is executable (`chmod +x json2csv.scm`)

#### Compiled
For better performance, you should consider compiling the script to a binary executable. To do so, first follow the installation steps above, then compile the script with

```sh
csc json2csv.scm -o json2csv
```

The resulting executable "json2csv" should be placed somewhere on your PATH.

## License

Copyright (C) 2015 Nicholas M. Van Horn

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
