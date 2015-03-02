# json2csv
Convert (and subset) json files to csv from the command line

`json2csv` converts a json stream (from file) to a comma separated values (csv) file. Conversion is fast and does not consume memory, allowing for conversion of arbitrarily large json files. Data fields can optionally be filtered during conversion so that only desired fields are retained in the resulting csv file. For example, by keeping only the most relevant data fields, `json2csv` reduced a 5.6GB file of Twitter data in json format to a 158MB csv file.

## Usage

Assuming conversion of a file called `mydata.json`, the following commands will create a csv file called `mydata.csv`.

**For usage help:**

    json2csv --help

**To convert the file mydata.json to mydata.csv:**

    json2csv mydata.json

**To list available data fields in mydata.json:**

    json2csv mydata.json --list

**To convert AND only keep the data fields "text" and "created_on":**

    json2csv mydata.json --keep text created_on

**To remove data fields, preface field names with a "-". The following will keep all fields EXCEPT "user" and "address":**

    json2csv mydata.json --remove user address

## Installation

### Install json2csv

#### Prerequisites
Currently, this software is not bundled as a self-contained package. Thus, you will need [Chicken Scheme](http://www.call-cc.org/) installed on your system. This software was developed and tested on Chicken 4.9.0. Your mileage may vary on other versions. 

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
