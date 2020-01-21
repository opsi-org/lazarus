# opsi doc generator
opsi doc generator generates Asciidoc files for Python or Opsiscript sourcecodes.
It converts Python docstrings or Opsiscript comments to Asciidoc.
And also converts asciidoc to html and shows the created html file in the default viewer.


# Getting started with opsi doc generator:

## Prerequisites
Lazarus IDE, asciidoctor

## Getting Set Up
1. Clone this repository. `git clone git@gitlab.uib.local:uib/lazarus.git`

2. Checkout the branch jeena. `git checkout jeena`

3. Build the project.

### Build instructions for GUI version
1. Run Lazarus and click on `Project->Open Project`.  Select
`opsi_doc_generator_gui.lpi` from the `/lazarus/helper/opsi-doc-generator` folder.

2. Click on `Run->Build` or press <kbd>SHIFT+F9</kbd>.

3. Double click on `opsi_doc_generator_gui.exe` to run it.


### Build instructions for Commandline version
1. Run Lazarus and click on `Project->Open Project`.  Select
`opsi_doc_generator.lpi` from the `/lazarus/helper/opsi-doc-generator` folder.

2. Click on `Run->Build` or press <kbd>SHIFT+F9</kbd>.

3. Run `opsi_doc_generator`

```bash
$ cd lazarus/helper/opsi-doc-generator/
$ ./opsi_doc_generator -h
```

Usage:

```bash
./opsi_doc_generator [Options] [filename]    	  Convert python or opsiscript source files to asciidoc.

Options:
	-h , --help                               Displays this message.
	-s , --os=                                Convert opsiscript source file to asciidoc and save it to the output file.
	-p , --py=                                Convert python source file to asciidoc and save it to the output file.
	-o , --out=                               Save asciidoc output to the specified file.
	-b , --build                              Build output in HTML.
	-v , --view                               Build output in HTML and view it in default viewer.
```

Examples:
##### Opsiscript:

Convert Opsiscript source code to asciidoc:

```bash
$ ./opsi_doc_generator -s /temp/xyz.opsiscript -s /temp/abc.opsiscript -o /temp/opsi.asciidoc
$ ./opsi_doc_generator --os=/temp/xyz.opsiscript --os=/temp/abc.opsiscript --out=/temp/opsi.asciidoc
```

Convert Opsiscript source code to asciidoc and build HTML:

```bash
$ ./opsi_doc_generator -s /temp/xyz.opsiscript -s /temp/abc.opsiscript -o /temp/opsi.asciidoc -b
$ ./opsi_doc_generator --os=/temp/xyz.opsiscript --os=/temp/abc.opsiscript --out=/temp/opsi.asciidoc --build
```

Convert Opsiscript source code to asciidoc, build it in HTML and view it in default viewer:

```bash
$ ./opsi_doc_generator -s /temp/xyz.opsiscript -s /temp/abc.opsiscript -o /temp/opsi.asciidoc -v
$ ./opsi_doc_generator --os=/temp/xyz.opsiscript --os=/temp/abc.opsiscript --out=/temp/opsi.asciidoc --view
```
##### Python:

Convert Python source code to asciidoc:

```bash
$ ./opsi_doc_generator -p /temp/xyz.py -p /temp/abc.py -o /temp/py.asciidoc
$ ./opsi_doc_generator --py=/temp/xyz.py --py=/temp/abc.py --out=/temp/py.asciidoc
```

Convert Python source code to asciidoc and build HTML:

```bash
$ ./opsi_doc_generator -p /temp/xyz.py -p /temp/abc.py -o /temp/py.asciidoc -b
$ ./opsi_doc_generator --py=/temp/xyz.py --py=/temp/abc.py --out=/temp/py.asciidoc --build
```

Convert Python source code to asciidoc, build it in HTML and view it in default viewer:

```bash
$ ./opsi_doc_generator -p /temp/xyz.py -p /temp/abc.py -o /temp/py.asciidoc -v
$ ./opsi_doc_generator --py=/temp/xyz.py --py=/temp/abc.py --out=/temp/py.asciidoc --view
```
