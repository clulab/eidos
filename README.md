# eidos

Machine reading system for World Modelers

The RAPShell (RAP = Representative Agricultural Pathway) is an interactive shell
for testing the output of Eidos. To run it, do

```
./shell
```

To run the webapp version of RAPShell locally, do:

```
sbt webapp/run
```

and then navigate to `localhost:9000` in a web browser.

Note: The default size of the memory allocation pool for the JVM is 1/4 of your
physical memory, but eidos may require more RAM than that.  For some operating
systems (apparently not Windows), you can increase the
memory allocation by specifying it in the `.sbtopts` file in the `eidos`
directory (the one in which this README resides): 

```
echo "-J-Xmx6g" >> .sbtopts
```

The flag enclosed in the quotes allocates 6 GB, which should be sufficient.

For Windows, it may be necessary to set an environment variable that influences how
much memory is allowed for Java in general.  On the traditional Command Prompt use

```
set JAVA_OPTS=-Xms6g
```

The corresponding command for PowerShell is

```
$env:JAVA_OPTS = "-Xms6g"
```
