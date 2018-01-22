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
physical memory, but eidos may require more RAM than that. You can increase the
memory allocation by specifying it in the `.sbtopts` file: 

```
echo "-J-Xmx6g" >> .sbtopts
```

The flag enclosed in the quotes allocates 6 GB, which should be sufficient.
