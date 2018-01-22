# eidos

Machine reading system for World Modelers

The RAPShell (RAP = Representative Agricultural Pathway) is an interactive shell
for testing the output of Eidos. To run it, do

```
./shell
```

To run the webapp version of RAPShell, do:

```
sbt webapp/run
```

Note: The default size of the memory allocation pool for the JVM is 1/4 of your
physical memory, but eidos may require more RAM than that. You can increase the
memory allocation by specifying it in the `.sbtopts` file. The flag enclosed in
the quotes below allocates 6 GB, which should be sufficient.

```
echo "-J-Xmx6g" >> .sbtopts
```

