## Info
This is a project I made as my 10th grade assembly project.

It is an interpreted programming language focusing on graphics and
readability.

## Build & run
To build, you must have DOSBox installed and have the base
directory mounted.

Through DOSBox, enter the drive you mounted the directory
into and
run the command `build`.
That'll build an executable called `main` that you can run.

To run a file, run `main <path-to-file>`.

In the `demos` directory there are example codes that you
can run.
To run the Hello World example, run `main demos/hello.txt`.

## Important information
The interpreter uses heap memory, but DOS does not give
enough of it normally, so before you're starting to execute
files you must run `loadfix` a couple of times to free
memory.

If you get an allocation error, try running `loadfix` again
a couple more times, and if you cannot run `loadfix` anymore
and there is still an allocation error, you've reached the
limit of how much code you can execute.

**Lines must be terminated by CRLF and not just LF.**
Most editors allow you to switch your line endings from LF
to CRLF so that shouldn't be a big problem.
