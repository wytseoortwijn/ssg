Compiling the program:
erl -make


Running the program can be done by using the following command:
erl -smp auto -pa ebin

The following parameters are used:
- The 'smp auto' parameter creates a number of schedulers equal to the number of processor cores
- The 'pa ebin' parameter tells the runtime to include all files located in the /ebin folder