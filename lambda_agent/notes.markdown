# Deployment
I haven't gone through the trouble to build it into an application yet, but eventually there will be a rekay.app erlang file.

Build the stuff:

 erl -make Emakefile

## Dependencies

Various Python (number crunching) dependencies are anticipated to be used, either for optimization routines, or prediction.

### Erlport
Erlport must be added to ERL\_LIB, or alternatively, placed in Erlang's lib directory.

1. cd to the lib directory for erlang, (code:lib\_dir/0 in the erl shell).
2. git clone https://github.com/hdima/erlport.git
3. cd erlport && make

