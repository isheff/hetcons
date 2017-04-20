# hetcons
A Heterogeneous Consensus based on Paxos

## Build Instructions
First, you have to build the Thrift generated source: In the directory `src/Thrift/`, run:

```
./flatten_const.py
```

Then, build everything else:

```
stack build
```
