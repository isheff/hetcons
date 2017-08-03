# hetcons
A Heterogeneous Consensus based on Paxos

## Build Instructions
First, you have to build the Thrift generated source: In the directory `src/Thrift/`, run:

```
./flatten_const.py
```

Then, build everything else.
If this is your first time building, you'll have to run:
```
stack setup
```
Thereafter, run:

```
stack build
```
To run all the unit tests, run
```
stack test hetcons
```

## Code Layout
### Where's the Protocol?
All message receipts trigger a `Hetcons_Transaction` (defined in `Receive_Message`), which is an atomic operation on some state (defined in `Hetcons_State`). 
This will produce a set of messages to be sent, and update the state (e.g. set of messages received thus far).
For each type of server, and each type of message, what to do upon message receipt is defined in `Receive`, so that's where the actual protocol is principally laid out. 

### Thrift?
We use Thrift, a cross-language messaging API language.
In principal, this means other implementations in other languages can all work together.

Our api is defined in src/Thrift/hetcons.thrift.
The Thrift generated source code lives in the src/Thrift directory, and we can't control its organization.
However, the `Hetcons` module (src/Hetcons) contains all the hand-written source.

### Message Types
In principal, we create the "Recursive" versions of each of the input types (defined in `Signed_Message`), which contain, instead of `Signed_Message` objects, actually parsed (and Recursive) versions of messages contained within them. 
Each message defines now it is parsed with the `parse` function, an element of the `Parsable` class, from the `Signed_Message` sub-module.
Each message's instantiation of `parse` and other class functions are defined in its `Instances_XX` sub-module.
1b and 2a share an `Instances_XX` sub-module, since they are so inter-dependent.

A `Verified` type (defined in `Signed_Message`) is a parsed message which can only have been produced by verifying a signed message.
You can extract the `original` and `signed` versions of the `Verified` message.
As a result, most functions demand the `Verified` Recursive version of most messages, as it guarantees at a type level that the messages were signed, parsed, and meet all requirements set forth in their parsing functions.

### Exceptions
Thrift allows us to throw exceptions into the IO Monad, and Thrift Exceptions are transported back over the wire.
In order to more specifically characterize what's an Exception we wrote and what's not, we created the `Hetcons_Exception` type, which wraps all the Exceptions for this project.
The Result is that many of our functions require `MonadError Hetcons_Exception`, meaning a monad in which they can throw a `Hetcons_Exception`.


### Memoizing
We memoize verifying stuff and calculating quorums into a Concurrent HashMap.
This means our `Hetcons_Transaction` monad has to keep those HashMaps in state, and calls to verify and calculate quorums are fundamentally monadic.
This makes quite a difference in terms of speed.

