# tcp_chat

This is a set of sample TCP Chat implementations for a Distributed Systems class, Computer Engineering MSc @ UMinho.

# Async

Set of implementations using callback style programming

## Echo

Simple echo chat using Async Java Callbacks.

## Multi

Multiclient implementation of the echo server. Workers are kept inside a hub, which is traversed each time we want to send a new message.

## Ring

Similar to the multiclient implementation, but instead of a shared worker hub, the workers are organized in a ring and the messages are passed as a token.

# Future

Set of implementations using CompletableFuture and [@spullara's Concurrent Future](https://github.com/spullara/java-future-jdk8)

Echo and Ring implementations similar to the ones in Async.
