# tcp_chat

This is a set of TCP Chat implementations for a Distributed Systems class, in my Computer Engineering MSc @ UMinho.

## Echo

Simple echo chat using Async Java Callbacks.

## Multi

Multiclient implementation of the echo server.

## Ring

Similar to the multiclient implementation, but instead of a shared worker hub, each worker knows who are the previous and next works, organized in a ring.
