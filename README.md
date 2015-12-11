# Room Chat

A simple text-based room chat application. Academic project for a Distributed Systems MSc class at University of Minho.

## Project Summary

Implement a chat server which allows users to authenticate, chose room and send text lines to other users in the same room. The service should be scalable in the number of connected users, and allow subscription of notable events.

## Requirements

The service should support the following features:

* User registration, given name and password; registration removal; a user should be authenticated to use the service;
* Choice of room (from existing ones), to which text messages will be sent;
* Sending of private messages to other connected users;
* Have a simple text-based protocol to allow simple chat clients, being usable by telnet;
* Have an API for management and description: e.g., room creation/removal, list of rooms, list of users in room;
* Have a notification API to allow subscribing to relevant events: room creation/removal, user joi- ning/leaving room;

### Clients

There should be three clients: a chat notificationClient for end users, which also allows listing rooms, chosing room and listing users; it should use a line-oriented text-based protocol (so it should be possible to use telnet as notificationClient, even if less pleasantly); an administration notificationClient to manage (create/remove/list) rooms; a notification console, which allows observing the system by chosing relevant events to subscribe to.

### Server

The server should be written in Java, using relevant paradigms for the several components, namely actors and message-orientation, through Quasar and ZeroMQ.
