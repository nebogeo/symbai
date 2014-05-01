Symbai/Starwisp
===============

This is a data syncing application for remote use with a raspberry pi and
android devices.

Development notes
=================

Data model
----------

The sqlite database is using an entity-attribute-value system so data types can
be added dynamically, with only an android application update required.

Each item in the database is an entity, which contains a version number,
a local id (the primary key) which is defined on the local database only
and a unique-id which is common to all databases across the
network. Entities are split into different types, and are associated
with attributes which make up their contents. The attributes of the
entity are stored in key-type-value attributes (ktv). Entities are
comprised of multiple ktv attributes.

When an entity is edited, it's flagged as dirty - along with the
individual attributes which are changed.

The entity dirty flag is used to determine which entities need to be
sent to the server when syncing, and the attribute's dirty flags
indicate which individual items need sending.

The server increments the entity version number on reciept of a changed
values, which is used to determine which entities are needed updating on
other clients.

In this way, data changes are merged and propagated through the system.


Syncing algorithm
-----------------

Sending data from client to server is all about dirty flags:

| Android (client)              |   Raspberry Pi (server       |
|-------------------------------|------------------------------|
| Data edited or created - cause entities and attributes to be flagged as 'dirty' |      |
| For each dirty entity, send all dirty attributes | Return message to confirm reciept |
|                               | Overwrite all dirty attributes, increment version number |

Receiving changes from the server to the client is all about version numbers:

| Android (client)              |   Raspberry Pi (server       |
|-------------------------------|------------------------------|
| Request all entity version numbers | Send versions...      |
| Check each local entity, request updates | Send complete entity, with all attributes |
| Overwrite/add new entities    |  |



Code dependancies
-----------------

Common code is stored in /eavdb

| Notes |   |  |
| ----|----------|----------------|
| Low level (data types and platform stuff) | ktv |   ktv-list      |   sql/racket-fix        |
| Entity read/write |   entity-values  | entity-insert | entity-get |
| Updating entities |          entity-update |
| Higher level features |   entity-sync    |        entity-filter |
| Top level stuff |       eavdb |
