eSpacewar
=========

Multiplayer Spacewar over websockets built on Erlang with Cowboy.

# What You Need

* [Erlang](http://erlang.org)
* [Rebar](https://github.com/basho/rebar)

# Run it
Before you try running it for the first time:

	$ rebar get-deps
	$ rebar compile

And launch Erlang with the correct ebin folders available:

	$ erl -pa ebin/ deps/cowboy/ebin/ deps/ranch/ebin/ deps/mimetypes/ebin/ deps/mochijson2/ebin/

And in the Erlang shell:

	1> spacewar1:start().

# Play It

Go to the following link (assuming you've run it locally):

[http://localhost:8080/sw/sw.html](http://localhost:8080/sw/sw.html)