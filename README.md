eSpacewar
=========

Multiplayer Spacewar over websockets built on Erlang with Cowboy.  See the [blog post](http://noisycode.com/blog/2013/03/09/espacewar-open-sourced/)

# What You Need

* [Erlang](http://erlang.org) - I'be been using R15B03
* [Rebar](https://github.com/basho/rebar)

# Run it
Before you try running it for the first time:

	$ rebar get-deps
	$ rebar compile

And launch it with the start script:

	[jeremy@mbp-0x90:~/code/erlang/eSpacewar(master)]$ ./start.sh

Or from the Erlang shell with correct binary folders on the path:

	1> espacewar:start().

# Play It

Go to the following link (assuming you've run it locally):

[http://localhost:8080/sw.html](http://localhost:8080/sw.html)