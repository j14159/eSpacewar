#!/bin/sh
erl -pa ebin/ deps/cowboy/ebin/ deps/ranch/ebin/ deps/mimetypes/ebin/ deps/mochijson2/ebin/ deps/lager/ebin/ -config app.config -eval "spacewar1:start()."
