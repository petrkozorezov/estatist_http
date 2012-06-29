The estatist_http application
=============================

Copyright (c) 2012 Petr Kozorezov


__Authors:__ Petr Kozorezov ([`petr.kozorezov@gmail.com`](mailto:petr.kozorezov@gmail.com)).


Overview
--------

The HTTP interface for [Estatist](https://github.com/petrkozorezov/estatist).


Build
-----

You can build with `make`, run dialyzer with `make dialyzer`.


Usage
-----

Configure [estatist](https://github.com/petrkozorezov/estatist#configuration-example) application.
Configure host and port:

      {estatist_http, [
        {host, "127.0.0.1"},
        {port, 8000}
      ]}

then start:

    estatist_http:start().

Get http://127.0.0.1:8000/.

Also you can specify what to get like [estatist:select/1](https://github.com/petrkozorezov/estatist/blob/master/doc/estatist.md#select-1).
http://127.0.0.1:8000/?names=all&types=all&params=all
