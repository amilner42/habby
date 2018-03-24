# Habby

A basic habit tracker app, created mostly to test out using
[Lacinia](http://lacinia.readthedocs.io/en/latest/).

### Stack

DB:
 - MongoDB

API:
 - Clojure
 - Lacinia

Web Client:
 - Elm


### Dev Instructions

You'll need a few local dependencies to get going, and aside from these the install scripts will get everything else
you need.

##### Local Dependencies
    - MongoDB (version: 3.2.9)
    - npm (3.10.3)
    - lein (2.7.1 running on java 1.8.0_101)

You don't need these exact versions, but having [major/minor version](https://semver.org/) correct will avoid possible
bugs.


##### Install Instructions

I like to run things in 2 terminals to keep the output cleaner.

Terminal 1:
```bash
cd web-client;
npm install; # Handles everything for you, including installing elm globally.

# Now to actually launch the frontend
npm start;
```

Terminal 2:
```bash
cd api;
lein deps; # Get's everything you need from project.clj.

# Now to actually launch the backend, first start the repl.
lein repl;
# Inside the repl in the user namespace run `(start)`, if you're not in the user namespace originally then run
# (ns user) to switch to the user namespace.
possbily-something-else => (ns user)
user => (start)
# If you have changed code within /habby/api since starting the repl, use `refresh` to start using the new code
# without needing to restart the repl
user => (refresh)
```

Great, you're good to develop now!

Keep in mind that the frontend will be running on `localhost:8080`.
The backend will be serving up the API on `localhost:8888/graphql`, which you can actually explore with graphiql by
visiting `localhost:8888`, a must-use feature while developing on the API.

### More Docs...

TODO
